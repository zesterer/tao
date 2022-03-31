use super::*;
use std::collections::VecDeque;

pub type InferMeta = (Span, TyVar);
pub type InferNode<T> = Node<T, InferMeta>;

#[derive(Clone, Debug, PartialEq)]
pub enum TyInfo {
    Ref(TyVar),
    Error(ErrorReason),
    Unknown(Option<Span>), // With optional instantiation origin
    Prim(ty::Prim),
    List(TyVar),
    Tuple(Vec<TyVar>),
    Record(BTreeMap<Ident, TyVar>),
    Func(TyVar, TyVar),
    Data(DataId, Vec<TyVar>),
    Gen(usize, GenScopeId, Span),
    SelfType,
    // An opaque associated type that *cannot* be determined due to lack of information
    Assoc(TyVar, ClassId, SrcNode<Ident>),
    Effect(EffectId, Vec<TyVar>, TyVar),
}

#[derive(Clone, Default, Debug)]
pub struct EqInfo {
    pub at: Option<Span>,
    pub reason: Option<String>,
}

impl From<Span> for EqInfo {
    fn from(span: Span) -> Self {
        Self { at: Some(span), reason: None }
    }
}

impl EqInfo {
    pub fn new(at: Span, reason: String) -> Self {
        Self { at: Some(at), reason: Some(reason) }
    }
}

#[derive(Debug)]
pub enum InferError {
    CannotCoerce(TyVar, TyVar, Option<(TyVar, TyVar)>, EqInfo),
    CannotInfer(TyVar, Option<Span>), // With optional instantiation origin
    // Type, recursive element
    Recursive(TyVar, TyVar),
    NoSuchItem(TyVar, Span, SrcNode<Ident>),
    NoSuchField(TyVar, Span, SrcNode<Ident>),
    InvalidUnaryOp(SrcNode<ast::UnaryOp>, TyVar),
    InvalidBinaryOp(SrcNode<ast::BinaryOp>, TyVar, TyVar),
    TypeDoesNotFulfil(ClassId, TyVar, Span, Option<Span>),
    RecursiveAlias(AliasId, TyVar, Span),
    PatternNotSupported(TyVar, SrcNode<ast::BinaryOp>, TyVar, Span),
    AmbiguousClassItem(SrcNode<Ident>, Vec<ClassId>),
}

#[derive(Clone, Debug)]
enum Constraint {
    // (record, field_name, field)
    Access(TyVar, SrcNode<Ident>, TyVar),
    Update(TyVar, SrcNode<Ident>, TyVar),
    Binary(SrcNode<ast::BinaryOp>, TyVar, TyVar, TyVar),
    Impl(TyVar, ClassId, Span, Vec<(SrcNode<Ident>, TyVar)>),
    ClassField(TyVar, ClassVar, SrcNode<Ident>, TyVar, Span),
    ClassAssoc(TyVar, ClassVar, SrcNode<Ident>, TyVar, Span),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TyVar(usize);

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct ClassVar(usize);

pub struct Infer<'a> {
    ctx: &'a mut Context,
    gen_scope: Option<GenScopeId>,
    vars: Vec<(Span, TyInfo, Result<(), ()>)>,
    class_vars: Vec<(Span, Option<ClassId>)>,
    constraints: VecDeque<Constraint>,
    errors: Vec<InferError>,
    self_type: Option<TyVar>,
    self_obligations: Vec<ClassId>,
}

impl<'a> Infer<'a> {
    pub fn new(ctx: &'a mut Context, gen_scope: Option<GenScopeId>) -> Self {
        Self {
            ctx,
            gen_scope,
            vars: Vec::new(),
            class_vars: Vec::new(),
            constraints: VecDeque::new(),
            errors: Vec::new(),
            self_type: None,
            self_obligations: Vec::new(),
        }
    }

    pub fn with_self_type(mut self, ty: TyId, span: Span) -> Self {
        self.self_type = Some(self.instantiate_local(ty, span));
        self
    }

    pub fn with_unknown_self(mut self, span: Span, self_obligations: Vec<ClassId>) -> Self {
        self.self_type = Some(self.insert(span, TyInfo::SelfType));
        self.self_obligations = self_obligations;
        self
    }

    pub fn self_type(&self) -> Option<TyVar> { self.self_type }

    pub fn ctx(&self) -> &Context { self.ctx }
    pub fn ctx_mut(&mut self) -> &mut Context { self.ctx }

    pub fn gen_scope(&self) -> Option<GenScopeId> {
        self.gen_scope
    }

    fn iter(&self) -> impl Iterator<Item = (TyVar, TyInfo)> + '_ {
        (0..self.vars.len())
            .map(|i| (TyVar(i), self.vars[i].1.clone()))
    }

    fn follow(&self, ty: TyVar) -> TyVar {
        match &self.vars[ty.0].1 {
            TyInfo::Ref(x) => self.follow(*x),
            _ => ty,
        }
    }

    fn set_info_inner(&mut self, ty: TyVar, info: TyInfo) -> TyVar {
        match self.vars[ty.0].1.clone() {
            TyInfo::Ref(x) => self.set_info_inner(x, info),
            _ => {
                self.vars[ty.0].1 = info;
                ty
            },
        }
    }

    fn set_info(&mut self, ty: TyVar, info: TyInfo) {
        let new_ty = self.set_info_inner(ty, info);
        if self.vars[ty.0].2.is_err() {
            self.vars[new_ty.0].2 = Err(());
        }
    }

    fn set_error(&mut self, ty: TyVar) {
        //self.vars[ty.0].1 = TyInfo::Error(ErrorReason::Unknown);
        if self.vars[ty.0].2.is_ok() {
            self.vars[ty.0].2 = Err(());
            match self.vars[ty.0].1.clone() {
                TyInfo::Ref(x) => return self.set_error(x),
                TyInfo::Error(_)
                | TyInfo::Unknown(_)
                | TyInfo::Prim(_)
                | TyInfo::Gen(..)
                | TyInfo::SelfType => {},
                TyInfo::List(item) => self.set_error(item),
                TyInfo::Tuple(fields) => fields
                    .into_iter()
                    .for_each(|field| self.set_error(field)),
                TyInfo::Record(fields) => fields
                    .into_iter()
                    .for_each(|(_, field)| self.set_error(field)),
                TyInfo::Func(i, o) => {
                    self.set_error(i);
                    self.set_error(o);
                },
                TyInfo::Data(_, args) => args
                    .into_iter()
                    .for_each(|arg| self.set_error(arg)),
                // Type is projected, so error does not propagate backwards
                // TODO: Should it?
                TyInfo::Assoc(_, _, _) => {},
                TyInfo::Effect(_, args, out) => {
                    args
                        .into_iter()
                        .for_each(|arg| self.set_error(arg));
                    self.set_error(out);
                },
            }
        }
    }

    fn is_error(&self, ty: TyVar) -> bool {
        match self.vars[ty.0].1.clone() {
            TyInfo::Ref(x) => self.is_error(x),
            _ => self.vars[ty.0].2.is_err(),
        }
    }

    fn info(&self, ty: TyVar) -> TyInfo {
        self.vars[ty.0].1.clone()
    }

    fn span(&self, ty: TyVar) -> Span {
        self.vars[ty.0].0
    }

    fn follow_info(&self, ty: TyVar) -> TyInfo {
        match &self.vars[ty.0].1 {
            TyInfo::Ref(x) => self.follow_info(*x),
            info => info.clone(),
        }
    }

    pub fn insert(&mut self, span: Span, info: TyInfo) -> TyVar {
        let id = TyVar(self.vars.len());
        let err = if matches!(&info, TyInfo::Error(_)) { Err(()) } else { Ok(()) };
        self.vars.push((span, info, err));
        id
    }

    pub fn instantiate_local(&mut self, ty: TyId, span: Span) -> TyVar {
        let mut gens = self.gen_scope
            .map(|gen_scope| {
                (0..self.ctx.tys.get_gen_scope(gen_scope).len())
                    .map(|idx| {
                        let span = self.ctx.tys.get_gen_scope(gen_scope).get(idx).name.span();
                        self.insert(span, TyInfo::Gen(idx, gen_scope, span))
                    })
                    .collect::<Vec<_>>()
            });
        self.instantiate(ty, span, &|idx, gen_scope, ctx| gens.as_ref().expect("No gen scope")[idx], None)
    }

    pub fn instantiate(&mut self, ty: TyId, span: impl Into<Option<Span>>, f: &impl Fn(usize, GenScopeId, &Context) -> TyVar, self_ty: Option<TyVar>) -> TyVar {
        let span = span.into();
        let info = match self.ctx.tys.get(ty) {
            Ty::Error(reason) => TyInfo::Error(reason),
            Ty::Prim(prim) => TyInfo::Prim(prim),
            Ty::List(item) => TyInfo::List(self.instantiate(item, span, f, self_ty)),
            Ty::Tuple(fields) => TyInfo::Tuple(fields
                .into_iter()
                .map(|field| self.instantiate(field, span, f, self_ty))
                .collect()),
            Ty::Record(fields) => TyInfo::Record(fields
                .into_iter()
                .map(|(name, field)| (name, self.instantiate(field, span, f, self_ty)))
                .collect()),
            Ty::Func(i, o) => TyInfo::Func(self.instantiate(i, span, f, self_ty), self.instantiate(o, span, f, self_ty)),
            Ty::Data(data, params) => TyInfo::Data(data, params
                .into_iter()
                .map(|param| self.instantiate(param, span, f, self_ty))
                .collect()),
            Ty::Gen(index, scope) => TyInfo::Ref(f(index, scope, self.ctx)), // TODO: Check scope is valid for recursive scopes
            Ty::SelfType => TyInfo::Ref(self_ty.expect("Found self type during instantiation but no self type is available to substitute")),
            Ty::Assoc(inner, class_id, assoc) => {
                let span = span.unwrap_or_else(|| self.ctx.tys.get_span(ty));
                let inner = self.instantiate(inner, span, f, self_ty);
                let assoc_ty = self.unknown(span);
                self.make_impl(inner, class_id, span, vec![(assoc, assoc_ty)]);
                TyInfo::Ref(assoc_ty)
            },
            Ty::Effect(eff, params, out) => TyInfo::Effect(eff, params
                .into_iter()
                .map(|param| self.instantiate(param, span, f, self_ty))
                .collect(), self.instantiate(out, span, f, self_ty)),
        };
        self.insert(span.unwrap_or_else(|| self.ctx.tys.get_span(ty)), info)
    }

    pub fn unknown(&mut self, span: Span) -> TyVar {
        self.insert(span, TyInfo::Unknown(None))
    }

    pub fn make_access(&mut self, record: TyVar, field_name: SrcNode<Ident>, field: TyVar) {
        self.constraints.push_back(Constraint::Access(record, field_name, field));
    }

    pub fn make_update(&mut self, record: TyVar, field_name: SrcNode<Ident>, field: TyVar) {
        self.constraints.push_back(Constraint::Update(record, field_name, field));
    }

    pub fn make_binary(&mut self, op: SrcNode<ast::BinaryOp>, a: TyVar, b: TyVar, output: TyVar) {
        self.constraints.push_back(Constraint::Binary(op, a, b, output));
    }

    // `unchecked_assoc` allows unification of type variables with an instance's associated type
    pub fn make_impl(&mut self, ty: TyVar, class: ClassId, span: Span, unchecked_assoc: Vec<(SrcNode<Ident>, TyVar)>) {
        self.constraints.push_back(Constraint::Impl(ty, class, span, unchecked_assoc));
    }

    pub fn make_class_field_known(&mut self, ty: TyVar, field_name: SrcNode<Ident>, class_id: Option<ClassId>, field_ty: TyVar, span: Span) -> ClassVar {
        let class = ClassVar(self.class_vars.len());
        self.class_vars.push((span, class_id));
        self.constraints.push_back(Constraint::ClassField(ty, class, field_name, field_ty, span));
        class
    }

    pub fn make_class_field(&mut self, ty: TyVar, field_name: SrcNode<Ident>, field_ty: TyVar, span: Span) -> ClassVar {
        let class = ClassVar(self.class_vars.len());
        self.class_vars.push((span, None));
        self.constraints.push_back(Constraint::ClassField(ty, class, field_name, field_ty, span));
        class
    }

    pub fn make_class_assoc(&mut self, ty: TyVar, assoc_name: SrcNode<Ident>, assoc_ty: TyVar, span: Span) -> ClassVar {
        let class = ClassVar(self.class_vars.len());
        self.class_vars.push((span, None));
        self.constraints.push_back(Constraint::ClassAssoc(ty, class, assoc_name, assoc_ty, span));
        class
    }

    pub fn emit(&mut self, err: InferError) {
        self.errors.push(err);
    }

    fn occurs_in_inner(&self, x: TyVar, y: TyVar, seen: &mut Vec<TyVar>) -> bool {
        if seen.contains(&y) {
            true
        } else {
            seen.push(y);

            let occurs = match self.info(y) {
                TyInfo::Unknown(_)
                | TyInfo::Error(_)
                | TyInfo::Prim(_)
                | TyInfo::SelfType
                | TyInfo::Gen(_, _, _) => false,
                TyInfo::Ref(y) => x == y || self.occurs_in_inner(x, y, seen),
                TyInfo::List(item) => x == item || self.occurs_in_inner(x, item, seen),
                TyInfo::Func(i, o) => x == i || x == o || self.occurs_in_inner(x, i, seen) || self.occurs_in_inner(x, o, seen),
                TyInfo::Tuple(ys) => ys
                    .into_iter()
                    .any(|y| x == y || self.occurs_in_inner(x, y, seen)),
                TyInfo::Record(ys) => ys
                    .into_iter()
                    .any(|(_, y)| x == y || self.occurs_in_inner(x, y, seen)),
                TyInfo::Data(_, ys) => ys
                    .into_iter()
                    .any(|y| x == y || self.occurs_in_inner(x, y, seen)),
                TyInfo::Assoc(inner, _, _) => x == inner || self.occurs_in_inner(x, inner, seen),
                TyInfo::Effect(_, ys, out) => ys
                    .into_iter()
                    .any(|y| x == y || self.occurs_in_inner(x, y, seen)) || self.occurs_in_inner(x, out, seen) ,
            };

            seen.pop();

            occurs
        }
    }

    // Returns true if `x` occurs in `y`.
    fn occurs_in(&self, x: TyVar, y: TyVar) -> bool {
        self.occurs_in_inner(x, y, &mut Vec::new())
    }

    // Flow the type `x` into the type `y`
    pub fn make_flow(&mut self, x: TyVar, y: TyVar, info: impl Into<EqInfo>) {
        if let Err((a, b)) = self.make_flow_inner(x, y) {
            if !self.is_error(a) && !self.is_error(b) {
                self.set_error(a);
                self.set_error(b);
                self.errors.push(InferError::CannotCoerce(x, y, Some((a, b)), info.into()));
            }
        }
    }

    fn make_flow_inner(&mut self, x: TyVar, y: TyVar) -> Result<(), (TyVar, TyVar)> {
        fn make_flow_many(
            infer: &mut Infer,
            xs: impl IntoIterator<Item = TyVar>,
            ys: impl IntoIterator<Item = TyVar>,
        ) -> Result<(), (TyVar, TyVar)> {
            xs
                .into_iter()
                .zip(ys.into_iter())
                .fold(None, |err, (x, y)| err.or(infer.make_flow_inner(x, y).err()))
                .map(Err).unwrap_or(Ok(()))
        }

        if x == y { return Ok(()) } // If the vars are equal, we have no need to check flow
        match (self.info(x), self.info(y)) {
            // Follow references
            (TyInfo::Ref(x), _) => self.make_flow_inner(x, y),
            (_, TyInfo::Ref(y)) => self.make_flow_inner(x, y),

            // Unify unknown or erronoeus types
            (TyInfo::Unknown(_), y_info) => if self.occurs_in(x, y) {
                self.errors.push(InferError::Recursive(y, self.follow(x)));
                self.set_info(x, TyInfo::Error(ErrorReason::Recursive));
                Ok(self.set_error(x)) // TODO: Not actually ok
            } else {
                Ok(self.set_info(x, TyInfo::Ref(y)))
            },
            (x_info, TyInfo::Unknown(_)) => if self.occurs_in(y, x) {
                self.errors.push(InferError::Recursive(x, self.follow(y)));
                self.set_info(y, TyInfo::Error(ErrorReason::Recursive));
                Ok(self.set_error(y)) // TODO: Not actually ok
            } else {
                Ok(self.set_info(y, TyInfo::Ref(x)))
            },

            // Unify errors
            (_, TyInfo::Error(_)) => {
                self.set_error(x);
                Ok(self.set_info(x, TyInfo::Ref(y)))
            },
            (TyInfo::Error(_), _) => {
                self.set_error(y);
                Ok(self.set_info(y, TyInfo::Ref(x)))
            },

            (TyInfo::Prim(x), TyInfo::Prim(y)) if x == y => Ok(()),
            (TyInfo::List(x), TyInfo::List(y)) => self.make_flow_inner(x, y),
            (TyInfo::Tuple(xs), TyInfo::Tuple(ys)) if xs.len() == ys.len() => make_flow_many(self, xs, ys),
            (TyInfo::Record(xs), TyInfo::Record(ys)) if xs.len() == ys.len() && xs
                .keys()
                .all(|x| ys.contains_key(x)) => xs
                    .into_iter()
                    .try_for_each(|(x, x_ty)| self.make_flow_inner(x_ty, ys[&x])),
            (TyInfo::Func(x_i, x_o), TyInfo::Func(y_i, y_o)) => {
                let i_err = self.make_flow_inner(y_i, x_i).err().map(|(a, b)| (b, a)); // Input is contravariant
                let o_err = self.make_flow_inner(x_o, y_o).err();
                i_err.or(o_err).map(Err).unwrap_or(Ok(()))
            },
            (TyInfo::Data(x_data, xs), TyInfo::Data(y_data, ys)) if x_data == y_data &&
                xs.len() == ys.len() /* TODO: Assert this! */ => {
                // TODO: Unnecessarily conservative, variance of data type generics should be determined
                let co_error = make_flow_many(self, xs.iter().copied(), ys.iter().copied()).err();
                let contra_error = make_flow_many(self, ys, xs).err().map(|(a, b)| (b, a));
                co_error.or(contra_error).map(Err).unwrap_or(Ok(()))
            },
            (TyInfo::Gen(a, a_scope, _), TyInfo::Gen(b, b_scope, _)) if a == b && a_scope == b_scope => Ok(()),
            (TyInfo::SelfType, TyInfo::SelfType) => Ok(()),
            (TyInfo::Assoc(x, class_x, assoc_x), TyInfo::Assoc(y, class_y, assoc_y))
                if class_x == class_y && assoc_x == assoc_y => {
                    // associated types are invariant
                    let co_error = self.make_flow_inner(x, y).err();
                    let contra_error = self.make_flow_inner(y, x).err().map(|(a, b)| (b, a));
                    co_error.or(contra_error).map(Err).unwrap_or(Ok(()))
                },
            (TyInfo::Effect(x_eff, xs, x_out), TyInfo::Effect(y_eff, ys, y_out)) if x_eff == y_eff &&
                xs.len() == ys.len() /* TODO: Assert this! */ => {
                // TODO: Unnecessarily conservative, variance of effect generics should be determined
                let co_error = make_flow_many(self, xs.iter().copied(), ys.iter().copied()).err();
                let contra_error = make_flow_many(self, ys, xs).err().map(|(a, b)| (b, a));
                let out_error = self.make_flow_inner(x_out, y_out).err();
                co_error
                    .or(contra_error)
                    .or(out_error)
                    .map(Err).unwrap_or(Ok(()))
            },
            (_, _) => Err((x, y)),
        }
    }

    /// Reinstantiate a type variable, replacing any known generic types with new unknown ones
    // TODO: Is this a good way to resolve the problem of type inference of recursive definitions in the presence of
    // polymorphism?
    pub fn try_reinstantiate(&mut self, span: Span, ty: TyVar) -> TyVar {
        match self.info(ty) {
            TyInfo::Ref(x) => self.try_reinstantiate(span, x),
            TyInfo::Error(reason) => self.insert(self.span(ty), TyInfo::Error(reason)),
            TyInfo::Unknown(_) | TyInfo::Prim(_) => ty,
            TyInfo::List(item) => {
                let item = self.try_reinstantiate(span, item);
                self.insert(self.span(ty), TyInfo::List(item))
            },
            TyInfo::Func(i, o) => {
                let i = self.try_reinstantiate(span, i);
                let o = self.try_reinstantiate(span, o);
                self.insert(self.span(ty), TyInfo::Func(i, o))
            },
            // TODO: Reinstantiate type parameters with fresh type variables, but without creating inference problems
            // TODO: Is this even correct?
            TyInfo::Gen(x, _, origin) => ty,//self.insert(span, TyInfo::Unknown(Some(origin))),
            TyInfo::Tuple(fields) => {
                let fields = fields
                    .into_iter()
                    .map(|field| self.try_reinstantiate(span, field))
                    .collect();
                self.insert(self.span(ty), TyInfo::Tuple(fields))
            },
            TyInfo::Record(fields) => {
                let fields = fields
                    .into_iter()
                    .map(|(name, field)| (name, self.try_reinstantiate(span, field)))
                    .collect();
                self.insert(self.span(ty), TyInfo::Record(fields))
            },
            TyInfo::Data(data, args) => {
                let args = args
                    .into_iter()
                    .map(|arg| self.try_reinstantiate(span, arg))
                    .collect();
                self.insert(self.span(ty), TyInfo::Data(data, args))
            },
            TyInfo::SelfType => todo!(), // ???
            TyInfo::Assoc(inner, class_id, assoc) => {
                let inner = self.try_reinstantiate(span, inner);
                self.insert(self.span(ty), TyInfo::Assoc(inner, class_id, assoc))
            },
            TyInfo::Effect(eff, args, out) => {
                let args = args
                    .into_iter()
                    .map(|arg| self.try_reinstantiate(span, arg))
                    .collect();
                let out = self.try_reinstantiate(span, out);
                self.insert(self.span(ty), TyInfo::Effect(eff, args, out))
            },
        }
    }

    fn resolve_access(&mut self, record: TyVar, field_name: &SrcNode<Ident>, field: TyVar, flow_out: bool) -> Option<bool> {
        match self.follow_info(record) {
            _ if self.is_error(record) => {
                self.set_error(field);
                // Trying to access a field on an error type counts as success because we don't want to emit more
                // errors than necessary.
                Some(true)
            },
            TyInfo::Unknown(_) => None,
            TyInfo::Record(fields) => if let Some(field_ty) = fields.get(&field_name) {
                self.make_flow(*field_ty, field, field_name.span());
                Some(true)
            } else {
                Some(false)
            },
            // Field access through a data type
            TyInfo::Data(data, params) => {
                // TODO: Use `self.ctx.follow_field_access(...)` but work out how to instantiate type parameters
                // throughout the chain.
                let data = self.ctx.datas.get_data(data);
                // Field access on data only works for single-variant, record datatypes
                if let (Some((_, ty)), true) = (data.cons.iter().next(), data.cons.len() == 1) {
                    if let Ty::Record(fields) = self.ctx.tys.get(*ty) {
                        if let Some(field_ty) = fields.get(&field_name) {
                            let field_ty = self.instantiate(*field_ty, self.span(record), &|index, _, _| params[index], None);
                            if flow_out {
                                self.make_flow(field_ty, field, field_name.span());
                            } else {
                                self.make_flow(field, field_ty, field_name.span());
                            }
                            Some(true)
                        } else {
                            Some(false)
                        }
                    } else {
                        Some(false)
                    }
                } else {
                    Some(false)
                }
            },
            _ => Some(false),
        }
    }

    fn resolve(&mut self, c: Constraint) -> Option<Result<(), InferError>> {
        use ast::BinaryOp::*;
        match c {
            Constraint::Access(record, field_name, field) => self.resolve_access(record, &field_name, field, true)
                .map(|success| if success {
                    Ok(())
                } else {
                    self.set_error(field);
                    Err(InferError::NoSuchField(record, self.span(record), field_name.clone()))
                }),
            Constraint::Update(record, field_name, field) => self.resolve_access(record, &field_name, field, false)
                .map(|success| if success {
                    Ok(())
                } else {
                    self.set_error(field);
                    Err(InferError::NoSuchField(record, self.span(record), field_name.clone()))
                }),
            Constraint::Binary(op, a, b, output) => match (&*op, self.follow_info(a), self.follow_info(b)) {
                (_, _, TyInfo::Error(reason)) => Some(Ok(TyInfo::Error(reason))),
                (_, TyInfo::Error(reason), _) => Some(Ok(TyInfo::Error(reason))),
                (_, _, TyInfo::Unknown(_)) => None,
                (_, TyInfo::Unknown(_), _) => None,
                (Join, TyInfo::List(a), TyInfo::List(b)) => {
                    let output_item = self.unknown(self.span(output));
                    self.make_flow(a, output_item, EqInfo::new(op.span(), "The types of joined lists must be equal".to_string()));
                    self.make_flow(b, output_item, EqInfo::new(op.span(), "The types of joined lists must be equal".to_string()));
                    Some(Ok(TyInfo::List(output_item)))
                },
                (_, TyInfo::Prim(prim_a), TyInfo::Prim(prim_b)) => {
                    use ty::Prim::*;
                    lazy_static::lazy_static! {
                        static ref PRIM_BINARY_IMPLS: HashMap<(ast::BinaryOp, ty::Prim, ty::Prim), ty::Prim> = [
                            // Bool
                            ((Eq, Bool, Bool), Bool),
                            ((NotEq, Bool, Bool), Bool),
                            ((And, Bool, Bool), Bool),
                            ((Or, Bool, Bool), Bool),
                            ((Xor, Bool, Bool), Bool),

                            // Nat
                            ((Add, Nat, Nat), Nat),
                            ((Sub, Nat, Nat), Int),
                            ((Mul, Nat, Nat), Nat),
                            ((Rem, Nat, Nat), Nat),
                            ((Div, Nat, Nat), Real),
                            ((Eq, Nat, Nat), Bool),
                            ((NotEq, Nat, Nat), Bool),
                            ((Less, Nat, Nat), Bool),
                            ((LessEq, Nat, Nat), Bool),
                            ((More, Nat, Nat), Bool),
                            ((MoreEq, Nat, Nat), Bool),

                            // Int
                            ((Add, Int, Int), Int),
                            ((Sub, Int, Int), Int),
                            ((Mul, Int, Int), Int),
                            ((Div, Int, Int), Real),
                            ((Eq, Int, Int), Bool),
                            ((NotEq, Int, Int), Bool),
                            ((Less, Int, Int), Bool),
                            ((LessEq, Int, Int), Bool),
                            ((More, Int, Int), Bool),
                            ((MoreEq, Int, Int), Bool),

                            // Char
                            ((Eq, Char, Char), Bool),
                            ((NotEq, Char, Char), Bool),

                            // TODO: Others
                        ]
                            .into_iter()
                            .collect();
                    }

                    if let Some(out) = PRIM_BINARY_IMPLS.get(&(*op, prim_a, prim_b)) {
                        Some(Ok(TyInfo::Prim(*out)))
                    } else {
                        self.set_error(output);
                        Some(Err(InferError::InvalidBinaryOp(op.clone(), a, b)))
                    }
                },
                _ => {
                    self.set_error(output);
                    Some(Err(InferError::InvalidBinaryOp(op.clone(), a, b)))
                },
            }
                .map(|info| info.map(|info| {
                    // TODO: Use correct span
                    let result_ty = self.insert(self.span(output), info);
                    self.make_flow(result_ty, output, self.span(output));
                })),
            Constraint::Impl(ty, obligation, span, unchecked_assoc) => self.resolve_obligation(ty, obligation, span).map(|res| match res {
                    Ok(member) => {
                        for (assoc, assoc_ty) in unchecked_assoc {
                            match member {
                                Ok(member) => {
                                    let member = self.ctx.classes.get_member(member);

                                    let mut links = HashMap::new();
                                    self.derive_links(member.member, ty, &mut |gen_idx, var| { links.insert(gen_idx, var); });

                                    if let Some(member_assoc_ty) = member.assoc_ty(*assoc) {
                                        let assoc_ty_inst = self.instantiate(member_assoc_ty, Some(span), &|idx, gen_scope, ctx| links[&idx], Some(ty));
                                        // TODO: Check ordering for soundness
                                        self.make_flow(assoc_ty_inst, assoc_ty, span);
                                    }
                                },
                                Err(true) => {
                                    // Errors propagate through projected associated types
                                    self.set_error(assoc_ty);
                                },
                                Err(false) => {
                                    let assoc_info = self.insert(span, TyInfo::Assoc(ty, obligation, assoc.clone()));
                                    // TODO: Check ordering for soundness
                                    self.make_flow(assoc_info, assoc_ty, span);
                                },
                            }
                        }
                        Ok(())
                    },
                    Err(err) => {
                        // The obligation produced an error, so propagate the error to associated type variables
                        for (_, assoc_ty) in unchecked_assoc {
                            self.set_error(assoc_ty);
                        }
                        Err(err)
                    },
                }),
            Constraint::ClassField(ty, class, field, field_ty, span) => self.try_resolve_class_from_field(ty, class, field.clone(), field_ty, span),
            Constraint::ClassAssoc(ty, class, assoc, assoc_ty, span) => self.try_resolve_class_from_assoc(ty, class, assoc.clone(), assoc_ty, span),
        }
    }

    fn try_resolve_class_from_assoc(&mut self, ty: TyVar, class_var: ClassVar, assoc: SrcNode<Ident>, assoc_ty: TyVar, span: Span) -> Option<Result<(), InferError>> {
        let possible_classes = if let Some(class) = self.class_vars[class_var.0].1 {
            std::iter::once(class).collect()
        } else {
            let Some(candidates) = self.find_class_candidates_from_item(ty, assoc.clone(), assoc_ty, true)
                else { return Some(Ok(())) }; // Resolving an error type always succeeds;
            candidates
        };
        match possible_classes.len() {
            0 => {
                self.set_error(assoc_ty);
                Some(Err(InferError::NoSuchItem(ty, span, assoc)))
            },
            1 => {
                let class_id = possible_classes.into_iter().next().unwrap();
                self.class_vars[class_var.0].1 = Some(class_id); // Can't fail

                // Require an implementation to exist
                self.make_impl(ty, class_id, span, vec![
                    (assoc, assoc_ty),
                ]);

                Some(Ok(()))
            },
            _ => {
                self.set_error(assoc_ty);
                // TODO: Return `None` here instead, wait for more inference info
                Some(Err(InferError::AmbiguousClassItem(assoc, possible_classes.into_iter().collect())))
            },
        }
    }

    fn try_resolve_class_from_field(&mut self, ty: TyVar, class_var: ClassVar, field: SrcNode<Ident>, field_ty: TyVar, span: Span) -> Option<Result<(), InferError>> {
        let possible_classes = if let Some(class) = self.class_vars[class_var.0].1 {
            std::iter::once(class).collect()
        } else {
            let Some(candidates) = self.find_class_candidates_from_item(ty, field.clone(), field_ty, false)
                else { return Some(Ok(())) }; // Resolving an error type always succeeds;
            candidates
        };
        match possible_classes.len() {
            0 => {
                self.set_error(field_ty);
                Some(Err(InferError::NoSuchItem(ty, span, field)))
            },
            1 => {
                let class_id = possible_classes.into_iter().next().unwrap();
                self.class_vars[class_var.0].1 = Some(class_id); // Can't fail
                self.make_impl(ty, class_id, span, Vec::new());
                let field_ty_id = **self.ctx.classes
                    .get(class_id)
                    .field(*field)
                    .unwrap();
                let inst_field_ty = self.instantiate(field_ty_id, field.span(), &|_, _, _| panic!("Tried to substitute generic type for non-generic class"), Some(ty));
                // TODO: Check soundness of flow relationship
                self.make_flow(inst_field_ty, field_ty, field.span());
                Some(Ok(()))
            },
            _ => {
                self.set_error(field_ty);
                Some(Err(InferError::AmbiguousClassItem(field, possible_classes.into_iter().collect())))
            },
        }
    }

    fn implied_obligations_for_gen(&self, gen_scope: GenScopeId, idx: usize) -> HashSet<ClassId> {
        let mut implied = HashSet::default();
        for obl in self.ctx.tys
            .get_gen_scope(gen_scope)
            .get(idx)
            .obligations()
        {
            match &**obl {
                Obligation::MemberOf(class) => self.walk_implied_obligations(&mut implied, *class),
            }
        }
        implied
    }

    fn find_class_candidates_from_item(&mut self, ty: TyVar, item: SrcNode<Ident>, item_ty: TyVar, assoc_ty: bool) -> Option<HashSet<ClassId>> {
        let implied_candidates = match self.follow_info(ty) {
            TyInfo::Error(_) => {
                self.set_error(ty);
                self.set_error(item_ty);
                return None
            }
            TyInfo::Unknown(_) => return None, // We don't know what the type is yet, so how can we possibly determine what classes it is a member of?
            TyInfo::Gen(gen_idx, gen_scope, _) => {
                return Some(self
                    .implied_obligations_for_gen(gen_scope, gen_idx)
                    .into_iter()
                    // Filter by class obligations that contain the given field
                    .filter(|class_id| if assoc_ty {
                        self.ctx.classes.get(*class_id).assoc_ty(*item).is_some()
                    } else {
                        self.ctx.classes.get(*class_id).field(*item).is_some()
                    })
                    .collect())
            },
            TyInfo::SelfType => {
                let mut implied = HashSet::default();
                for obl in &self.self_obligations {
                    self.walk_implied_obligations(&mut implied, *obl);
                }
                implied
                    .into_iter()
                    .filter(|class_id| if assoc_ty {
                        self.ctx.classes.get(*class_id).assoc_ty(*item).is_some()
                    } else {
                        self.ctx.classes.get(*class_id).field(*item).is_some()
                    })
                    .collect()
            },
            _ => Vec::new(),
        };

        let external_candidates = self.ctx.classes
            .iter()
            // Filter by classes that contain the given field
            .filter(|(_, class)| if assoc_ty {
                class.assoc_ty(*item).is_some()
            } else {
                class.field(*item).is_some()
            })
            // Filter further by classes that have members that cover our type
            .filter(|(class_id, _)| self.ctx.classes
                .members_of(*class_id)
                .find(|(_, member)| Self::covers_var(self, ty, member.member))
                .is_some())
            .map(|(class_id, _)| class_id);

        Some(external_candidates
            .chain(implied_candidates)
            .collect())
    }

    // Returns true if ty covers var (i.e: var is a structural subset of ty)
    fn covers_var(&self, var: TyVar, ty: TyId) -> bool {
        match (self.follow_info(var), self.ctx.tys.get(ty)) {
            (_, Ty::Gen(_, _)) => true, // Blanket impls match everything
            (TyInfo::Prim(x), Ty::Prim(y)) if x == y => true,
            (TyInfo::List(x), Ty::List(y)) => self.covers_var(x, y),
            (TyInfo::Tuple(xs), Ty::Tuple(ys)) if xs.len() == ys.len() => xs
                .into_iter()
                .zip(ys.into_iter())
                .all(|(x, y)| self.covers_var(x, y)),
            (TyInfo::Record(xs), Ty::Record(ys)) if xs.len() == ys.len() => xs
                .into_iter()
                .zip(ys.into_iter())
                .all(|((_, x), (_, y))| self.covers_var(x, y)),
            (TyInfo::Func(x_i, x_o), Ty::Func(y_i, y_o)) => {
                self.covers_var(x_i, y_i) && self.covers_var(x_o, y_o)
            },
            (TyInfo::Data(x, xs), Ty::Data(y, ys)) if x == y && xs.len() == ys.len() => xs
                .into_iter()
                .zip(ys.into_iter())
                .all(|(x, y)| self.covers_var(x, y)),
            (TyInfo::Assoc(x, class_x, assoc_x), Ty::Assoc(y, class_y, assoc_y))
                if class_x == class_y && assoc_x == assoc_y => self.covers_var(x, y),
            (TyInfo::Effect(x, xs, x_out), Ty::Effect(y, ys, y_out)) if x == y && xs.len() == ys.len() => xs
                .into_iter()
                .zip(ys.into_iter())
                .all(|(x, y)| self.covers_var(x, y)) && self.covers_var(x_out, y_out),
            _ => false,
        }
    }

    // Link the generic types of a class member with type variables in the current scope so obligations
    // can be generated
    fn derive_links(&self, member: TyId, ty: TyVar, link_gen: &mut impl FnMut(usize, TyVar)) {
        match (self.ctx.tys.get(member), self.follow_info(ty)) {
            (Ty::Gen(gen_idx, _), _) => link_gen(gen_idx, ty),
            (Ty::List(x), TyInfo::List(y)) => self.derive_links(x, y, link_gen),
            (Ty::Tuple(xs), TyInfo::Tuple(ys)) => xs
                .into_iter()
                .zip(ys.into_iter())
                .for_each(|(x, y)| self.derive_links(x, y, link_gen)),
            (Ty::Record(xs), TyInfo::Record(ys)) => xs
                .into_iter()
                .zip(ys.into_iter())
                .for_each(|((_, x), (_, y))| self.derive_links(x, y, link_gen)),
            (Ty::Func(x_i, x_o), TyInfo::Func(y_i, y_o)) => {
                self.derive_links(x_i, y_i, link_gen);
                self.derive_links(x_o, y_o, link_gen);
            },
            (Ty::Data(_, xs), TyInfo::Data(_, ys)) => xs
                .into_iter()
                .zip(ys.into_iter())
                .for_each(|(x, y)| self.derive_links(x, y, link_gen)),
            (Ty::Assoc(x, class_x, assoc_x), TyInfo::Assoc(y, class_y, assoc_y))
                if class_x == class_y && assoc_x == assoc_y => self.derive_links(x, y, link_gen),
            (_, TyInfo::SelfType) => panic!("Self type not permitted here"),
            (Ty::Effect(_, xs, x_out), TyInfo::Effect(_, ys, y_out)) => {
                xs
                    .into_iter()
                    .zip(ys.into_iter())
                    .for_each(|(x, y)| self.derive_links(x, y, link_gen));
                self.derive_links(x_out, y_out, link_gen);
            },
            _ => {}, // Only type constructors and generic types generate obligations
        }
    }

    // Extract a list of all classes that the type is a member of
    fn walk_implied_obligations(&self, classes: &mut HashSet<ClassId>, class: ClassId) {
        if classes.insert(class) {
            for obl in self.ctx.classes.get(class).obligations.as_ref().expect("Obligations must be known here") {
                match obl.inner() {
                    Obligation::MemberOf(class) => self.walk_implied_obligations(classes, *class),
                }
            }
        }
    }

    /// Resolve a class obligation for a type, returning the ID of the type's membership. If no member can be provided
    /// (because, for example, the membership is implied by a generic bound) then `Err(false)` is returned instead. If
    /// resolution failed due to an existing error, `Err(true)` is returned.
    fn resolve_obligation(&mut self, ty: TyVar, obligation: ClassId, span: Span) -> Option<Result<Result<MemberId, bool>, InferError>> {
        // TODO: Resolve possible infinite loop when resolving by having an obligation cache
        match self.follow_info(ty) {
            TyInfo::Error(_) => {
                self.set_error(ty);
                return Some(Ok(Err(true))); // Resolving an error type always succeeds
            },
            TyInfo::Unknown(_) => None, // No idea if it implements the trait yet
            TyInfo::Gen(gen_idx, gen_scope, _) if self
                .implied_obligations_for_gen(gen_scope, gen_idx)
                .into_iter()
                // .any(|c| self.ctx.classes.implies(c, obligation))
                .any(|c| c == obligation) => {
                    Some(Ok(Err(false)))
            },
            TyInfo::SelfType if {
                let mut implied = HashSet::default();
                for obl in &self.self_obligations {
                    self.walk_implied_obligations(&mut implied, *obl);
                }
                implied.contains(&obligation)
            } => {
                Some(Ok(Err(false)))
            },
            info => {
                // Find a class member declaration that covers our type
                let covering_member = self.ctx.classes
                    .members_of(obligation)
                    .find(|(_, member)| Self::covers_var(self, ty, member.member));

                if let Some((covering_member_id, covering_member)) = covering_member {
                    let scope = self.ctx.tys.get_gen_scope(covering_member.gen_scope);

                    let mut links = HashSet::new();
                    self.derive_links(covering_member.member, ty, &mut |gen_idx, var| { links.insert((gen_idx, var)); });

                    for (gen_idx, ty) in links {
                        for obl in scope.get(gen_idx).obligations.as_ref().unwrap() {
                            match &**obl {
                                Obligation::MemberOf(class) => {
                                    self.constraints.push_back(Constraint::Impl(ty, *class, span, Vec::new()));
                                },
                            }
                        }
                    }

                    Some(Ok(Ok(covering_member_id)))
                } else {
                    Some(Err(InferError::TypeDoesNotFulfil(
                        obligation,
                        ty,
                        span,
                        if let TyInfo::Gen(gen_idx, gen_scope, _) = info {
                            Some(self.ctx.tys.get_gen_scope(gen_scope).get(gen_idx).name.span())
                        } else {
                            None
                        },
                    )))
                }
            },
        }
    }

    fn resolve_constraints(&mut self) {
        let mut tries = self.constraints.len();
        while tries > 0 {
            if let Some(c) = self.constraints.pop_front() {
                tries -= 1;
                match self.resolve(c.clone()) {
                    // Constraint resolved
                    Some(res) => {
                        // Record any errors while resolving the constraint
                        if let Err(e) = res {
                            self.errors.push(e);
                        }
                        // A constraint being resolved resets the counter
                        tries = self.constraints.len();
                    },
                    None => self.constraints.push_back(c), // Still unresolved...
                }
            } else {
                break
            }
        }

        // Generate errors for all remaining constraints
        for c in std::mem::take(&mut self.constraints) {
            self.errors.push(match c {
                Constraint::Access(record, field_name, _field) => {
                    InferError::NoSuchItem(record, self.span(record), field_name.clone())
                },
                Constraint::Update(record, field_name, _field) => {
                    InferError::NoSuchItem(record, self.span(record), field_name.clone())
                },
                Constraint::Binary(op, a, b, _output) => {
                    InferError::InvalidBinaryOp(op.clone(), a, b)
                },
                Constraint::Impl(ty, obligation, span, _) => {
                    InferError::TypeDoesNotFulfil(obligation, ty, span, None)
                },
                Constraint::ClassField(_ty, _class, field, _field_ty, _span) => {
                    InferError::AmbiguousClassItem(field, Vec::new())
                },
                Constraint::ClassAssoc(_ty, _class, assoc, _assoc_ty, _span) => {
                    InferError::AmbiguousClassItem(assoc, Vec::new())
                },
            });
        }
    }

    pub fn into_checked(mut self) -> (Checked<'a>, Vec<Error>) {
        self.resolve_constraints();

        let mut errors = std::mem::take(&mut self.errors);

        // Report errors for types that cannot be inferred
        let tys = self.iter().collect::<Vec<_>>();
        for (ty, info) in tys {
            if let TyInfo::Unknown(origin) = info {
                if !self.is_error(ty) {
                    errors.push(InferError::CannotInfer(ty, origin));
                    self.set_error(ty);
                }
            }
        }

        let mut checked = Checked {
            cache: HashMap::default(),
            infer: self,
        };

        let errors = errors
            .into_iter()
            .map(|error| match error {
                InferError::CannotCoerce(x, y, inner, info) => {
                    let inner = inner.map(|(a, b)| (checked.reify(a), checked.reify(b)));
                    Error::CannotCoerce(checked.reify(x), checked.reify(y), inner, info)
                },
                InferError::CannotInfer(a, origin) => Error::CannotInfer(checked.reify(a), origin),
                InferError::Recursive(a, part) => Error::Recursive(checked.reify(a), checked.infer.span(a), checked.infer.span(part)),
                InferError::NoSuchItem(a, record_span, field) => Error::NoSuchItem(checked.reify(a), record_span, field),
                InferError::NoSuchField(a, record_span, field) => Error::NoSuchField(checked.reify(a), record_span, field),
                InferError::InvalidUnaryOp(op, a) => Error::InvalidUnaryOp(op, checked.reify(a), checked.infer.span(a)),
                InferError::InvalidBinaryOp(op, a, b) => Error::InvalidBinaryOp(op, checked.reify(a), checked.infer.span(a), checked.reify(b), checked.infer.span(b)),
                InferError::TypeDoesNotFulfil(class, ty, span, gen_span) => Error::TypeDoesNotFulfil(class, checked.reify(ty), span, gen_span),
                InferError::RecursiveAlias(alias, a, span) => Error::RecursiveAlias(alias, checked.reify(a), span),
                InferError::PatternNotSupported(lhs, op, rhs, span) => Error::PatternNotSupported(checked.reify(lhs), op, checked.reify(rhs), span),
                InferError::AmbiguousClassItem(field, candidate_classes) => Error::AmbiguousClassItem(field, candidate_classes),
            })
            .collect();

        (checked, errors)
    }
}

pub struct Checked<'a> {
    cache: HashMap<TyVar, TyId>,
    infer: Infer<'a>,
}

impl<'a> Checked<'a> {
    pub fn ctx(&self) -> &Context { &self.infer.ctx }
    pub fn ctx_mut(&mut self) -> &mut Context { &mut self.infer.ctx }

    fn reify_inner(&mut self, var: TyVar) -> TyId {
        if let Some(ty) = self.cache.get(&var) {
            *ty
        } else {
            let ty = match self.infer.info(var) {
                // Follow references
                TyInfo::Ref(x) => return self.reify_inner(x),
                // Unknown types are treated as errors from here on out
                TyInfo::Unknown(_) => Ty::Error(ErrorReason::Unknown),
                TyInfo::Error(reason) => Ty::Error(reason),
                TyInfo::Prim(prim) => Ty::Prim(prim),
                TyInfo::List(item) => Ty::List(self.reify_inner(item)),
                TyInfo::Tuple(items) => Ty::Tuple(items
                    .into_iter()
                    .map(|item| self.reify_inner(item))
                    .collect()),
                TyInfo::Record(fields) => Ty::Record(fields
                    .into_iter()
                    .map(|(name, field)| (name, self.reify_inner(field)))
                    .collect()),
                TyInfo::Func(i, o) => Ty::Func(self.reify_inner(i), self.reify_inner(o)),
                TyInfo::Data(data, args) => Ty::Data(data, args
                    .into_iter()
                    .map(|arg| self.reify_inner(arg))
                    .collect()),
                TyInfo::Gen(name, scope, _) => Ty::Gen(name, scope),
                TyInfo::SelfType => Ty::SelfType,
                TyInfo::Assoc(inner, class_id, assoc) => Ty::Assoc(self.reify_inner(inner), class_id, assoc),
                TyInfo::Effect(eff, args, out) => Ty::Effect(eff, args
                    .into_iter()
                    .map(|arg| self.reify_inner(arg))
                    .collect(), self.reify_inner(out)),
            };
            self.infer.ctx.tys.insert(self.infer.span(var), ty)
        }
    }

    pub fn reify(&mut self, var: TyVar) -> TyId {
        let ty = self.reify_inner(var);
        self.cache.insert(var, ty);
        ty
    }

    pub fn reify_class(&mut self, class: ClassVar) -> Option<ClassId> {
        self.infer.class_vars[class.0].1
    }
}
