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
}

#[derive(Default, Debug)]
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
    Mismatch(TyVar, TyVar, EqInfo),
    CannotInfer(TyVar, Option<Span>), // With optional instantiation origin
    // Type, recursive element
    Recursive(TyVar, TyVar),
    NoSuchField(TyVar, Span, SrcNode<Ident>),
    InvalidUnaryOp(SrcNode<ast::UnaryOp>, TyVar),
    InvalidBinaryOp(SrcNode<ast::BinaryOp>, TyVar, TyVar),
    TypeDoesNotFulfil(ClassId, TyVar, Span, Option<Span>),
    RecursiveAlias(AliasId, TyVar, Span),
    PatternNotSupported(TyVar, SrcNode<ast::BinaryOp>, TyVar, Span),
    AmbiguousClassItem(SrcNode<Ident>, Vec<ClassId>),
}

#[derive(Clone)]
enum Constraint {
    // (record, field_name, field)
    Access(TyVar, SrcNode<Ident>, TyVar),
    Unary(SrcNode<ast::UnaryOp>, TyVar, TyVar),
    Binary(SrcNode<ast::BinaryOp>, TyVar, TyVar, TyVar),
    Impl(TyVar, ClassId, Span),
    ClassField(TyVar, ClassVar, SrcNode<Ident>, TyVar, Span),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
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
}

impl<'a> Infer<'a> {
    pub fn new(ctx: &'a mut Context, gen_scope: Option<GenScopeId>, self_type: Option<Span>) -> Self {
        let mut this = Self {
            ctx,
            gen_scope,
            vars: Vec::new(),
            class_vars: Vec::new(),
            constraints: VecDeque::new(),
            errors: Vec::new(),
            self_type: None,
        };

        if let Some(span) = self_type {
            this.self_type = Some(this.insert(span, TyInfo::SelfType));
        }

        this
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

    pub fn instantiate(&mut self, ty: TyId, span: Span, f: &impl Fn(usize, GenScopeId, &Context) -> TyVar, self_ty: Option<TyVar>) -> TyVar {
        let info = match self.ctx.tys.get(ty) {
            Ty::Error(reason) => TyInfo::Error(reason),
            Ty::Prim(prim) => TyInfo::Prim(prim),
            Ty::List(item) => TyInfo::List(self.instantiate(item, span, f, self_ty)),
            Ty::Tuple(items) => TyInfo::Tuple(items
                .into_iter()
                .map(|item| self.instantiate(item, span, f, self_ty))
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
        };
        self.insert(self.ctx.tys.get_span(ty) /*span*/, info)
    }

    pub fn unknown(&mut self, span: Span) -> TyVar {
        self.insert(span, TyInfo::Unknown(None))
    }

    // pub fn usage(&mut self, span: Span, ty: TyVar) -> TyVar {
    //     self.insert(span, TyInfo::Ref(ty))
    // }

    pub fn make_access(&mut self, record: TyVar, field_name: SrcNode<Ident>, field: TyVar) {
        self.constraints.push_back(Constraint::Access(record, field_name, field));
    }

    pub fn make_unary(&mut self, op: SrcNode<ast::UnaryOp>, a: TyVar, output: TyVar) {
        self.constraints.push_back(Constraint::Unary(op, a, output));
    }

    pub fn make_binary(&mut self, op: SrcNode<ast::BinaryOp>, a: TyVar, b: TyVar, output: TyVar) {
        self.constraints.push_back(Constraint::Binary(op, a, b, output));
    }

    pub fn make_impl(&mut self, ty: TyVar, class: ClassId, span: Span) {
        self.constraints.push_back(Constraint::Impl(ty, class, span));
    }

    pub fn make_class_field(&mut self, ty: TyVar, field_name: SrcNode<Ident>, field_ty: TyVar, span: Span) -> ClassVar {
        let class = ClassVar(self.class_vars.len());
        self.class_vars.push((span, None));
        self.constraints.push_back(Constraint::ClassField(ty, class, field_name, field_ty, span));
        class
    }

    pub fn emit(&mut self, err: InferError) {
        self.errors.push(err);
    }

    fn occurs_in_inner(&self, x: TyVar, y: TyVar, seen: &mut Vec<TyVar>) -> bool {
        if seen.contains(&y) {
            return true;
        } else {
            seen.push(y);
        }

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
        };

        seen.pop();

        occurs
    }

    // Returns true if `x` occurs in `y`.
    fn occurs_in(&self, x: TyVar, y: TyVar) -> bool {
        self.occurs_in_inner(x, y, &mut Vec::new())
    }

    pub fn make_eq(&mut self, x: TyVar, y: TyVar, info: impl Into<EqInfo>) {
        if let Err((a, b)) = self.make_eq_inner(x, y) {
            if !self.is_error(a) && !self.is_error(b) {
                self.set_error(a);
                self.set_error(b);
                self.errors.push(InferError::Mismatch(x, y, info.into()));
            }
        }
    }

    pub fn make_eq_inner(&mut self, x: TyVar, y: TyVar) -> Result<(), (TyVar, TyVar)> {
        fn make_eq_many(
            infer: &mut Infer,
            xs: impl IntoIterator<Item = TyVar>,
            ys: impl IntoIterator<Item = TyVar>,
        ) -> Result<(), (TyVar, TyVar)> {
            xs
                .into_iter()
                .zip(ys.into_iter())
                .fold(None, |err, (x, y)| err.or(infer.make_eq_inner(x, y).err()))
                .map(Err).unwrap_or(Ok(()))
        }

        if x == y { return Ok(()) } // If the vars are equal, we have no need to check equivalence
        match (self.info(x), self.info(y)) {
            // Follow references
            (TyInfo::Ref(x), _) => self.make_eq_inner(x, y),
            (_, TyInfo::Ref(y)) => self.make_eq_inner(x, y),

            // Unify unknown or erronoeus types
            (TyInfo::Unknown(_), _) => if self.occurs_in(x, y) {
                self.errors.push(InferError::Recursive(y, self.follow(x)));
                self.set_info(x, TyInfo::Error(ErrorReason::Recursive));
                Ok(self.set_error(x)) // TODO: Not actually ok
            } else {
                Ok(self.set_info(x, TyInfo::Ref(y)))
            },
            (_, TyInfo::Unknown(_)) => self.make_eq_inner(y, x),

            (_, TyInfo::Error(_)) => {
                self.set_error(x);
                Ok(self.set_info(x, TyInfo::Ref(y)))
            },
            (TyInfo::Error(_), _) => self.make_eq_inner(y, x),

            (TyInfo::Prim(x), TyInfo::Prim(y)) if x == y => Ok(()),
            (TyInfo::List(x), TyInfo::List(y)) => self.make_eq_inner(x, y),
            (TyInfo::Tuple(xs), TyInfo::Tuple(ys)) if xs.len() == ys.len() => make_eq_many(self, xs, ys),
            (TyInfo::Record(xs), TyInfo::Record(ys)) if xs.len() == ys.len() && xs
                .keys()
                .all(|x| ys.contains_key(x)) => xs
                    .into_iter()
                    .try_for_each(|(x, x_ty)| self.make_eq_inner(x_ty, ys[&x])),
            (TyInfo::Func(x_i, x_o), TyInfo::Func(y_i, y_o)) => {
                let i_err = self.make_eq_inner(x_i, y_i).err();
                let o_err = self.make_eq_inner(x_o, y_o).err();
                i_err.or(o_err).map(Err).unwrap_or(Ok(()))
            },
            (TyInfo::Data(x_data, xs), TyInfo::Data(y_data, ys)) if x_data == y_data &&
                xs.len() == ys.len() => make_eq_many(self, xs, ys),
            (TyInfo::Gen(a, a_scope, _), TyInfo::Gen(b, b_scope, _)) if a == b && a_scope == b_scope => Ok(()),
            (_, _) => Err((x, y)),//self.errors.push(InferError::Mismatch(x, y)),
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
        }
    }

    fn resolve(&mut self, c: Constraint) -> Option<Result<(), InferError>> {
        use ast::{UnaryOp::*, BinaryOp::*};
        match c {
            Constraint::Access(record, field_name, field) => match self.follow_info(record) {
                _ if self.is_error(record) => {
                    self.set_error(field);
                    // Trying to access a field on an error type counts as success because we don't want to emit more
                    // errors than necessary.
                    Some(true)
                },
                TyInfo::Unknown(_) => None,
                TyInfo::Record(fields) => if let Some(field_ty) = fields.get(&field_name) {
                    self.make_eq(*field_ty, field, field_name.span());
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
                                self.make_eq(field_ty, field, field_name.span());
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
                .map(|success| if success {
                    Ok(())
                } else {
                    self.set_error(field);
                    Err(InferError::NoSuchField(record, self.span(record), field_name.clone()))
                }),
            Constraint::Unary(op, a, output) => match (&*op, self.follow_info(a)) {
                (_, TyInfo::Error(reason)) => Some(Ok(TyInfo::Error(reason))),
                (_, TyInfo::Unknown(_)) => None,
                (Neg, TyInfo::Prim(Prim::Num)) => Some(Ok(TyInfo::Prim(Prim::Num))),
                (Neg, TyInfo::Prim(Prim::Nat)) => Some(Ok(TyInfo::Prim(Prim::Int))),
                (Neg, TyInfo::Prim(Prim::Int)) => Some(Ok(TyInfo::Prim(Prim::Int))),
                (Not, TyInfo::Prim(Prim::Bool)) => Some(Ok(TyInfo::Prim(Prim::Bool))),
                _ => {
                    self.set_error(output);
                    Some(Err(InferError::InvalidUnaryOp(op.clone(), a)))
                },
            }
                .map(|info| info.map(|info| {
                    // TODO: Use correct span
                    let result_ty = self.insert(op.span(), info);
                    self.make_eq(output, result_ty, self.span(output));
                })),
            Constraint::Binary(op, a, b, output) => match (&*op, self.follow_info(a), self.follow_info(b)) {
                (_, _, TyInfo::Error(reason)) => Some(Ok(TyInfo::Error(reason))),
                (_, TyInfo::Error(reason), _) => Some(Ok(TyInfo::Error(reason))),
                (_, _, TyInfo::Unknown(_)) => None,
                (_, TyInfo::Unknown(_), _) => None,
                (Join, TyInfo::List(a), TyInfo::List(b)) => {
                    self.make_eq(a, b, EqInfo::new(op.span(), "The types of joined lists must be equal".to_string()));
                    Some(Ok(TyInfo::List(a)))
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
                            ((Div, Nat, Nat), Num),
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
                            ((Div, Int, Int), Num),
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
                    self.make_eq(output, result_ty, self.span(output));
                })),
            Constraint::Impl(ty, obligation, span) => self.resolve_obligation(ty, obligation, span),
            Constraint::ClassField(ty, class, field, field_ty, span) => self.try_resolve_class_from_field(ty, class, field.clone(), field_ty, span),
        }
    }

    fn try_resolve_class_from_field(&mut self, ty: TyVar, class_var: ClassVar, field: SrcNode<Ident>, field_ty: TyVar, span: Span) -> Option<Result<(), InferError>> {
        let possible_classes: Vec<_> = match self.follow_info(ty) {
            TyInfo::Error(_) => {
                self.set_error(ty);
                self.set_error(field_ty);
                return Some(Ok(())); // Resolving an error type always succeeds
            }
            TyInfo::Unknown(_) => return None, // We don't know what the type is yet, so how can we possibly determine what classes it is a member of?
            TyInfo::Gen(gen_idx, gen_scope, _) => {
                let gen_scope = self.ctx.tys.get_gen_scope(gen_scope);
                gen_scope
                    .find_obligations_for(self.ctx, gen_idx)
                    .into_iter()
                    // Filter by class obligations that contain the given field
                    .filter(|class_id| self.ctx.classes.get(*class_id).unwrap().field(*field).is_some())
                    .collect()
            },
            _ => self.ctx.classes
                .iter()
                // Filter by classes that contain the given field
                .filter(|(_, class)| class.field(*field).is_some())
                // Filter further by classes that have members that cover our type
                .filter(|(class_id, _)| self.ctx.classes
                    .members_of(*class_id)
                    .find(|member| Self::covers_var(self, ty, member.member))
                    .is_some())
                .map(|(class_id, _)| class_id)
                .collect(),
        };

        match possible_classes.len() {
            0 => Some(Err(InferError::NoSuchField(ty, span, field))),
            1 => {
                let class_id = *possible_classes.first().unwrap();
                self.class_vars[class_var.0].1 = Some(class_id); // Can't fail
                self.make_impl(ty, class_id, span);
                let field_ty_id = **self.ctx.classes
                    .get(class_id)
                    .unwrap()
                    .field(*field)
                    .unwrap();
                let inst_field_ty = self.instantiate(field_ty_id, field.span(), &|_, _, _| panic!("Tried to substitute generic type for non-generic class"), Some(ty));
                self.make_eq(
                    field_ty,
                    inst_field_ty,
                    field.span(),
                );
                Some(Ok(()))
            },
            _ => {
                // TODO: Return `None` here instead, wait for more inference info
                self.set_error(field_ty);
                Some(Err(InferError::AmbiguousClassItem(field, possible_classes)))
            },
        }
    }

    // Returns true if ty covers var (i.e: var is a structural subset of ty)
    fn covers_var(infer: &Infer, var: TyVar, ty: TyId) -> bool {
        match (infer.follow_info(var), infer.ctx.tys.get(ty)) {
            (_, Ty::Gen(_, _)) => true, // Blanket impls match everything
            (TyInfo::Prim(x), Ty::Prim(y)) if x == y => true,
            (TyInfo::List(x), Ty::List(y)) => Self::covers_var(infer, x, y),
            (TyInfo::Tuple(xs), Ty::Tuple(ys)) if xs.len() == ys.len() => xs
                .into_iter()
                .zip(ys.into_iter())
                .all(|(x, y)| Self::covers_var(infer, x, y)),
            (TyInfo::Record(xs), Ty::Record(ys)) if xs.len() == ys.len() => xs
                .into_iter()
                .zip(ys.into_iter())
                .all(|((_, x), (_, y))| Self::covers_var(infer, x, y)),
            (TyInfo::Func(x_i, x_o), Ty::Func(y_i, y_o)) => {
                Self::covers_var(infer, x_i, y_i) && Self::covers_var(infer, x_o, y_o)
            },
            (TyInfo::Data(x, xs), Ty::Data(y, ys)) if x == y && xs.len() == ys.len() => xs
                .into_iter()
                .zip(ys.into_iter())
                .all(|(x, y)| Self::covers_var(infer, x, y)),
            _ => false,
        }
    }

    fn resolve_obligation(&mut self, ty: TyVar, obligation: ClassId, span: Span) -> Option<Result<(), InferError>> {
        match self.follow_info(ty) {
            TyInfo::Error(_) => {
                self.set_error(ty);
                return Some(Ok(())); // Resolving an error type always succeeds
            },
            TyInfo::Unknown(_) => None, // No idea if it implements the trait yet
            TyInfo::Gen(gen_idx, gen_scope, _) => {
                let gen_scope = self.ctx.tys.get_gen_scope(gen_scope);
                if gen_scope
                    .get(gen_idx)
                    .obligations
                    .as_ref()
                    .expect("Generic constraints must be resolved during inference")
                    .iter()
                    .find(|c| match c {
                        Obligation::MemberOf(class) if *class == obligation => true,
                        _ => false,
                    })
                    .is_some()
                {
                    Some(Ok(()))
                } else {
                    Some(Err(InferError::TypeDoesNotFulfil(
                        obligation,
                        ty,
                        span,
                        Some(gen_scope.get(gen_idx).name.span()),
                    )))
                }
            },
            _ => {
                // Find a class member declaration that covers our type
                let covering_member = self.ctx.classes
                    .members_of(obligation)
                    .find(|member| Self::covers_var(self, ty, member.member));

                if let Some(covering_member) = covering_member {
                    // Link the generic types of a class member with type variables in the current scope so obligations
                    // can be generated
                    fn derive_links(infer: &Infer, member: TyId, ty: TyVar, link_gen: &mut impl FnMut(usize, TyVar)) {
                        match (infer.ctx.tys.get(member), infer.follow_info(ty)) {
                            (Ty::Gen(gen_idx, _), _) => link_gen(gen_idx, ty),
                            (Ty::List(x), TyInfo::List(y)) => derive_links(infer, x, y, link_gen),
                            (Ty::Tuple(xs), TyInfo::Tuple(ys)) => xs
                                .into_iter()
                                .zip(ys.into_iter())
                                .for_each(|(x, y)| derive_links(infer, x, y, link_gen)),
                            (Ty::Record(xs), TyInfo::Record(ys)) => xs
                                .into_iter()
                                .zip(ys.into_iter())
                                .for_each(|((_, x), (_, y))| derive_links(infer, x, y, link_gen)),
                            (Ty::Func(x_i, x_o), TyInfo::Func(y_i, y_o)) => {
                                derive_links(infer, x_i, y_i, link_gen);
                                derive_links(infer, x_o, y_o, link_gen);
                            },
                            (Ty::Data(_, xs), TyInfo::Data(_, ys)) => xs
                                .into_iter()
                                .zip(ys.into_iter())
                                .for_each(|(x, y)| derive_links(infer, x, y, link_gen)),
                            (_, TyInfo::SelfType) => panic!("Self type not permitted here"),
                            _ => {}, // Only type constructors and generic types generate obligations
                        }
                    }

                    let scope = self.ctx.tys.get_gen_scope(covering_member.gen_scope);

                    let mut links = HashSet::new();
                    derive_links(self, covering_member.member, ty, &mut |gen_idx, var| { links.insert((gen_idx, var)); });

                    for (gen_idx, ty) in links {
                        for obl in scope.get(gen_idx).obligations.as_ref().unwrap() {
                            match obl {
                                Obligation::MemberOf(class) => {
                                    self.constraints.push_back(Constraint::Impl(ty, *class, span));
                                },
                            }
                        }
                    }

                    Some(Ok(()))
                } else {
                    Some(Err(InferError::TypeDoesNotFulfil(
                        obligation,
                        ty,
                        span,
                        None,
                    )))
                }
            },
        }
    }

    fn resolve_constraints(&mut self) {
        let mut tries = self.constraints.len();
        while let (Some(c), true) = (self.constraints.pop_front(), tries > 0) {
            tries -= 1;
            match self.resolve(c.clone()) {
                // Constraint resolved
                Some(res) => {
                    // Record any errors while resolving the constraint
                    if let Err(e) = res {
                        self.errors.push(e);
                    }
                    tries = self.constraints.len();
                },
                None => self.constraints.push_back(c), // Still unresolved...
            }
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
                InferError::Mismatch(a, b, info) => Error::Mismatch(checked.reify(a), checked.reify(b), info),
                InferError::CannotInfer(a, origin) => Error::CannotInfer(checked.reify(a), origin),
                InferError::Recursive(a, part) => Error::Recursive(checked.reify(a), checked.infer.span(a), checked.infer.span(part)),
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
            return *ty;
        }

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
        };
        self.infer.ctx.tys.insert(self.infer.span(var), ty)
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
