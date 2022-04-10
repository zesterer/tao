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
    Assoc(TyVar, ClassVar, SrcNode<Ident>),
    // Effect, output type, opaque type variable for tracking
    Effect(EffectVar, TyVar, TyVar),
    Opaque(usize),
}

#[derive(Clone, Debug, PartialEq)]
pub enum EffectInfo {
    Unknown,
    Ref(EffectVar),
    Known(EffectDeclId, Vec<TyVar>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum ClassInfo {
    Unknown,
    Ref(ClassVar),
    Known(ClassId, Vec<TyVar>),
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
    CannotInferEffect(EffectVar),
    // Type, recursive element
    Recursive(TyVar, TyVar),
    NoSuchItem(TyVar, Span, SrcNode<Ident>),
    NoSuchField(TyVar, Span, SrcNode<Ident>),
    // (_, _, obligation span, span of original generic, usage span)
    TypeDoesNotFulfil(ClassVar, TyVar, Span, Option<Span>, Span),
    RecursiveAlias(AliasId, TyVar, Span),
    PatternNotSupported(TyVar, SrcNode<ast::BinaryOp>, TyVar, Span),
    AmbiguousClassItem(SrcNode<Ident>, Vec<ClassId>),
    CycleWhenResolving(TyVar, (ClassId, Vec<TyVar>), Span),
}

#[derive(Clone, Debug)]
enum Constraint {
    // (record, field_name, field)
    Access(TyVar, SrcNode<Ident>, TyVar),
    Update(TyVar, SrcNode<Ident>, TyVar),
    Impl(TyVar, ClassVar, Span, Vec<(SrcNode<Ident>, TyVar)>, Span),
    ClassField(TyVar, ClassVar, SrcNode<Ident>, TyVar, Span),
    ClassAssoc(TyVar, ClassVar, SrcNode<Ident>, TyVar, Span),
    EffectSendRecv(EffectVar, TyVar, TyVar, Span),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TyVar(usize);

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct ClassVar(usize);

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct EffectVar(usize);

pub struct Infer<'a> {
    ctx: &'a mut Context,
    gen_scope: Option<GenScopeId>,
    // TODO: Add EffectSet to each type var, like
    // enum EffectSet {
    //     Unknown,
    //     // Effect set can grow through inference
    //     Open(Vec<EffectVar>),
    //     // Effect set cannot grow through inference and instead just generates check constraints
    //     Closed(Vec<EffectVar>),
    // }
    vars: Vec<(Span, TyInfo, Result<(), ()>)>,
    class_vars: Vec<(Span, ClassInfo)>,
    effect_vars: Vec<(Span, EffectInfo)>,
    opaque_id: usize,
    constraints: VecDeque<Constraint>,
    errors: Vec<InferError>,
    self_type: Option<TyVar>,
    // self_obligations: Vec<ClassId>,
    implied_members: Vec<InferImpliedMember>,
}

impl<'a> Infer<'a> {
    // gen_scope: Some((_, true)) means that generic bounds are implied (almost always what you want)
    // gen_scope: Some((_, false)) means that generic bounds are not implied (used when inferring the bounds themselves)
    pub fn new(ctx: &'a mut Context, gen_scope: Option<GenScopeId>) -> Self {
        let mut this = Self {
            ctx,
            gen_scope: gen_scope,
            vars: Vec::new(),
            class_vars: Vec::new(),
            effect_vars: Vec::new(),
            opaque_id: 0,
            constraints: VecDeque::new(),
            errors: Vec::new(),
            self_type: None,
            // self_obligations: Vec::new(),
            implied_members: Vec::new(),
        };

        this
    }

    pub fn with_gen_scope_implied(mut self) -> Self {
        if let Some(gen_scope_id) = self.gen_scope {
            let gen_scope = self.ctx.tys.get_gen_scope(gen_scope_id);
            for member in gen_scope
                .implied_members
                .as_ref()
                .expect("Implied members must be known")
                .clone()
            {
                let member_ty = self.instantiate_local(*member.member, member.member.span());
                let args = member.args
                    .iter()
                    .map(|arg| self.instantiate_local(*arg, member.member.span()))
                    .collect();
                let items = match &member.items {
                    ImpliedItems::Real(member) => ImpliedItems::Real(*member),
                    ImpliedItems::Eq(assoc) => ImpliedItems::Eq(assoc
                        .iter()
                        .map(|(name, assoc)| (name.clone(), self.instantiate_local(*assoc, member.member.span())))
                        .collect()),
                };
                self.add_implied_member(InferImpliedMember {
                    member: SrcNode::new(member_ty, member.member.span()),
                    args,
                    class: member.class.clone(),
                    items,
                });
            }
        }
        self
    }

    pub fn with_self_type(mut self, ty: TyId, span: Span) -> Self {
        self.self_type = Some(self.instantiate_local(ty, span));
        self
    }

    pub fn set_self_unknown(&mut self, span: Span) -> TyVar {
        let ty = self.insert(span, TyInfo::SelfType);
        self.self_type = Some(ty);
        ty
    }

    pub fn self_type(&self) -> Option<TyVar> { self.self_type }

    fn add_implied_member_inner(&mut self, searched: &mut HashSet<ClassId>, member: InferImpliedMember) {
        searched.insert(*member.class);
        // Recursively search for member implied by the class
        let gen_scope_id = self.ctx.classes.get(*member.class).gen_scope;
        for new_member in self.ctx.tys
            .get_gen_scope(gen_scope_id)
            .implied_members
            .as_ref()
            .expect("Implied members must be known!")
            .clone()
        {
            if !searched.contains(&*new_member.class) {
                let member_ty = self.instantiate(*new_member.member, new_member.member.span(), &|idx, _, _| member.args.get(idx).copied(), Some(*member.member));
                let member_args = new_member.args
                    .iter()
                    .map(|arg| self.instantiate(*arg, new_member.member.span(), &|idx, _, _| member.args.get(idx).copied(), Some(*member.member)))
                    .collect();
                let items = match &new_member.items {
                    ImpliedItems::Real(member) => ImpliedItems::Real(*member),
                    ImpliedItems::Eq(assoc) => ImpliedItems::Eq(assoc
                        .iter()
                        .map(|(name, assoc)| (name.clone(), self.instantiate(*assoc, name.span(), &|idx, _, _| member.args.get(idx).copied(), Some(*member.member))))
                        .collect()),
                };

                self.add_implied_member_inner(searched, InferImpliedMember {
                    member: SrcNode::new(member_ty, member.member.span()),
                    class: new_member.class.clone(),
                    args: member_args,
                    items,
                });
            }
        }

        self.implied_members.push(member);
    }

    pub fn add_implied_member(&mut self, member: InferImpliedMember) {
        self.add_implied_member_inner(&mut HashSet::new(), member);
    }

    // Add an implied member, but don't recurse to find more (because implied members on classes might not be derived yet)
    pub fn add_implied_member_single(&mut self, member: InferImpliedMember) {
        self.implied_members.push(member);
    }

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
                | TyInfo::Opaque(_)
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
                TyInfo::Effect(_eff, out, opaque) => {
                    // TODO: Set error for eff
                    self.set_error(out);
                    self.set_error(opaque);
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
        self.instantiate(ty, span, &|idx, gen_scope, ctx| gens.as_ref().expect("No gen scope").get(idx).copied(), self.self_type)
    }

    pub fn instantiate(&mut self, ty: TyId, span: impl Into<Option<Span>>, f: &impl Fn(usize, GenScopeId, &Context) -> Option<TyVar>, self_ty: Option<TyVar>) -> TyVar {
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
             // TODO: Check scope is valid for recursive scopes
            Ty::Gen(index, scope) => match f(index, scope, self.ctx) {
                Some(ty) => TyInfo::Ref(ty),
                None => {
                    // TODO: Can only occur if there's a mismatch in generic parameters, for which we already report an error
                    TyInfo::Error(ErrorReason::Invalid)
                },
            },
            Ty::SelfType => if let Some(self_ty) = self_ty {
                TyInfo::Ref(self_ty)
            } else {
                let span = span.unwrap_or_else(|| self.ctx.tys.get_span(ty));
                self.ctx.emit(Error::SelfNotValidHere(span));
                TyInfo::Error(ErrorReason::Invalid)
            },
            Ty::Assoc(inner, (class_id, args), assoc) => {
                let span = span.unwrap_or_else(|| self.ctx.tys.get_span(ty));
                let inner = self.instantiate(inner, span, f, self_ty);
                let args = args
                    .iter()
                    .map(|arg| self.instantiate(*arg, span, f, self_ty))
                    .collect();
                let assoc_ty = self.unknown(span);
                self.make_impl(inner, (class_id, args), span, vec![(assoc, assoc_ty)], span);
                TyInfo::Ref(assoc_ty)
            },
            Ty::Effect(eff, out) => match self.ctx.tys.get_effect(eff) {
                Effect::Error => TyInfo::Error(ErrorReason::Invalid),
                Effect::Known(decl, args) => {
                    let args = args
                        .into_iter()
                        .map(|param| self.instantiate(param, span, f, self_ty))
                        .collect();
                    let eff = self.insert_effect(
                        span.unwrap_or_else(|| self.ctx.tys.get_span(ty)),
                        EffectInfo::Known(decl, args),
                    );
                    let opaque = self.opaque(span.unwrap_or_else(|| self.ctx.tys.get_span(ty)));
                    TyInfo::Effect(eff, self.instantiate(out, span, f, self_ty), opaque)
                },
            },
        };
        self.insert(span.unwrap_or_else(|| self.ctx.tys.get_span(ty)), info)
    }

    pub fn unknown(&mut self, span: Span) -> TyVar {
        self.insert(span, TyInfo::Unknown(None))
    }

    pub fn opaque(&mut self, span: Span) -> TyVar {
        let ty = self.insert(span, TyInfo::Opaque(self.opaque_id));
        self.opaque_id += 1;
        ty
    }

    pub fn make_access(&mut self, record: TyVar, field_name: SrcNode<Ident>, field: TyVar) {
        self.constraints.push_back(Constraint::Access(record, field_name, field));
    }

    pub fn make_update(&mut self, record: TyVar, field_name: SrcNode<Ident>, field: TyVar) {
        self.constraints.push_back(Constraint::Update(record, field_name, field));
    }

    // `unchecked_assoc` allows unification of type variables with an instance's associated type
    pub fn make_impl(&mut self, ty: TyVar, (class_id, args): (ClassId, Vec<TyVar>), obl_span: Span, unchecked_assoc: Vec<(SrcNode<Ident>, TyVar)>, use_span: Span) {
        let class = ClassVar(self.class_vars.len());
        self.class_vars.push((use_span, ClassInfo::Known(class_id, args)));
        self.constraints.push_back(Constraint::Impl(ty, class, obl_span, unchecked_assoc, use_span));
    }

    fn instantiate_class(&mut self, class_id: ClassId, inst_span: Span, self_ty: Option<TyVar>) -> Vec<TyVar> {
        let gen_scope_id = self.ctx.classes.class_gen_scope(class_id);
        let gen_scope = self.ctx.tys.get_gen_scope(gen_scope_id);
        let generic_tys = (0..gen_scope.len())
            .map(|i| gen_scope.get(i).name.span())
            .collect::<Vec<_>>()
            .into_iter()
            .map(|origin| self.insert(inst_span, TyInfo::Unknown(Some(origin))))
            .collect::<Vec<_>>();

        // TODO: Move this function?
        lower::enforce_generic_obligations(
            self,
            gen_scope_id,
            &generic_tys,
            inst_span,
            self.ctx.classes.get(class_id).name.span(),
            self_ty,
        ).expect("Wrong number of generic params");

        generic_tys
    }

    pub fn insert_class(&mut self, span: Span, class: ClassInfo) -> ClassVar {
        let id = ClassVar(self.class_vars.len());
        self.class_vars.push((span, class));
        id
    }

    pub fn make_class_field_known(&mut self, ty: TyVar, field_name: SrcNode<Ident>, (class_id, args): (ClassId, Vec<TyVar>), field_ty: TyVar, span: Span) -> ClassVar {
        let class = self.insert_class(span, ClassInfo::Known(class_id, args));
        self.constraints.push_back(Constraint::ClassField(ty, class, field_name, field_ty, span));
        class
    }

    pub fn make_class_field(&mut self, ty: TyVar, field_name: SrcNode<Ident>, field_ty: TyVar, span: Span) -> ClassVar {
        let class = self.class_var_unknown(span);
        self.constraints.push_back(Constraint::ClassField(ty, class, field_name, field_ty, span));
        class
    }

    pub fn class_var_unknown(&mut self, span: Span) -> ClassVar {
        self.insert_class(span, ClassInfo::Unknown)
    }

    pub fn make_class_assoc(&mut self, ty: TyVar, assoc_name: SrcNode<Ident>, class: ClassVar, assoc_ty: TyVar, span: Span) {
        self.constraints.push_back(Constraint::ClassAssoc(ty, class, assoc_name, assoc_ty, span));
    }

    fn follow_class(&self, class: ClassVar) -> ClassInfo {
        match &self.class_vars[class.0].1 {
            ClassInfo::Ref(class) => self.follow_class(*class),
            info => info.clone(),
        }
    }

    fn effect_span(&self, eff: EffectVar) -> Span {
        self.effect_vars[eff.0].0
    }

    fn iter_effects(&self) -> impl Iterator<Item = (EffectVar, EffectInfo)> + '_ {
        (0..self.effect_vars.len())
            .map(|i| (EffectVar(i), self.effect_vars[i].1.clone()))
    }

    fn follow_effect(&self, eff: EffectVar) -> EffectInfo {
        match &self.effect_vars[eff.0].1 {
            EffectInfo::Ref(eff) => self.follow_effect(*eff),
            info => info.clone(),
        }
    }

    pub fn insert_effect(&mut self, span: Span, eff: EffectInfo) -> EffectVar {
        let id = EffectVar(self.effect_vars.len());
        self.effect_vars.push((span, eff));
        id
    }

    pub fn unknown_effect(&mut self, span: Span) -> EffectVar {
        self.insert_effect(span, EffectInfo::Unknown)
    }

    pub fn make_effect_send_recv(&mut self, eff: EffectVar, send: TyVar, recv: TyVar, span: Span) {
        self.constraints.push_back(Constraint::EffectSendRecv(eff, send, recv, span));
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
                | TyInfo::Opaque(_)
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
                TyInfo::Assoc(inner, class, _) => x == inner
                    || self.occurs_in_inner(x, inner, seen)
                    || match self.follow_class(class) {
                        ClassInfo::Ref(_) => unreachable!(),
                        ClassInfo::Unknown => false,
                        ClassInfo::Known(_, args) => args
                            .into_iter()
                            .any(|y| x == y || self.occurs_in_inner(x, y, seen)),
                    },
                // Opaque type is not checked, it's always opaque... hopefully
                TyInfo::Effect(eff, out, _opaque) => self.occurs_in_inner(x, out, seen) || match self.follow_effect(eff) {
                    EffectInfo::Unknown => false,
                    EffectInfo::Ref(_) => unreachable!(),
                    EffectInfo::Known(_, params) => params
                        .into_iter()
                        .any(|y| x == y || self.occurs_in_inner(x, y, seen)),
                },
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

        // TODO: Allow errors that mention effects instead of types
        fn make_flow_effect(
            infer: &mut Infer,
            (x, x_ty): (EffectVar, TyVar),
            (y, y_ty): (EffectVar, TyVar),
        ) -> Result<(), (TyVar, TyVar)> {
            match (infer.follow_effect(x), infer.follow_effect(y)) {
                // TODO: These shouldn't be reachable?
                (EffectInfo::Ref(x), _) => make_flow_effect(infer, (x, x_ty), (y, y_ty)),
                (_, EffectInfo::Ref(y)) => make_flow_effect(infer, (x, x_ty), (y, y_ty)),

                (EffectInfo::Unknown, _) => Ok(infer.effect_vars[x.0].1 = EffectInfo::Ref(y)),
                (_, EffectInfo::Unknown) => Ok(infer.effect_vars[y.0].1 = EffectInfo::Ref(x)),
                (EffectInfo::Known(x, xs), EffectInfo::Known(y, ys)) if x == y => {
                    // TODO: Unnecessarily conservative, variance of effect generics should be determined
                    let co_error = make_flow_many(infer, xs.iter().copied(), ys.iter().copied()).err();
                    let contra_error = make_flow_many(infer, ys, xs).err().map(|(a, b)| (b, a));
                    co_error.or(contra_error).map(Err).unwrap_or(Ok(()))
                },
                (_, _) => Err((x_ty, y_ty)),
            }
        }

        // TODO: Allow errors that mention classes instead of types
        fn make_flow_class(
            infer: &mut Infer,
            (x, x_ty): (ClassVar, TyVar),
            (y, y_ty): (ClassVar, TyVar),
        ) -> Result<(), (TyVar, TyVar)> {
            match (infer.follow_class(x), infer.follow_class(y)) {
                // TODO: These shouldn't be reachable?
                (ClassInfo::Ref(x), _) => make_flow_class(infer, (x, x_ty), (y, y_ty)),
                (_, ClassInfo::Ref(y)) => make_flow_class(infer, (x, x_ty), (y, y_ty)),

                (ClassInfo::Unknown, _) => Ok(infer.class_vars[x.0].1 = ClassInfo::Ref(y)),
                (_, ClassInfo::Unknown) => Ok(infer.class_vars[y.0].1 = ClassInfo::Ref(x)),
                (ClassInfo::Known(class_id_x, xs), ClassInfo::Known(class_id_y, ys)) if class_id_x == class_id_y => {
                    // Class generics are always invariant
                    let co_error = make_flow_many(infer, xs.iter().copied(), ys.iter().copied()).err();
                    let contra_error = make_flow_many(infer, ys, xs).err().map(|(a, b)| (b, a));
                    co_error.or(contra_error).map(Err).unwrap_or(Ok(()))
                },
                (_, _) => Err((x_ty, y_ty)),
            }
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
                if assoc_x == assoc_y => {
                    // associated types are invariant
                    let co_error = self.make_flow_inner(x, y).err();
                    let contra_error = self.make_flow_inner(y, x).err().map(|(a, b)| (b, a));

                    let class_err = make_flow_class(self, (class_x, x), (class_y, y)).err();

                    co_error
                        .or(contra_error)
                        .or(class_err)
                        .map(Err)
                        .unwrap_or(Ok(()))
                },
            (TyInfo::Effect(x_eff, x_out, x_opaque), TyInfo::Effect(y_eff, y_out, y_opaque)) => {
                let eff_err = make_flow_effect(self, (x_eff, x), (y_eff, y)).err();
                let o_err = self.make_flow_inner(x_out, y_out).err();
                let opaque_err = self.make_flow_inner(x_opaque, y_opaque).err().map(|_| (x, y));
                o_err.or(eff_err).or(opaque_err).map(Err).unwrap_or(Ok(()))
            },
            (TyInfo::Opaque(x_id), TyInfo::Opaque(y_id)) if x_id == y_id => Ok(()),
            (_, _) => Err((x, y)),
        }
    }

    /// Reinstantiate a type variable, replacing any known generic types with new unknown ones
    // TODO: Is this a good way to resolve the problem of type inference of recursive definitions in the presence of
    // polymorphism?
    pub fn reinstantiate(&mut self, span: Span, ty: TyVar) -> TyVar {
        match self.info(ty) {
            TyInfo::Ref(x) => self.reinstantiate(span, x),
            TyInfo::Error(reason) => self.insert(self.span(ty), TyInfo::Error(reason)),
            TyInfo::Opaque(_) => self.opaque(span),
            TyInfo::Unknown(_) | TyInfo::Prim(_) => ty,
            TyInfo::List(item) => {
                let item = self.reinstantiate(span, item);
                self.insert(self.span(ty), TyInfo::List(item))
            },
            TyInfo::Func(i, o) => {
                let i = self.reinstantiate(span, i);
                let o = self.reinstantiate(span, o);
                self.insert(self.span(ty), TyInfo::Func(i, o))
            },
            // TODO: Reinstantiate type parameters with fresh type variables, but without creating inference problems
            // TODO: Is this even correct?
            TyInfo::Gen(x, _, origin) => ty,//self.insert(span, TyInfo::Unknown(Some(origin))),
            TyInfo::Tuple(fields) => {
                let fields = fields
                    .into_iter()
                    .map(|field| self.reinstantiate(span, field))
                    .collect();
                self.insert(self.span(ty), TyInfo::Tuple(fields))
            },
            TyInfo::Record(fields) => {
                let fields = fields
                    .into_iter()
                    .map(|(name, field)| (name, self.reinstantiate(span, field)))
                    .collect();
                self.insert(self.span(ty), TyInfo::Record(fields))
            },
            TyInfo::Data(data, args) => {
                let args = args
                    .into_iter()
                    .map(|arg| self.reinstantiate(span, arg))
                    .collect();
                self.insert(self.span(ty), TyInfo::Data(data, args))
            },
            TyInfo::SelfType => todo!(), // ???
            TyInfo::Assoc(inner, class, assoc) => {
                let class = match self.follow_class(class) {
                    ClassInfo::Unknown => class,
                    ClassInfo::Ref(_) => unreachable!(),
                    ClassInfo::Known(class_id, args) => {
                        let args = args
                            .into_iter()
                            .map(|arg| self.reinstantiate(span, arg))
                            .collect();
                        self.insert_class(self.span(ty), ClassInfo::Known(class_id, args))
                    },
                };
                let inner = self.reinstantiate(span, inner);
                self.insert(self.span(ty), TyInfo::Assoc(inner, class, assoc))
            },
            TyInfo::Effect(eff, out, opaque) => {
                let eff = match self.follow_effect(eff) {
                    EffectInfo::Unknown => eff,
                    EffectInfo::Ref(_) => unreachable!(), // `follow_effect` shouldn't ever return Ref
                    EffectInfo::Known(eff, args) => {
                        let args = args
                            .into_iter()
                            .map(|arg| self.reinstantiate(span, arg))
                            .collect();
                        self.insert_effect(self.span(ty), EffectInfo::Known(eff, args))
                    },
                };
                let out = self.reinstantiate(span, out);
                let opaque = self.reinstantiate(span, opaque);
                self.insert(self.span(ty), TyInfo::Effect(eff, out, opaque))
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
                            let field_ty = self.instantiate(*field_ty, self.span(record), &|index, _, _| params.get(index).copied(), Some(record));
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
            Constraint::Impl(ty, class, obl_span, unchecked_assoc, use_span) => {
                if let ClassInfo::Known(class_id, args) = self.follow_class(class) {
                    self.resolve_obligation(&mut Vec::new(), ty, (class_id, args.clone()), unchecked_assoc.clone(), obl_span, use_span)
                        .map(|res| match res {
                            Ok(member) => {
                                for (assoc, assoc_ty) in unchecked_assoc {
                                    match member {
                                        Ok(ImpliedItems::Real(member)) => {
                                            let member = self.ctx.classes.get_member(member);

                                            let mut links = HashMap::new();
                                            self.derive_links(member.member, ty, &mut |gen_idx, var| { links.insert(gen_idx, var); });
                                            for (member_arg, arg) in member.args.iter().zip(args.iter()) {
                                                self.derive_links(*member_arg, *arg, &mut |gen_idx, var| { links.insert(gen_idx, var); });
                                            }

                                            if let Some(member_assoc_ty) = member.assoc_ty(*assoc) {
                                                let assoc_ty_inst = self.instantiate(member_assoc_ty, obl_span, &|idx, gen_scope, ctx| links.get(&idx).copied(), Some(ty));
                                                // TODO: Check ordering for soundness
                                                self.make_flow(assoc_ty_inst, assoc_ty, obl_span);
                                            }
                                        },
                                        Ok(ImpliedItems::Eq(ref assoc_set)) => {
                                            if let Some((name, ty)) = assoc_set.iter().find(|(name, _)| **name == *assoc) {
                                                self.make_flow(*ty, assoc_ty, name.span());
                                            } else {
                                                let assoc_info = self.insert(obl_span, TyInfo::Assoc(ty, class, assoc.clone()));
                                                // TODO: Check ordering for soundness
                                                self.make_flow(assoc_info, assoc_ty, obl_span);
                                            }
                                        },
                                        Err(()) => {
                                            // Errors propagate through projected associated types
                                            self.set_error(assoc_ty);
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
                        })
                } else {
                    None
                }
            },
            Constraint::ClassField(ty, class, field, field_ty, span) => self.try_resolve_class_from_field(ty, class, field.clone(), field_ty, span),
            Constraint::ClassAssoc(ty, class, assoc, assoc_ty, span) => self.try_resolve_class_from_assoc(ty, class, assoc.clone(), assoc_ty, span),
            Constraint::EffectSendRecv(eff, send, recv, span) => match self.follow_effect(eff) {
                EffectInfo::Unknown => None,
                EffectInfo::Ref(_) => unreachable!(),
                EffectInfo::Known(decl, args) => {
                    let send_ty = self.instantiate(
                        self.ctx.effects.get_decl(decl).send.expect("Send must be init"),
                        span,
                        &|idx, _gen_scope, _ctx| args.get(idx).copied(),
                        None,
                    );
                    self.make_flow(send, send_ty, span);
                    let recv_ty = self.instantiate(
                        self.ctx.effects.get_decl(decl).recv.expect("Recv must be init"),
                        span,
                        &|idx, _gen_scope, _ctx| args.get(idx).copied(),
                        None,
                    );
                    // TODO: The variance here is a bit fucked, this needs to swap when we're
                    // inferring flow into (from a handler) vs flow out of (via a propagation)
                    self.make_flow(recv, recv_ty, span);
                    Some(Ok(()))
                },
            },
        }
    }

    fn try_resolve_class_from_assoc(&mut self, ty: TyVar, class_var: ClassVar, assoc: SrcNode<Ident>, assoc_ty: TyVar, span: Span) -> Option<Result<(), InferError>> {
        let (class_id, args) = match self.select_member_from_item(ty, class_var, assoc.clone(), assoc_ty, span, true) {
            Some(Ok((class_id, args))) => (class_id, args),
            Some(Err(err)) => return Some(Err(err)),
            None => return None,
        };

        self.class_vars[class_var.0].1 = ClassInfo::Known(class_id, args.clone());

        // Require an implementation to exist
        self.make_impl(ty, (class_id, args), span, vec![
            (assoc, assoc_ty),
        ], span);

        Some(Ok(()))
    }

    fn try_resolve_class_from_field(&mut self, ty: TyVar, class_var: ClassVar, field: SrcNode<Ident>, field_ty: TyVar, span: Span) -> Option<Result<(), InferError>> {
        let (class_id, args) = match self.select_member_from_item(ty, class_var, field.clone(), field_ty, span, false) {
            Some(Ok((class_id, args))) => (class_id, args),
            Some(Err(err)) => return Some(Err(err)),
            None => return None,
        };

        self.class_vars[class_var.0].1 = ClassInfo::Known(class_id, args.clone());

        self.make_impl(ty, (class_id, args.clone()), span, Vec::new(), span);
        let field_ty_id = **self.ctx.classes
            .get(class_id)
            .field(*field)
            .unwrap();
        let inst_field_ty = self.instantiate(field_ty_id, field.span(), &|idx, _, _| args.get(idx).copied(), Some(ty));

        // TODO: Check soundness of flow relationship
        self.make_flow(inst_field_ty, field_ty, field.span());
        Some(Ok(()))
    }

    fn select_member_from_item(&mut self, ty: TyVar, class_var: ClassVar, item: SrcNode<Ident>, item_ty: TyVar, span: Span, is_assoc: bool) -> Option<Result<(ClassId, Vec<TyVar>), InferError>> {
        if let ClassInfo::Known(class_id, args) = self.follow_class(class_var) {
            Some(Ok((class_id, args)))
        } else {
            let Some((implied_candidates, external_candidates)) = self.find_class_candidates_from_item(ty, item.clone(), item_ty, is_assoc)
                else { return None }; // Resolving an error type always succeeds;

            match (implied_candidates.len(), external_candidates.len()) {
                // Easy case: we found exactly 1 candidate implied by the gen scope, so we know it much be the one we're looking for.
                (1, _) => {
                    // Can't fail
                    let member = implied_candidates.into_iter().next().unwrap();
                    Some(Ok((*member.class, member.args)))
                },
                // Exactly one external candidate was found, so instantiate it
                (0, 1) => {
                    // Can't fail
                    let class_id = external_candidates.into_iter().next().unwrap();
                    // TODO: Should we make use of member information? external_candidates should probably be HashSet<MemberId>
                    Some(Ok((class_id, self.instantiate_class(class_id, item.span(), Some(ty)))))
                },
                // No implied or external candidates were found, so we can't resolve the member
                (0, 0) => {
                    // No external candidates match either, so bail
                    // TODO: Should this really generate an error? It might be that we just need more info
                    self.set_error(item_ty);
                    Some(Err(InferError::NoSuchItem(ty, span, item)))
                    // None
                },
                (_, _) => {
                    self.set_error(item_ty);
                    let possible_classes = implied_candidates
                        .into_iter()
                        .map(|member| *member.class)
                        .chain(external_candidates
                            .into_iter())
                        .collect();
                    Some(Err(InferError::AmbiguousClassItem(item, possible_classes)))
                },
            }
        }
    }

    fn find_class_candidates_from_item(&mut self, ty: TyVar, item: SrcNode<Ident>, item_ty: TyVar, is_assoc: bool) -> Option<(Vec<InferImpliedMember>, HashSet<ClassId>)> {
        let mut implied_candidates = HashMap::new();
        for member in self.implied_members
            .iter()
            .filter(|member| if is_assoc {
                self.ctx.classes.get(*member.class).assoc_ty(*item).is_some()
            } else {
                self.ctx.classes.get(*member.class).field(*item).is_some()
            })
        {
            if self.var_covers_var(ty, *member.member)? {
                // TODO: Only replace if the new member has 'better' information than the previous one
                implied_candidates.insert(*member.class, member);
            }
        }
        let implied_candidates = implied_candidates
            .values()
            .cloned()
            .cloned()
            .collect();

        let mut external_candidates = HashSet::new();
        for (class_id, class) in self.ctx.classes
            .iter()
            // Filter by classes that contain the given field
            .filter(|(_, class)| if is_assoc {
                class.assoc_ty(*item).is_some()
            } else {
                class.field(*item).is_some()
            })
        {
            let mut covers = false;
            // Filter further by classes that have members that cover our type
            for (_, member) in self.ctx.classes.members_of(class_id) {
                if Self::covers_var(self, ty, member.member)? {
                    covers = true;
                }
            }
            if covers {
                external_candidates.insert(class_id);
            }
        }

        Some((implied_candidates, external_candidates))
    }

    // Returns true if ty covers var (i.e: var is a structural subset of ty)
    // Return None if we don't yet have information to determine whether covering occurs
    // TODO: Flip arg order
    fn var_covers_var(&self, var: TyVar, ty: TyVar) -> Option<bool> {
        match (self.follow_info(var), self.follow_info(ty)) {
            (TyInfo::Unknown(_), _) => None,
            (_, TyInfo::Unknown(_)) => Some(false),
            (TyInfo::Gen(x, _, _), TyInfo::Gen(y, _, _)) if x == y => Some(true),
            (TyInfo::Prim(x), TyInfo::Prim(y)) if x == y => Some(true),
            (TyInfo::List(x), TyInfo::List(y)) => self.var_covers_var(x, y),
            (TyInfo::Tuple(xs), TyInfo::Tuple(ys)) if xs.len() == ys.len() => xs
                .into_iter()
                .zip(ys.into_iter())
                .fold(Some(true), |a, (x, y)| Some(a? && self.var_covers_var(x, y)?)),
            (TyInfo::Record(xs), TyInfo::Record(ys)) if xs.len() == ys.len() => xs
                .into_iter()
                .zip(ys.into_iter())
                .fold(Some(true), |a, (x, y)| Some(a? && self.var_covers_var(x.1, y.1)?)),
            (TyInfo::Func(x_i, x_o), TyInfo::Func(y_i, y_o)) => {
                Some(self.var_covers_var(x_i, y_i)? && self.var_covers_var(x_o, y_o)?)
            },
            (TyInfo::Data(x, xs), TyInfo::Data(y, ys)) if x == y && xs.len() == ys.len() => xs
                .into_iter()
                .zip(ys.into_iter())
                .fold(Some(true), |a, (x, y)| Some(a? && self.var_covers_var(x, y)?)),
            (TyInfo::Assoc(x, class_x, assoc_x), TyInfo::Assoc(y, class_y, assoc_y))
                if assoc_x == assoc_y => Some(self.var_covers_var(x, y)? && self.class_covers_class(class_x, class_y)?),
            (TyInfo::SelfType, TyInfo::SelfType) => Some(true),
            (_, _) => Some(false),
        }
    }

    fn class_covers_class(&self, x: ClassVar, y: ClassVar) -> Option<bool> {
        match (self.follow_class(x), self.follow_class(y)) {
            (ClassInfo::Unknown, _) => Some(false),
            (_, ClassInfo::Unknown) => None,
            (ClassInfo::Ref(_), _) | (_, ClassInfo::Ref(_)) => unreachable!(),
            (ClassInfo::Known(class_id_x, args_x), ClassInfo::Known(class_id_y, args_y)) if class_id_x == class_id_y => args_x
                .into_iter()
                .zip(args_y.into_iter())
                .fold(Some(true), |a, (x, y)| Some(a? && self.var_covers_var(x, y)?)),
            (_, _) => Some(false),
        }
    }

    // Returns true if ty covers var (i.e: var is a structural subset of ty)
    // TODO: Flip arg order
    fn covers_var(&self, var: TyVar, ty: TyId) -> Option<bool> {
        match (self.follow_info(var), self.ctx.tys.get(ty)) {
            (TyInfo::Unknown(_), _) => None,
            (_, Ty::Gen(_, _)) => Some(true), // Blanket impls match everything
            (TyInfo::Prim(x), Ty::Prim(y)) if x == y => Some(true),
            (TyInfo::List(x), Ty::List(y)) => self.covers_var(x, y),
            (TyInfo::Tuple(xs), Ty::Tuple(ys)) if xs.len() == ys.len() => xs
                .into_iter()
                .zip(ys.into_iter())
                .fold(Some(true), |a, (x, y)| Some(a? && self.covers_var(x, y)?)),
            (TyInfo::Record(xs), Ty::Record(ys)) if xs.len() == ys.len() => xs
                .into_iter()
                .zip(ys.into_iter())
                .fold(Some(true), |a, ((_, x), (_, y))| Some(a? && self.covers_var(x, y)?)),
            (TyInfo::Func(x_i, x_o), Ty::Func(y_i, y_o)) => {
                Some(self.covers_var(x_i, y_i)? && self.covers_var(x_o, y_o)?)
            },
            (TyInfo::Data(x, xs), Ty::Data(y, ys)) if x == y && xs.len() == ys.len() => xs
                .into_iter()
                .zip(ys.into_iter())
                .fold(Some(true), |a, (x, y)| Some(a? && self.covers_var(x, y)?)),
            (TyInfo::Assoc(x, class_x, assoc_x), Ty::Assoc(y, (class_id_y, args_y), assoc_y))
                if assoc_x == assoc_y => Some(self.covers_var(x, y)? && match self.follow_class(class_x) {
                    ClassInfo::Ref(_) => unreachable!(),
                    ClassInfo::Unknown => None?, // TODO: correct?
                    ClassInfo::Known(class_id_x, args_x) => class_id_x == class_id_y && args_x
                        .into_iter()
                        .zip(args_y.into_iter())
                        .fold(Some(true), |a, (x, y)| Some(a? && self.covers_var(x, y)?))?,
                }),
            _ => Some(false),
        }
    }

    // Link the generic types of a class member with type variables in the current scope so obligations
    // can be generated
    // TODO: Derive links for effects
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
            (Ty::Assoc(x, (class_id_x, args_x), assoc_x), TyInfo::Assoc(y, class_y, assoc_y))
                if assoc_x == assoc_y => {
                    self.derive_links(x, y, link_gen);
                    match self.follow_class(class_y) {
                        ClassInfo::Unknown => panic!("Deriving links for unknown class!"),
                        ClassInfo::Ref(_) => unreachable!(),
                        ClassInfo::Known(class_id_y, args_y) => {
                            assert!(class_id_x == class_id_y);
                            args_x
                                .into_iter()
                                .zip(args_y.into_iter())
                                .for_each(|(x, y)| self.derive_links(x, y, link_gen));
                        },
                    }
                },
            (_, TyInfo::SelfType) => panic!("Self type not permitted here"),
            _ => {}, // Only type constructors and generic types generate obligations
        }
    }

    /// Resolve a class obligation for a type, returning the ID of the type's membership. If no member can be provided
    /// (because, for example, the membership is implied by a generic bound) then `Err(false)` is returned instead. If
    /// resolution failed due to an existing error, `Err(true)` is returned.
    fn resolve_obligation(
        &mut self,
        proof_stack: &mut Vec<(TyVar, ClassId, Vec<TyVar>)>,
        ty: TyVar,
        (class_id, class_args): (ClassId, Vec<TyVar>),
        assoc: Vec<(SrcNode<Ident>, TyVar)>,
        obl_span: Span,
        use_span: Span,
    ) -> Option<Result<Result<InferImpliedItems, ()>, InferError>> {
        // TODO: Resolve possible infinite loop when resolving by having an obligation cache
        match self.follow_info(ty) {
            TyInfo::Error(_) => {
                self.set_error(ty);
                Some(Ok(Err(()))) // Resolving an error type always succeeds
            },
            // First, search through real members to resolve the obligation
            info => {
                // If searching through real obligations failed, search through implied obligations
                if let Some(member) = {
                    let mut selected = None;
                    for member in self.implied_members.iter() {
                        if class_id == *member.class
                            && self.var_covers_var(ty, *member.member)?
                            && class_args.iter()
                                .zip(member.args.iter())
                                .fold(Some(true), |a, (ty, arg)| Some(a? && Self::var_covers_var(self, *ty, *arg)?))?
                        {
                            // Try to select an implied member where the real member is known
                            // TODO: Is multiple covering impls an error? Should coherence prevent this case?
                            selected = Some(selected
                                .zip_with(Some(member), |s: &InferImpliedMember, m| if matches!(&s.items, ImpliedItems::Real(_)) {
                                    s
                                } else {
                                    m
                                })
                                .unwrap_or(member));
                        }
                    }
                    selected
                } {
                    let items = member.items.clone();

                    // Check constrained associated types
                    let class = self.insert_class(obl_span, ClassInfo::Known(class_id, class_args.clone()));
                    if let ImpliedItems::Eq(assoc_set) = &items {
                        for (assoc, assoc_ty) in assoc {
                            if let Some((name, ty)) = assoc_set.iter().find(|(name, _)| **name == *assoc) {
                                self.make_flow(*ty, assoc_ty, name.span());
                            }
                        }
                    }

                    Some(Ok(Ok(items)))
                } else {
                    // Find a class member declaration that covers our type
                    let mut covering_members = Vec::new();
                    for (member_id, member) in self.ctx.classes.members_of(class_id) {
                        // println!("Checking coverage of {:?} by {} via member {}", self.follow_info(ty), *self.ctx.classes.get(class_id).name, self.ctx.tys.display(&self.ctx, member.member));
                        let covers = Self::covers_var(self, ty, member.member)?
                        && class_args.iter()
                            .zip(member.args.iter())
                            .fold(Some(true), |a, (ty, arg)| Some(a? && Self::covers_var(self, *ty, *arg)?))?;
                        // println!("Result = {}", covers);
                        if covers {
                            covering_members.push((member_id, member));
                        }
                    }

                    match covering_members.len() {
                        // No covering members (perhaps we need more information?
                        0 => {
                            // println!(
                            //     "Failed to solve {:?} as member of {}{}!",
                            //     self.follow_info(ty),
                            //     *self.ctx.classes.get(class_id).name,
                            //     class_args.iter().map(|arg| format!(" {:?}", self.follow_info(*arg))).collect::<String>(),
                            // );
                            let gen_span = if let TyInfo::Gen(gen_idx, gen_scope, _) = self.follow_info(ty) {
                                Some(self.ctx.tys.get_gen_scope(gen_scope).get(gen_idx).name.span())
                            } else {
                                None
                            };
                            Some(Err(InferError::TypeDoesNotFulfil(self.insert_class(obl_span, ClassInfo::Known(class_id, class_args)), ty, obl_span, gen_span, use_span)))
                            // None
                        },
                        // Exactly one covering member: great, we know what to substitute!
                        1 => {
                            // Can't fail
                            let (covering_member_id, covering_member) = covering_members.into_iter().next().unwrap();

                            let gen_scope = self.ctx.tys.get_gen_scope(covering_member.gen_scope);

                            let mut links = HashMap::new();
                            self.derive_links(covering_member.member, ty, &mut |gen_idx, var| { links.insert(gen_idx, var); });
                            for (member_arg, arg) in covering_member.args.iter().zip(class_args.iter()) {
                                self.derive_links(*member_arg, *arg, &mut |gen_idx, var| { links.insert(gen_idx, var); });
                            }

                            for member in gen_scope.implied_members.as_ref().expect("Implied members must be known here").clone() {
                                let member_ty = self.instantiate(*member.member, member.member.span(), &|idx, _, _| links.get(&idx).copied(), Some(ty));
                                let member_args = member.args
                                    .iter()
                                    .map(|arg| self.instantiate(*arg, member.member.span(), &|idx, _, _| links.get(&idx).copied(), Some(ty)))
                                    .collect::<Vec<_>>();

                                let member_assoc = match &member.items {
                                    ImpliedItems::Eq(assoc) => assoc
                                        .iter()
                                        .map(|(name, assoc)| (name.clone(), self.instantiate(*assoc, member.member.span(), &|idx, _, _| links.get(&idx).copied(), Some(ty))))
                                        .collect::<Vec<_>>(),
                                    ImpliedItems::Real(_) => Vec::new(),
                                };

                                if proof_stack.contains(&(member_ty, *member.class, class_args.clone())) {
                                    return Some(Err(InferError::CycleWhenResolving(ty, (class_id, class_args.clone()), member.span())));
                                } else {
                                    // println!(
                                    //     "Trying to prove that {:?} is a member of {}{}!",
                                    //     self.follow_info(member_ty),
                                    //     *self.ctx.classes.get(*member.class).name,
                                    //     member_args.iter().map(|arg| format!(" {:?}", self.follow_info(*arg))).collect::<String>(),
                                    // );
                                    proof_stack.push((member_ty, *member.class, class_args.clone()));
                                    let res = self.resolve_obligation(proof_stack, member_ty, (*member.class, member_args.clone()), member_assoc, member.class.span(), use_span);
                                    // if matches!(res, Some(Ok(_))) {
                                    //     println!("Successfully proved!");
                                    // } else if matches!(res, Some(Err(_))) {
                                    //     println!("Failed to prove!");
                                    // } else {
                                    //     println!("Not enough information to prove...");
                                    // }
                                    proof_stack.pop();
                                    match res {
                                        Some(Ok(Ok(_))) => {},
                                        Some(Ok(Err(()))) => return Some(Ok(Err(()))),
                                        Some(Err(err)) => return Some(Err(err)),
                                        None => {
                                            // Not enough information to resolve it yet? That's okay! Take it as a win
                                            // for now, but require that this gets solved later!
                                            self.make_impl(member_ty, (*member.class, member_args), member.class.span(), vec![], use_span);
                                        },
                                    }
                                }
                            }

                            Some(Ok(Ok(ImpliedItems::Real(covering_member_id))))
                        },
                        // Multiple covering members: we don't know which one to pick!
                        // TODO: Instead of bailing out, perhaps try each one in turn?
                        // TODO: Should the coherence checker prevent this?
                        _ => {
                            // println!("Incoherence! This is probably bad!");
                            None
                        },
                    }
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

        // Report errors for effects that cannot be inferred
        let effects = self.iter_effects().collect::<Vec<_>>();
        for (eff, info) in effects {
            if let EffectInfo::Unknown = info {
                errors.push(InferError::CannotInferEffect(eff));
            }
        }

        // Generate errors for all remaining constraints
        for c in std::mem::take(&mut self.constraints) {
            match c {
                Constraint::Access(record, field_name, _field) => {
                    errors.push(InferError::NoSuchItem(record, self.span(record), field_name.clone()))
                },
                Constraint::Update(record, field_name, _field) => {
                    errors.push(InferError::NoSuchItem(record, self.span(record), field_name.clone()))
                },
                Constraint::Impl(ty, class, obl_span, assoc, use_span) => {
                    let gen_span = if let TyInfo::Gen(gen_idx, gen_scope, _) = self.follow_info(ty) {
                        Some(self.ctx.tys.get_gen_scope(gen_scope).get(gen_idx).name.span())
                    } else {
                        None
                    };
                    // Propagate errors to associated types to avoid spurious errors
                    for (_, ty) in assoc {
                        self.set_error(ty);
                    }
                    errors.push(InferError::TypeDoesNotFulfil(class, ty, obl_span, gen_span, use_span));
                },
                Constraint::ClassField(_ty, _class, field, _field_ty, _span) => {
                    errors.push(InferError::AmbiguousClassItem(field, Vec::new()))
                },
                Constraint::ClassAssoc(_ty, _class, assoc, _assoc_ty, _span) => {
                    errors.push(InferError::AmbiguousClassItem(assoc, Vec::new()))
                },
                Constraint::EffectSendRecv(eff, send, recv, span) => errors.push(InferError::CannotInferEffect(eff)),
            }
        }

        let mut checked = Checked {
            ty_cache: HashMap::default(),
            eff_cache: HashMap::default(),
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
                InferError::CannotInferEffect(eff) => Error::CannotInferEffect(checked.reify_effect(eff)),
                InferError::Recursive(a, part) => Error::Recursive(checked.reify(a), checked.infer.span(a), checked.infer.span(part)),
                InferError::NoSuchItem(a, record_span, field) => Error::NoSuchItem(checked.reify(a), record_span, field),
                InferError::NoSuchField(a, record_span, field) => Error::NoSuchField(checked.reify(a), record_span, field),
                InferError::TypeDoesNotFulfil(class, ty, obl_span, gen_span, use_span) => {
                    let class = match checked.infer.follow_class(class) {
                        ClassInfo::Unknown => None,
                        ClassInfo::Ref(_) => unreachable!(),
                        ClassInfo::Known(class_id, args) => Some((class_id, args
                            .iter()
                            .map(|arg| checked.reify(*arg))
                            .collect())),
                    };
                    Error::TypeDoesNotFulfil(class, checked.reify(ty), obl_span, gen_span, use_span)
                },
                InferError::RecursiveAlias(alias, a, span) => Error::RecursiveAlias(alias, checked.reify(a), span),
                InferError::PatternNotSupported(lhs, op, rhs, span) => Error::PatternNotSupported(checked.reify(lhs), op, checked.reify(rhs), span),
                InferError::AmbiguousClassItem(field, candidate_classes) => Error::AmbiguousClassItem(field, candidate_classes),
                InferError::CycleWhenResolving(ty, (class_id, args), cycle_span) => Error::CycleWhenResolving(checked.reify(ty), (class_id, args
                    .iter()
                    .map(|arg| checked.reify(*arg))
                    .collect()), cycle_span),
            })
            .collect();

        (checked, errors)
    }
}

pub struct Checked<'a> {
    ty_cache: HashMap<TyVar, TyId>,
    eff_cache: HashMap<EffectVar, EffectId>,
    infer: Infer<'a>,
}

impl<'a> Checked<'a> {
    pub fn ctx(&self) -> &Context { &self.infer.ctx }
    pub fn ctx_mut(&mut self) -> &mut Context { &mut self.infer.ctx }

    fn reify_inner(&mut self, var: TyVar) -> TyId {
        if let Some(ty) = self.ty_cache.get(&var) {
            *ty
        } else {
            let ty = match self.infer.info(var) {
                // Follow references
                TyInfo::Ref(x) => return self.reify_inner(x),
                // Unknown types are treated as errors from here on out
                TyInfo::Unknown(_) => Ty::Error(ErrorReason::Unknown),
                // TODO: Not actual an error, but shouldn't appear in an actual type signature
                TyInfo::Opaque(_) => Ty::Error(ErrorReason::Unknown),
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
                TyInfo::Assoc(inner, class, assoc) => match self.infer.follow_class(class) {
                    ClassInfo::Unknown => Ty::Error(ErrorReason::Unknown),
                    ClassInfo::Ref(_) => unreachable!(),
                    ClassInfo::Known(class_id, args) => Ty::Assoc(self.reify_inner(inner), (class_id, args
                        .into_iter()
                        .map(|arg| self.reify_inner(arg))
                        .collect()), assoc),
                },
                TyInfo::Effect(eff, out, _opaque) => Ty::Effect(self.reify_effect(eff), self.reify_inner(out)),
            };
            self.infer.ctx.tys.insert(self.infer.span(var), ty)
        }
    }

    pub fn reify(&mut self, var: TyVar) -> TyId {
        let ty = self.reify_inner(var);
        self.ty_cache.insert(var, ty);
        ty
    }

    pub fn reify_effect(&mut self, var: EffectVar) -> EffectId {
        let eff = if let Some(eff) = self.eff_cache.get(&var) {
            *eff
        } else {
            let eff = match self.infer.follow_effect(var) {
                EffectInfo::Unknown => Effect::Error,
                EffectInfo::Ref(_) => unreachable!(),
                EffectInfo::Known(eff, args) => Effect::Known(eff, args
                    .into_iter()
                    .map(|arg| self.reify_inner(arg))
                    .collect()),
            };
            self.infer.ctx.tys.insert_effect(self.infer.effect_span(var), eff)
        };
        self.eff_cache.insert(var, eff);
        eff
    }

    pub fn reify_class(&mut self, class: ClassVar) -> Option<(ClassId, Vec<TyId>)> {
        match self.infer.follow_class(class) {
            ClassInfo::Unknown => None,
            ClassInfo::Ref(_) => unreachable!(),
            ClassInfo::Known(class_id, args) => Some((class_id, args
                .into_iter()
                .map(|arg| self.reify_inner(arg))
                .collect())),
        }
    }
}
