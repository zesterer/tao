use super::*;
use std::{
    cmp::Ordering,
    collections::VecDeque,
};

pub type InferMeta = (Span, TyVar);
pub type InferNode<T> = Node<T, InferMeta>;

#[derive(Clone, Debug, PartialEq)]
pub enum TyInfo {
    Ref(TyVar),
    Error(ErrorReason),
    Unknown(Option<Span>), // With optional instantiation origin
    Prim(ty::Prim),
    List(TyVar),
    // (_, is_tuple)
    Record(BTreeMap<Ident, TyVar>, bool),
    Func(TyVar, TyVar),
    Data(DataId, Vec<TyVar>),
    Gen(usize, GenScopeId, Span),
    SelfType,
    // An opaque associated type that *cannot* be determined due to lack of information
    Assoc(TyVar, ClassVar, SrcNode<Ident>),
    // Effect, output type, opaque type variable for tracking
    Effect(EffectVar, TyVar, TyVar),
    Opaque(usize, bool),
}

impl TyInfo {
    pub fn tuple(i: impl IntoIterator<Item = TyVar>) -> Self {
        Self::Record(i
            .into_iter()
            .enumerate()
            .map(|(i, ty)| (Ident::new(format!("{}", i)), ty))
            .collect(), true)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum EffectInfo {
    Ref(EffectVar),
    Open(Vec<EffectInstVar>),
    Closed(Vec<EffectInstVar>, Option<EffectVar>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum EffectInstInfo {
    Unknown,
    Error,
    Ref(EffectInstVar),
    Gen(usize, GenScopeId),
    Known(EffectDeclId, Vec<TyVar>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum ClassInfo {
    Unknown,
    Ref(ClassVar),
    Known(ClassId, Vec<TyVar>, Vec<EffectVar>),
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

trait FlowInfer<'a>: AsRef<Infer<'a>> {
    fn infer(&self) -> &Infer<'_>;
    fn is_check(&self) -> bool;
    fn set_info(&mut self, x: TyVar, info: TyInfo);
    fn set_effect_info(&mut self, x: EffectVar, info: EffectInfo);
    fn set_effect_inst_info(&mut self, x: EffectInstVar, info: EffectInstInfo);
    fn set_class_info(&mut self, x: ClassVar, info: ClassInfo);
    fn set_error(&mut self, x: TyVar);
    fn set_unknown_flow(&mut self, x: TyVar, y: TyVar);
    fn emit_error(&mut self, err: InferError);
    fn make_check_eff(&mut self, x: (EffectVar, TyVar), y: (EffectVar, TyVar)) -> bool;
    fn make_check_eff_empty(&mut self, x: (EffectVar, TyVar), other: TyVar) -> bool;
}

struct CheckFlow<'a, 'b>(&'b Infer<'a>, Option<bool>);
impl<'a, 'b> AsRef<Infer<'a>> for CheckFlow<'a, 'b> { fn as_ref(&self) -> &Infer<'a> { self.0 } }
impl<'a, 'b> FlowInfer<'a> for CheckFlow<'a, 'b> {
    fn infer(&self) -> &Infer<'_> { self.0 }
    fn is_check(&self) -> bool { true }
    fn set_info(&mut self, x: TyVar, info: TyInfo) {}
    fn set_effect_info(&mut self, x: EffectVar, info: EffectInfo) {}
    fn set_effect_inst_info(&mut self, x: EffectInstVar, info: EffectInstInfo) {}
    fn set_class_info(&mut self, x: ClassVar, info: ClassInfo) {}
    fn set_error(&mut self, x: TyVar) { self.1 = Some(false); }
    fn set_unknown_flow(&mut self, x: TyVar, y: TyVar) { if self.1 == Some(true) { self.1 = None; } }
    fn emit_error(&mut self, err: InferError) { self.1 = Some(false); }
    fn make_check_eff(&mut self, x: (EffectVar, TyVar), y: (EffectVar, TyVar)) -> bool { false }
    fn make_check_eff_empty(&mut self, x: (EffectVar, TyVar), other: TyVar) -> bool { false }
}

struct MakeFlow<'a, 'b>(&'b mut Infer<'a>, &'b EqInfo);
impl<'a, 'b> AsRef<Infer<'a>> for MakeFlow<'a, 'b> { fn as_ref(&self) -> &Infer<'a> { self.0 } }
impl<'a, 'b> FlowInfer<'a> for MakeFlow<'a, 'b> {
    fn infer(&self) -> &Infer<'_> { self.0 }
    fn is_check(&self) -> bool { false }
    fn set_info(&mut self, x: TyVar, info: TyInfo) { self.0.set_info(x, info) }
    fn set_effect_info(&mut self, x: EffectVar, info: EffectInfo) { self.0.effect_vars[x.0].1 = info; }
    fn set_effect_inst_info(&mut self, x: EffectInstVar, info: EffectInstInfo) { self.0.effect_inst_vars[x.0].1 = info; }
    fn set_class_info(&mut self, x: ClassVar, info: ClassInfo) { self.0.class_vars[x.0].1 = info; }
    fn set_error(&mut self, x: TyVar) {
        self.0.set_error(x);
    }
    fn set_unknown_flow(&mut self, x: TyVar, y: TyVar) {}
    fn emit_error(&mut self, err: InferError) {
        self.0.errors.push(err);
    }
    fn make_check_eff(&mut self, x: (EffectVar, TyVar), y: (EffectVar, TyVar)) -> bool {
        self.0.checks.push_back(Check::CheckFlowEffect(x, y, self.1.clone()));
        true
    }
    fn make_check_eff_empty(&mut self, x: (EffectVar, TyVar), other: TyVar) -> bool {
        self.0.checks.push_back(Check::CheckEffectEmpty(x, other, self.1.clone()));
        true
    }
}

#[derive(Debug)]
pub enum InferError {
    CannotCoerce(TyVar, TyVar, Option<(TyVar, TyVar)>, EqInfo),
    CannotInfer(TyVar, Option<Span>), // With optional instantiation origin
    CannotInferEffect(EffectInstVar),
    // Type, recursive element
    Recursive(TyVar, TyVar),
    NoSuchItem(TyVar, Span, SrcNode<Ident>),
    NoSuchField(TyVar, Span, SrcNode<Ident>),
    // (_, _, obligation span, span of original generic, usage span)
    TypeDoesNotFulfil(ClassVar, TyVar, Span, Option<Span>, Span),
    RecursiveAlias(AliasId, TyVar, Span),
    PatternNotSupported(TyVar, SrcNode<ast::BinaryOp>, TyVar, Span),
    AmbiguousClassItem(SrcNode<Ident>, Vec<ClassId>),
    CycleWhenResolving(TyVar, (ClassId, Vec<TyVar>, Vec<EffectVar>), Span),
}

#[derive(Clone, Debug)]
enum Constraint {
    // (record, field_name, field)
    Access(TyVar, SrcNode<Ident>, TyVar),
    Update(TyVar, SrcNode<Ident>, TyVar),
    Impl(TyVar, ClassVar, Span, Vec<(SrcNode<Ident>, TyVar)>, Span),
    ClassField(TyVar, ClassVar, SrcNode<Ident>, TyVar, Span),
    ClassAssoc(TyVar, ClassVar, SrcNode<Ident>, TyVar, Span),
    EffectSendRecv(EffectInstVar, TyVar, TyVar, Span),
}

#[derive(Clone, Debug)]
pub enum Check {
    CheckFlowEffect((EffectVar, TyVar), (EffectVar, TyVar), EqInfo),
    CheckEffectEmpty((EffectVar, TyVar), TyVar, EqInfo),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TyVar(usize);

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct ClassVar(usize);

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct EffectVar(usize);

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct EffectInstVar(usize);

pub struct Infer<'a> {
    ctx: &'a mut Context,
    gen_scope: Option<GenScopeId>,
    vars: Vec<(Span, TyInfo, Result<(), ()>, std::panic::Location<'static>)>,
    class_vars: Vec<(Span, ClassInfo)>,
    effect_vars: Vec<(Span, EffectInfo)>,
    effect_inst_vars: Vec<(Span, EffectInstInfo)>,
    opaque_id: usize,
    constraints: VecDeque<Constraint>,
    checks: VecDeque<Check>,
    errors: Vec<InferError>,
    self_type: Option<TyVar>,
    implied_members: Vec<InferImpliedMember>,
    pub debug: bool,
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
            effect_inst_vars: Vec::new(),
            opaque_id: 0,
            constraints: VecDeque::new(),
            checks: VecDeque::new(),
            errors: Vec::new(),
            self_type: None,
            // self_obligations: Vec::new(),
            implied_members: Vec::new(),
            debug: false,
        };

        this
    }

    pub fn with_debug(mut self, debug: bool) -> Self {
        self.debug = debug;
        self
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
                let gen_tys = member.gen_tys
                    .iter()
                    .map(|ty| self.instantiate_local(*ty, member.member.span()))
                    .collect();
                let gen_effs = member.gen_effs
                    .iter()
                    .map(|eff| match eff.map(|eff| self.instantiate_local_eff(eff, member.member.span())) {
                        Some(Ok(Some(eff))) => eff,
                        // TODO: Make sure this is fine in all other cases. It should be?
                        _ => self.insert_effect(member.member.span(), EffectInfo::Open(Vec::new())),
                    })
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
                    gen_tys,
                    gen_effs,
                    class: member.class.clone(),
                    items,
                });
            }
        }
        self
    }

    pub fn with_self_var(mut self, ty: TyVar) -> Self {
        self.self_type = Some(ty);
        self
    }

    pub fn with_self_type(mut self, ty: TyId, span: Span) -> Self {
        let var = self.instantiate_local(ty, span);
        self.with_self_var(var)
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
            // TODO: Consider class args too when deciding to recurse!
            if !searched.contains(&*new_member.class) {
                let member_ty = self.instantiate(
                    *new_member.member,
                    new_member.member.span(),
                    &mut |idx, _, _| member.gen_tys.get(idx).copied(),
                    &mut |idx, _| todo!(),
                    Some(*member.member),
                );
                let member_gen_tys = new_member.gen_tys
                    .iter()
                    .map(|ty| self.instantiate(
                        *ty,
                        new_member.member.span(),
                        &mut |idx, _, _| member.gen_tys.get(idx).copied(),
                        &mut |idx, _| todo!(),
                        Some(*member.member),
                    ))
                    .collect();
                let member_gen_effs = new_member.gen_effs
                    .iter()
                    .map(|eff| match eff.map(|eff| self.instantiate_eff(
                            eff,
                            new_member.member.span(),
                            &mut |idx, _, _| member.gen_tys.get(idx).copied(),
                            &mut |idx, _| member.gen_effs.get(idx).copied(),
                            Some(*member.member),
                        ))
                    {
                        Some(Ok(Some(eff))) => eff,
                        // TODO: Make sure this is fine in all other cases. It should be?
                        _ => self.insert_effect(new_member.member.span(), EffectInfo::Open(Vec::new())),
                    })
                    .collect();
                let items = match &new_member.items {
                    ImpliedItems::Real(member) => ImpliedItems::Real(*member),
                    ImpliedItems::Eq(assoc) => ImpliedItems::Eq(assoc
                        .iter()
                        .map(|(name, assoc)| (name.clone(), self.instantiate(
                            *assoc,
                            name.span(),
                            &mut |idx, _, _| member.gen_tys.get(idx).copied(),
                            &mut |idx, _| member.gen_effs.get(idx).copied(),
                            Some(*member.member),
                        )))
                        .collect()),
                };

                self.add_implied_member_inner(searched, InferImpliedMember {
                    member: SrcNode::new(member_ty, member.member.span()),
                    class: new_member.class.clone(),
                    gen_tys: member_gen_tys,
                    gen_effs: member_gen_effs,
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
                | TyInfo::Opaque(_, _)
                | TyInfo::SelfType => {},
                TyInfo::List(item) => self.set_error(item),
                TyInfo::Record(fields, _) => fields
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

    #[track_caller]
    pub fn insert(&mut self, span: Span, info: TyInfo) -> TyVar {
        let id = TyVar(self.vars.len());
        let err = if matches!(&info, TyInfo::Error(_)) { Err(()) } else { Ok(()) };
        self.vars.push((span, info, err, *std::panic::Location::caller()));
        id
    }

    fn instantiate_local_helpers(&mut self) -> (
        impl Fn(usize, GenScopeId, &mut Infer<'a>) -> Option<TyVar>,
        impl Fn(usize, &mut Infer<'a>) -> Option<EffectVar>,
    ) {
        let gens = self.gen_scope
            .map(|gen_scope| (
                (0..self.ctx.tys.get_gen_scope(gen_scope).len())
                    .map(|idx| {
                        let span = self.ctx.tys.get_gen_scope(gen_scope).get(idx).name.span();
                        self.insert(span, TyInfo::Gen(idx, gen_scope, span))
                    })
                    .collect::<Vec<_>>(),
                (0..self.ctx.tys.get_gen_scope(gen_scope).len_eff())
                    .map(|idx| {
                        let span = self.ctx.tys.get_gen_scope(gen_scope).get_eff(idx).name.span();
                        let eff_inst = self.insert_effect_inst(span, EffectInstInfo::Gen(idx, gen_scope));
                        self.insert_effect(span, EffectInfo::Closed(vec![eff_inst], None))
                    })
                    .collect::<Vec<_>>(),
            ));
        (
            { let gens = gens.clone(); move |idx, gen_scope, ctx| gens.as_ref().expect("No gen scope").0.get(idx).copied() },
            move |idx, ctx| gens.as_ref().expect("No gen scope").1.get(idx).copied(),
        )
    }

    pub fn instantiate_local(&mut self, ty: TyId, span: Span) -> TyVar {
        let (mut gen_ty, mut gen_eff) = self.instantiate_local_helpers();
        self.instantiate(ty, span, &mut gen_ty, &mut gen_eff, self.self_type)
    }

    pub fn instantiate_local_eff(&mut self, eff: EffectId, span: Span) -> Result<Option<EffectVar>, ()> {
        let (mut gen_ty, mut gen_eff) = self.instantiate_local_helpers();
        self.instantiate_eff(eff, span, &mut gen_ty, &mut gen_eff, self.self_type)
    }

    // Ok(Some(eff)) => lowered effect
    // Ok(None) => lowered effect had no members in its set and should be flattened
    // Err(()) => lowering error
    pub fn instantiate_eff(
        &mut self,
        eff: EffectId,
        span: Span,
        gen_ty: &mut impl FnMut(usize, GenScopeId, &mut Self) -> Option<TyVar>,
        gen_eff: &mut impl FnMut(usize, &mut Self) -> Option<EffectVar>,
        self_ty: Option<TyVar>,
    ) -> Result<Option<EffectVar>, ()> {
        match self.ctx.tys.get_effect(eff) {
            Effect::Error => Err(()),
            // Empty effect sets get flattened
            Effect::Known(effs) if effs.is_empty() => Ok(None),
            Effect::Known(effs) => {
                let mut opener = None;
                let effs = effs
                    .into_iter()
                    .flat_map(|eff| {
                        let inst = match eff {
                            Ok(EffectInst::Gen(idx, _)) => match gen_eff(idx, self) {
                                Some(eff) => {
                                    // TODO: This seems dumb
                                    opener = opener.or(Some(eff));
                                    return None;
                                },
                                None => todo!("Tried to instantiate effect that wasn't present in signature, a free effect set should be generated instead"),
                            },
                            Ok(EffectInst::Concrete(decl, args)) => {
                                let args = args
                                    .iter()
                                    .map(|param| self.instantiate(*param, span, gen_ty, gen_eff, self_ty))
                                    .collect();
                                EffectInstInfo::Known(decl, args)
                            },
                            Err(()) => EffectInstInfo::Error, // TODO: Error instead
                        };
                        Some(self.insert_effect_inst(span, inst))
                    })
                    .collect();
                Ok(Some(self.insert_effect(span, EffectInfo::Closed(effs, opener))))
            },
        }
    }

    pub fn instantiate(
        &mut self,
        ty: TyId,
        span: impl Into<Option<Span>>,
        gen_ty: &mut impl FnMut(usize, GenScopeId, &mut Self) -> Option<TyVar>,
        gen_eff: &mut impl FnMut(usize, &mut Self) -> Option<EffectVar>,
        self_ty: Option<TyVar>,
    ) -> TyVar {
        let span = span.into();
        let info = match self.ctx.tys.get(ty) {
            Ty::Error(reason) => TyInfo::Error(reason),
            Ty::Prim(prim) => TyInfo::Prim(prim),
            Ty::List(item) => TyInfo::List(self.instantiate(item, span, gen_ty, gen_eff, self_ty)),
            Ty::Record(fields, is_tuple) => TyInfo::Record(fields
                .into_iter()
                .map(|(name, field)| (name, self.instantiate(field, span, gen_ty, gen_eff, self_ty)))
                .collect(), is_tuple),
            Ty::Func(i, o) => TyInfo::Func(self.instantiate(i, span, gen_ty, gen_eff, self_ty), self.instantiate(o, span, gen_ty, gen_eff, self_ty)),
            Ty::Data(data, params) => TyInfo::Data(data, params
                .into_iter()
                .map(|param| self.instantiate(param, span, gen_ty, gen_eff, self_ty))
                .collect()),
             // TODO: Check scope is valid for recursive scopes
            Ty::Gen(index, scope) => match gen_ty(index, scope, self) {
                Some(ty) => TyInfo::Ref(ty),
                None => {
                    println!("Generic type length mismatch! Tried to get idx = {}", index);
                    let span = span.unwrap_or_else(|| self.ctx.tys.get_span(ty));
                    // TODO: A better error, if this occurs? (it shouldn't)
                    self.ctx.emit(Error::WrongNumberOfGenerics(span, 999, span, index));
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
            Ty::Assoc(inner, (class_id, gen_tys, gen_effs), assoc) => {
                let span = span.unwrap_or_else(|| self.ctx.tys.get_span(ty));
                let inner = self.instantiate(inner, span, gen_ty, gen_eff, self_ty);
                let gen_tys = gen_tys
                    .iter()
                    .map(|ty| self.instantiate(*ty, span, gen_ty, gen_eff, self_ty))
                    .collect();
                let gen_effs = gen_effs
                    .iter()
                    .map(|eff| match eff.map(|eff| self.instantiate_eff(eff, span, gen_ty, gen_eff, self_ty)) {
                        Some(Ok(Some(eff))) => eff,
                        // TODO: Make sure this is fine in all other cases. It should be?
                        _ => self.insert_effect(span, EffectInfo::Open(Vec::new())),
                    })
                    .collect();
                let assoc_ty = self.unknown(span);
                self.make_impl(inner, (class_id, gen_tys, gen_effs), span, vec![(assoc, assoc_ty)], span);
                TyInfo::Ref(assoc_ty)
            },
            Ty::Effect(eff, out) => match self.instantiate_eff(eff, span.unwrap_or_else(|| self.ctx.tys.get_span(ty)), gen_ty, gen_eff, self_ty) {
                Ok(Some(eff)) => {
                    // TODO: Should this be strict instead of relaxed?
                    let opaque = self.opaque(span.unwrap_or_else(|| self.ctx.tys.get_span(ty)), true);
                    TyInfo::Effect(eff, self.instantiate(out, span, gen_ty, gen_eff, self_ty), opaque)
                },
                Ok(None) => return self.instantiate(out, span, gen_ty, gen_eff, self_ty),
                Err(()) => TyInfo::Error(ErrorReason::Invalid),
            },
        };
        self.insert(span.unwrap_or_else(|| self.ctx.tys.get_span(ty)), info)
    }

    #[track_caller]
    pub fn unknown(&mut self, span: Span) -> TyVar {
        self.insert(span, TyInfo::Unknown(None))
    }

    pub fn opaque(&mut self, span: Span, relaxed: bool) -> TyVar {
        let ty = self.insert(span, TyInfo::Opaque(self.opaque_id, relaxed));
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
    pub fn make_impl(
        &mut self,
        ty: TyVar,
        (class_id, gen_tys, gen_effs): (ClassId, Vec<TyVar>, Vec<EffectVar>),
        obl_span: Span,
        unchecked_assoc: Vec<(SrcNode<Ident>, TyVar)>,
        use_span: Span,
    ) {
        let class = ClassVar(self.class_vars.len());
        self.class_vars.push((use_span, ClassInfo::Known(class_id, gen_tys, gen_effs)));
        self.constraints.push_back(Constraint::Impl(ty, class, obl_span, unchecked_assoc, use_span));
    }

    fn instantiate_class(&mut self, class_id: ClassId, inst_span: Span, self_ty: Option<TyVar>) -> (Vec<TyVar>, Vec<EffectVar>) {
        let gen_scope_id = self.ctx.classes.class_gen_scope(class_id);
        let gen_scope = self.ctx.tys.get_gen_scope(gen_scope_id);
        let gen_tys = (0..gen_scope.len())
            .map(|i| gen_scope.get(i).name.span())
            .collect::<Vec<_>>();
        let gen_effs = (0..gen_scope.len_eff())
            .map(|i| gen_scope.get_eff(i).name.span())
            .collect::<Vec<_>>();
        let gen_tys = gen_tys
            .into_iter()
            .map(|origin| self.insert(inst_span, TyInfo::Unknown(Some(origin))))
            .collect::<Vec<_>>();
        let gen_effs = gen_effs
            .into_iter()
            .map(|origin| self.insert_effect(inst_span, EffectInfo::Open(Vec::new())))
            .collect::<Vec<_>>();

        // TODO: Move this function?
        lower::enforce_generic_obligations(
            self,
            gen_scope_id,
            &gen_tys,
            &gen_effs,
            inst_span,
            self.ctx.classes.get(class_id).name.span(),
            self_ty,
        ).expect("Wrong number of generic params");

        (gen_tys, gen_effs)
    }

    pub fn insert_class(&mut self, span: Span, class: ClassInfo) -> ClassVar {
        let id = ClassVar(self.class_vars.len());
        self.class_vars.push((span, class));
        id
    }

    pub fn make_class_field_known(
        &mut self,
        ty: TyVar,
        field_name: SrcNode<Ident>,
        (class_id, gen_tys, gen_effs): (ClassId, Vec<TyVar>, Vec<EffectVar>),
        field_ty: TyVar,
        span: Span,
    ) -> ClassVar {
        let class = self.insert_class(span, ClassInfo::Known(class_id, gen_tys, gen_effs));
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

    fn follow_effect_inst(&self, eff: EffectInstVar) -> EffectInstInfo {
        match &self.effect_inst_vars[eff.0].1 {
            EffectInstInfo::Ref(eff) => self.follow_effect_inst(*eff),
            info => info.clone(),
        }
    }

    pub fn insert_gen_eff(&mut self, span: Span, idx: usize, gen_scope: GenScopeId) -> EffectVar {
        let eff = self.insert_effect_inst(span, EffectInstInfo::Gen(idx, gen_scope));
        self.insert_effect(span, EffectInfo::Closed(vec![eff], None))
    }

    pub fn insert_effect(&mut self, span: Span, eff: EffectInfo) -> EffectVar {
        let id = EffectVar(self.effect_vars.len());
        self.effect_vars.push((span, eff));
        id
    }

    pub fn insert_effect_inst(&mut self, span: Span, eff: EffectInstInfo) -> EffectInstVar {
        let id = EffectInstVar(self.effect_inst_vars.len());
        self.effect_inst_vars.push((span, eff));
        id
    }

    pub fn make_effect_send_recv(&mut self, eff: EffectInstVar, send: TyVar, recv: TyVar, span: Span) {
        self.constraints.push_back(Constraint::EffectSendRecv(eff, send, recv, span));
    }

    pub fn emit(&mut self, err: InferError) {
        self.errors.push(err);
    }

    fn occurs_in_eff_inner(&self, x: TyVar, y: EffectVar, seen: &mut Vec<TyVar>) -> bool {
        match self.follow_effect(y) {
            EffectInfo::Ref(_) => unreachable!(),
            EffectInfo::Closed(effs, opener) => effs
                .iter()
                .any(|eff| match self.follow_effect_inst(*eff) {
                    EffectInstInfo::Unknown => false,
                    EffectInstInfo::Error => false,
                    EffectInstInfo::Ref(_) => unreachable!(),
                    EffectInstInfo::Gen(_, _) => false,
                    EffectInstInfo::Known(_, params) => params
                        .into_iter()
                        .any(|y| x == y || self.occurs_in_inner(x, y, seen)),
                }) || opener.map_or(false, |opener| self.occurs_in_eff_inner(x, opener, seen)),
            EffectInfo::Open(effs) => effs
                .iter()
                .any(|eff| match self.follow_effect_inst(*eff) {
                    EffectInstInfo::Unknown => false,
                    EffectInstInfo::Error => false,
                    EffectInstInfo::Ref(_) => unreachable!(),
                    EffectInstInfo::Gen(_, _) => false,
                    EffectInstInfo::Known(_, params) => params
                        .into_iter()
                        .any(|y| x == y || self.occurs_in_inner(x, y, seen)),
                }),
        }
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
                | TyInfo::Opaque(_, _)
                | TyInfo::Gen(_, _, _) => false,
                TyInfo::Ref(y) => x == y || self.occurs_in_inner(x, y, seen),
                TyInfo::List(item) => x == item || self.occurs_in_inner(x, item, seen),
                TyInfo::Func(i, o) => x == i || x == o || self.occurs_in_inner(x, i, seen) || self.occurs_in_inner(x, o, seen),
                TyInfo::Record(ys, _) => ys
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
                        ClassInfo::Known(_, gen_tys, gen_effs) => gen_tys
                            .into_iter()
                            .any(|y| x == y || self.occurs_in_inner(x, y, seen)) || gen_effs
                                .into_iter()
                                .any(|y| self.occurs_in_eff_inner(x, y, seen)),
                    },
                // Opaque type is not checked, it's always opaque... hopefully
                TyInfo::Effect(eff, out, _opaque) => self.occurs_in_inner(x, out, seen) || self.occurs_in_eff_inner(x, eff, seen),
            };

            seen.pop();

            occurs
        }
    }

    // Returns true if `x` occurs in `y`.
    fn occurs_in(&self, x: TyVar, y: TyVar) -> bool {
        self.occurs_in_inner(x, y, &mut Vec::new())
    }

    fn collect_eff_insts(&self, var: EffectVar) -> Vec<EffectInstVar> {
        match self.follow_effect(var) {
            EffectInfo::Ref(_) => unreachable!(),
            EffectInfo::Open(effs) => effs.clone(),
            EffectInfo::Closed(effs, opener) => {
                let mut effs = effs.clone();
                if let Some(opener) = opener {
                    effs.append(&mut self.collect_eff_insts(opener))
                }
                effs
            },
        }
    }

    // Flow the type `x` into the type `y`
    pub fn make_flow(&mut self, x: TyVar, y: TyVar, info: impl Into<EqInfo>) {
        let mut eq_info = info.into();
        if let Err((a, b)) = Self::flow_inner(&mut MakeFlow(self, &eq_info), x, y) {
            if !self.is_error(a) && !self.is_error(b) {
                self.set_error(a);
                self.set_error(b);
                // TODO: Don't put this here, it's a bit silly
                if let (TyInfo::Effect(_, _, _), TyInfo::Effect(_, _, _)) = (self.info(a), self.info(b)) {
                    eq_info.reason = Some("Effect objects have unique types and cannot be substituted for one-another".to_string());
                }
                self.errors.push(InferError::CannotCoerce(x, y, Some((a, b)), eq_info));
            }
        }
    }

    // Flow the effect `x` into the effect `y`
    pub fn make_flow_effect(&mut self, x: (EffectVar, TyVar), y: (EffectVar, TyVar), info: impl Into<EqInfo>) {
        let mut eq_info = info.into();
        if let Err((a, b)) = Self::flow_effect(&mut MakeFlow(self, &eq_info), x, y) {
            if !self.is_error(a) && !self.is_error(b) {
                self.set_error(a);
                self.set_error(b);
                // TODO: Don't put this here, it's a bit silly
                if let (TyInfo::Effect(_, _, _), TyInfo::Effect(_, _, _)) = (self.info(a), self.info(b)) {
                    eq_info.reason = Some("Effect objects have unique types and cannot be substituted for one-another".to_string());
                }
                self.errors.push(InferError::CannotCoerce(x.1, y.1, Some((a, b)), eq_info));
            }
        }
    }

    // Check to see whether the type `x` may flow into the type `y`. Returns Some(true) for correct flow, Some(false)
    // for an error, and None for a flow that may or may not be permitted
    pub fn check_flow(&self, x: TyVar, y: TyVar) -> Option<bool> {
        let mut check = CheckFlow(self, Some(true));
        let res = Self::flow_inner(&mut check, x, y);
        match check.1 {
            Some(true) => Some(res.is_ok()),
            None => if !res.is_ok() { Some(false) } else { None },
            Some(false) => Some(false)
        }
    }

    // Check to see whether the effect `x` may flow into the effect `y`. Returns Some(true) for correct flow, Some(false)
    // for an error, and None for a flow that may or may not be permitted
    pub fn check_flow_eff(&self, (x, x_ty): (EffectVar, TyVar), (y, y_ty): (EffectVar, TyVar)) -> Option<bool> {
        let mut check = CheckFlow(self, Some(true));
        let res = Self::flow_effect(&mut check, (x, x_ty), (y, y_ty));
        match check.1 {
            Some(true) => Some(res.is_ok()),
            None => if !res.is_ok() { Some(false) } else { None },
            Some(false) => Some(false),
        }
    }

    fn flow_many<'b, I: FlowInfer<'b>>(
        infer: &mut I,
        xs: impl IntoIterator<Item = TyVar>,
        ys: impl IntoIterator<Item = TyVar>,
    ) -> Result<(), (TyVar, TyVar)> {
        xs
            .into_iter()
            .zip(ys.into_iter())
            .fold(None, |err, (x, y)| err.or(Infer::flow_inner(infer, x, y).err()))
            .map(Err).unwrap_or(Ok(()))
    }

    fn flow_many_effect<'b, I: FlowInfer<'b>>(
        infer: &mut I,
        (xs, x_ty): (impl IntoIterator<Item = EffectVar>, TyVar),
        (ys, y_ty): (impl IntoIterator<Item = EffectVar>, TyVar),
    ) -> Result<(), (TyVar, TyVar)> {
        xs
            .into_iter()
            .zip(ys.into_iter())
            .fold(None, |err, (x, y)| err.or(Infer::flow_effect(infer, (x, x_ty), (y, y_ty)).err()))
            .map(Err).unwrap_or(Ok(()))
    }

    fn flow_effect_inst<'b, I: FlowInfer<'b>>(
        infer: &mut I,
        (x, x_ty): (EffectInstVar, TyVar),
        (y, y_ty): (EffectInstVar, TyVar),
    ) -> Result<(), (TyVar, TyVar)> {
        match (infer.as_ref().follow_effect_inst(x), infer.as_ref().follow_effect_inst(y)) {
            (EffectInstInfo::Error, _) => Ok(()),
            (_, EffectInstInfo::Error) => Ok(()),
            (EffectInstInfo::Unknown, _) => Ok(infer.set_effect_inst_info(x, EffectInstInfo::Ref(y))),
            (_, EffectInstInfo::Unknown) => Ok(infer.set_effect_inst_info(y, EffectInstInfo::Ref(x))),
            (EffectInstInfo::Known(x, x_args), EffectInstInfo::Known(y, y_args)) if x == y => x_args
                .into_iter()
                .zip(y_args)
                .fold(None, |err, (x, y)| err.or(Infer::flow_inner(infer, x, y).err()))
                .map(Err).unwrap_or(Ok(())),
            (EffectInstInfo::Gen(x, _), EffectInstInfo::Gen(y, _)) if x == y => Ok(()),
            (x, y) => {
                Err((x_ty, y_ty))
            },
        }
    }

    // TODO: Allow errors that mention effects instead of types
    fn check_eff_empty(
        &self,
        (x, x_ty): (EffectVar, TyVar),
    ) -> Option<bool> {
        match self.follow_effect(x) {
            EffectInfo::Ref(x) => self.check_eff_empty((x, x_ty)),
            EffectInfo::Open(x_effs) | EffectInfo::Closed(x_effs, None) => Some(x_effs.is_empty()),
            EffectInfo::Closed(x_effs, Some(opener)) => Some(x_effs.is_empty() && self.check_eff_empty((opener, x_ty))?),
        }
    }

    // TODO: Allow errors that mention effects instead of types
    fn flow_effect<'b, I: FlowInfer<'b>>(
        infer: &mut I,
        (x, x_ty): (EffectVar, TyVar),
        (y, y_ty): (EffectVar, TyVar),
    ) -> Result<(), (TyVar, TyVar)> {
        match (infer.as_ref().follow_effect(x), infer.as_ref().follow_effect(y)) {
            (EffectInfo::Ref(_), _) => unreachable!(),
            (_, EffectInfo::Ref(_)) => unreachable!(),

            // Closed with opener into closed
            (EffectInfo::Closed(x_effs, Some(x_opener)), EffectInfo::Closed(y_effs, None) | EffectInfo::Open(y_effs)) if x_effs.is_empty() => {
                Self::flow_effect(infer, (x_opener, x_ty), (y, y_ty))
            },
            // Closed into closed with opener
            (EffectInfo::Closed(x_effs, None) | EffectInfo::Open(x_effs), EffectInfo::Closed(y_effs, Some(y_opener))) if y_effs.is_empty() => {
                Self::flow_effect(infer, (x, x_ty), (y_opener, y_ty))
            },
            // Closed with opener into closed with opener
            (EffectInfo::Closed(x_effs, Some(x_opener)), EffectInfo::Closed(y_effs, Some(y_opener))) if x_effs.is_empty() && y_effs.is_empty() => {
                Self::flow_effect(infer, (x_opener, x_ty), (y_opener, y_ty))
            },
            // Ensure closed into closed
            (EffectInfo::Closed(x_effs, None), EffectInfo::Closed(y_effs, None)) => {
                if x_effs
                    .iter()
                    .all(|x| y_effs
                        .iter()
                        .any(|y| Self::flow_effect_inst(infer, (*x, x_ty), (*y, y_ty)) == Ok(())))
                {
                    Ok(())
                } else if infer.make_check_eff((x, x_ty), (y, y_ty)) {
                    Ok(())
                } else {
                    Err((x_ty, y_ty))
                }
            },
            // Grow open into one-another
            (EffectInfo::Open(mut x_effs), EffectInfo::Open(mut y_effs)) => {
                let mut all_effs = y_effs.clone();
                all_effs.append(&mut x_effs);
                infer.set_effect_info(y, EffectInfo::Open(all_effs));
                Ok(())
            },
            // Grow closed into open
            (EffectInfo::Closed(mut x_effs, opener), EffectInfo::Open(mut y_effs)) => {
                y_effs.append(&mut x_effs);
                y_effs.append(&mut opener.map(|opener| infer.infer().collect_eff_insts(opener)).unwrap_or_default());
                infer.set_effect_info(y, EffectInfo::Open(y_effs));
                Ok(())
            },
            // Defer check open into closed
            (EffectInfo::Open(mut x_effs), EffectInfo::Closed(mut y_effs, None)) => {
                if x_effs
                    .iter()
                    .all(|x| y_effs
                        .iter()
                        .any(|y| Self::flow_effect_inst(infer, (*x, x_ty), (*y, y_ty)) == Ok(())))
                {
                    // If the rhs is closed, flow backwards
                    // TODO: This is done to avoid a backend problem where an empty effect set accidentally gets
                    // flattened when a caller expects it to not be flattener, and should probably be fixed
                    x_effs.append(&mut y_effs);
                    infer.set_effect_info(x, EffectInfo::Open(x_effs));
                    Ok(())
                } else if infer.make_check_eff((x, x_ty), (y, y_ty)) {
                    // If the rhs is closed, flow backwards
                    // TODO: This is done to avoid a backend problem where an empty effect set accidentally gets
                    // flattened when a caller expects it to not be flattener, and should probably be fixed
                    x_effs.append(&mut y_effs);
                    infer.set_effect_info(x, EffectInfo::Open(x_effs));
                    Ok(())
                } else {
                    Err((x_ty, y_ty))
                }
            },
            (x_info, y_info) => todo!("{:?} into {:?}", x_info, y_info),
        }
    }

    // TODO: Allow errors that mention classes instead of types
    fn flow_class<'b, I: FlowInfer<'b>>(
        infer: &mut I,
        (x, x_ty): (ClassVar, TyVar),
        (y, y_ty): (ClassVar, TyVar),
    ) -> Result<(), (TyVar, TyVar)> {
        match (infer.as_ref().follow_class(x), infer.as_ref().follow_class(y)) {
            (ClassInfo::Unknown, _) => Ok(infer.set_class_info(x, ClassInfo::Ref(y))),
            (_, ClassInfo::Unknown) => Ok(infer.set_class_info(y, ClassInfo::Ref(x))),
            (ClassInfo::Known(class_id_x, xs, e_xs), ClassInfo::Known(class_id_y, ys, e_ys)) if class_id_x == class_id_y => {
                // Class generics are always invariant
                let co_error = Self::flow_many(infer, xs.iter().copied(), ys.iter().copied()).err();
                let contra_error = Self::flow_many(infer, ys, xs).err().map(|(a, b)| (b, a));

                // TODO: What about effects? I guess invariant too...
                let e_co_error = Self::flow_many_effect(infer, (e_xs.iter().copied(), x_ty), (e_ys.iter().copied(), y_ty)).err();
                let e_contra_error = Self::flow_many_effect(infer, (e_ys, y_ty), (e_xs, x_ty)).err();
                co_error
                    .or(contra_error)
                    .or(e_co_error)
                    .or(e_contra_error)
                    .map(Err)
                    .unwrap_or(Ok(()))
            },
            (_, _) => Err((x_ty, y_ty)),
        }
    }

    fn flow_inner<I: FlowInfer<'a>>(infer: &mut I, x: TyVar, y: TyVar) -> Result<(), (TyVar, TyVar)> {
        if x == y { return Ok(()) } // If the vars are equal, we have no need to check flow
        match (infer.as_ref().info(x), infer.as_ref().info(y)) {
            // Follow references
            (TyInfo::Ref(x), _) => Self::flow_inner(infer, x, y),
            (_, TyInfo::Ref(y)) => Self::flow_inner(infer, x, y),

            // Unify unknown or erronoeus types
            (TyInfo::Unknown(_), y_info) => if infer.as_ref().occurs_in(x, y) {
                infer.emit_error(InferError::Recursive(y, infer.as_ref().follow(x)));
                infer.set_info(x, TyInfo::Error(ErrorReason::Recursive));
                Ok(infer.set_error(x)) // TODO: Not actually ok
            } else {
                infer.set_unknown_flow(x, y);
                Ok(infer.set_info(x, TyInfo::Ref(y)))
            },
            (x_info, TyInfo::Unknown(_)) => if infer.as_ref().occurs_in(y, x) {
                infer.emit_error(InferError::Recursive(x, infer.as_ref().follow(y)));
                infer.set_info(y, TyInfo::Error(ErrorReason::Recursive));
                Ok(infer.set_error(y)) // TODO: Not actually ok
            } else {
                infer.set_unknown_flow(x, y);
                Ok(infer.set_info(y, TyInfo::Ref(x)))
            },

            // Unify errors
            (_, TyInfo::Error(_)) => {
                infer.set_error(x);
                Ok(infer.set_info(x, TyInfo::Ref(y)))
            },
            (TyInfo::Error(_), _) => {
                infer.set_error(y);
                Ok(infer.set_info(y, TyInfo::Ref(x)))
            },

            (TyInfo::Prim(x), TyInfo::Prim(y)) if x == y => Ok(()),
            (TyInfo::List(x), TyInfo::List(y)) => Self::flow_inner(infer, x, y),
            (TyInfo::Record(xs, _), TyInfo::Record(ys, _)) if xs.len() == ys.len() && xs
                .keys()
                .all(|x| ys.contains_key(x)) => xs
                    .into_iter()
                    .try_for_each(|(x, x_ty)| Self::flow_inner(infer, x_ty, ys[&x])),
            (TyInfo::Func(x_i, x_o), TyInfo::Func(y_i, y_o)) => {
                let i_err = Self::flow_inner(infer, y_i, x_i).err().map(|(a, b)| (b, a)); // Input is contravariant
                let o_err = Self::flow_inner(infer, x_o, y_o).err();
                i_err.or(o_err).map(Err).unwrap_or(Ok(()))
            },
            (TyInfo::Data(x_data, xs), TyInfo::Data(y_data, ys)) if x_data == y_data &&
                xs.len() == ys.len() /* TODO: Assert this! */ => {
                // TODO: Unnecessarily conservative, variance of data type generics should be determined
                let co_error = Self::flow_many(infer, xs.iter().copied(), ys.iter().copied()).err();
                let contra_error = Self::flow_many(infer, ys, xs).err().map(|(a, b)| (b, a));
                co_error.or(contra_error).map(Err).unwrap_or(Ok(()))
            },
            (TyInfo::Gen(a, a_scope, _), TyInfo::Gen(b, b_scope, _)) if a == b && a_scope == b_scope => Ok(()),
            (TyInfo::SelfType, TyInfo::SelfType) => Ok(()),
            (TyInfo::Assoc(x, class_x, assoc_x), TyInfo::Assoc(y, class_y, assoc_y))
                if assoc_x == assoc_y => {
                    // associated types are invariant
                    let co_error = Self::flow_inner(infer, x, y).err();
                    let contra_error = Self::flow_inner(infer, y, x).err().map(|(a, b)| (b, a));

                    let class_err = Self::flow_class(infer, (class_x, x), (class_y, y)).err();

                    co_error
                        .or(contra_error)
                        .or(class_err)
                        .map(Err)
                        .unwrap_or(Ok(()))
                },
            (TyInfo::Effect(x_eff, x_out, x_opaque), TyInfo::Effect(y_eff, y_out, y_opaque)) => {
                let eff_err = Self::flow_effect(infer, (x_eff, x), (y_eff, y)).err();
                let o_err = Self::flow_inner(infer, x_out, y_out).err();
                let opaque_err = Self::flow_inner(infer, x_opaque, y_opaque).err().map(|_| (x, y));
                o_err.or(eff_err).or(opaque_err).map(Err).unwrap_or(Ok(()))
            },
            (TyInfo::Effect(x_eff, x_out, x_opaque), y_info) => {
                infer.make_check_eff_empty((x_eff, x), y);
                let o_err = Self::flow_inner(infer, x_out, y).err();
                // TODO: opaque_err?
                o_err.map(Err).unwrap_or(Ok(()))
            },
            (x_info, TyInfo::Effect(y_eff, y_out, y_opaque)) => {
                infer.make_check_eff_empty((y_eff, x), y);
                let o_err = Self::flow_inner(infer, x, y_out).err();
                // TODO: opaque_err?
                o_err.map(Err).unwrap_or(Ok(()))
            },
            (TyInfo::Opaque(x_id, _), TyInfo::Opaque(y_id, _)) if x_id == y_id => Ok(()),
            (TyInfo::Opaque(x_id, relaxed_x), TyInfo::Opaque(y_id, relaxed_y)) if relaxed_x || relaxed_y => {
                if relaxed_y {
                    infer.set_info(y, TyInfo::Ref(x));
                } else {
                    infer.set_info(x, TyInfo::Ref(y));
                }
                Ok(())
            },
            (_, _) => Err((x, y)),
        }
    }

    pub fn reinstantiate_eff(&mut self, span: Span, eff: EffectVar) -> EffectVar {
        match self.follow_effect(eff) {
            EffectInfo::Ref(_) => unreachable!(), // `follow_effect` shouldn't ever return Ref
            // TODO: Is it sound to recreate the effects if effect sets can grow?
            EffectInfo::Open(effs) => {
                let effs = effs
                    .iter()
                    .map(|eff| match self.follow_effect_inst(*eff) {
                        EffectInstInfo::Unknown => *eff,
                        EffectInstInfo::Error => self.insert_effect_inst(span, EffectInstInfo::Error),
                        EffectInstInfo::Ref(_) => unreachable!(),
                        EffectInstInfo::Gen(_, _) => *eff,
                        EffectInstInfo::Known(eff, args) => {
                            let inst_info = EffectInstInfo::Known(eff, args
                                .iter()
                                .map(|arg| self.reinstantiate(span, *arg))
                                .collect());
                            self.insert_effect_inst(span, inst_info)
                        },
                    })
                    .collect();
                self.insert_effect(span, EffectInfo::Open(effs))
            },
            // TODO: Is Closed even required?
            EffectInfo::Closed(effs, opener) => {
                let effs = effs
                    .iter()
                    .map(|eff| match self.follow_effect_inst(*eff) {
                        EffectInstInfo::Unknown => *eff,
                        EffectInstInfo::Error => self.insert_effect_inst(span, EffectInstInfo::Error),
                        EffectInstInfo::Ref(_) => unreachable!(),
                        EffectInstInfo::Gen(_, _) => *eff,
                        EffectInstInfo::Known(eff, args) => {
                            let inst_info = EffectInstInfo::Known(eff, args
                                .iter()
                                .map(|arg| self.reinstantiate(span, *arg))
                                .collect());
                            self.insert_effect_inst(span, inst_info)
                        },
                    })
                    .collect();
                // let opener = opener.map(|opener| self.reinstantiate_eff(span, opener));
                self.insert_effect(span, EffectInfo::Closed(effs, opener))
            },
        }
    }

    /// Reinstantiate a type variable, replacing any known generic types with new unknown ones
    // TODO: Is this a good way to resolve the problem of type inference of recursive definitions in the presence of
    // polymorphism?
    pub fn reinstantiate(&mut self, span: Span, ty: TyVar) -> TyVar {
        match self.info(ty) {
            TyInfo::Ref(x) => self.reinstantiate(span, x),
            TyInfo::Error(reason) => self.insert(self.span(ty), TyInfo::Error(reason)),
            TyInfo::Opaque(_, _) => self.opaque(span, false),
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
            TyInfo::Record(fields, is_tuple) => {
                let fields = fields
                    .into_iter()
                    .map(|(name, field)| (name, self.reinstantiate(span, field)))
                    .collect();
                self.insert(self.span(ty), TyInfo::Record(fields, is_tuple))
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
                    ClassInfo::Known(class_id, gen_tys, gen_effs) => {
                        let gen_tys = gen_tys
                            .into_iter()
                            .map(|ty| self.reinstantiate(span, ty))
                            .collect();
                        let gen_effs = gen_effs
                            .into_iter()
                            .map(|eff| self.reinstantiate_eff(span, eff))
                            .collect();
                        self.insert_class(self.span(ty), ClassInfo::Known(class_id, gen_tys, gen_effs))
                    },
                };
                let inner = self.reinstantiate(span, inner);
                self.insert(self.span(ty), TyInfo::Assoc(inner, class, assoc))
            },
            TyInfo::Effect(eff, out, opaque) => {
                let eff = self.reinstantiate_eff(self.span(ty), eff);
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
            TyInfo::Record(fields, _) => if let Some(field_ty) = fields.get(&field_name) {
                self.make_flow(*field_ty, field, field_name.span());
                Some(true)
            } else {
                Some(false)
            },
            TyInfo::Effect(eff, out, _) => {
                let res = self.resolve_access(out, field_name, field, flow_out);

                // If projecting through the effect worked, make sure that the effect ends up being empty eventually
                if res == Some(true) {
                    self.checks.push_back(Check::CheckEffectEmpty((eff, record), out, EqInfo::from(field_name.span())));
                }

                res
            },
            // Field access through a data type
            TyInfo::Data(data, params) => {
                // TODO: Use `self.ctx.follow_field_access(...)` but work out how to instantiate type parameters
                // throughout the chain.
                let data = self.ctx.datas.get_data(data);
                // Field access on data only works for single-variant, record datatypes
                if let (Some((_, ty)), true) = (data.cons.iter().next(), data.cons.len() == 1) {
                    if let Ty::Record(fields, _) = self.ctx.tys.get(*ty) {
                        if let Some(field_ty) = fields.get(&field_name) {
                            let field_ty = self.instantiate(
                                *field_ty,
                                self.span(record),
                                &mut |index, _, _| params.get(index).copied(),
                                &mut |idx, _| todo!(),
                                Some(record),
                            );
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
                if let ClassInfo::Known(class_id, gen_tys, gen_effs) = self.follow_class(class) {
                    self.resolve_obligation(&mut Vec::new(), ty, (class_id, gen_tys.clone(), gen_effs.clone()), unchecked_assoc.clone(), obl_span, use_span)
                        .map(|res| match res {
                            Ok(member) => {
                                for (assoc, assoc_ty) in unchecked_assoc {
                                    match member {
                                        Ok(ImpliedItems::Real(member_id)) => {
                                            let member = self.ctx.classes.get_member(member_id);
                                            let member_member = member.member;
                                            let member_gen_tys = member.gen_tys.clone();
                                            let member_gen_effs = member.gen_effs.clone();

                                            let mut ty_links = HashMap::new();
                                            let mut eff_links = HashMap::new();
                                            self.derive_links(member_member, ty, Some(ty), use_span, &mut ty_links, &mut eff_links);
                                            for (member_gen_ty, gen_ty) in member_gen_tys.iter().zip(gen_tys.iter()) {
                                                self.derive_links(*member_gen_ty, *gen_ty, Some(ty), use_span, &mut ty_links, &mut eff_links);
                                            }
                                            for (member_gen_eff, gen_eff) in member_gen_effs.iter().zip(gen_effs.iter()) {
                                                if let Some(member_gen_eff) = *member_gen_eff {
                                                    self.derive_links_eff(member_gen_eff, *gen_eff, Some(ty), use_span, &mut ty_links, &mut eff_links);
                                                }
                                            }

                                            let member = self.ctx.classes.get_member(member_id);
                                            if let Some(member_assoc_ty) = member.assoc_ty(*assoc) {
                                                let assoc_ty_inst = self.instantiate(
                                                    member_assoc_ty,
                                                    obl_span,
                                                    &mut |idx, gen_scope, ctx| ty_links.get(&idx).copied(),
                                                    &mut |idx, ctx| eff_links.get(&idx).copied(),
                                                    Some(ty),
                                                );
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
            Constraint::EffectSendRecv(eff, send, recv, span) => match self.follow_effect_inst(eff) {
                EffectInstInfo::Unknown => None,
                EffectInstInfo::Error => Some(Ok(())), // Error, assume valid
                EffectInstInfo::Ref(_) => unreachable!(),
                EffectInstInfo::Gen(_, _) => todo!("Cannot handle or suspend generic effect"),//TODO: Some(Err(InferError::CannotHandleSuspendGenericEffect(*eff, span))),
                EffectInstInfo::Known(decl, args) => {
                    let send_ty = self.instantiate(
                        self.ctx.effects.get_decl(decl).send.expect("Send must be init"),
                        span,
                        &mut |idx, _gen_scope, _ctx| args.get(idx).copied(),
                        &mut |idx, _| todo!(),
                        None,
                    );
                    self.make_flow(send, send_ty, span);
                    let recv_ty = self.instantiate(
                        self.ctx.effects.get_decl(decl).recv.expect("Recv must be init"),
                        span,
                        &mut |idx, _gen_scope, _ctx| args.get(idx).copied(),
                        &mut |idx, _| todo!(),
                        None,
                    );
                    // TODO: The variance here is a bit fucked, this needs to swap when we're
                    // inferring flow into (from a handler) vs flow out of (via a propagation)
                    self.make_flow(recv, recv_ty, self.span(recv));
                    Some(Ok(()))
                },
            },
        }
    }

    fn check(&mut self, c: Check) -> Result<(), InferError> {
        match c {
            Check::CheckFlowEffect((x, x_ty), (y, y_ty), eq_info) => self.check_flow_eff((x, x_ty), (y, y_ty))
                .and_then(|ok| if ok {
                    Some(())
                } else {
                    None
                })
                .ok_or_else(|| InferError::CannotCoerce(x_ty, y_ty, None, eq_info)),
            Check::CheckEffectEmpty((x, x_ty), other, eq_info) => self.check_eff_empty((x, x_ty))
                .and_then(|ok| if ok {
                    Some(())
                } else {
                    None
                })
                .ok_or_else(|| InferError::CannotCoerce(x_ty, other, None, eq_info)),
        }
    }

    fn try_resolve_class_from_assoc(&mut self, ty: TyVar, class_var: ClassVar, assoc: SrcNode<Ident>, assoc_ty: TyVar, span: Span) -> Option<Result<(), InferError>> {
        let (class_id, gen_tys, gen_effs) = match self.select_member_from_item(ty, class_var, assoc.clone(), assoc_ty, span, true) {
            Some(Ok(Some((class_id, gen_tys, gen_effs)))) => (class_id, gen_tys, gen_effs),
            Some(Ok(None)) => return None,
            Some(Err(err)) => return Some(Err(err)),
            None => return None,
        };

        self.class_vars[class_var.0].1 = ClassInfo::Known(class_id, gen_tys.clone(), gen_effs.clone());

        // Require an implementation to exist
        self.make_impl(ty, (class_id, gen_tys, gen_effs), span, vec![
            (assoc, assoc_ty),
        ], span);

        Some(Ok(()))
    }

    fn try_resolve_class_from_field(&mut self, ty: TyVar, class_var: ClassVar, field: SrcNode<Ident>, field_ty: TyVar, span: Span) -> Option<Result<(), InferError>> {
        let (class_id, gen_tys, gen_effs) = match self.select_member_from_item(ty, class_var, field.clone(), field_ty, span, false) {
            Some(Ok(Some((class_id, gen_tys, gen_effs)))) => (class_id, gen_tys, gen_effs),
            Some(Ok(None)) => return Some(Ok(())),
            Some(Err(err)) => return Some(Err(err)),
            None => return None,
        };

        self.class_vars[class_var.0].1 = ClassInfo::Known(class_id, gen_tys.clone(), gen_effs.clone());

        self.make_impl(ty, (class_id, gen_tys.clone(), gen_effs.clone()), span, Vec::new(), span);
        let field_ty_id = **self.ctx.classes
            .get(class_id)
            .field(*field)
            .unwrap();
        let inst_field_ty = self.instantiate(
            field_ty_id,
            field.span(),
            &mut |idx, _, _| gen_tys.get(idx).copied(),
            &mut |idx, _| gen_effs.get(idx).copied(),
            Some(ty),
        );

        // TODO: Check soundness of flow relationship
        self.make_flow(inst_field_ty, field_ty, field.span());

        Some(Ok(()))
    }

    fn select_member_from_item(
        &mut self,
        ty: TyVar,
        class_var: ClassVar,
        item: SrcNode<Ident>,
        item_ty: TyVar,
        span: Span,
        is_assoc: bool,
    ) -> Option<Result<Option<(ClassId, Vec<TyVar>, Vec<EffectVar>)>, InferError>> {
        if let ClassInfo::Known(class_id, gen_tys, gen_effs) = self.follow_class(class_var) {
            Some(Ok(Some((class_id, gen_tys, gen_effs))))
        } else {
            let Some((implied_candidates, external_candidates)) = self.find_class_candidates_from_item(ty, item.clone(), item_ty, is_assoc)
                else { return Some(Ok(None)) }; // Resolving an error type always succeeds;

            match (implied_candidates.len(), external_candidates.len()) {
                // Easy case: we found exactly 1 candidate implied by the gen scope, so we know it much be the one we're looking for.
                (1, _) => {
                    // Can't fail
                    let member = implied_candidates.into_iter().next().unwrap();
                    Some(Ok(Some((*member.class, member.gen_tys, member.gen_effs))))
                },
                // Exactly one external candidate was found, so instantiate it
                (0, 1) => {
                    // Can't fail
                    let class_id = external_candidates.into_iter().next().unwrap();
                    // TODO: Should we make use of member information? external_candidates should probably be HashSet<MemberId>
                    let (gen_tys, gen_effs) = self.instantiate_class(class_id, item.span(), Some(ty));
                    Some(Ok(Some((class_id, gen_tys, gen_effs))))
                },
                // No implied or external candidates were found, so we can't resolve the member
                (0, 0) => {
                    // No external candidates match either, so bail
                    // TODO: Should this really generate an error? It might be that we just need more info
                    self.set_error(item_ty);
                    Some(Err(InferError::NoSuchItem(ty, self.span(ty), item)))
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
                let mut ty_gens = HashMap::default();
                let mut eff_gens = HashMap::default();
                // TODO: Also check member args?
                // TODO: Deal with maybe covering?
                // Checking these cases isn't essential for now, we can be as coarse as we like. We're only generating
                // a candidate list: checking of the finally selected member will occur later.
                if matches!(self.covers_var(ty, member.member, &mut ty_gens, &mut eff_gens), Ok(_)) {
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
            (_, TyInfo::Unknown(_)) => Some(false), // TODO: Why does this need to be Some(false)?
            (TyInfo::Gen(x, _, _), TyInfo::Gen(y, _, _)) if x == y => Some(true),
            (TyInfo::Prim(x), TyInfo::Prim(y)) if x == y => Some(true),
            (TyInfo::List(x), TyInfo::List(y)) => self.var_covers_var(x, y),
            (TyInfo::Record(xs, _), TyInfo::Record(ys, _)) if xs.len() == ys.len() => xs
                .into_iter()
                .zip(ys.into_iter())
                .fold(Some(true), |a, (x, y)| Some(a? && x.0 == y.0 && self.var_covers_var(x.1, y.1)?)),
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
            (TyInfo::Effect(x_eff, x_out, _), TyInfo::Effect(y_eff, y_out, _)) =>
                Some(self.var_covers_var(x_out, y_out)? && self.eff_covers_eff(x_eff, y_eff)?),
            // TODO: This probably isn't correct by itself, we need to enforce that `eff` is indeed empty for this to
            // be valid
            (TyInfo::Effect(_eff, out, _), _) => self.var_covers_var(out, ty),
            (_, TyInfo::Effect(_eff, out, _)) => self.var_covers_var(var, out),
            (x, y) => {
                // dbg!("{:?} covers {:?}", x, y);
                Some(false)
            },
        }
    }

    fn effs_of(&self, info: EffectInfo) -> Vec<EffectInstVar> {
        match info {
            EffectInfo::Ref(_) => unreachable!(),
            EffectInfo::Open(effs) => effs,
            EffectInfo::Closed(effs, None) => effs,
            EffectInfo::Closed(mut effs, Some(opener)) => {
                effs.append(&mut self.collect_eff_insts(opener));
                effs
            },
        }
    }

    fn eff_covers_eff(&self, var: EffectVar, eff: EffectVar) -> Option<bool> {
        let effs_cover_effs = |effs: Vec<EffectInstVar>, effs_of: Vec<EffectInstVar>| if effs
            .into_iter()
            .all(|eff| effs_of
                .iter()
                .any(|var| match (self.follow_effect_inst(*var), self.follow_effect_inst(eff)) {
                    (EffectInstInfo::Unknown, _) => false,
                    (_, EffectInstInfo::Unknown) => false,
                    (EffectInstInfo::Gen(x, _), EffectInstInfo::Gen(y, _)) if x == y => true,
                    (EffectInstInfo::Known(x, x_gen_tys), EffectInstInfo::Known(y, y_gen_tys)) if x == y => x_gen_tys
                        .into_iter()
                        .zip(y_gen_tys.into_iter())
                        .fold(Some(true), |a, (x, y)| Some(a? && self.var_covers_var(x, y)?))
                        .unwrap_or(false),
                    (_, _) => false,
                }))
        {
            Some(true)
        } else {
            dbg!("{:?} covers {:?}", self.follow_effect(var), self.follow_effect(eff));
            None
        };

        // TODO: These cases generate an obligation that lhs covers rhs. A check probably needs to be generated for this!
        match (self.follow_effect(var), self.follow_effect(eff)) {
            (var, EffectInfo::Open(effs)| EffectInfo::Closed(effs, None)) => {
                effs_cover_effs(effs, self.effs_of(var))
            },
            (EffectInfo::Closed(x_effs, Some(x_opener)), EffectInfo::Closed(y_effs, Some(y_opener))) if x_effs.is_empty() && y_effs.is_empty() => {
                self.eff_covers_eff(x_opener, y_opener)
            },
            (x, y) => {
                dbg!("{:?} covers {:?}", x, y);
                None
            },
        }
    }

    fn class_covers_class(&self, x: ClassVar, y: ClassVar) -> Option<bool> {
        match (self.follow_class(x), self.follow_class(y)) {
            (ClassInfo::Unknown, _) => Some(false),
            (_, ClassInfo::Unknown) => None,
            (ClassInfo::Ref(_), _) | (_, ClassInfo::Ref(_)) => unreachable!(),
            (ClassInfo::Known(class_id_x, x_gen_tys, x_gen_effs), ClassInfo::Known(class_id_y, y_gen_tys, y_gen_effs)) if class_id_x == class_id_y => Some(x_gen_tys
                .into_iter()
                .zip(y_gen_tys.into_iter())
                .fold(Some(true), |a, (x, y)| Some(a? && self.var_covers_var(x, y)?))? && x_gen_effs
                    .into_iter()
                    .zip(y_gen_effs.into_iter())
                    .fold(Some(true), |a, (x, y)| Some(a? && self.eff_covers_eff(x, y)?))?),
            (x, y) => {
                dbg!("{:?} covers {:?}", x, y);
                Some(false)
            },
        }
    }

    // Returns:
    // Ok(true) => ty definitely covers var (i.e: var is a structural subset of ty)
    // Ok(false) => ty may cover var
    // Err(()) => ty definitely does not cover var
    // This method also checks that generic types in `ty` all uniquely map to a corresponding type within `var`.
    // This prevents `(A, A)` from covering `(Nat, Bool)`, since obviously the generic type `A` does not *uniquely*
    // correspond to both `Nat` and `Bool` at the same time.
    // TODO: Flip arg order
    fn covers_var(
        &self,
        var: TyVar,
        ty: TyId,
        ty_gens: &mut HashMap<usize, TyVar>,
        eff_gens: &mut HashMap<usize, (EffectVar, TyVar)>,
    ) -> Result<bool, ()> {
        match (self.follow_info(var), self.ctx.tys.get(ty)) {
            // Blanket impls match everything
            // TODO: Match instantiations of each generic type with one-another
            (_, Ty::Gen(idx, _)) => {
                let other_gen_var = *ty_gens.entry(idx).or_insert(var);
                // Invariance check of generic types with one-another
                match (self.check_flow(var, other_gen_var), self.check_flow(other_gen_var, var)) {
                    (Some(true), Some(true)) => Ok(true),
                    (Some(false), _) | (_, Some(false)) => Err(()),
                    (_, _) => Ok(false),
                }
            },
            (TyInfo::Prim(x), Ty::Prim(y)) if x == y => Ok(true),
            (TyInfo::List(x), Ty::List(y)) => self.covers_var(x, y, ty_gens, eff_gens),
            // TODO: Care about field names!
            (TyInfo::Record(xs, _), Ty::Record(ys, _)) if xs.len() == ys.len() => xs
                .into_iter()
                .zip(ys.into_iter())
                .try_fold(true, |a, ((_, x), (_, y))| Ok(a && self.covers_var(x, y, ty_gens, eff_gens)?)),
            (TyInfo::Func(x_i, x_o), Ty::Func(y_i, y_o)) => {
                Ok(self.covers_var(x_i, y_i, ty_gens, eff_gens)? && self.covers_var(x_o, y_o, ty_gens, eff_gens)?)
            },
            (TyInfo::Data(x, xs), Ty::Data(y, ys)) if x == y && xs.len() == ys.len() => xs
                .into_iter()
                .zip(ys.into_iter())
                .try_fold(true, |a, (x, y)| Ok(a && self.covers_var(x, y, ty_gens, eff_gens)?)),
            (TyInfo::Assoc(x, class_x, assoc_x), Ty::Assoc(y, (class_id_y, y_gen_tys, y_gen_effs), assoc_y))
                if assoc_x == assoc_y => Ok(self.covers_var(x, y, ty_gens, eff_gens)? && match self.follow_class(class_x) {
                    ClassInfo::Ref(_) => unreachable!(),
                    ClassInfo::Unknown => false, // TODO: correct?
                    ClassInfo::Known(class_id_x, x_gen_tys, x_gen_effs) => class_id_x == class_id_y && x_gen_tys
                        .into_iter()
                        .zip(y_gen_tys.into_iter())
                        .try_fold(true, |a, (x, y)| Ok(a && self.covers_var(x, y, ty_gens, eff_gens)?))?,
                }),
            (TyInfo::Effect(x_eff, x_out, _), Ty::Effect(y_eff, y_out)) =>
                Ok(self.covers_var(x_out, y_out, ty_gens, eff_gens)? && self.covers_var_eff((x_eff, var), y_eff, ty_gens, eff_gens)?),
            // TODO: This probably isn't correct by itself, we need to enforce that `eff` is indeed empty for this to
            // be valid
            (TyInfo::Effect(_eff, out, _), _) => self.covers_var(out, ty, ty_gens, eff_gens),
            (_, Ty::Effect(_eff, out)) => self.covers_var(var, out, ty_gens, eff_gens),
            // (_, Ty::Effect(_eff, out)) => self.covers_var(var, out, gens),

            // Unknown types *could* match, maybe
            (TyInfo::Unknown(_), _) => Ok(false),
            _ => Err(()),
        }
    }

    fn covers_var_eff(
        &self,
        (var, var_ty): (EffectVar, TyVar),
        eff: EffectId,
        ty_gens: &mut HashMap<usize, TyVar>,
        eff_gens: &mut HashMap<usize, (EffectVar, TyVar)>,
    ) -> Result<bool, ()> {
        match (self.follow_effect(var), self.ctx.tys.get_effect(eff)) {
            (EffectInfo::Open(var_effs) | EffectInfo::Closed(var_effs, None), Effect::Known(effs)) => {
                if var_effs
                    .into_iter()
                    .all(|x| effs
                        .iter()
                        .any(|y| match (self.follow_effect_inst(x), y) {
                            (_, Ok(EffectInst::Gen(idx, _))) => if let Some((other_gen_var, other_ty)) = eff_gens.get(&idx) {
                                // Invariance check of generic types with one-another
                                match (
                                    self.check_flow_eff((var, var_ty), (*other_gen_var, *other_ty)),
                                    self.check_flow_eff((*other_gen_var, *other_ty), (var, var_ty)),
                                ) {
                                    (Some(true), Some(true)) => true,
                                    (Some(false), _) | (_, Some(false)) => false,
                                    (_, _) => false,
                                }
                            } else {
                                eff_gens.insert(*idx, (var, var_ty));
                                true
                            },
                            (EffectInstInfo::Known(x, x_gen_tys), Ok(EffectInst::Concrete(y, y_gen_tys))) if x == *y => x_gen_tys
                                .into_iter()
                                .zip(y_gen_tys.into_iter())
                                .try_fold(true, |a, (x, y)| Ok(a && self.covers_var(x, *y, ty_gens, eff_gens)?))
                                .unwrap_or_else(|()| false),
                            (x, y) => {
                                dbg!("{:?} covers {:?}", x, y);
                                false
                            },
                        }))
                {
                    Ok(true)
                } else {
                    dbg!("{:?} covers {:?}", self.follow_effect(var), self.ctx.tys.get_effect(eff));
                    Err(())
                }
            },
            (EffectInfo::Closed(var_effs, Some(opener)), _) if var_effs.is_empty() => {
                self.covers_var_eff((opener, var_ty), eff, ty_gens, eff_gens)
            },
            (x, y) => {
                dbg!("{:?} covers {:?}", x, y);
                Err(())
            }, // TODO: Other cases
        }
    }

    // Link the generic types of a class member with type variables in the current scope so obligations
    // can be generated
    // TODO: Derive links for effects
    fn derive_links(
        &mut self,
        member: TyId,
        ty: TyVar,
        self_ty: Option<TyVar>,
        use_span: Span,
        ty_links: &mut HashMap<usize, TyVar>,
        eff_links: &mut HashMap<usize, EffectVar>,
    ) {
        match (self.ctx.tys.get(member), self.follow_info(ty)) {
            (Ty::Gen(gen_idx, _), _) => { ty_links.insert(gen_idx, ty); },
            // If we try to derive links through an unknown type variable, we instantiate the reified type into the
            // current context, generating free type variables for any as-yet unseen generics. This allows
            // maybe-covering impls to work.
            // TODO: This can potentially result in orphaned free type variables that never get inferred, despite the
            // program being well-typed, which could cause phantom errors, or maybe this is fine?
            (_, TyInfo::Unknown(inst_span)) => {
                let span = self.ctx().tys.get_span(member);
                let inst_ty = self.instantiate(
                    member,
                    span,
                    &mut |idx, _, this: &mut Self| Some(*ty_links.entry(idx).or_insert_with(|| this.insert(use_span, TyInfo::Unknown(Some(this.span(ty)))))),
                    &mut |idx, this: &mut Self| Some(*eff_links.entry(idx).or_insert_with(|| this.insert_effect(use_span, EffectInfo::Open(Vec::new())))),
                    self_ty,
                );
                self.make_flow(ty, inst_ty, use_span);
            },
            (Ty::List(x), TyInfo::List(y)) => self.derive_links(x, y, self_ty, use_span, ty_links, eff_links),
            (Ty::Record(xs, _), TyInfo::Record(ys, _)) => xs
                .into_iter()
                .zip(ys.into_iter())
                .for_each(|((_, x), (_, y))| self.derive_links(x, y, self_ty, use_span, ty_links, eff_links)),
            (Ty::Func(x_i, x_o), TyInfo::Func(y_i, y_o)) => {
                self.derive_links(x_i, y_i, self_ty, use_span, ty_links, eff_links);
                self.derive_links(x_o, y_o, self_ty, use_span, ty_links, eff_links);
            },
            (Ty::Data(_, xs), TyInfo::Data(_, ys)) => xs
                .into_iter()
                .zip(ys.into_iter())
                .for_each(|(x, y)| self.derive_links(x, y, self_ty, use_span, ty_links, eff_links)),
            (Ty::Assoc(x, (class_id_x, x_gen_tys, x_gen_effs), assoc_x), TyInfo::Assoc(y, class_y, assoc_y))
                if assoc_x == assoc_y => {
                    self.derive_links(x, y, self_ty, use_span, ty_links, eff_links);
                    match self.follow_class(class_y) {
                        ClassInfo::Unknown => panic!("Deriving links for unknown class!"),
                        ClassInfo::Ref(_) => unreachable!(),
                        ClassInfo::Known(class_id_y, y_gen_tys, y_gen_effs) => {
                            assert!(class_id_x == class_id_y);
                            x_gen_tys
                                .into_iter()
                                .zip(y_gen_tys.into_iter())
                                .for_each(|(x, y)| self.derive_links(x, y, self_ty, use_span, ty_links, eff_links));
                            x_gen_effs
                                .into_iter()
                                .zip(y_gen_effs.into_iter())
                                .for_each(|(x, y)| {
                                    if let Some(x) = x {
                                        self.derive_links_eff(x, y, self_ty, use_span, ty_links, eff_links)
                                    }
                                });
                        },
                    }
                },
            (Ty::Effect(_, _), TyInfo::Effect(_, _, _)) => todo!(),
            (Ty::Effect(eff, out), _) => {
                let eff_var = self.insert_effect(self.span(ty), EffectInfo::Closed(Vec::new(), None));
                self.derive_links_eff(eff, eff_var, self_ty, use_span, ty_links, eff_links);
                self.derive_links(out, ty, self_ty, use_span, ty_links, eff_links)
            },
            (_, TyInfo::Effect(eff, out, _)) => {
                // Make sure we actually can see through this effect!
                // TODO: Make this generate better errors
                self.checks.push_back(Check::CheckEffectEmpty((eff, ty), ty, EqInfo::default()));

                self.derive_links(member, out, self_ty, use_span, ty_links, eff_links)
            },
            (_, TyInfo::SelfType) => panic!("Self type not permitted here"),
            _ => {}, // Only type constructors and generic types generate obligations
        }
    }

    fn derive_links_eff(
        &mut self,
        member: EffectId,
        eff: EffectVar,
        self_ty: Option<TyVar>,
        use_span: Span,
        ty_links: &mut HashMap<usize, TyVar>,
        eff_links: &mut HashMap<usize, EffectVar>,
    ) {
        match (self.ctx.tys.get_effect(member), self.follow_effect(eff)) {
            (Effect::Known(effs), EffectInfo::Open(member_effs)| EffectInfo::Closed(member_effs, None)) if effs.is_empty() && member_effs.is_empty() => {}
            (Effect::Known(mut effs), info) if effs.len() == 1 => {
                let eff_id = effs.remove(0);
                for eff_var in self.effs_of(info) {
                    match (&eff_id, self.follow_effect_inst(eff_var)) {
                        (Ok(EffectInst::Gen(idx, _)), _) => { eff_links.insert(*idx, eff); },
                        (Ok(EffectInst::Concrete(x, x_gen_tys)), EffectInstInfo::Known(y, y_gen_tys)) if *x == y => x_gen_tys
                            .into_iter()
                            .zip(y_gen_tys.into_iter())
                            .for_each(|(x, y)| self.derive_links(*x, y, self_ty, use_span, ty_links, eff_links)),
                        (x, y) => todo!("Derive links between {:?} and {:?}", x, y),
                    }
                }
            }
            (_, EffectInfo::Closed(member_effs, Some(opener))) if member_effs.is_empty() => {
                self.derive_links_eff(member, opener, self_ty, use_span, ty_links, eff_links)
            },
            (x, y) => todo!("Derive links between {:?} and {:?} (effs_of(y) = {:?})", x, y, self.effs_of(y.clone())),
        }
    }

    /// Resolve a class obligation for a type, returning the ID of the type's membership. If no member can be provided
    /// (because, for example, the membership is implied by a generic bound) then `Err(false)` is returned instead. If
    /// resolution failed due to an existing error, `Err(true)` is returned.
    fn resolve_obligation(
        &mut self,
        proof_stack: &mut Vec<(TyVar, ClassId, Vec<TyVar>)>,
        ty: TyVar,
        (class_id, class_gen_tys, class_gen_effs): (ClassId, Vec<TyVar>, Vec<EffectVar>),
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
                            && class_gen_tys.iter()
                                .zip(member.gen_tys.iter())
                                .fold(Some(true), |a, (ty, arg)| Some(a? && Self::var_covers_var(self, *ty, *arg)?))?
                            && class_gen_effs.iter()
                                .zip(member.gen_effs.iter())
                                .fold(Some(true), |a, (eff, arg)| Some(a? && Self::eff_covers_eff(self, *eff, *arg)?))?
                        {
                            // Try to select an implied member where the real member is known
                            // TODO: Is multiple covering impls an error? Should coherence prevent this case?
                            selected = Some(selected
                                .zip_with(Some(member), |s: &InferImpliedMember, m| if matches!(&s.items, ImpliedItems::Eq(_)) {
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
                    let class = self.insert_class(obl_span, ClassInfo::Known(class_id, class_gen_tys.clone(), class_gen_effs.clone()));
                    // if let ImpliedItems::Eq(assoc_set) = &items {
                    //     for (assoc, assoc_ty) in assoc {
                    //         if let Some((name, ty)) = assoc_set.iter().find(|(name, _)| **name == *assoc) {
                    //             self.make_flow(*ty, assoc_ty, name.span());
                    //         }
                    //     }
                    // }

                    Some(Ok(Ok(items)))
                } else {
                    // Find a class member declaration that covers our type
                    let mut covering_members = Vec::new();
                    let mut maybe_covering_members = Vec::new();
                    for (member_id, member) in self.ctx.classes.members_of(class_id) {
                        // println!("Checking coverage of {:?} by {} via member {}", self.follow_info(ty), *self.ctx.classes.get(class_id).name, self.ctx.tys.display(&self.ctx, member.member));
                        let mut ty_gens = HashMap::new();
                        let mut eff_gens = HashMap::new();
                        // TODO: Instantiation should occur here, in a temporary inference context, to allow type projections to occur
                        let (covers, maybe_covers) = match Self::covers_var(self, ty, member.member, &mut ty_gens, &mut eff_gens)
                            .and_then(|covers| Ok(covers && class_gen_tys.iter()
                                .zip(member.gen_tys.iter())
                                .try_fold(true, |a, (ty, arg)| Ok(a && Self::covers_var(self, *ty, *arg, &mut ty_gens, &mut eff_gens)?))?))
                            .and_then(|covers| Ok(covers && class_gen_effs.iter()
                                .zip(member.gen_effs.iter())
                                .try_fold(true, |a, (eff, arg)| if let Some(arg) = *arg {
                                    Ok(a && Self::covers_var_eff(self, (*eff, ty), arg, &mut ty_gens, &mut eff_gens)?)
                                } else {
                                    // Errors always fail
                                    Err(())
                                })?))
                        {
                            Ok(true) => (true, false),
                            Ok(false) => (false, true),
                            Err(()) => (false, false),
                        };

                        if covers { covering_members.push((member_id, member)); }
                        if maybe_covers { maybe_covering_members.push((member_id, member)); }
                    }

                    match (covering_members.len(), maybe_covering_members.len()) {
                        // No covering members (perhaps we need more information?
                        (0, 0) => {
                            // println!(
                            //     "Failed to solve {:?} as member of {}{}!",
                            //     self.follow_info(ty),
                            //     *self.ctx.classes.get(class_id).name,
                            //     gen_tys.iter().map(|ty| format!(" {:?}", self.follow_info(*ty))).collect::<String>(),
                            // );
                            let gen_span = if let TyInfo::Gen(gen_idx, gen_scope, _) = self.follow_info(ty) {
                                Some(self.ctx.tys.get_gen_scope(gen_scope).get(gen_idx).name.span())
                            } else {
                                None
                            };
                            Some(Err(InferError::TypeDoesNotFulfil(self.insert_class(obl_span, ClassInfo::Known(class_id, class_gen_tys, class_gen_effs)), ty, obl_span, gen_span, use_span)))
                            // None
                        },
                        // Exactly one definitely covering member, one one possible-covering member: great, we know
                        // what to substitute!
                        (1, _) | (0, 1) => {
                            // Can't fail
                            let (covering_member_id, covering_member) = covering_members
                                .into_iter().next()
                                .or(maybe_covering_members.into_iter().next())
                                .unwrap();

                            let covering_member_member = covering_member.member;
                            let covering_member_gen_tys = covering_member.gen_tys.clone();
                            let covering_member_gen_effs = covering_member.gen_effs.clone();
                            let covering_member_gen_scope = covering_member.gen_scope;
                            let covering_member_assoc = covering_member.assoc.clone();

                            // Derive generic types that need substituting from the member
                            let mut ty_links = HashMap::new();
                            let mut eff_links = HashMap::new();
                            self.derive_links(covering_member_member, ty, Some(ty), use_span, &mut ty_links, &mut eff_links);
                            for (member_gen_ty, gen_ty) in covering_member_gen_tys.iter().zip(class_gen_tys.iter()) {
                                self.derive_links(*member_gen_ty, *gen_ty, Some(ty), use_span, &mut ty_links, &mut eff_links);
                            }
                            for (member_gen_eff, gen_eff) in covering_member_gen_effs.iter().zip(class_gen_effs.iter()) {
                                if let Some(member_gen_eff) = member_gen_eff {
                                    self.derive_links_eff(*member_gen_eff, *gen_eff, Some(ty), use_span, &mut ty_links, &mut eff_links);
                                }
                            }

                            let covering_gen_scope = self.ctx.tys.get_gen_scope(covering_member_gen_scope);
                            for member in covering_gen_scope.implied_members.as_ref().expect("Implied members must be known here").clone() {
                                // println!("Deriving links from {}, {:?} in {}", self.ctx().tys.display(self.ctx(), covering_member_member), self.follow_info(ty), self.ctx().tys.get_span(*member.member).src());
                                let member_ty = self.instantiate(
                                    *member.member,
                                    member.member.span(),
                                    &mut |idx, _, _| ty_links.get(&idx).copied(),
                                    &mut |idx, _| eff_links.get(&idx).copied(),
                                    Some(ty),
                                );
                                let member_gen_tys = member.gen_tys
                                    .iter()
                                    .map(|gen_ty| self.instantiate(
                                        *gen_ty,
                                        member.member.span(),
                                        &mut |idx, _, _| ty_links.get(&idx).copied(),
                                        &mut |idx, _| eff_links.get(&idx).copied(),
                                        Some(ty),
                                    ))
                                    .collect::<Vec<_>>();
                                let member_gen_effs = member.gen_effs
                                    .iter()
                                    .map(|gen_eff| match gen_eff.map(|gen_eff| self.instantiate_eff(
                                            gen_eff,
                                            member.member.span(),
                                            &mut |idx, _, _| ty_links.get(&idx).copied(),
                                            &mut |idx, _| eff_links.get(&idx).copied(),
                                            Some(ty),
                                        ))
                                    {
                                        Some(Ok(Some(eff))) => eff,
                                        // TODO: Make sure this is fine in all other cases. It should be?
                                        _ => self.insert_effect(member.member.span(), EffectInfo::Open(Vec::new())),
                                    })
                                    .collect::<Vec<_>>();

                                let member_assoc = match &member.items {
                                    ImpliedItems::Eq(assoc) => assoc
                                        .iter()
                                        .map(|(name, assoc)| (name.clone(), self.instantiate(
                                            *assoc,
                                            member.member.span(),
                                            &mut |idx, _, _| ty_links.get(&idx).copied(),
                                            &mut |idx, _| eff_links.get(&idx).copied(),
                                            Some(ty),
                                        )))
                                        .collect::<Vec<_>>(),
                                    ImpliedItems::Real(_) => Vec::new(),
                                };

                                let proof_key = (member_ty, *member.class, member_gen_tys.clone());
                                if proof_stack.contains(&proof_key) {
                                    return Some(Err(InferError::CycleWhenResolving(ty, (class_id, class_gen_tys.clone(), class_gen_effs.clone()), member.span())));
                                } else {
                                    // println!(
                                    //     "Trying to prove that {:?} is a member of {}{}!",
                                    //     self.follow_info(member_ty),
                                    //     *self.ctx.classes.get(*member.class).name,
                                    //     member_args.iter().map(|arg| format!(" {:?}", self.follow_info(*arg))).collect::<String>(),
                                    // );
                                    proof_stack.push(proof_key);
                                    let res = self.resolve_obligation(
                                        proof_stack,
                                        member_ty,
                                        (*member.class, member_gen_tys.clone(), member_gen_effs.clone()),
                                        member_assoc,
                                        member.class.span(),
                                        use_span,
                                    );
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
                                        // Successful, but only because an existing error occurred
                                        // TODO: Return immediately, or continue to generate errors?
                                        Some(Ok(Err(()))) => {},//return Some(Ok(Err(()))),
                                        Some(Err(err)) => return Some(Err(err)),
                                        None => {
                                            // Not enough information to resolve it yet? That's okay! Take it as a win
                                            // for now, but require that this gets solved later!
                                            self.make_impl(
                                                member_ty,
                                                (*member.class, member_gen_tys, member_gen_effs),
                                                member.class.span(),
                                                vec![],
                                                use_span,
                                            );
                                        },
                                    }
                                }
                            }

                            // Unify member and class args: we did it, patrick!
                            // TODO: Do we *need* to do this?
                            // TODO: Move assoc flowing from obligation constraint to here too
                            let member_ty = self.instantiate(
                                covering_member_member,
                                use_span,
                                &mut |idx, _, _| ty_links.get(&idx).copied(),
                                &mut |idx, _| eff_links.get(&idx).copied(),
                                Some(ty),
                            );
                            self.make_flow(ty, member_ty, EqInfo::from(use_span));
                            for (gen_ty, covering_gen_ty) in class_gen_tys.iter().zip(covering_member_gen_tys.iter()) {
                                let covering_gen_ty = self.instantiate(
                                    *covering_gen_ty,
                                    use_span,
                                    &mut |idx, _, _| ty_links.get(&idx).copied(),
                                    &mut |idx, _| eff_links.get(&idx).copied(),
                                    Some(ty),
                                );
                                self.make_flow(*gen_ty, covering_gen_ty, EqInfo::from(use_span));
                            }

                            for (gen_eff, covering_gen_eff) in class_gen_effs.iter().zip(covering_member_gen_effs.iter()) {
                                if let Some(covering_gen_eff) = *covering_gen_eff {
                                    let covering_gen_eff = match self.instantiate_eff(
                                        covering_gen_eff,
                                        use_span,
                                        &mut |idx, _, _| ty_links.get(&idx).copied(),
                                        &mut |idx, infer| Some(*eff_links
                                            .entry(idx)
                                            .or_insert_with(|| infer.insert_effect(use_span, EffectInfo::Open(Vec::new())))),
                                        Some(ty),
                                    ) {
                                        Ok(Some(eff)) => eff,
                                        // TODO: Make sure this is fine in all other cases. It should be?
                                        _ => self.insert_effect(use_span, EffectInfo::Open(Vec::new())),
                                    };
                                    self.make_flow_effect((*gen_eff, member_ty), (covering_gen_eff, member_ty), EqInfo::from(use_span));
                                }
                            }

                            // TODO: This is currently done by the caller, I think
                            // if let Some(covering_member_assoc) = covering_member_assoc {
                            //     for (assoc, assoc_ty) in assoc {
                            //         if let Some((name, covering_assoc)) = covering_member_assoc.iter().find(|(name, _)| **name == *assoc) {
                            //             let covering_assoc = self.instantiate(
                            //                 *covering_assoc,
                            //                 use_span,
                            //                 &mut |idx, _, _| ty_links.get(&idx).copied(),
                            //                 &mut |idx, _| eff_links.get(&idx).copied(),
                            //                 Some(ty),
                            //             );
                            //             self.make_flow(covering_assoc, assoc_ty, EqInfo::from(use_span));
                            //         }
                            //     }
                            // } else {
                            //     println!("Covering member assoc not defined...");
                            // }

                            Some(Ok(Ok(ImpliedItems::Real(covering_member_id))))
                        },
                        // Multiple covering members: we don't know which one to pick!
                        // This *probably* happened because we don't have have enough information about the member or
                        // about available members, so bail for now and resolve this later.
                        // TODO: Consider constraints?
                        (2.., _) => {
                            println!(
                                "Incoherence when solving {:?} as member of {}{}!",
                                self.follow_info(ty),
                                *self.ctx.classes.get(class_id).name,
                                class_gen_tys.iter().map(|arg| format!(" {:?}", self.follow_info(*arg))).collect::<String>(),
                            );
                            println!("Members are:");
                            for (member_id, member) in covering_members {
                                println!(
                                    "{} < {}{} (in {})",
                                    self.ctx().tys.display(self.ctx(), member.member),
                                    *self.ctx.classes.get(class_id).name,
                                    member.gen_tys.iter().map(|ty| format!(" {}", self.ctx().tys.display(self.ctx(), *ty))).collect::<String>(),
                                    self.ctx().tys.get_span(member.member).src(),
                                );
                            }
                            println!("This probably should be caught by a member overlap/coherence checker");
                            println!("Note that Tao's understanding of a member overlap is current unnecessarily conservative and does not consider type obligations");
                            None
                        },
                        (_, _) => None,
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

        for check in std::mem::take(&mut self.checks) {
            match self.check(check) {
                Ok(()) => {},
                Err(e) => self.errors.push(e),
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
                    // println!("Uninferred type generated at {:?}", self.vars[ty.0].3);
                    errors.push(InferError::CannotInfer(ty, origin));
                    self.set_error(ty);
                }
            }
        }
        for (span, info) in self.effect_inst_vars.clone() {
            if let EffectInstInfo::Unknown = info {
                self.ctx.emit(Error::CannotInferEffect(span, Err(())));
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
                Constraint::ClassAssoc(ty, _class, assoc, assoc_ty, _span) => {
                    if !self.is_error(ty) && !self.is_error(assoc_ty) {
                        errors.push(InferError::AmbiguousClassItem(assoc, Vec::new()))
                    }
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
                InferError::CannotInferEffect(eff) => Error::CannotInferEffect(checked.infer.effect_inst_vars[eff.0].0, checked.reify_effect_inst(eff)),
                InferError::Recursive(a, part) => Error::Recursive(checked.reify(a), checked.infer.span(a), checked.infer.span(part)),
                InferError::NoSuchItem(a, record_span, field) => Error::NoSuchItem(checked.reify(a), record_span, field),
                InferError::NoSuchField(a, record_span, field) => Error::NoSuchField(checked.reify(a), record_span, field),
                InferError::TypeDoesNotFulfil(class, ty, obl_span, gen_span, use_span) => {
                    let class = match checked.infer.follow_class(class) {
                        ClassInfo::Unknown => None,
                        ClassInfo::Ref(_) => unreachable!(),
                        ClassInfo::Known(class_id, gen_tys, gen_effs) => Some((
                            class_id,
                            gen_tys
                                .iter()
                                .map(|ty| checked.reify(*ty))
                                .collect(),
                            gen_effs
                                .iter()
                                .map(|eff| checked.reify_effect(*eff))
                                .collect(),
                        )),
                    };
                    Error::TypeDoesNotFulfil(class, checked.reify(ty), obl_span, gen_span, use_span)
                },
                InferError::RecursiveAlias(alias, a, span) => Error::RecursiveAlias(alias, checked.reify(a), span),
                InferError::PatternNotSupported(lhs, op, rhs, span) => Error::PatternNotSupported(checked.reify(lhs), op, checked.reify(rhs), span),
                InferError::AmbiguousClassItem(field, candidate_classes) => Error::AmbiguousClassItem(field, candidate_classes),
                InferError::CycleWhenResolving(ty, (class_id, gen_tys, gen_effs), cycle_span) => Error::CycleWhenResolving(
                    checked.reify(ty),
                    (
                        class_id,
                        gen_tys
                            .iter()
                            .map(|ty| checked.reify(*ty))
                            .collect(),
                        gen_effs
                            .iter()
                            .map(|eff| checked.reify_effect(*eff))
                            .collect(),
                    ),
                    cycle_span,
                ),
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
                TyInfo::Opaque(_, _) => Ty::Error(ErrorReason::Unknown),
                TyInfo::Error(reason) => Ty::Error(reason),
                TyInfo::Prim(prim) => Ty::Prim(prim),
                TyInfo::List(item) => Ty::List(self.reify_inner(item)),
                TyInfo::Record(fields, is_tuple) => Ty::Record(fields
                    .into_iter()
                    .map(|(name, field)| (name, self.reify_inner(field)))
                    .collect(), is_tuple),
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
                    ClassInfo::Known(class_id, gen_tys, gen_effs) => Ty::Assoc(
                        self.reify_inner(inner),
                        (
                            class_id,
                            gen_tys
                                .into_iter()
                                .map(|ty| self.reify_inner(ty))
                                .collect(),
                            gen_effs
                                .into_iter()
                                .map(|eff| self.reify_effect(eff))
                                .collect(),
                        ),
                        assoc,
                    ),
                },
                TyInfo::Effect(eff, out, _opaque) => match self.reify_effect(eff) {
                    Some(eff) => Ty::Effect(eff, self.reify_inner(out)),
                    None => return self.reify_inner(out),
                },
            };
            self.infer.ctx.tys.insert(self.infer.span(var), ty)
        }
    }

    pub fn reify(&mut self, var: TyVar) -> TyId {
        let ty = self.reify_inner(var);
        self.ty_cache.insert(var, ty);
        ty
    }

    pub fn reify_effect_inst(&mut self, var: EffectInstVar) -> Result<EffectInst, ()> {
        match self.infer.follow_effect_inst(var) {
            EffectInstInfo::Unknown => Err(()),
            EffectInstInfo::Error => Err(()),
            EffectInstInfo::Ref(x) => return self.reify_effect_inst(x),
            EffectInstInfo::Gen(idx, gen_scope) => Ok(EffectInst::Gen(idx, gen_scope)),
            EffectInstInfo::Known(eff, args) => Ok(EffectInst::Concrete(eff, args
                .into_iter()
                .map(|arg| self.reify_inner(arg))
                .collect())),
        }
    }

    // None means that there is no effect
    pub fn reify_effect(&mut self, var: EffectVar) -> Option<EffectId> {
        let eff = if let Some(eff) = self.eff_cache.get(&var) {
            *eff
        } else {
            let sort_and_order = |this: &mut Self, effs: &mut Vec<_>| {
                // Sort effects into a canonical order so we can compare them easily later
                effs.sort_by(|x, y| match (x, y) {
                    (Ok(EffectInst::Concrete(x, x_params)), Ok(EffectInst::Concrete(y, y_params))) => x.cmp(y).then_with(|| x_params
                        .iter()
                        .zip(y_params)
                        .fold(Ordering::Equal, |a, (x, y)| a.then_with(|| this.infer.ctx().tys.cmp_ty(*x, *y)))),
                    (Ok(EffectInst::Gen(x, _)), Ok(EffectInst::Gen(y, _))) => x.cmp(y),
                    (_, _) => Ordering::Equal,
                });
                effs.dedup_by(|x, y| match (x, y) {
                    (Ok(EffectInst::Concrete(x, x_params)), Ok(EffectInst::Concrete(y, y_params))) => x == y && x_params
                        .iter()
                        .zip(y_params)
                        .all(|(x, y)| this.infer.ctx().tys.cmp_ty(*x, *y).is_eq()),
                    (Ok(EffectInst::Gen(x, _)), Ok(EffectInst::Gen(y, _))) => x == y,
                    _ => false,
                });
            };

            let eff = match self.infer.follow_effect(var) {
                EffectInfo::Ref(_) => unreachable!(),
                EffectInfo::Open(effs) | EffectInfo::Closed(effs, None) if effs.is_empty() => return None,
                EffectInfo::Open(effs) | EffectInfo::Closed(effs, None) => {
                    let mut effs = effs
                        .into_iter()
                        .map(|eff| self.reify_effect_inst(eff))
                        .collect::<Vec<_>>();
                    sort_and_order(self, &mut effs);
                    Effect::Known(effs)
                },
                EffectInfo::Closed(effs, Some(opener)) => {
                    let opener = self.infer.collect_eff_insts(opener)
                        .into_iter()
                        .map(|eff| self.reify_effect_inst(eff))
                        .collect::<Vec<_>>();
                    let mut effs = effs
                        .into_iter()
                        .map(|eff| self.reify_effect_inst(eff))
                        .chain(opener)
                        .collect::<Vec<_>>();
                    sort_and_order(self, &mut effs);
                    Effect::Known(effs)
                },
            };
            self.infer.ctx.tys.insert_effect(self.infer.effect_span(var), eff)
        };
        self.eff_cache.insert(var, eff);
        Some(eff)
    }

    pub fn reify_class(&mut self, class: ClassVar) -> Option<(ClassId, Vec<TyId>, Vec<Option<EffectId>>)> {
        match self.infer.follow_class(class) {
            ClassInfo::Unknown => None,
            ClassInfo::Ref(_) => unreachable!(),
            ClassInfo::Known(class_id, gen_tys, gen_effs) => Some((
                class_id,
                gen_tys
                    .into_iter()
                    .map(|ty| self.reify_inner(ty))
                    .collect(),
                gen_effs
                    .into_iter()
                    .map(|eff| self.reify_effect(eff))
                    .collect(),
            )),
        }
    }
}
