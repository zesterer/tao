use super::*;
use std::{
    cmp::Ordering,
    rc::Rc,
};

pub type TyMeta = (Span, TyId);
pub type TyNode<T> = Node<T, TyMeta>;

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Prim {
    Nat,
    Int,
    Real,
    Char,
    Universe,
}

impl fmt::Display for Prim {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Prim::Nat => write!(f, "Nat"),
            Prim::Int => write!(f, "Int"),
            Prim::Real => write!(f, "Real"),
            Prim::Char => write!(f, "Char"),
            Prim::Universe => write!(f, "@"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ErrorReason {
    Unknown,
    Recursive,
    Invalid,
}

#[derive(Clone, Debug)]
pub enum Ty {
    Error(ErrorReason),
    Prim(Prim),
    List(TyId),
    // (_, is_tuple)
    Record(BTreeMap<Ident, TyId>, bool),
    Func(TyId, TyId),
    Data(DataId, Vec<TyId>),
    Gen(usize, GenScopeId),
    SelfType,
    Assoc(TyId, (ClassId, Vec<TyId>, Vec<Option<EffectId>>), SrcNode<Ident>),
    Effect(EffectId, TyId),
}

pub type TyId = Id<(Span, Ty)>;

#[derive(Clone, Debug)]
pub enum EffectInst {
    Concrete(EffectDeclId, Vec<TyId>),
    Gen(usize, GenScopeId),
}

#[derive(Clone, Debug)]
pub enum Effect {
    Error,
    Known(Vec<Result<EffectInst, ()>>),
}

pub type EffectId = Id<(Span, Effect)>;

#[derive(Default)]
pub struct Types {
    tys: Index<(Span, Ty)>,
    effects: Index<(Span, Effect)>,
    scopes: Vec<GenScope>,
}

impl Types {
    pub fn get_gen_scope(&self, scope: GenScopeId) -> &GenScope {
        &self.scopes[scope.0]
    }

    pub fn get_gen_scope_mut(&mut self, scope: GenScopeId) -> &mut GenScope {
        &mut self.scopes[scope.0]
    }

    pub fn insert_gen_scope(&mut self, gen_scope: GenScope) -> GenScopeId {
        let id = GenScopeId(self.scopes.len());
        self.scopes.push(gen_scope);
        id
    }

    pub fn gen_scope_ids(&self) -> impl Iterator<Item = GenScopeId> {
        (0..self.scopes.len()).map(GenScopeId)
    }

    pub fn get(&self, ty: TyId) -> Ty {
        self.tys[ty].1.clone()
    }

    pub fn get_span(&self, ty: TyId) -> Span {
        self.tys[ty].0
    }

    pub fn insert(&mut self, span: Span, ty: Ty) -> TyId {
        self.tys.add((span, ty))
    }

    pub fn cmp_eff(&self, x: EffectId, y: EffectId) -> Ordering {
        match (self.get_effect(x), self.get_effect(y)) {
            (Effect::Error, _) => Ordering::Equal,
            (_, Effect::Error) => Ordering::Equal,
            // Assumes canonical order
            (Effect::Known(xs), Effect::Known(ys)) => xs.len().cmp(&ys.len()).then_with(|| xs
                .into_iter()
                .zip(ys)
                .fold(Ordering::Equal, |a, (x, y)| a.then_with(|| match (x, y) {
                    (Ok(EffectInst::Concrete(x_decl, x_args)), Ok(EffectInst::Concrete(y_decl, y_args))) => x_decl
                        .cmp(&y_decl)
                        .then_with(|| x_args
                            .into_iter()
                            .zip(y_args)
                            .fold(Ordering::Equal, |a, (x, y)| a.then_with(|| self.cmp_ty(x, y)))),
                    (Ok(EffectInst::Gen(x, _)), Ok(EffectInst::Gen(y, _))) => x.cmp(&y),
                    _ => Ordering::Equal, // Errors always equal (bad?)
                }))),
        }
    }

    // Ignores gen_scope
    pub fn is_eq(&self, x: TyId, y: TyId) -> bool {
        self.cmp_ty(x, y) == Ordering::Equal
    }

    // Ignores gen_scope
    // Derive a canonical ordering for types (note: unrelated to subtyping!)
    pub fn cmp_ty(&self, x: TyId, y: TyId) -> Ordering {
        match (self.get(x), self.get(y)) {
            (Ty::Error(_), _) | (_, Ty::Error(_)) => Ordering::Equal,
            (Ty::Prim(x), Ty::Prim(y)) => x.cmp(&y),
            (Ty::List(x), Ty::List(y)) => self.cmp_ty(x, y),
            (Ty::Record(_, _), Ty::Record(_, _)) => todo!("Record equality"),
            (Ty::Func(x_i, x_o), Ty::Func(y_i, y_o)) => self.cmp_ty(x_i, y_i).then_with(|| self.cmp_ty(x_o, y_o)),
            (Ty::Data(x, xs), Ty::Data(y, ys)) => x.cmp(&y).then_with(|| xs
                .into_iter()
                .zip(ys)
                .fold(Ordering::Equal, |a, (x, y)| a.then_with(|| self.cmp_ty(x, y)))),
            (Ty::Gen(x, x_scope), Ty::Gen(y, y_scope)) => if x_scope == y_scope {
                x.cmp(&y)
            } else {
                todo!("ordering of generic types in different scopes... does reordering need to occur with every reinstantiation?!")
            },
            (Ty::SelfType, Ty::SelfType) => Ordering::Equal,
            // TODO: Check equality of effect parameters?
            (Ty::Assoc(x_ty, (x_class_id, x_gen_tys, x_gen_effs), x_name), Ty::Assoc(y_ty, (y_class_id, y_gen_tys, y_gen_effs), y_name)) => self.cmp_ty(x_ty, y_ty)
                .then_with(|| x_class_id.cmp(&y_class_id))
                .then_with(|| x_name.cmp(&y_name))
                .then_with(|| x_gen_tys
                    .into_iter()
                    .zip(y_gen_tys)
                    .fold(Ordering::Equal, |a, (x, y)| a.then_with(|| self.cmp_ty(x, y)))),
            (Ty::Effect(x, x_out), Ty::Effect(y, y_out)) =>
                // TODO: Actually compare effects
                x.cmp(&y).then_with(|| self.cmp_ty(x_out, y_out)),
            (x, y) => {
                // Generate an ordering for all other types, fairly arbitrary
                // Would be nice to use std::mem::Discriminant for this, but it's not ordered
                let rank_of = |x: &Ty| match x {
                    Ty::Error(_) => 0,
                    Ty::Prim(_) => 1,
                    Ty::List(_) => 2,
                    Ty::Record(_, _) => 3,
                    Ty::Func(_, _) => 4,
                    Ty::Data(_, _) => 5,
                    Ty::Gen(_, _) => 6,
                    Ty::SelfType => 7,
                    Ty::Assoc(_, _, _) => 8,
                    Ty::Effect(_, _) => 9,
                };

                rank_of(&x).cmp(&rank_of(&y))
            },
        }
    }

    pub fn has_inhabitants(&self, datas: &Datas, ty: TyId, gen: &mut dyn FnMut(usize) -> bool) -> bool {
        match self.get(ty) {
            Ty::Error(_) => false,
            Ty::Prim(_) => true,
            Ty::List(_) => true, // Empty list
            Ty::Record(fields, _) => fields
                .into_iter()
                .all(|(_, field)| self.has_inhabitants(datas, field, gen)),
            Ty::Func(_, _) => true,
            Ty::Data(data, args) => datas
                .get_data(data)
                .cons
                .iter()
                .any(|(_, ty)| {
                    self.has_inhabitants(datas, *ty, &mut |id| self.has_inhabitants(datas, args[id], gen))
                }),
            Ty::Gen(id, _) => gen(id),
            Ty::SelfType => true,
            Ty::Assoc(_, _, _) => true,
            // An effect is always an inhabited object until propagated, even if the output type is not inhabited
            Ty::Effect(_, _) => true,
        }
    }

    pub fn display<'a>(&'a self, ctx: &'a Context, ty: TyId) -> TyDisplay<'a> {
        TyDisplay {
            ctx,
            ty: Ok(ty),
            lhs_exposed: false,
            substitutes: Vec::new(),
        }
    }

    pub fn display_eff<'a>(&'a self, ctx: &'a Context, eff: EffectId) -> TyDisplay<'a> {
        TyDisplay {
            ctx,
            ty: Err(eff),
            lhs_exposed: false,
            substitutes: Vec::new(),
        }
    }

    pub fn get_effect(&self, eff: EffectId) -> Effect {
        self.effects[eff].1.clone()
    }

    pub fn get_effect_span(&self, eff: EffectId) -> Span {
        self.effects[eff].0
    }

    pub fn insert_effect(&mut self, span: Span, eff: Effect) -> EffectId {
        self.effects.add((span, eff))
    }
}

#[derive(Clone)]
pub struct TyDisplay<'a> {
    ctx: &'a Context,
    ty: Result<TyId, EffectId>,
    lhs_exposed: bool,
    substitutes: Vec<(TyId, Rc<dyn Fn(&mut fmt::Formatter) -> fmt::Result + 'a>)>,
}

impl<'a> TyDisplay<'a> {
    fn with_ty(&self, ty: TyId, lhs_exposed: bool) -> Self {
        Self { ty: Ok(ty), lhs_exposed, ..self.clone() }
    }

    fn with_eff(&self, eff: EffectId, lhs_exposed: bool) -> Self {
        Self { ty: Err(eff), lhs_exposed, ..self.clone() }
    }

    pub fn substitute(mut self, ty: TyId, sub: impl Fn(&mut fmt::Formatter) -> fmt::Result + 'a) -> Self {
        self.substitutes.push((ty, Rc::new(sub)));
        self
    }
}

impl<'a> fmt::Display for TyDisplay<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some((_, sub)) = self.substitutes
            .iter()
            .find(|(ty, _)| Ok(*ty) == self.ty)
        {
            return sub(f);
        }

        match self.ty {
            Ok(ty) => match self.ctx.tys.get(ty) {
                Ty::Error(ErrorReason::Unknown) => write!(f, "?"),
                Ty::Error(ErrorReason::Recursive) => write!(f, "..."),
                Ty::Error(ErrorReason::Invalid) => write!(f, "!"),
                Ty::Prim(prim) => write!(f, "{}", prim),
                Ty::List(item) => write!(f, "[{}]", self.with_ty(item, false)),
                Ty::Record(fields, is_tuple) => if is_tuple {
                    write!(f, "({})", fields
                        .values()
                        .map(|field| format!("{}", self.with_ty(*field, false)))
                        .collect::<Vec<_>>()
                        .join(", "))
                } else {
                    write!(f, "{{ {} }}", fields
                        .into_iter()
                        .map(|(name, field)| format!("{}: {}", name, self.with_ty(field, false)))
                        .collect::<Vec<_>>()
                        .join(", "))
                },
                Ty::Func(i, o) if self.lhs_exposed => write!(f, "({} -> {})", self.with_ty(i, true), self.with_ty(o, self.lhs_exposed)),
                Ty::Func(i, o) => write!(f, "{} -> {}", self.with_ty(i, true), self.with_ty(o, self.lhs_exposed)),
                Ty::Data(name, params) if self.lhs_exposed && params.len() > 0 => write!(f, "({}{})", *self.ctx.datas.get_data(name).name, params
                    .iter()
                    .map(|param| format!(" {}", self.with_ty(*param, true)))
                    .collect::<String>()),
                Ty::Data(name, params) => write!(f, "{}{}", *self.ctx.datas.get_data(name).name, params
                    .iter()
                    .map(|param| format!(" {}", self.with_ty(*param, true)))
                    .collect::<String>()),
                Ty::Gen(index, scope) => write!(f, "{}", **self.ctx.tys.get_gen_scope(scope).get(index).name),
                // TODO: Include class_id?
                Ty::Assoc(inner, (class_id, gen_tys, gen_effs), assoc) => {
                    let class = format!("{}{}", *self.ctx.classes.get(class_id).name, gen_tys
                        .iter()
                        .map(|ty| format!(" {}", self.with_ty(*ty, true)))
                        .chain(gen_effs
                            .iter()
                            .map(|eff| match *eff {
                                Some(eff) => format!(" {}", self.with_eff(eff, true)),
                                None => format!(" !"),
                            }))
                        .collect::<String>());
                    write!(f, "<{} as {}>.{}", self.with_ty(inner, true), class, *assoc)
                },
                Ty::SelfType => write!(f, "Self"),
                Ty::Effect(eff, out) => {
                    if self.lhs_exposed {
                        write!(f, "(")?;
                    }
                    write!(f, "{}", self.with_eff(eff, true))?;
                    write!(f, " ~ {}", self.with_ty(out, true))?;
                    if self.lhs_exposed {
                        write!(f, ")")?;
                    }
                    Ok(())
                },
            },
            Err(eff) => match self.ctx.tys.get_effect(eff) {
                Effect::Error => write!(f, "!"),
                Effect::Known(effs) => {
                    let effs = effs
                        .iter()
                        .map(|eff| match eff {
                            Ok(EffectInst::Gen(idx, scope)) => format!("{}", **self.ctx.tys.get_gen_scope(*scope).get_eff(*idx).name),
                            Ok(EffectInst::Concrete(decl, args)) => format!("{}{}", *self.ctx.effects.get_decl(*decl).name, args
                                .iter()
                                .map(|arg| format!(" {}", self.with_ty(*arg, true)))
                                .collect::<String>()),
                            Err(()) => format!("!"),
                        })
                        .collect::<Vec<_>>();
                    if effs.is_empty() {
                        write!(f, "pure")
                    } else {
                        write!(f, "{}", effs.join(" + "))
                    }
                },
            },
        }
    }
}

#[derive(Clone, Debug)]
pub enum ImpliedItems<M: Meta> {
    // Items are derived from a real member
    Real(MemberId),
    // Items are implied through equality constraints
    Eq(Vec<(SrcNode<Ident>, M::Ty)>),
}

pub type InferImpliedItems = ImpliedItems<InferMeta>;

#[derive(Clone)]
pub struct ImpliedMember<M: Meta> {
    pub member: SrcNode<M::Ty>,
    pub class: SrcNode<ClassId>,
    pub gen_tys: Vec<M::Ty>,
    pub gen_effs: Vec<M::Effect>,
    pub items: ImpliedItems<M>,
}

pub type TyImpliedMember = ImpliedMember<TyMeta>;
pub type InferImpliedMember = ImpliedMember<InferMeta>;

pub struct GenTy {
    pub name: SrcNode<Ident>,
}

pub struct GenEff {
    pub name: SrcNode<Ident>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct GenScopeId(usize);

pub struct GenScope {
    pub item_span: Span,
    types: Vec<GenTy>,
    effects: Vec<GenEff>,
    // TODO: Don't store this here, it's silly
    pub ast_implied_members: Vec<SrcNode<ast::ImpliedMember>>,
    pub implied_members: Option<Vec<SrcNode<TyImpliedMember>>>,
}

impl GenScope {
    pub fn from_ast(
        generics: &ast::Generics,
        item_span: Span,
        mut mentions_ty: impl FnMut(Ident) -> bool,
        mut mentions_eff: impl FnMut(Ident) -> bool,
    ) -> (Self, Vec<Error>) {
        let mut existing = HashMap::new();

        let mut errors = Vec::new();
        for gen in &generics.tys {
            if let Some(old_span) = existing.insert(*gen.name, gen.name.span()) {
                errors.push(Error::DuplicateGenName(*gen.name, old_span, gen.name.span()));
            }
        }

        (Self {
            item_span,
            types: generics.tys
                .iter()
                .map(|gen_ty| {
                    if !mentions_ty(*gen_ty.name) {
                        errors.push(Error::NotMentioned(gen_ty.name.clone()));
                    }
                    GenTy { name: gen_ty.name.clone() }
                })
                .collect(),
            effects: generics.effs
                .iter()
                .map(|gen_eff| {
                    if !mentions_eff(*gen_eff.name) {
                        errors.push(Error::NotMentioned(gen_eff.name.clone()));
                    }
                    GenEff { name: gen_eff.name.clone() }
                })
                .collect(),
            ast_implied_members: generics.implied_members.clone(),
            implied_members: None,
        }, errors)
    }

    pub fn len(&self) -> usize { self.types.len() }
    pub fn len_eff(&self) -> usize { self.effects.len() }

    pub fn get(&self, index: usize) -> &GenTy {
        &self.types[index]
    }

    pub fn get_eff(&self, index: usize) -> &GenEff {
        &self.effects[index]
    }

    pub fn find(&self, name: Ident) -> Option<(usize, &GenTy)> {
        self.types.iter().enumerate().find(|(_, ty)| &*ty.name == &name)
    }

    pub fn find_eff(&self, name: Ident) -> Option<(usize, &GenEff)> {
        self.effects.iter().enumerate().find(|(_, eff)| &*eff.name == &name)
    }
}
