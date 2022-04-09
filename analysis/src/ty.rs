use super::*;
use std::rc::Rc;

pub type TyMeta = (Span, TyId);
pub type TyNode<T> = Node<T, TyMeta>;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Prim {
    Nat,
    Int,
    Real,
    Bool,
    Char,
    Universe,
}

impl fmt::Display for Prim {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Prim::Nat => write!(f, "Nat"),
            Prim::Int => write!(f, "Int"),
            Prim::Real => write!(f, "Real"),
            Prim::Bool => write!(f, "Bool"),
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
    Tuple(Vec<TyId>),
    Record(BTreeMap<Ident, TyId>),
    Func(TyId, TyId),
    Data(DataId, Vec<TyId>),
    Gen(usize, GenScopeId),
    SelfType,
    Assoc(TyId, (ClassId, Vec<TyId>), SrcNode<Ident>),
    Effect(EffectId, TyId),
}

pub type TyId = Id<(Span, Ty)>;

#[derive(Clone, Debug)]
pub enum Effect {
    Error,
    Known(EffectDeclId, Vec<TyId>),
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

    // Ignores gen_scope
    pub fn is_eq(&self, x: TyId, y: TyId) -> bool {
        match (self.get(x), self.get(y)) {
            (Ty::Error(_), _) | (_, Ty::Error(_)) => true,
            (Ty::Prim(x), Ty::Prim(y)) => x == y,
            (Ty::List(x), Ty::List(y)) => self.is_eq(x, y),
            (Ty::Tuple(xs), Ty::Tuple(ys)) if xs.len() == ys.len() => xs
                .into_iter()
                .zip(ys)
                .all(|(x, y)| self.is_eq(x, y)),
            (Ty::Record(_), Ty::Record(_)) => todo!("Record equality"),
            (Ty::Func(x_i, x_o), Ty::Func(y_i, y_o)) => self.is_eq(x_i, y_i) && self.is_eq(x_o, y_o),
            (Ty::Data(x, xs), Ty::Data(y, ys)) => x == y && xs.len() == ys.len() && xs
                .into_iter()
                .zip(ys)
                .all(|(x, y)| self.is_eq(x, y)),
            (Ty::Gen(x, x_scope), Ty::Gen(y, y_scope)) => x == y && x_scope == y_scope,
            (Ty::SelfType, Ty::SelfType) => true,
            (Ty::Assoc(x_ty, (x_class_id, x_args), x_name), Ty::Assoc(y_ty, (y_class_id, y_args), y_name)) => self.is_eq(x_ty, y_ty)
                && x_class_id == y_class_id
                && *x_name == *y_name
                && x_args
                    .into_iter()
                    .zip(y_args)
                    .all(|(x, y)| self.is_eq(x, y)),
            (Ty::Effect(x, x_out), Ty::Effect(y, y_out)) =>
                x == y &&
                match (self.get_effect(x), self.get_effect(y)) {
                    (Effect::Error, _) => true,
                    (_, Effect::Error) => true,
                    (Effect::Known(x, xs), Effect::Known(y, ys)) => x == y && xs.len() == ys.len() && xs
                        .into_iter()
                        .zip(ys)
                        .all(|(x, y)| self.is_eq(x, y)),
                },
            _ => false,
        }
    }

    pub fn has_inhabitants(&self, datas: &Datas, ty: TyId, gen: &mut dyn FnMut(usize) -> bool) -> bool {
        match self.get(ty) {
            Ty::Error(_) => false,
            Ty::Prim(_) => true,
            Ty::List(_) => true, // Empty list
            Ty::Tuple(fields) => fields
                .into_iter()
                .all(|field| self.has_inhabitants(datas, field, gen)),
            Ty::Record(fields) => fields
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
            ty,
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
    ty: TyId,
    lhs_exposed: bool,
    substitutes: Vec<(TyId, Rc<dyn Fn(&mut fmt::Formatter) -> fmt::Result + 'a>)>,
}

impl<'a> TyDisplay<'a> {
    fn with_ty(&self, ty: TyId, lhs_exposed: bool) -> Self {
        Self { ty, lhs_exposed, ..self.clone() }
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
            .find(|(ty, _)| *ty == self.ty)
        {
            return sub(f);
        }

        match self.ctx.tys.get(self.ty) {
            Ty::Error(ErrorReason::Unknown) => write!(f, "?"),
            Ty::Error(ErrorReason::Recursive) => write!(f, "..."),
            Ty::Error(ErrorReason::Invalid) => write!(f, "!"),
            Ty::Prim(prim) => write!(f, "{}", prim),
            Ty::List(item) => write!(f, "[{}]", self.with_ty(item, false)),
            Ty::Tuple(fields) => write!(f, "({}{})", fields
                .iter()
                .map(|field| format!("{}", self.with_ty(*field, false)))
                .collect::<Vec<_>>()
                .join(", "), if fields.len() == 1 { "," } else { "" }),
            Ty::Record(fields) => write!(f, "{{ {} }}", fields
                .into_iter()
                .map(|(name, field)| format!("{}: {}", name, self.with_ty(field, false)))
                .collect::<Vec<_>>()
                .join(", ")),
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
            Ty::Assoc(inner, (class_id, args), assoc) => {
                let class = format!("{}{}", *self.ctx.classes.get(class_id).name, args
                    .iter()
                    .map(|arg| format!(" {}", self.with_ty(*arg, true)))
                    .collect::<String>());
                write!(f, "<{} as {}>.{}", self.with_ty(inner, true), class, *assoc)
            },
            Ty::SelfType => write!(f, "Self"),
            Ty::Effect(eff, out) => {
                let eff = match self.ctx.tys.get_effect(eff) {
                    Effect::Error => "!".to_string(),
                    Effect::Known(decl, args) => format!("{}{}", *self.ctx.effects.get_decl(decl).name, args
                        .iter()
                        .map(|arg| format!(" {}", self.with_ty(*arg, true)))
                        .collect::<String>()),
                };
                if self.lhs_exposed {
                    write!(f, "({} ~ {})", eff, self.with_ty(out, true))
                } else {
                    write!(f, "{} ~ {}", eff, self.with_ty(out, true))
                }
            },
        }
    }
}

#[derive(Clone)]
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
    pub args: Vec<M::Ty>,
    pub items: ImpliedItems<M>,
}

pub type TyImpliedMember = ImpliedMember<TyMeta>;
pub type InferImpliedMember = ImpliedMember<InferMeta>;

pub struct GenTy {
    pub name: SrcNode<Ident>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct GenScopeId(usize);

pub struct GenScope {
    pub item_span: Span,
    types: Vec<GenTy>,
    // TODO: Don't store this here, it's silly
    pub ast_implied_members: Vec<SrcNode<ast::ImpliedMember>>,
    pub implied_members: Option<Vec<SrcNode<TyImpliedMember>>>,
}

impl GenScope {
    pub fn from_ast(generics: &ast::Generics, item_span: Span) -> (Self, Vec<Error>) {
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
                .map(|gen_ty| GenTy {
                    name: gen_ty.name.clone(),
                })
                .collect(),
            ast_implied_members: generics.implied_members.clone(),
            implied_members: None,
        }, errors)
    }

    pub fn len(&self) -> usize { self.types.len() }

    pub fn get(&self, index: usize) -> &GenTy {
        &self.types[index]
    }

    pub fn find(&self, name: Ident) -> Option<(usize, &GenTy)> {
        self.types.iter().enumerate().find(|(_, ty)| &*ty.name == &name)
    }
}
