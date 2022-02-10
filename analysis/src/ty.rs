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
}

impl fmt::Display for Prim {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Prim::Nat => write!(f, "Nat"),
            Prim::Int => write!(f, "Int"),
            Prim::Real => write!(f, "Real"),
            Prim::Bool => write!(f, "Bool"),
            Prim::Char => write!(f, "Char"),
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
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct TyId(usize);

#[derive(Default)]
pub struct Types {
    tys: Vec<(Span, Ty)>,
    scopes: Vec<GenScope>,
}

impl Types {
    pub fn get_gen_scope(&self, scope: GenScopeId) -> &GenScope {
        &self.scopes[scope.0]
    }

    pub fn insert_gen_scope(&mut self, gen_scope: GenScope) -> GenScopeId {
        let id = GenScopeId(self.scopes.len());
        self.scopes.push(gen_scope);
        id
    }

    pub fn check_gen_scopes(&mut self, classes: &Classes) -> Vec<Error> {
        let mut errors = Vec::new();
        for scope in &mut self.scopes {
            scope.check(classes, &mut errors);
        }
        assert!(self.scopes
            .iter()
            .all(|s| s.types
                .iter()
                .all(|t| t.obligations.is_some())), "All generic scope obligations must be checked");
        errors
    }

    pub fn get(&self, ty: TyId) -> Ty {
        self.tys[ty.0].1.clone()
    }

    pub fn get_span(&self, ty: TyId) -> Span {
        self.tys[ty.0].0
    }

    pub fn insert(&mut self, span: Span, ty: Ty) -> TyId {
        let id = TyId(self.tys.len());
        self.tys.push((span, ty));
        id
    }

    pub fn display<'a>(&'a self, datas: &'a Datas, ty: TyId) -> TyDisplay<'a> {
        TyDisplay {
            types: self,
            datas,
            ty,
            lhs_exposed: false,
            substitutes: Vec::new(),
        }
    }
}

#[derive(Clone)]
pub struct TyDisplay<'a> {
    types: &'a Types,
    datas: &'a Datas,
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

        match self.types.get(self.ty) {
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
            Ty::Data(name, params) if self.lhs_exposed && params.len() > 0 => write!(f, "({}{})", self.datas.get_data(name).name, params
                .iter()
                .map(|param| format!(" {}", self.with_ty(*param, true)))
                .collect::<String>()),
            Ty::Data(name, params) => write!(f, "{}{}", self.datas.get_data(name).name, params
                .iter()
                .map(|param| format!(" {}", self.with_ty(*param, true)))
                .collect::<String>()),
            Ty::Gen(index, scope) => write!(f, "{}", **self.types.get_gen_scope(scope).get(index).name),
            Ty::SelfType => write!(f, "Self"),
        }
    }
}

#[derive(Clone)]
pub enum Obligation {
    MemberOf(ClassId),
}

pub struct GenTy {
    pub name: SrcNode<Ident>,
    // TODO: Don't store this here, it's silly
    pub ast_obligations: Vec<SrcNode<Ident>>,
    pub obligations: Option<Vec<Obligation>>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct GenScopeId(usize);

pub struct GenScope {
    pub span: Span,
    types: Vec<GenTy>,
}

impl GenScope {
    pub fn from_ast(generics: &SrcNode<ast::Generics>) -> (Self, Vec<Error>) {
        let mut existing = HashMap::new();

        let mut errors = Vec::new();
        for gen in &generics.tys {
            if let Some(old_span) = existing.insert(*gen.name, gen.name.span()) {
                errors.push(Error::DuplicateGenName(*gen.name, old_span, gen.name.span()));
            }
        }

        (Self {
            span: generics.span(),
            types: generics.tys
                .iter()
                .map(|gen_ty| GenTy {
                    name: gen_ty.name.clone(),
                    ast_obligations: gen_ty.obligations.clone(),
                    obligations: None,
                })
                .collect(),
        }, errors)
    }

    pub fn len(&self) -> usize { self.types.len() }

    pub fn get(&self, index: usize) -> &GenTy {
        &self.types[index]
    }

    pub fn find(&self, name: Ident) -> Option<(usize, &GenTy)> {
        self.types.iter().enumerate().find(|(_, ty)| &*ty.name == &name)
    }

    fn check(&mut self, classes: &Classes, errors: &mut Vec<Error>) {
        for ty in &mut self.types {
            let obligations = ty
                .ast_obligations
                .iter()
                .filter_map(|class| if let Some(class) = classes.lookup(**class) {
                    Some(Obligation::MemberOf(class))
                } else {
                    errors.push(Error::NoSuchClass(class.clone()));
                    None
                })
                .collect();
            ty.obligations = Some(obligations);
        }
    }

    pub fn find_obligations_for(&self, ctx: &Context, idx: usize) -> HashSet<ClassId> {
        let mut classes = HashSet::new();

        // Extract a list of all classes that the type is a member of
        fn walk_classes(ctx: &Context, classes: &mut HashSet<ClassId>, class: ClassId) {
            if classes.insert(class) {
                for obl in ctx.classes.get(class).obligations.as_ref().expect("Obligations must be known here") {
                    match obl.inner() {
                        Obligation::MemberOf(class) => walk_classes(ctx, classes, *class),
                    }
                }
            }
        }

        // Determine primary obligations
        self.get(idx)
            .obligations
            .as_ref()
            .expect("Lookup on unchecked gen scope")
            .iter()
            .for_each(|c| match c {
                Obligation::MemberOf(class) => walk_classes(ctx, &mut classes, *class),
            });

        classes
    }
}
