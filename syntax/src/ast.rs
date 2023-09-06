use super::*;
use internment::Intern;
use std::ops::Deref;

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Ident(Intern<String>);

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}", self.0) }
}

impl fmt::Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "`{}`", self.0) }
}

impl Ident {
    pub fn new<S: ToString>(s: S) -> Self { Self(Intern::new(s.to_string())) }
    pub fn as_ref(self) -> &'static String { self.0.as_ref() }
}

impl Deref for Ident {
    type Target = String;

    fn deref(&self) -> &Self::Target { &self.0 }
}

// #[derive(Copy, Clone, Debug, PartialEq, Eq)]
// pub enum PathBase {
//     Root,
//     Parent,
//     This,
// }

// pub struct Item {
//     pub name: SrcNode<Ident>,
//     pub base: SrcNode<PathBase>,
//     pub path: Vec<SrcNode<Ident>>,
// }

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
    Not,
    Propagate,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Neg => write!(f, "-"),
            Self::Not => write!(f, "!"),
            Self::Propagate => write!(f, "?"),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    // Sum
    Add, Sub,
    // Product
    Mul, Div, Rem,
    // Equality
    Eq, NotEq,
    // Comparison
    Less, LessEq,
    More, MoreEq,
    // Logical
    And, Or, Xor,
    // Lists
    Join,
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::Rem => write!(f, "%"),
            Self::Eq => write!(f, "="),
            Self::NotEq => write!(f, "!="),
            Self::Less => write!(f, "<"),
            Self::LessEq => write!(f, "<="),
            Self::More => write!(f, ">"),
            Self::MoreEq => write!(f, ">="),
            Self::And => write!(f, "and"),
            Self::Or => write!(f, "or"),
            Self::Xor => write!(f, "xor"),
            Self::Join => write!(f, "++"),
        }
    }
}

#[derive(Copy, Clone, PartialEq)]
pub enum Literal {
    Nat(u64),
    Int(i64),
    Real(f64),
    Char(char),
    Str(Intern<String>),
}

impl fmt::Debug for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Nat(x) => write!(f, "`{}`", x),
            Self::Int(x) => write!(f, "`{}`", x),
            Self::Real(x) => write!(f, "`{}`", x),
            Self::Char(c) => write!(f, "`{}`", c),
            Self::Str(s) => write!(f, "`\"{}\"`", s),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct EffectSet {
    pub effs: Vec<(SrcNode<Ident>, Vec<SrcNode<Type>>, Vec<SrcNode<EffectSet>>)>,
}

impl EffectSet {
    pub fn mentions_ty(&self, name: Ident) -> bool {
        self.effs
            .iter()
            .any(|(_, gen_tys, gen_effs)| gen_tys
                .iter()
                .any(|p| p.mentions_ty(name)) || gen_effs
                    .iter()
                    .any(|e| e.mentions_ty(name)))
    }

    pub fn mentions_eff(&self, name: Ident) -> bool {
        self.effs
            .iter()
            .any(|(eff, gen_tys, gen_effs)| **eff == name || gen_tys
                .iter()
                .any(|p| p.mentions_eff(name)) || gen_effs
                .iter()
                .any(|e| e.mentions_eff(name)))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    // Generated only by parser errors.
    Error,
    Universe,
    Unknown,
    List(SrcNode<Self>),
    Tuple(Vec<SrcNode<Self>>),
    Record(Vec<(SrcNode<Ident>, SrcNode<Self>)>),
    Func(SrcNode<Self>, SrcNode<Self>),
    // TODO: Replace name with `Item` when ready
    Data(SrcNode<Ident>, Vec<SrcNode<Self>>, Vec<SrcNode<EffectSet>>),
    Assoc(SrcNode<Self>, Option<SrcNode<ClassInst>>, SrcNode<Ident>),
    Effect(SrcNode<EffectSet>, SrcNode<Self>),
}

impl Type {
    pub fn for_children(&self, mut f: &mut impl FnMut(&Self)) {
        fn for_eff(eff: &EffectSet, mut f: &mut impl FnMut(&Type)) {
            for (_, gen_tys, gen_effs) in &eff.effs {
                for ty in gen_tys {
                    ty.for_children(&mut*f);
                }
                for eff in gen_effs {
                    for_eff(eff, &mut*f);
                }
            }
        }

        match self {
            Self::Error | Self::Universe | Self::Unknown => {},
            Self::List(x) => f(x),
            Self::Tuple(fields) => fields
                .iter()
                .for_each(|field| f(field)),
            Self::Record(fields) => fields
                .iter()
                .for_each(|(_, field)| f(field)),
            Self::Func(i, o) => {
                f(i);
                f(o);
            },
            Self::Data(data, gen_tys, gen_effs) => {
                gen_tys
                    .iter()
                    .for_each(|ty| f(ty));
                gen_effs
                    .iter()
                    .for_each(|effs| effs.effs
                        .iter()
                        .for_each(|(_, gen_tys, gen_effs)| {
                            gen_tys
                                .iter()
                                .for_each(|ty| f(ty));
                            gen_effs
                                .iter()
                                .for_each(|eff| for_eff(eff, f));
                        }));
            },
            // TODO: Recurse into class inst?
            Self::Assoc(inner, _, _) => f(inner),
            Self::Effect(set, out) => {
                f(out);
                set.effs
                    .iter()
                    .for_each(|(_, gen_tys, gen_effs)| {
                        gen_tys
                            .iter()
                            .for_each(|ty| f(ty));
                        gen_effs
                            .iter()
                            .for_each(|e| for_eff(e, f));
                    });
            },
        }
    }

    pub fn is_fully_specified(&self) -> bool {
        let mut specified = match self {
            Self::Unknown => false,
            _ => true,
        };
        self.for_children(&mut |ty| specified &= ty.is_fully_specified());
        specified
    }

    pub fn mentions_ty(&self, name: Ident) -> bool {
        let mut mentions = match self {
            Self::Data(data, _, _) => **data == name,
            _ => false,
        };
        self.for_children(&mut |ty| mentions |= ty.mentions_ty(name));
        mentions
    }

    pub fn mentions_eff(&self, name: Ident) -> bool {
        let mut mentions = match self {
            Self::Effect(set, _) => set.effs
                .iter()
                .any(|(eff, _, _)| **eff == name),
            Self::Data(_, _, gen_effs) => gen_effs
                .iter()
                .any(|set| set.effs
                    .iter()
                    .any(|(eff, _, _)| **eff == name),),
            _ => false,
        };
        self.for_children(&mut |ty| mentions |= ty.mentions_eff(name));
        mentions
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ClassInst {
    pub name: SrcNode<Ident>,
    pub gen_tys: Vec<SrcNode<Type>>,
    pub gen_effs: Vec<SrcNode<EffectSet>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Pat {
    // Generated only by parser errors.
    Error,
    Wildcard,
    Literal(Literal),
    Single(SrcNode<Binding>),
    Binary(SrcNode<BinaryOp>, SrcNode<Binding>, SrcNode<Literal>), // x + N, only for nats
    Tuple(Vec<SrcNode<Binding>>),
    Record(Vec<(SrcNode<Ident>, SrcNode<Binding>)>),
    ListExact(Vec<SrcNode<Binding>>),
    ListFront(Vec<SrcNode<Binding>>, Option<SrcNode<Binding>>),
    // TODO: Replace name with `Item` when ready
    Deconstruct(SrcNode<Ident>, SrcNode<Binding>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Binding {
    pub pat: SrcNode<Pat>,
    pub name: Option<SrcNode<Ident>>,
    pub ty: Option<SrcNode<Type>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    // Generated only by parser errors.
    Error,
    Literal(Literal),
    // TODO: replace with `Item` when scoping is added
    Local(Ident),
    Tuple(Vec<SrcNode<Self>>),
    List(Vec<SrcNode<Self>>, Vec<SrcNode<Self>>),
    Record(Vec<(SrcNode<Ident>, SrcNode<Self>)>),
    Access(SrcNode<Self>, SrcNode<Ident>),
    Unary(SrcNode<UnaryOp>, SrcNode<Self>),
    Binary(SrcNode<BinaryOp>, SrcNode<Self>, SrcNode<Self>),
    Let(Vec<(SrcNode<Binding>, SrcNode<Self>)>, SrcNode<Self>),
    Match(SrcNode<Vec<SrcNode<Self>>>, Vec<(SrcNode<Vec<SrcNode<Binding>>>, SrcNode<Self>)>),
    If(SrcNode<Self>, SrcNode<Self>, SrcNode<Self>),
    Func(SrcNode<Vec<(SrcNode<Vec<SrcNode<Binding>>>, SrcNode<Self>)>>),
    Apply(SrcNode<Self>, SrcNode<Self>),
    Cons(SrcNode<Ident>, SrcNode<Self>),
    ClassAccess(SrcNode<Type>, SrcNode<Ident>),
    Intrinsic(SrcNode<Ident>, Vec<SrcNode<Self>>),
    Update(SrcNode<Self>, Vec<(SrcNode<Ident>, SrcNode<Self>)>),
    Basin(Vec<(Option<SrcNode<Binding>>, SrcNode<Self>)>, SrcNode<Self>),
    Block(Vec<(Option<SrcNode<Binding>>, SrcNode<Self>)>, SrcNode<Self>),
    Handle {
        expr: SrcNode<Self>,
        handlers: Vec<Handler>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct Handler {
    pub eff_name: SrcNode<Ident>,
    pub eff_gen_tys: Vec<SrcNode<Type>>,
    pub eff_gen_effs: Vec<SrcNode<EffectSet>>,
    pub send: SrcNode<Binding>,
    pub state: Option<SrcNode<Binding>>,
    pub recv: SrcNode<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ImpliedMember {
    pub member: SrcNode<Type>,
    pub class: SrcNode<ClassInst>,
    pub assoc: Vec<(SrcNode<Ident>, SrcNode<Type>)>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct GenericTy {
    pub name: SrcNode<Ident>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct GenericEffect {
    pub name: SrcNode<Ident>,
}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct Generics {
    pub tys: Vec<GenericTy>,
    pub effs: Vec<GenericEffect>,
    pub implied_members: Vec<SrcNode<ImpliedMember>>,
}

impl Generics {
    pub fn from_tys_and_implied(
        tys: Vec<(ast::GenericTy, Vec<SrcNode<ast::ImpliedMember>>)>,
        effs: Vec<ast::GenericEffect>,
        mut implied_members: Vec<SrcNode<ast::ImpliedMember>>,
    ) -> Self {
        Self {
            tys: tys
                .into_iter()
                .map(|(ty, mut implied)| {
                    implied_members.append(&mut implied);
                    ty
                })
                .collect(),
            effs,
            implied_members,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Data {
    pub name: SrcNode<Ident>,
    pub generics: Generics,
    pub variants: Vec<(SrcNode<Ident>, SrcNode<Type>)>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Alias {
    pub name: SrcNode<Ident>,
    pub generics: Generics,
    pub ty: SrcNode<Type>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Def {
    pub name: SrcNode<Ident>,
    pub generics: Generics,
    pub ty_hint: SrcNode<Type>,
    pub body: SrcNode<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ClassItem {
    Value {
        name: SrcNode<Ident>,
        ty: SrcNode<Type>,
    },
    Type {
        name: SrcNode<Ident>,
        obligations: SrcNode<Vec<SrcNode<ClassInst>>>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct Class {
    pub name: SrcNode<Ident>,
    pub generics: Generics,
    pub items: Vec<ClassItem>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum MemberItem {
    Value {
        name: SrcNode<Ident>,
        val: SrcNode<Expr>,
    },
    Type {
        name: SrcNode<Ident>,
        ty: SrcNode<Type>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct Member {
    pub generics: Generics,
    pub member: SrcNode<Type>,
    pub class: SrcNode<ClassInst>,
    pub items: Vec<MemberItem>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Effect {
    pub name: SrcNode<Ident>,
    pub generics: Generics,
    pub send: SrcNode<Type>,
    pub recv: SrcNode<Type>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct EffectAlias {
    pub name: SrcNode<Ident>,
    pub generics: Generics,
    pub effects: Vec<(SrcNode<Ident>, Vec<SrcNode<Type>>, Vec<SrcNode<EffectSet>>)>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ItemKind {
    Data(Data),
    Alias(Alias),
    Def(Def),
    Class(Class),
    Member(Member),
    Effect(Effect),
    EffectAlias(EffectAlias),
    ModuleDecl(ModuleDecl),
}

#[derive(Clone, Debug, PartialEq)]
pub struct ModuleDecl {
    pub name: SrcNode<Ident>,
    pub module: Module,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Attr {
    pub name: SrcNode<Ident>,
    pub args: Option<Vec<SrcNode<Self>>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Item {
    pub kind: ItemKind,
    pub attrs: Vec<SrcNode<Attr>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Module {
    pub modules: Vec<(SrcNode<Ident>, SrcNode<Intern<String>>)>,
    pub imports: Vec<SrcNode<Intern<String>>>,
    pub items: Vec<Item>,
}

impl Module {
    pub fn classes(&self) -> impl Iterator<Item = (&[SrcNode<Attr>], &Class)> + '_ {
        self.items
            .iter()
            .flat_map(|item| match &item.kind {
                ItemKind::Class(class) => vec![(item.attrs.as_slice(), class)],
                ItemKind::ModuleDecl(decl) => decl.module.classes().collect(),
                _ => Vec::new(),
            })
    }

    pub fn datas(&self) -> impl Iterator<Item = (&[SrcNode<Attr>], &Data)> + '_ {
        self.items
            .iter()
            .flat_map(|item| match &item.kind {
                ItemKind::Data(data) => vec![(item.attrs.as_slice(), data)],
                ItemKind::ModuleDecl(decl) => decl.module.datas().collect(),
                _ => Vec::new(),
            })
    }

    pub fn aliases(&self) -> impl Iterator<Item = (&[SrcNode<Attr>], &Alias)> + '_ {
        self.items
            .iter()
            .flat_map(|item| match &item.kind {
                ItemKind::Alias(alias) => vec![(item.attrs.as_slice(), alias)],
                ItemKind::ModuleDecl(decl) => decl.module.aliases().collect(),
                _ => Vec::new(),
            })
    }

    pub fn members(&self) -> impl Iterator<Item = (&[SrcNode<Attr>], &Member)> + '_ {
        self.items
            .iter()
            .flat_map(|item| match &item.kind {
                ItemKind::Member(member) => vec![(item.attrs.as_slice(), member)],
                ItemKind::ModuleDecl(decl) => decl.module.members().collect(),
                _ => Vec::new(),
            })
    }

    pub fn defs(&self) -> impl Iterator<Item = (&[SrcNode<Attr>], &Def)> + '_ {
        self.items
            .iter()
            .flat_map(|item| match &item.kind {
                ItemKind::Def(def) => vec![(item.attrs.as_slice(), def)],
                ItemKind::ModuleDecl(decl) => decl.module.defs().collect(),
                _ => Vec::new(),
            })
    }

    pub fn effects(&self) -> impl Iterator<Item = (&[SrcNode<Attr>], &Effect)> + '_ {
        self.items
            .iter()
            .flat_map(|item| match &item.kind {
                ItemKind::Effect(eff) => vec![(item.attrs.as_slice(), eff)],
                ItemKind::ModuleDecl(decl) => decl.module.effects().collect(),
                _ => Vec::new(),
            })
    }

    pub fn effect_aliases(&self) -> impl Iterator<Item = (&[SrcNode<Attr>], &EffectAlias)> + '_ {
        self.items
            .iter()
            .flat_map(|item| match &item.kind {
                ItemKind::EffectAlias(eff) => vec![(item.attrs.as_slice(), eff)],
                ItemKind::ModuleDecl(decl) => decl.module.effect_aliases().collect(),
                _ => Vec::new(),
            })
    }
}
