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
    Bool(bool),
    Char(char),
    Str(Intern<String>),
}

impl fmt::Debug for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Nat(x) => write!(f, "`{}`", x),
            Self::Int(x) => write!(f, "`{}`", x),
            Self::Real(x) => write!(f, "`{}`", x),
            Self::Bool(x) => write!(f, "`{}`", x),
            Self::Char(c) => write!(f, "`{}`", c),
            Self::Str(s) => write!(f, "`\"{}\"`", s),
        }
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
    Data(SrcNode<Ident>, Vec<SrcNode<Self>>),
    Assoc(SrcNode<Self>, Option<SrcNode<ClassInst>>, SrcNode<Ident>),
    Effect(SrcNode<Ident>, Vec<SrcNode<Self>>, SrcNode<Self>),
}

impl Type {
    pub fn is_fully_specified(&self) -> bool {
        match self {
            Self::Error | Self::Universe => true,
            Self::Unknown => false,
            Self::List(item) => item.is_fully_specified(),
            Self::Tuple(fields) => fields
                .iter()
                .all(|field| field.is_fully_specified()),
            Self::Record(fields) => fields
                .iter()
                .all(|(_, field)| field.is_fully_specified()),
            Self::Func(i, o) => i.is_fully_specified() && o.is_fully_specified(),
            Self::Data(_, args) => args
                .iter()
                .all(|arg| arg.is_fully_specified()),
            Self::Assoc(inner, _, _) => inner.is_fully_specified(),
            Self::Effect(_, args, out) => args
                .iter()
                .all(|arg| arg.is_fully_specified()) && out.is_fully_specified(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ClassInst {
    pub name: SrcNode<Ident>,
    pub params: Vec<SrcNode<Type>>,
}

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub struct Binding {
    pub pat: SrcNode<Pat>,
    pub name: Option<SrcNode<Ident>>,
    pub ty: Option<SrcNode<Type>>,
}

#[derive(Debug, PartialEq)]
pub enum LangDef {
    IoUnit,
    IoBind,
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    // Generated only by parser errors.
    Error,
    Literal(Literal),
    // TODO: replace with `Item` when scoping is added
    Local(Ident),
    LangDef(LangDef),
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
    Block(Vec<SrcNode<Self>>, SrcNode<Self>),
    Handle {
        expr: SrcNode<Self>,
        eff_name: SrcNode<Ident>,
        eff_args: Vec<SrcNode<Type>>,
        send: SrcNode<Binding>,
        recv: SrcNode<Self>
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct ImpliedMember {
    pub member: SrcNode<Type>,
    pub class: SrcNode<ClassInst>,
}

#[derive(Debug, PartialEq)]
pub struct GenericTy {
    pub name: SrcNode<Ident>,
}

#[derive(Debug, PartialEq, Default)]
pub struct Generics {
    pub tys: Vec<GenericTy>,
    pub implied_members: Vec<SrcNode<ImpliedMember>>,
}

impl Generics {
    pub fn from_tys_and_implied(
        tys: Vec<(ast::GenericTy, Vec<SrcNode<ast::ImpliedMember>>)>,
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
            implied_members,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Data {
    pub name: SrcNode<Ident>,
    pub generics: Generics,
    pub variants: Vec<(SrcNode<Ident>, SrcNode<Type>)>,
}

#[derive(Debug, PartialEq)]
pub struct Alias {
    pub name: SrcNode<Ident>,
    pub generics: Generics,
    pub ty: SrcNode<Type>,
}

#[derive(Debug, PartialEq)]
pub struct Def {
    pub name: SrcNode<Ident>,
    pub generics: Generics,
    pub ty_hint: SrcNode<Type>,
    pub body: SrcNode<Expr>,
}

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub struct Class {
    pub name: SrcNode<Ident>,
    pub generics: Generics,
    pub items: Vec<ClassItem>,
}

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub struct Member {
    pub generics: Generics,
    pub member: SrcNode<Type>,
    pub class: SrcNode<ClassInst>,
    pub items: Vec<MemberItem>,
}

#[derive(Debug, PartialEq)]
pub struct Effect {
    pub name: SrcNode<Ident>,
    pub generics: Generics,
    pub send: SrcNode<Type>,
    pub recv: SrcNode<Type>,
}

#[derive(Debug, PartialEq)]
pub enum ItemKind {
    Data(Data),
    Alias(Alias),
    Def(Def),
    Class(Class),
    Member(Member),
    Effect(Effect),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Attr {
    pub name: SrcNode<Ident>,
    pub args: Option<Vec<SrcNode<Self>>>,
}

#[derive(Debug, PartialEq)]
pub struct Item {
    pub kind: ItemKind,
    pub attrs: Vec<SrcNode<Attr>>,
}

#[derive(Debug, PartialEq)]
pub struct Module {
    pub imports: Vec<SrcNode<Intern<String>>>,
    pub items: Vec<Item>,
}

impl Module {
    pub fn classes(&self) -> impl Iterator<Item = (&[SrcNode<Attr>], &Class)> + '_ {
        self.items
            .iter()
            .filter_map(|item| match &item.kind {
                ItemKind::Class(class) => Some((item.attrs.as_slice(), class)),
                _ => None,
            })
    }

    pub fn datas(&self) -> impl Iterator<Item = (&[SrcNode<Attr>], &Data)> + '_ {
        self.items
            .iter()
            .filter_map(|item| match &item.kind {
                ItemKind::Data(data) => Some((item.attrs.as_slice(), data)),
                _ => None,
            })
    }

    pub fn aliases(&self) -> impl Iterator<Item = (&[SrcNode<Attr>], &Alias)> + '_ {
        self.items
            .iter()
            .filter_map(|item| match &item.kind {
                ItemKind::Alias(alias) => Some((item.attrs.as_slice(), alias)),
                _ => None,
            })
    }

    pub fn members(&self) -> impl Iterator<Item = (&[SrcNode<Attr>], &Member)> + '_ {
        self.items
            .iter()
            .filter_map(|item| match &item.kind {
                ItemKind::Member(member) => Some((item.attrs.as_slice(), member)),
                _ => None,
            })
    }

    pub fn defs(&self) -> impl Iterator<Item = (&[SrcNode<Attr>], &Def)> + '_ {
        self.items
            .iter()
            .filter_map(|item| match &item.kind {
                ItemKind::Def(def) => Some((item.attrs.as_slice(), def)),
                _ => None,
            })
    }

    pub fn effects(&self) -> impl Iterator<Item = (&[SrcNode<Attr>], &Effect)> + '_ {
        self.items
            .iter()
            .filter_map(|item| match &item.kind {
                ItemKind::Effect(eff) => Some((item.attrs.as_slice(), eff)),
                _ => None,
            })
    }
}
