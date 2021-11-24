use super::*;
use internment::Intern;
use std::ops::Deref;

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
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
    // Sum
    Neg,
    // Logical
    Not,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Neg => write!(f, "-"),
            Self::Not => write!(f, "!"),
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
    Num(f64),
    Bool(bool),
    Char(char),
    Str(Intern<String>),
}

impl fmt::Debug for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Nat(x) => write!(f, "`{}`", x),
            Self::Num(x) => write!(f, "`{}`", x),
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
    Unknown,
    List(SrcNode<Self>),
    Tuple(Vec<SrcNode<Self>>),
    Record(Vec<(SrcNode<Ident>, SrcNode<Self>)>),
    Func(SrcNode<Self>, SrcNode<Self>),
    // TODO: Replace name with `Item` when ready
    Data(SrcNode<Ident>, Vec<SrcNode<Self>>),
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
pub enum Expr {
    // Generated only by parser errors.
    Error,
    Literal(Literal),
    // TODO: replace with `Item` when scoping is added
    Local(Ident),
    Tuple(Vec<SrcNode<Self>>),
    List(Vec<SrcNode<Self>>),
    ListFront(Vec<SrcNode<Self>>, SrcNode<Self>),
    Record(Vec<(SrcNode<Ident>, SrcNode<Self>)>),
    Access(SrcNode<Self>, SrcNode<Ident>),
    Unary(SrcNode<UnaryOp>, SrcNode<Self>),
    Binary(SrcNode<BinaryOp>, SrcNode<Self>, SrcNode<Self>),
    Let(Vec<(SrcNode<Binding>, SrcNode<Self>)>, SrcNode<Self>),
    Match(SrcNode<Vec<SrcNode<Self>>>, Vec<(SrcNode<Vec<SrcNode<Binding>>>, SrcNode<Self>)>),
    If(SrcNode<Self>, SrcNode<Self>, SrcNode<Self>),
    Func(Vec<(SrcNode<Vec<SrcNode<Binding>>>, SrcNode<Self>)>),
    Apply(SrcNode<Self>, SrcNode<Self>),
    Cons(SrcNode<Ident>, SrcNode<Self>),
}

#[derive(Debug, PartialEq)]
pub struct Generics {
    pub tys: Vec<SrcNode<Ident>>,
}

#[derive(Debug, PartialEq)]
pub struct Data {
    pub name: SrcNode<Ident>,
    pub generics: SrcNode<Generics>,
    pub variants: Vec<(SrcNode<Ident>, SrcNode<Type>)>,
}

#[derive(Debug, PartialEq)]
pub struct Alias {
    pub name: SrcNode<Ident>,
    pub generics: SrcNode<Generics>,
    pub ty: SrcNode<Type>,
}

#[derive(Debug, PartialEq)]
pub struct Def {
    pub name: SrcNode<Ident>,
    pub generics: SrcNode<Generics>,
    pub ty_hint: SrcNode<Type>,
    pub body: SrcNode<Expr>,
}

#[derive(Debug, PartialEq)]
pub enum ItemKind {
    Data(Data),
    Alias(Alias),
    Def(Def),
}

pub type Attr = Vec<SrcNode<Ident>>;

#[derive(Debug, PartialEq)]
pub struct Item {
    pub kind: ItemKind,
    pub attr: Attr,
}

#[derive(Debug, PartialEq)]
pub struct Module {
    pub items: Vec<Item>,
}

impl Module {
    pub fn datas(&self) -> impl Iterator<Item = (&Attr, &Data)> + '_ {
        self.items
            .iter()
            .filter_map(|item| match &item.kind {
                ItemKind::Data(data) => Some((&item.attr, data)),
                _ => None,
            })
    }

    pub fn aliases(&self) -> impl Iterator<Item = (&Attr, &Alias)> + '_ {
        self.items
            .iter()
            .filter_map(|item| match &item.kind {
                ItemKind::Alias(alias) => Some((&item.attr, alias)),
                _ => None,
            })
    }

    pub fn defs(&self) -> impl Iterator<Item = (&Attr, &Def)> + '_ {
        self.items
            .iter()
            .filter_map(|item| match &item.kind {
                ItemKind::Def(def) => Some((&item.attr, def)),
                _ => None,
            })
    }
}
