use super::*;
use internment::Intern;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Ident(Intern<String>);

impl Ident {
    pub fn new<S: ToString>(s: S) -> Self { Self(Intern::new(s.to_string())) }
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

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
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

#[derive(Debug, PartialEq)]
pub enum Literal {
    Nat(u64),
    Num(f64),
    Bool(bool),
    Char(char),
    Str(Intern<String>),
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    // Generated only by parser errors
    Error,
    Literal(Literal),
    // TODO: replace with `Item` when scoping is added
    Local(Ident),
    Unary(SrcNode<UnaryOp>, SrcNode<Self>),
    Binary(SrcNode<BinaryOp>, SrcNode<Self>, SrcNode<Self>),
}
