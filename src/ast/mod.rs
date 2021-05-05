pub mod loader;
pub mod lex;
pub mod parse;

pub use self::{
    loader::{Src, SrcId, Loader, LoadCache},
    lex::lex,
};

use std::fmt;
use internment::Intern;
use crate::util::SrcNode;

pub type SrcStr = Intern<String>;
pub type Ident = Intern<String>;

#[derive(Debug, PartialEq, Eq)]
pub enum PathBase {
    Root,
    Super,
    This,
}

pub struct Item {
    pub name: SrcNode<Ident>,
    pub base: Option<SrcNode<PathBase>>,
    pub path: Vec<SrcNode<Ident>>,
}

impl Item {
    pub fn local(name: SrcNode<Ident>) -> Self {
        Self {
            name,
            base: None,
            path: Vec::new(),
        }
    }
}

impl fmt::Debug for Item {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(base) = &self.base {
            write!(f, "{:?}::", base.inner())?;
        }
        for part in self.path.iter() {
            write!(f, "{}::", part.inner().as_str())?;
        }
        write!(f, "{}", self.name.inner().as_str())
    }
}

#[derive(Clone, Debug)]
pub enum Literal {
    Nat(SrcStr),
    Int(SrcStr),
    Num(SrcStr),
    Char(char),
    Bool(bool),
    Str(SrcStr),
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Literal::Nat(s) => write!(f, "{}", s),
            Literal::Int(s) => write!(f, "{}", s),
            Literal::Num(s) => write!(f, "{}", s),
            Literal::Char(c) => write!(f, "{}", c),
            Literal::Bool(b) => write!(f, "{}", b),
            Literal::Str(s) => write!(f, "{}", s),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum UnaryOp {
    Neg,
    Not,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            UnaryOp::Neg => write!(f, "-"),
            UnaryOp::Not => write!(f, "!"),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum BinaryOp {
    Add, Sub,

    Mul, Div, Rem,

    Eq, NotEq,
    Less, LessEq,
    More, MoreEq,

    And, Or,

    Join,
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BinaryOp::Add => write!(f, "+"),
            BinaryOp::Sub => write!(f, "-"),
            BinaryOp::Mul => write!(f, "*"),
            BinaryOp::Div => write!(f, "/"),
            BinaryOp::Rem => write!(f, "%"),
            BinaryOp::Eq => write!(f, "="),
            BinaryOp::NotEq => write!(f, "!="),
            BinaryOp::Less => write!(f, "<"),
            BinaryOp::LessEq => write!(f, "<="),
            BinaryOp::More => write!(f, ">"),
            BinaryOp::MoreEq => write!(f, ">="),
            BinaryOp::And => write!(f, "and"),
            BinaryOp::Or => write!(f, "or"),
            BinaryOp::Join => write!(f, "++"),
        }
    }
}

#[derive(Debug)]
pub enum Type {
    Unknown,
    List(SrcNode<Self>),
    Tuple(Vec<SrcNode<Self>>),
    Record(Vec<(SrcNode<Ident>, SrcNode<Self>)>),
    Func(SrcNode<Self>, SrcNode<Self>),
    Data(SrcNode<Item>, Vec<SrcNode<Self>>),
}

#[derive(Debug)]
pub enum Pat {
    Wildcard,
    Literal(Literal),
    List(Vec<SrcNode<Binding>>),
    ListFront(Vec<SrcNode<Binding>>, Option<SrcNode<Ident>>),
    Tuple(Vec<SrcNode<Binding>>),
    Record(Vec<(SrcNode<Ident>, SrcNode<Binding>)>),
    Deconstruct(SrcNode<Item>, SrcNode<Binding>),
}

#[derive(Debug)]
pub struct Binding {
    pub pat: SrcNode<Pat>,
    pub binding: Option<SrcNode<Ident>>,
    pub ty: SrcNode<Type>,
}

#[derive(Debug)]
pub struct MatchArm {
    pub binding: SrcNode<Binding>,
    pub body: SrcNode<Expr>,
}

#[derive(Debug)]
pub enum DoStmt {
    Expr(SrcNode<Expr>),
    Bind(SrcNode<Binding>, SrcNode<Expr>),
    Return(SrcNode<Expr>),
}

#[derive(Debug)]
pub enum Expr {
    Literal(Literal),
    Item(Item),

    Coerce(SrcNode<Self>),
    Unary(SrcNode<UnaryOp>, SrcNode<Self>),
    Binary(SrcNode<BinaryOp>, SrcNode<Self>, SrcNode<Self>),
    Intrinsic(SrcNode<Ident>, Vec<SrcNode<Self>>),

    Let(SrcNode<Binding>, SrcNode<Self>, SrcNode<Self>),
    If(SrcNode<Self>, SrcNode<Self>, SrcNode<Self>),
    Match(SrcNode<Self>, Vec<MatchArm>),

    Func(SrcNode<Binding>, SrcNode<Self>),
    Apply(SrcNode<Self>, SrcNode<Self>),

    List(Vec<SrcNode<Self>>),
    Tuple(Vec<SrcNode<Self>>),
    Record(Vec<(SrcNode<Ident>, SrcNode<Self>)>),
    Construct(SrcNode<Item>, SrcNode<Self>),

    Access(SrcNode<Self>, SrcNode<Ident>),
    Update(SrcNode<Self>, SrcNode<Ident>, SrcNode<Self>),

    Do(Vec<SrcNode<DoStmt>>),
}

#[derive(Debug)]
pub struct Generics {
    pub params: Vec<SrcNode<Ident>>,
}

#[derive(Debug)]
pub struct Alias {
    pub name: SrcNode<Ident>,
    pub generics: SrcNode<Generics>,
    pub ty: SrcNode<Type>,
}

#[derive(Debug)]
pub struct Data {
    pub name: SrcNode<Ident>,
    pub generics: SrcNode<Generics>,
    pub variants: Vec<(SrcNode<Ident>, Option<SrcNode<Type>>)>,
}

#[derive(Debug)]
pub struct Def {
    pub name: SrcNode<Ident>,
    pub generics: SrcNode<Generics>,
    pub ty: SrcNode<Type>,
    pub body: SrcNode<Expr>,
}

#[derive(Debug, Default)]
pub struct Module {
    pub aliases: Vec<Alias>,
    pub datas: Vec<Data>,
    pub defs: Vec<Def>,
    pub submodules: Vec<(SrcNode<Ident>, Module)>,
}

#[derive(Debug)]
pub struct Program {
    pub root: SrcNode<Module>,
}

impl Program {
    pub fn new(root: SrcNode<Module>) -> Self {
        Self { root }
    }
}
