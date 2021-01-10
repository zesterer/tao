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

#[derive(Debug)]
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

#[derive(Clone, Debug)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Clone, Debug)]
pub enum BinaryOp {
    Add, Sub,

    Mul, Div, Rem,

    Eq, NotEq,
    Less, LessEq,
    More, MoreEq,

    And, Or,

    Join,
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

pub struct Generics {
    pub params: Vec<SrcNode<Ident>>,
}

pub struct Alias {
    pub name: SrcNode<Ident>,
    pub generics: SrcNode<Generics>,
    pub ty: SrcNode<Type>,
}

pub struct Data {
    pub name: SrcNode<Ident>,
    pub generics: SrcNode<Generics>,
    pub variants: Vec<(SrcNode<Ident>, Option<SrcNode<Type>>)>,
}

pub struct Def {
    pub name: SrcNode<Ident>,
    pub generics: SrcNode<Generics>,
    pub ty: SrcNode<Type>,
    pub body: SrcNode<Expr>,
}

pub struct Module {
    pub name: SrcNode<Ident>,
    pub aliases: Vec<Alias>,
    pub datas: Vec<Data>,
    pub defs: Vec<Def>,
    pub submodules: Vec<Module>,
}
