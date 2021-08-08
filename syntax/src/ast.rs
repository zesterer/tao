use super::*;
use internment::Intern;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Ident(Intern<String>);

impl Ident {
    pub fn new<S: ToString>(s: S) -> Self { Self(Intern::new(s.to_string())) }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum PathBase {
    Root,
    Parent,
    This,
}

pub struct Item {
    pub name: SrcNode<Ident>,
    pub base: SrcNode<PathBase>,
    pub path: Vec<SrcNode<Ident>>,
}

pub enum Literal {
    Nat(u64),
    Num(f64),
    Char(char),
    Str(Intern<String>),
}
