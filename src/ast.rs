use std::ops::Deref;
use internment::LocalIntern;
use parze::prelude::*;
use crate::{
    node,
    lex::{Token, Delimiter, Op},
    src::SrcRegion,
    error::Error,
};

type Ident = LocalIntern<String>;

#[derive(Debug)]
pub struct Node<T>(Box<T>, SrcRegion);

impl<T> Deref for Node<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target { &*self.0 }
}

#[derive(Debug)]
pub enum Literal {
    Number(f64),
    String(LocalIntern<String>),
    Boolean(bool),
}

#[derive(Debug)]
pub struct Path(Vec<Ident>);

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum UnaryOp {
    Neg,

    Not,

    Head,
    Tail,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,

    Eq,
    Less,
    More,
    LessEq,
    MoreEq,

    Join,

    Apply,
}

#[derive(Debug)]
pub enum Pat {
    Wildcard,
    Literal(Literal),
    Ident(Ident),
    List(Vec<Node<Pat>>),
    ListFront(Vec<Node<Pat>>),
    ListBack(Vec<Node<Pat>>),
    Tuple(Vec<Node<Pat>>),
}

#[derive(Debug)]
pub enum Type {
    Ident(Ident),
    Complex(Node<Type>, Vec<Node<Type>>),
    Tuple(Vec<Node<Type>>),
    Func(Node<Type>, Node<Type>),
}

#[derive(Debug)]
pub enum Expr {
    Literal(Literal),
    Path(Path),
    Unary(Node<UnaryOp>, Node<Expr>),
    Binary(Node<BinaryOp>, Node<Expr>, Node<Expr>),
    If(Node<Expr>, Node<Expr>, Node<Expr>),
    Match(Node<Expr>, Vec<(Node<Pat>, Node<Expr>)>),
    Func(Node<Pat>, Node<Expr>),
    Apply(Node<Expr>, Node<Expr>),
    Let(Node<Pat>, Option<Node<Type>>, Node<Expr>, Node<Expr>),
    List(Vec<Node<Expr>>),
    Tuple(Vec<Node<Expr>>),
}

/*
fn ident_parser() -> Parser<impl Pattern<Error, Input=node::Node<Token>, Output=Node<Ident>>, Error> {
    todo!()
}
*/
