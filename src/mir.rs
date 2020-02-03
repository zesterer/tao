use std::collections::HashMap;
use internment::LocalIntern;
use crate::{
    ast,
    ty::Type,
    node2::TypeNode,
    hir2::Path,
};

type Ident = LocalIntern<String>;

pub type Unary = ast::UnaryOp;
pub type Binary = ast::BinaryOp;

// Raw types have had their name erased (since type checking and inference has already occurred)
pub enum RawType {
    List(Box<Self>),
    Tuple(Vec<Self>),
    Sum(Vec<Self>),
    Func(Box<Self>, Box<Self>),
}

pub enum Expr {
    // Get the value of the given global
    GetGlobal(LocalIntern<Path>),
    // Perform a built-in unary operation
    Unary(Unary, TypeNode<Expr>),
    // Perform a built-in binary operation
    Binary(Binary, TypeNode<Expr>, TypeNode<Expr>),
    // Call the given expression with the given arguments
    Call(TypeNode<Expr>, Vec<TypeNode<Expr>>),
    // Create a function with the given parameters and body
    MakeFunc(Vec<TypeNode<Expr>>, TypeNode<Expr>),
}

pub struct Module {
    defs: HashMap<LocalIntern<Path>, Expr>,
}
