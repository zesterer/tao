pub mod infer;
pub mod ty;
pub mod data;
pub mod intrinsic;
pub mod lower;
pub mod check;

pub use self::{
    infer::{TyVar, InferCtx, TyInfo},
    ty::{Ty, TyId, TyCtx, TyDisplay, Primitive},
    data::DataCtx,
    intrinsic::Intrinsic,
};
pub use crate::ast::{Ident, Literal, UnaryOp, BinaryOp};

use internment::Intern;
use crate::{
    util::{Span, Node, SrcNode, InferNode, TyNode},
    Error,
};
use std::collections::HashMap;

#[derive(Debug)]
pub enum Path {
    Root,
    Branch(Intern<Path>, Ident),
}

#[derive(Debug)]
pub struct Item(Intern<Path>, Ident);

#[derive(Debug)]
pub struct Generics<A> {
    pub params: HashMap<Ident, (SrcNode<()>, A)>,
}

pub type InferGenerics = SrcNode<Generics<(Span, TyVar)>>;
pub type TyGenerics = SrcNode<Generics<Span>>;

#[derive(Debug)]
pub enum Pat<A> {
    Wildcard,
    Literal(Literal),
    List(Vec<Node<Binding<A>, A>>),
    ListFront(Vec<Node<Binding<A>, A>>, Option<SrcNode<Ident>>),
    Tuple(Vec<Node<Binding<A>, A>>),
    Record(HashMap<Ident, (SrcNode<Ident>, Node<Binding<A>, A>)>),
    Deconstruct(SrcNode<Item>, Node<Binding<A>, A>),
}

#[derive(Debug)]
pub struct Binding<A> {
    pub pat: SrcNode<Pat<A>>,
    pub binding: Option<SrcNode<Ident>>,
}

pub type InferBinding = InferNode<Binding<(Span, TyVar)>>;
pub type TyBinding = TyNode<Binding<TyId>>;

#[derive(Debug)]
pub struct MatchArm<A> {
    pub binding: Node<Binding<A>, A>,
    pub body: Node<Expr<A>, A>,
}

#[derive(Debug)]
pub enum Expr<A> {
    /// A node that previously generated an error
    Error,

    Literal(Literal),
    Local(Ident),
    Global(Item, Generics<A>),

    Unary(SrcNode<UnaryOp>, Node<Self, A>),
    Binary(SrcNode<BinaryOp>, Node<Self, A>, Node<Self, A>),
    Intrinsic(SrcNode<Intrinsic>, Vec<Node<Self, A>>),

    Match(Node<Self, A>, Vec<MatchArm<A>>),

    Func(Node<Binding<A>, A>, Node<Self, A>),
    Apply(Node<Self, A>, Node<Self, A>),

    List(Vec<Node<Self, A>>),
    Tuple(Vec<Node<Self, A>>),
    Record(HashMap<Ident, (SrcNode<Ident>, Node<Self, A>)>),
    Construct(SrcNode<Item>, Node<Self, A>),

    Access(Node<Self, A>, SrcNode<Ident>),
    Update(Node<Self, A>, SrcNode<Ident>, Node<Self, A>),
}

pub type InferExpr = InferNode<Expr<(Span, TyVar)>>;
pub type TyExpr = TyNode<Expr<TyId>>;

pub struct Def {
    pub name: SrcNode<Ident>,
    pub generics: TyGenerics,
    pub body: TyExpr,
}

pub struct DefCtx {
    pub defs: HashMap<Item, Def>
}

pub struct Program {
    pub data_ctx: DataCtx,
    pub def_ctx: DefCtx,
}

// Ctx

#[derive(Default)]
pub struct Ctx {
    ty: TyCtx,
    pub errors: Vec<Error>,
}

impl Ctx {
    pub fn emit_error(&mut self, error: Error) {
        self.errors.push(error);
    }

    pub fn display_ty(&self, id: TyId) -> TyDisplay<'_> {
        self.ty.display(id)
    }
}
