pub mod infer;
pub mod ty;
pub mod data;
pub mod intrinsic;
pub mod lower;
pub mod check;

pub use self::{
    infer::{TyVar, InferCtx, TyInfo, SolvedTys, EquateReason},
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
use std::{fmt, collections::HashMap};

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Path {
    Root,
    Branch(Intern<Path>, Ident),
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Item(pub Intern<Path>, pub Ident);

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

    Coerce(Node<Self, A>),
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

#[derive(Debug)]
pub struct Def {
    pub name: SrcNode<Ident>,
    pub generics: TyGenerics,
    pub body: TyExpr,
}

#[derive(Debug, Default)]
pub struct DefCtx {
    pub defs: HashMap<Item, Def>,
}

#[derive(Debug, Default)]
pub struct Program {
    pub data_ctx: DataCtx,
    pub def_ctx: DefCtx,
}

// Ctx

#[derive(Default)]
pub struct Ctx {
    ty: TyCtx,
    errors: Vec<Error>,
}

impl Ctx {
    pub fn emit_error(&mut self, error: Error) {
        self.errors.push(error);
    }

    pub fn take_errors(&mut self) -> Vec<Error> {
        self.errors.sort_by_key(|e| e.span);
        std::mem::take(&mut self.errors)
    }

    pub fn display_ty(&self, id: TyId) -> TyDisplay<'_> {
        self.ty.display(id)
    }

    pub fn fmt_expr(&self, f: &mut fmt::Formatter, expr: &TyExpr, depth: usize) -> fmt::Result {
        match expr.inner() {
            Expr::Error => write!(f, "!"),
            Expr::Literal(litr) => write!(f, "{}", litr),
            Expr::Local(ident) => write!(f, "{}", ident),
            Expr::Match(pred, arms) => write!(f, "match {} {{\n{}{}}}", self.display_custom(pred, depth, Self::fmt_expr), arms
                .iter()
                .map(|arm| format!(
                    "{}| {} => {}\n",
                    std::iter::repeat("    ").take(depth + 1).collect::<String>(),
                    self.display_custom(&arm.binding, depth + 1, Self::fmt_binding),
                    self.display_custom(&arm.body, depth + 1, Self::fmt_expr),
                ))
                .collect::<Vec<_>>()
                .join(""), std::iter::repeat("    ").take(depth).collect::<String>()),
            Expr::List(items) => write!(f, "[{}]", items
                .iter()
                .map(|item| format!("{}", self.display_custom(item, depth, Self::fmt_expr)))
                .collect::<Vec<_>>()
                .join(", ")),
            Expr::Tuple(fields) => write!(f, "({})", fields
                .iter()
                .map(|field| format!("{}", self.display_custom(field, depth, Self::fmt_expr)))
                .collect::<Vec<_>>()
                .join(", ")),
            Expr::Func(param, body) => write!(f, "|{}| {}", self.display_custom(param, depth, Self::fmt_binding), self.display_custom(body, depth, Self::fmt_expr)),
            Expr::Apply(func, arg) => write!(f, "({})({})", self.display_custom(func, depth, Self::fmt_expr), self.display_custom(arg, depth, Self::fmt_expr)),
            expr => todo!("{:?}", expr),
        }
    }

    pub fn fmt_binding(&self, f: &mut fmt::Formatter, binding: &TyBinding, depth: usize) -> fmt::Result {
        if let Some(ident) = &binding.binding {
            write!(f, "{}", ident.inner())?;

            match binding.pat.inner() {
                Pat::Wildcard => {},
                _ => write!(f, ": ")?,
            }
        }

        match binding.pat.inner() {
            Pat::Wildcard => {},
            Pat::Literal(litr) => write!(f, "{}", litr)?,
            Pat::List(items) => write!(f, "[{}]", items
                .iter()
                .map(|item| format!("{}", self.display_custom(item, depth, Self::fmt_binding)))
                .collect::<Vec<_>>()
                .join(", "))?,
            Pat::Tuple(fields) => write!(f, "({})", fields
                .iter()
                .map(|field| format!("{}", self.display_custom(field, depth, Self::fmt_binding)))
                .collect::<Vec<_>>()
                .join(", "))?,
            binding => todo!("{:?}", binding),
        }
        write!(f, " :: {}", self.display_ty(binding.ty()))
    }

    fn display_custom<'a, T>(&'a self, x: &'a T, depth: usize, f: fn(&Ctx, &mut fmt::Formatter, &T, usize) -> fmt::Result) -> impl fmt::Display + 'a {
        struct DisplayExpr<'a, T> {
            ctx: &'a Ctx,
            x: &'a T,
            f: fn(&Ctx, &mut fmt::Formatter, &T, usize) -> fmt::Result,
            depth: usize,
        }

        impl<'a, T> fmt::Display for DisplayExpr<'a, T> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                (self.f)(self.ctx, f, self.x, self.depth)
            }
        }

        DisplayExpr { ctx: self, x, f, depth }
    }

    pub fn display_expr<'a>(&'a self, expr: &'a TyExpr) -> impl fmt::Display + 'a {
        self.display_custom(expr, 0, Self::fmt_expr)
    }
}
