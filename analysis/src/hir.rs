use super::*;

#[derive(Debug)]
pub enum Pat<M> {
    Error,
    Wildcard,
    Literal(ast::Literal),
    Single(Node<Binding<M>, M>),
    Tuple(Vec<Node<Binding<M>, M>>),
    Record(HashMap<Ident, Node<Binding<M>, M>>),
    ListExact(Vec<Node<Binding<M>, M>>),
    ListFront(Vec<Node<Binding<M>, M>>, Option<Node<Binding<M>, M>>),
    Decons(SrcNode<DataId>, Node<Binding<M>, M>),
}

#[derive(Debug)]
pub struct Binding<M> {
    pub pat: SrcNode<Pat<M>>,
    pub name: Option<SrcNode<Ident>>,
}

impl<M> Binding<M> {
    pub fn from_pat(pat: SrcNode<Pat<M>>) -> Self {
        Self { pat, name: None }
    }

    pub fn wildcard(name: SrcNode<Ident>) -> Self {
        Self { pat: SrcNode::new(hir::Pat::Wildcard, name.span()), name: Some(name) }
    }
}

impl Binding<InferMeta> {
    pub fn get_bindings(self: &InferNode<Self>) -> Vec<(SrcNode<Ident>, TyVar)> {
        let mut bindings = Vec::new();
        self.get_bindings_inner(&mut bindings);
        bindings
    }

    fn get_bindings_inner(self: &InferNode<Self>, bindings: &mut Vec<(SrcNode<Ident>, TyVar)>) {
        // TODO: Check for duplicates!
        if let Some(name) = &self.name { bindings.push((name.clone(), self.meta().1)) };
        match &*self.pat {
            Pat::Error => {},
            Pat::Wildcard => {},
            Pat::Literal(_) => {},
            Pat::Single(inner) => inner.get_bindings_inner(bindings),
            Pat::Tuple(items) => items
                .iter()
                .for_each(|item| item.get_bindings_inner(bindings)),
            Pat::Record(fields) => fields
                .values()
                .for_each(|field| field.get_bindings_inner(bindings)),
            Pat::ListExact(items) => items
                .iter()
                .for_each(|item| item.get_bindings_inner(bindings)),
            Pat::ListFront(items, tail) => {
                items
                    .iter()
                    .for_each(|item| item.get_bindings_inner(bindings));
                if let Some(tail) = tail { tail.get_bindings_inner(bindings); }
            },
            Pat::Decons(_, inner) => inner.get_bindings_inner(bindings),
        }
    }
}

pub type InferBinding = InferNode<Binding<InferMeta>>;
pub type TyBinding = TyNode<Binding<TyMeta>>;

#[derive(Debug)]
pub enum Expr<M> {
    Error,
    Literal(ast::Literal),
    // TODO: replace with `Item` when scoping is added
    Local(Ident),
    Global(Ident),
    Tuple(Vec<Node<Self, M>>),
    List(Vec<Node<Self, M>>),
    Record(Vec<(SrcNode<Ident>, Node<Self, M>)>),
    Access(Node<Self, M>, SrcNode<Ident>),
    Unary(SrcNode<ast::UnaryOp>, Node<Self, M>),
    Binary(SrcNode<ast::BinaryOp>, Node<Self, M>, Node<Self, M>),
    Match(Node<Self, M>, Vec<(Node<Binding<M>, M>, Node<Self, M>)>),
    Func(Node<Binding<M>, M>, Node<Self, M>),
    Apply(Node<Self, M>, Node<Self, M>),
    Cons(SrcNode<DataId>, Node<Self, M>),
}

pub type InferExpr = InferNode<Expr<InferMeta>>;
pub type TyExpr = TyNode<Expr<TyMeta>>;