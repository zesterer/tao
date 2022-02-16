use super::*;

pub use ast::Literal as Literal;

pub trait Meta {
    type Ty;
    type Class;
}

impl Meta for InferMeta {
    type Ty = TyVar;
    type Class = ClassVar;
}

impl Meta for TyMeta {
    type Ty = TyId;
    type Class = Option<ClassId>; // Required because we don't have proper error classes yet
}

impl Meta for ConMeta {
    type Ty = ConTyId;
    type Class = !;
}

#[derive(Debug)]
pub enum Pat<M: Meta> {
    Error,
    Wildcard,
    Literal(Literal),
    Single(Node<Binding<M>, M>),
    Add(Node<Binding<M>, M>, SrcNode<u64>),
    Tuple(Vec<Node<Binding<M>, M>>),
    Record(BTreeMap<Ident, Node<Binding<M>, M>>),
    ListExact(Vec<Node<Binding<M>, M>>),
    ListFront(Vec<Node<Binding<M>, M>>, Option<Node<Binding<M>, M>>),
    Decons(SrcNode<DataId>, Ident, Node<Binding<M>, M>),
}

#[derive(Debug)]
pub struct Binding<M: Meta> {
    pub pat: SrcNode<Pat<M>>,
    pub name: Option<SrcNode<Ident>>,
}

impl<M: Meta> Binding<M> {
    pub fn from_pat(pat: SrcNode<Pat<M>>) -> Self {
        Self { pat, name: None }
    }

    pub fn wildcard(name: SrcNode<Ident>) -> Self {
        Self { pat: SrcNode::new(hir::Pat::Wildcard, name.span()), name: Some(name) }
    }

    pub fn is_refutable(&self) -> bool {
        match &*self.pat {
            Pat::Error => true,
            Pat::Wildcard => false,
            Pat::Literal(_) => true,
            Pat::Single(inner) => inner.is_refutable(),
            Pat::Add(lhs, rhs) => **rhs > 0 || lhs.is_refutable(),
            Pat::Tuple(fields) => fields
                .iter()
                .any(|field| field.is_refutable()),
            Pat::Record(fields) => fields
                .iter()
                .any(|(_, field)| field.is_refutable()),
            Pat::ListExact(_) => true,
            Pat::ListFront(items, tail) => !items.is_empty() || tail
                .as_ref()
                .map_or(false, |tail| tail.is_refutable()),
            Pat::Decons(_, _, _) => true,
        }
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
            Pat::Add(lhs, _) => lhs.get_bindings_inner(bindings),
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
            Pat::Decons(_, _, inner) => inner.get_bindings_inner(bindings),
        }
    }
}

pub type InferBinding = InferNode<Binding<InferMeta>>;
pub type TyBinding = TyNode<Binding<TyMeta>>;
pub type ConBinding = ConNode<Binding<ConMeta>>;

#[derive(Clone, Debug)]
pub enum Intrinsic {
    TypeName,
}

#[derive(Debug)]
pub enum Expr<M: Meta> {
    Error,
    Literal(Literal),
    // TODO: replace with `Item` when scoping is added
    Local(Ident),
    Global(DefId, Vec<M>),
    Tuple(Vec<Node<Self, M>>),
    Union(Node<Self, M>),
    List(Vec<Node<Self, M>>),
    ListFront(Vec<Node<Self, M>>, Node<Self, M>),
    Record(Vec<(SrcNode<Ident>, Node<Self, M>)>),
    Access(Node<Self, M>, SrcNode<Ident>),
    Unary(SrcNode<ast::UnaryOp>, Node<Self, M>),
    Binary(SrcNode<ast::BinaryOp>, Node<Self, M>, Node<Self, M>),
    // hidden_outer
    Match(bool, Node<Self, M>, Vec<(Node<Binding<M>, M>, Node<Self, M>)>),
    Func(Node<Ident, M>, Node<Self, M>),
    Apply(Node<Self, M>, Node<Self, M>),
    Cons(SrcNode<DataId>, Ident, Node<Self, M>),

    // member, class, field
    ClassAccess(M, M::Class, SrcNode<Ident>),

    Debug(Node<Self, M>),
    Intrinsic(SrcNode<Intrinsic>, Vec<Node<Self, M>>),
}

pub type InferExpr = InferNode<Expr<InferMeta>>;
pub type TyExpr = TyNode<Expr<TyMeta>>;
pub type ConExpr = ConNode<Expr<ConMeta>>;
