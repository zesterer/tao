use std::{
    fmt,
    ops::{Deref, DerefMut},
    cmp::{PartialEq, Eq},
};
use crate::{
    util::Span,
    hir::{TyVar, TyId},
};

#[derive(Clone)]
pub struct Node<T, U>(Box<T>, U);

impl<T, U> Node<T, U> {
    pub fn new(item: T, attr: U) -> Self {
        Self(Box::new(item), attr)
    }

    pub fn inner(&self) -> &T {
        &self.0
    }

    pub fn into_inner(self) -> T {
        *self.0
    }

    pub fn attr(&self) -> &U { &self.1 }

    pub fn map_inner<V>(self, f: impl FnOnce(T) -> V) -> Node<V, U> {
        let Node(inner, meta) = self;
        Node::new(f(*inner), meta)
    }

    pub fn map_attr<V>(self, f: impl FnOnce(U) -> V) -> Node<T, V> {
        let Node(inner, attr) = self;
        Node(inner, f(attr))
    }
}

impl<T, U> Deref for Node<T, U> {
    type Target = T;

    fn deref(&self) -> &Self::Target { &*self.0 }
}
impl<T, U> DerefMut for Node<T, U> {
    fn deref_mut(&mut self) -> &mut Self::Target { &mut *self.0 }
}

impl<T: fmt::Debug, U: fmt::Debug> fmt::Debug for Node<T, U> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if f.alternate() {
            write!(f, "({:#?}: {:#?})", self.inner(), self.attr())
        } else {
            write!(f, "({:?}: {:?})", self.inner(), self.attr())
        }
    }
}

impl<T: PartialEq, U> PartialEq for Node<T, U> {
    fn eq(&self, other: &Self) -> bool {
        self.inner() == other.inner()
    }
}
impl<T: Eq, U> Eq for Node<T, U> {}

// SrcNode

pub type SrcNode<T> = Node<T, Span>;

impl<T> SrcNode<T> {
    pub fn span(&self) -> Span {
        *self.attr()
    }

    pub fn or_span(self, span: Span) -> Self {
        if self.span().range().is_some() {
            self
        } else {
            self.map_attr(|_| span)
        }
    }
}

// InferNode

pub type InferNode<T> = Node<T, (Span, TyVar)>;

impl<T> InferNode<T> {
    pub fn span(&self) -> Span {
        self.attr().0
    }

    pub fn ty(&self) -> TyVar {
        self.attr().1
    }
}

// TyNode

pub type TyNode<T> = Node<T, TyId>;

impl<T> TyNode<T> {
    pub fn ty(&self) -> TyId {
        *self.attr()
    }
}
