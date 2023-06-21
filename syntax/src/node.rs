use super::*;
use std::{
    ops::{Deref, DerefMut},
    cmp::{PartialEq, Eq, PartialOrd, Ord, Ordering},
    fmt,
};

#[derive(Clone)]
pub struct Node<T, M = ()> {
    inner: Box<T>, // TODO: Replace with smallbox or similar optimisation?
    meta: M,
}

impl<T, M> Node<T, M> {
    /// Create a new node with the given inner value and metadata.
    pub fn new(inner: T, meta: M) -> Self {
        Node { inner: Box::new(inner), meta }
    }

    /// Get a reference to the inner value.
    pub fn inner(&self) -> &T {
        &self.inner
    }

    /// Get a mutable to the inner value.
    pub fn inner_mut(&mut self) -> &mut T {
        &mut self.inner
    }

    // Take the node's inner value.
    pub fn into_inner(self) -> T { *self.inner }

    /// Map the node's inner value.
    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Node<U, M> {
        Node {
            inner: Box::new(f(*self.inner)),
            meta: self.meta,
        }
    }

    /// Get a reference to the metadata.
    pub fn meta(&self) -> &M { &self.meta }

    /// Get a mutable reference to the metadata.
    pub fn meta_mut(&mut self) -> &mut M { &mut self.meta }

    pub fn as_mut(&mut self) -> (&mut T, &mut M) { (&mut self.inner, &mut self.meta) }
}

impl<T, M> Deref for Node<T, M> {
    type Target = T;
    fn deref(&self) -> &Self::Target { self.inner() }
}

impl<T, M> DerefMut for Node<T, M> {
    fn deref_mut(&mut self) -> &mut Self::Target { self.inner_mut() }
}

impl<T: PartialEq, M> PartialEq for Node<T, M> {
    fn eq(&self, other: &Self) -> bool {
        // Only compare inner
        self.inner == other.inner
    }
}

impl<T: Eq, M> Eq for Node<T, M> {}

impl<T: Ord, M> Ord for Node<T, M> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.inner.cmp(&other.inner)
    }
}

impl<T: PartialOrd, M> PartialOrd for Node<T, M> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.inner.partial_cmp(&other.inner)
    }
}

impl<T: fmt::Debug, M: fmt::Debug> fmt::Debug for Node<T, M> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:#?} @ {:?}", self.inner, self.meta)
    }
}

pub type SrcNode<T> = Node<T, Span>;

impl<T> SrcNode<T> {
    pub fn span(&self) -> Span { self.meta }
}
