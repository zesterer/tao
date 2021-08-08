use super::*;
use std::ops::Deref;

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

    /// Get a reference to the metadata.
    pub fn meta(&self) -> &M { &self.meta }
}

impl<T, M> Deref for Node<T, M> {
    type Target = T;
    fn deref(&self) -> &Self::Target { self.inner() }
}

pub type SrcNode<T> = Node<T, Span>;
