use std::{
    ops::Deref,
    cmp::PartialEq,
};
use crate::src::SrcRegion;

#[derive(Clone, Debug, Hash)]
pub struct Node<T>(Box<T>, SrcRegion);

impl<T> Node<T> {
    pub fn new(inner: T, region: SrcRegion) -> Self {
        Self(Box::new(inner), region)
    }

    pub fn inner(&self) -> &T {
        &self.0
    }

    pub fn into_inner(self) -> T {
        *self.0
    }

    pub fn region(&self) -> SrcRegion {
        self.1
    }

    pub fn map_inner<U>(self, f: impl FnOnce(T) -> U) -> Node<U> {
        Node(Box::new(f(*self.0)), self.1)
    }
}

impl<T> Deref for Node<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.inner()
    }
}

impl<T, U: PartialEq<T>> PartialEq<T> for Node<U> {
    fn eq(&self, other: &T) -> bool {
        &*self.0 == other
    }
}

impl<T: PartialEq<Node<T>>> Eq for Node<T> {}
