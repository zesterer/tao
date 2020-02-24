use std::{
    fmt,
    ops::{Deref, DerefMut},
};
use crate::{
    src::SrcRegion,
    ty::{TypeId, Type},
    mir::RawType,
};

#[derive(Clone)]
pub struct Node<T, U>(Box<T>, U);

impl<T, U> Node<T, U> {
    pub fn new(item: T, attr: U) -> Self {
        Self(Box::new(item), attr)
    }

    pub fn into_inner(self) -> T {
        *self.0
    }

    pub fn attr(&self) -> &U { &self.1 }

    pub fn attr_mut(&mut self) -> &mut U { &mut self.1 }
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
            write!(f, "({:#?}: {:#?})", self.0, self.attr())
        } else {
            write!(f, "({:?}: {:?})", self.0, self.attr())
        }
    }
}

// SrcNode

pub type SrcNode<T> = Node<T, SrcRegion>;

impl<T> Node<T, SrcRegion> {
    pub fn region(&self) -> SrcRegion {
        *self.attr()
    }
}

// TypeNode

pub type TypeNode<T> = Node<T, Type>;

impl<T> Node<T, Type> {
    pub fn ty(&self) -> &Type {
        self.attr()
    }
}

// RawTypeNode

pub type RawTypeNode<T> = Node<T, RawType>;

impl<T> Node<T, RawType> {
    pub fn ty(&self) -> &RawType {
        self.attr()
    }
}
