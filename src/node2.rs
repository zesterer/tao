use std::ops::Deref;
use crate::{
    src::SrcRegion,
    ty::{TypeId, Type},
    mir::RawType,
};

#[derive(Clone, Debug)]
pub struct Node<T, U>(Box<T>, U);

impl<T, U> Node<T, U> {
    pub fn new(item: T, attr: U) -> Self {
        Self(Box::new(item), attr)
    }

    pub fn attr(&self) -> &U { &self.1 }
}

impl<T, U> Deref for Node<T, U> {
    type Target = T;

    fn deref(&self) -> &Self::Target { &*self.0 }
}

// SrcNode

pub type SrcNode<T> = Node<T, SrcRegion>;

impl<T> Node<T, SrcRegion> {
    pub fn region(&self) -> SrcRegion {
        *self.attr()
    }
}

// SrcTypeNode

pub type SrcTypeNode<T> = Node<T, (SrcRegion, TypeId)>;

impl<T> Node<T, (SrcRegion, TypeId)> {
    pub fn ty(&self) -> TypeId {
        self.attr().1
    }

    pub fn region(&self) -> SrcRegion {
        self.attr().0
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
