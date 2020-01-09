use std::{
    fmt,
    ops::{Deref, DerefMut},
    cmp::PartialEq,
    hash::{Hash, Hasher},
};
use crate::src::SrcRegion;

#[derive(Clone)]
pub struct Node<T, U = ()> {
    pub inner: Box<T>,
    pub region: SrcRegion,
    pub meta: U
}

impl<T, U> Node<T, U> {
    pub fn new(inner: T, region: SrcRegion, meta: U) -> Self {
        Self {
            inner: Box::new(inner),
            region,
            meta,
        }
    }

    pub fn inner(&self) -> &T {
        &self.inner
    }

    pub fn inner_mut(&mut self) -> &mut T {
        &mut self.inner
    }

    pub fn meta(&self) -> &U {
        &self.meta
    }

    pub fn meta_mut(&mut self) -> &mut U {
        &mut self.meta
    }

    pub fn into_inner(self) -> T {
        *self.inner
    }

    pub fn region(&self) -> SrcRegion {
        self.region
    }

    pub fn at(mut self, region: SrcRegion) -> Self {
        self.region = region;
        self
    }

    pub fn map_inner<V>(self, f: impl FnOnce(T) -> V) -> Node<V, U> {
        Node {
            inner: Box::new(f(*self.inner)),
            region: self.region,
            meta: self.meta,
        }
    }
}

impl<T: Default, U: Default> Default for Node<T, U> {
    fn default() -> Self {
        Self::new(Default::default(), SrcRegion::none(), Default::default())
    }
}

impl<T: Hash, U> Hash for Node<T, U> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.inner().hash(state);
    }
}

impl<T, U> Deref for Node<T, U> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.inner()
    }
}

impl<T, U> DerefMut for Node<T, U> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.inner_mut()
    }
}

impl<T, U: PartialEq<T>, V> PartialEq<T> for Node<U, V> {
    fn eq(&self, other: &T) -> bool {
        &*self.inner == other
    }
}

impl<U, T: PartialEq<Node<T, U>>> Eq for Node<T, U> {}

impl<T: fmt::Debug, U: fmt::Debug> fmt::Debug for Node<T, U> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if f.alternate() {
            write!(f, "Node({:#?}, {:#?}, {:#?})", self.inner, self.region, self.meta)
        } else {
            write!(f, "Node({:?}, {:?}, {:?})", self.inner, self.region, self.meta)
        }
    }
}
