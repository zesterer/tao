use super::*;
use core::{
    ops::{Deref, DerefMut},
    marker::PhantomData,
    hash::Hash,
};

#[derive(Clone, Debug, PartialEq)]
pub struct SrcNode<T> {
    pub inner: T,
    pub span: Span,
}

impl<T> Deref for SrcNode<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T> DerefMut for SrcNode<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl<T> SrcNode<T> {
    pub fn new(inner: T, span: Span) -> Self {
        Self { inner, span }
    }
}

// Store

#[derive(Debug)]
pub struct Id<T>(usize, PhantomData<T>);

impl<T> Copy for Id<T> {}
impl<T> Clone for Id<T> {
    fn clone(&self) -> Self { *self }
}

#[derive(Debug)]
pub struct Store<T, N> {
    items: Vec<T>,
    lut: HashMap<N, Id<T>>,
}

impl<T, N> Default for Store<T, N> {
    fn default() -> Self {
        Self {
            items: Vec::new(),
            lut: HashMap::default(),
        }
    }
}

impl<T, N: Hash + Eq> Store<T, N> {
    // Insert a new item into the store.
    pub fn add(&mut self, name: N, item: T) -> Result<Id<T>, &T> {
        match self.lut.entry(name) {
            Entry::Occupied(e) => Err(&self.items[e.get().0]),
            Entry::Vacant(e) => {
                let id = Id(self.items.len(), PhantomData);
                self.items.push(item);
                e.insert(id);
                Ok(id)
            },
        }
    }

    pub fn lookup(&self, name: &N) -> Option<Id<T>> { self.lut.get(name).copied() }

    pub fn get(&self, id: Id<T>) -> &T { &self.items[id.0] }
    pub fn get_mut(&mut self, id: Id<T>) -> &mut T { &mut self.items[id.0] }
}
