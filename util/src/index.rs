use std::{
    cmp::{Eq, Ord, Ordering, PartialEq, PartialOrd},
    fmt, hash,
    marker::PhantomData,
};

pub struct Id<T>(usize, PhantomData<T>);

impl<T> Copy for Id<T> {}
impl<T> Clone for Id<T> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<T> Eq for Id<T> {}
impl<T> PartialEq for Id<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
impl<T> Ord for Id<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.cmp(&(other.0))
    }
}
impl<T> PartialOrd for Id<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl<T> fmt::Debug for Id<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Id<{}>({})", std::any::type_name::<T>(), self.0)
    }
}
impl<T> hash::Hash for Id<T> {
    fn hash<H: hash::Hasher>(&self, h: &mut H) {
        self.0.hash(h);
    }
}

#[derive(Clone)]
pub struct Index<T> {
    items: Vec<T>,
}

impl<T> Default for Index<T> {
    fn default() -> Self {
        Self { items: Vec::new() }
    }
}

impl<T> Index<T> {
    pub fn keys(&self) -> impl Iterator<Item = Id<T>> + '_ {
        (0..self.items.len()).map(|idx| Id(idx, PhantomData))
    }

    pub fn values(&self) -> impl Iterator<Item = &T> + '_ {
        self.items.iter()
    }

    pub fn iter(&self) -> impl Iterator<Item = (Id<T>, &T)> + '_ {
        self.items
            .iter()
            .enumerate()
            .map(|(idx, item)| (Id(idx, PhantomData), item))
    }

    pub fn add(&mut self, item: T) -> Id<T> {
        let id = Id(self.items.len(), PhantomData);
        self.items.push(item);
        id
    }
}

impl<T> std::ops::Index<Id<T>> for Index<T> {
    type Output = T;
    fn index(&self, id: Id<T>) -> &Self::Output {
        &self.items[id.0]
    }
}

impl<T> std::ops::IndexMut<Id<T>> for Index<T> {
    fn index_mut(&mut self, id: Id<T>) -> &mut Self::Output {
        &mut self.items[id.0]
    }
}
