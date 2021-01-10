use super::{Ident};
use std::{collections::HashMap, rc::Rc};
use internment::Intern;
use crate::Error;

#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub struct SrcId(Intern<Vec<Ident>>);

pub struct Src {
    pub name: String,
    pub code: String,
}

pub trait Loader {
    fn load(&mut self, path: &[Ident]) -> Result<Src, Error>;
}

pub struct LoadCache<L: Loader> {
    cache: HashMap<SrcId, Rc<Src>>,
    loader: L,
}

impl<L: Loader> From<L> for LoadCache<L> {
    fn from(loader: L) -> Self {
        Self {
            cache: HashMap::default(),
            loader,
        }
    }
}

impl<L: Loader> LoadCache<L> {
    pub fn load(&mut self, id: SrcId) -> Result<Rc<Src>, Error> {
        if !self.cache.contains_key(&id) {
            self.cache.insert(id, Rc::new(self.loader.load(&id.0)?));
        }
        Ok(self.cache.get(&id).unwrap().clone())
    }

    pub fn load_from_path(&mut self, path: impl Iterator<Item=Ident>) -> Result<(Rc<Src>, SrcId), Error> {
        let id = SrcId(Intern::new(path.collect()));
        self.load(id).map(|src| (src, id))
    }
}
