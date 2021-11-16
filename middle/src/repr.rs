use super::*;
use hashbrown::hash_map::Entry;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct ReprId(usize);

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Prim {
    Nat,
    Int,
    Char,
    Bool,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Repr {
    Prim(Prim),
    List(ReprId),
    Tuple(Vec<ReprId>),
}

#[derive(Default)]
pub struct Reprs {
    reprs: Vec<Repr>,
    lut: HashMap<Repr, ReprId>,
}

impl Reprs {
    pub fn get(&self, repr: ReprId) -> &Repr {
        &self.reprs[repr.0]
    }

    // Always performs deduplication
    pub fn get_or_insert(&mut self, repr: Repr) -> ReprId {
        match self.lut.entry(repr.clone()) {
            Entry::Occupied(repr) => *repr.get(),
            Entry::Vacant(entry) => {
                let id = ReprId(self.reprs.len());
                self.reprs.push(repr);
                entry.insert(id);
                id
            },
        }
    }
}
