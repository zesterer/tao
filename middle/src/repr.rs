use super::*;
use hashbrown::hash_map::Entry;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Prim {
    Nat,
    Int,
    Real,
    Char,
    Bool,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Repr {
    Prim(Prim),
    List(Box<Repr>),
    Tuple(Vec<Repr>),
    Sum(Vec<Repr>),
    Data(ConDataId),
    Func(Box<Repr>, Box<Repr>),
}

#[derive(Default)]
pub struct Reprs {
    pub datas: HashMap<ConDataId, Option<Repr>>,
}

impl Reprs {
    pub fn get(&self, data: ConDataId) -> &Repr {
        self.datas
            .get(&data)
            .unwrap()
            .as_ref()
            .expect("Repr declared but not defined")
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut Repr> {
        self.datas
            .values_mut()
            .filter_map(|r| r.as_mut())
    }

    pub fn declare(&mut self, data: ConDataId) -> bool {
        match self.datas.entry(data) {
            Entry::Occupied(data) => false,
            Entry::Vacant(data) => {
                data.insert(None);
                true
            }
        }
    }

    pub fn define(&mut self, data: ConDataId, repr: Repr) {
        assert!(self.datas.insert(data, Some(repr)).unwrap().is_none());
    }
}
