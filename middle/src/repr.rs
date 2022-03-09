use super::*;
use hashbrown::hash_map::Entry;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Prim {
    Nat,
    Int,
    Real,
    Char,
    Bool,
    Universe,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Repr {
    Prim(Prim),
    List(Box<Repr>),
    Tuple(Vec<Repr>),
    Sum(Vec<Repr>),
    Data(ConDataId),
    Func(Box<Repr>, Box<Repr>),
    Union(Vec<Repr>),
}

pub struct Data {
    pub is_recursive: bool,
    pub repr: Repr,
}

#[derive(Default)]
pub struct Reprs {
    pub datas: HashMap<ConDataId, Option<Data>>,
}

impl Reprs {
    pub fn get(&self, data: ConDataId) -> &Data {
        self.datas
            .get(&data)
            .unwrap()
            .as_ref()
            .expect("Data declared but not defined")
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut Data> {
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

    pub fn define(&mut self, id: ConDataId, data: Data) {
        assert!(self.datas.insert(id, Some(data)).unwrap().is_none());
    }

    pub fn has_inhabitants(&self, repr: &Repr) -> bool {
        match repr {
            Repr::Prim(_) => true,
            Repr::List(_) => true, // Empty list
            Repr::Tuple(xs) => xs
                .iter()
                .all(|x| self.has_inhabitants(x)),
            Repr::Sum(variants) => variants
                .iter()
                .any(|v| self.has_inhabitants(v)),
            Repr::Data(data) => self.has_inhabitants(&self.get(*data).repr),
            Repr::Func(_, _) => true,
            Repr::Union(variants) => variants
                .iter()
                .any(|v| self.has_inhabitants(v)),
        }
    }
}
