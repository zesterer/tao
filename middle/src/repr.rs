use super::*;
use hashbrown::hash_map::Entry;

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
    List(Box<Repr>),
    Tuple(Vec<Repr>),
    Sum(Vec<Repr>),
    Data(DataId, Vec<Repr>),
    Func(Box<Repr>, Box<Repr>),
}

#[derive(Default)]
pub struct Reprs {
    datas: HashMap<(DataId, Vec<Repr>), Option<Repr>>,
}

impl Reprs {
    pub fn get(&self, data: DataId, params: Vec<Repr>) -> &Repr {
        self.datas
            .get(&(data, params))
            .unwrap()
            .as_ref()
            .expect("Repr declared but not defined")
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut Repr> {
        self.datas
            .values_mut()
            .filter_map(|r| r.as_mut())
    }

    pub fn declare(&mut self, data: DataId, args: Vec<Repr>) -> bool {
        match self.datas.entry((data, args)) {
            Entry::Occupied(data) => false,
            Entry::Vacant(data) => {
                data.insert(None);
                true
            }
        }
    }

    pub fn define(&mut self, data: DataId, args: Vec<Repr>, repr: Repr) {
        assert!(self.datas.insert((data, args), Some(repr)).unwrap().is_none());
    }
}
