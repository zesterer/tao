use super::*;

pub type ProcId = ConProcId;

pub struct Proc {
    pub body: mir::MirNode<mir::Expr>,
    pub is_recursive: bool,
}

#[derive(Default)]
pub struct Procs {
    pub procs: BTreeMap<ProcId, Proc>,
}

impl Procs {
    pub fn id_of_con(def: ConProcId) -> ProcId {
        def
    }

    pub fn get(&self, id: ProcId) -> Option<&Proc> {
        self.procs.get(&id)
    }

    pub fn get_mut(&mut self, id: ProcId) -> Option<&mut Proc> {
        self.procs.get_mut(&id)
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (ProcId, &mut Proc)> {
        self.procs.iter_mut().map(|(id, proc)| (*id, proc))
    }

    pub fn iter(&self) -> impl Iterator<Item = (ProcId, &Proc)> {
        self.procs.iter().map(|(id, proc)| (*id, proc))
    }

    pub fn insert(&mut self, id: ProcId, proc: Proc) {
        assert!(self.procs.insert(id, proc).is_none(), "Proc already inserted");
    }
}
