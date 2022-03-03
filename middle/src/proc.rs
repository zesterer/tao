use super::*;

pub type ProcId = ConDefId;

pub struct Proc {
    pub body: mir::MirNode<mir::Expr>,
}

#[derive(Default)]
pub struct Procs {
    pub procs: BTreeMap<ProcId, Option<Proc>>,
}

impl Procs {
    pub fn id_of_con(&self, def: ConDefId) -> ProcId {
        def
    }

    pub fn is_declared(&self, id: ProcId) -> bool {
        self.procs.contains_key(&id)
    }

    pub fn get(&self, id: ProcId) -> Option<&Proc> {
        self.procs.get(&id).and_then(|p| p.as_ref())
    }

    pub fn get_mut(&mut self, id: ProcId) -> Option<&mut Proc> {
        self.procs.get_mut(&id).and_then(|p| p.as_mut())
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (ProcId, &mut Proc)> {
        self.procs.iter_mut().filter_map(|(id, proc)| Some((*id, proc.as_mut()?)))
    }

    pub fn iter(&self) -> impl Iterator<Item = (ProcId, &Proc)> {
        self.procs.iter().filter_map(|(id, proc)| Some((*id, proc.as_ref()?)))
    }

    pub fn declare(&mut self, id: ProcId) {
        assert!(self.procs.insert(id, None).is_none(), "Proc declared twice");
    }

    pub fn define(&mut self, id: ProcId, proc: Proc) {
        assert!(self.procs.insert(id, Some(proc)).unwrap().is_none(), "Proc defined without declaration");
    }
}
