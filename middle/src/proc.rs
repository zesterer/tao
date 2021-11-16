use super::*;

pub type ProcId = Intern<(DefId, Vec<ReprId>)>;

pub struct Proc {
    pub body: mir::MirNode<mir::Expr>,
}

#[derive(Default)]
pub struct Procs {
    procs: HashMap<ProcId, Proc>,
}

impl Procs {
    pub fn get(&self, id: ProcId) -> &Proc {
        &self.procs[&id]
    }

    pub fn insert(&mut self, id: DefId, gen: Vec<ReprId>, proc: Proc) -> ProcId {
        let id = Intern::new((id, gen));

        assert!(self.procs.insert(id, proc).is_none(), "Proc inserted twice (type recursion?!)");

        id
    }
}
