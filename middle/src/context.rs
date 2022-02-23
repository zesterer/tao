use super::*;

pub struct Context {
    pub reprs: Reprs,
    pub procs: Procs,
    pub entry: Option<ProcId>,
}

impl Context {
    pub fn from_concrete(hir: &HirContext, con: &ConContext) -> Self {
        let mut this = Self {
            reprs: Reprs::default(),
            procs: Procs::default(),
            entry: None,
        };

        this.entry = Some(this.lower_def(hir, con, con.entry_def()));

        this
    }

    pub fn optimize(&mut self) {
        opt::prepare(self);

        let debug = false;

        if debug {
            println!("\nMIR before optimisation:\n\n");
            for (id, proc) in self.procs.iter() {
                println!("PROCEDURE {:?}\n\n{}\n", id, proc.body.print());
            }
        }

        for _ in 0..2 {
            opt::FlattenSingleField::default().run(self, debug);
            opt::ConstFold::default().run(self, debug);
            opt::RemoveUnusedBindings::default().run(self, debug);
        }
    }

    fn reachable_procs_from(&self, proc: ProcId, globals: &mut HashSet<ProcId>) {
        globals.insert(proc);

        let required = self.procs.get(proc).unwrap().body.required_globals();
        for req in required {
            if globals.insert(req) {
                self.reachable_procs_from(req, globals);
            }
        }
    }

    pub fn reachable_procs(&self) -> HashSet<ProcId> {
        let mut globals = HashSet::new();

        if let Some(entry) = self.entry {
            self.reachable_procs_from(entry, &mut globals);
        }

        globals
    }
}
