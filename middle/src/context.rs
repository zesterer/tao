use super::*;

use std::{fmt, str::FromStr};

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

        // Find special compiler types
        // TODO: Quite hacky
        this.reprs.r#bool = con.r#bool;

        this.entry = Some(Lowerer {
            ctx: &mut this,
            hir,
            con,
            lower_stack: Vec::new(),
        }.lower_proc(con.entry_proc()));

        this
    }

    pub fn optimize(&mut self, opt_mode: OptMode) {
        if matches!(opt_mode, OptMode::None) {
            return;
        }

        let debug_before = false;
        let debug = false;

        if debug_before || debug {
            println!("\nMIR before optimisation:\n\n");
            for (id, proc) in self.procs.iter() {
                println!("PROCEDURE {:?}\n\n{}\n", id, proc.body.print());
            }
            println!("\n======\n");
        }

        for _ in 0..3 {
            opt::Inline::default().run(self, debug);
            opt::CommuteBranches::default().run(self, debug);
            opt::FlattenSingleField::default().run(self, debug);
            opt::ConstFold::default().run(self, debug);
            opt::RemoveUnusedBindings::default().run(self, debug);
            opt::RemoveDeadProc::default().run(self, debug);
            opt::SimplifyArithmetic::default().run(self, debug);
            opt::RemoveIdentityBranches::default().run(self, debug);
        }
    }

    fn reachable_procs_from(&self, proc: ProcId, globals: &mut BTreeSet<ProcId>) {
        globals.insert(proc);

        let required = self.procs.get(proc).unwrap().body.required_globals();
        for req in required {
            if globals.insert(req) {
                self.reachable_procs_from(req, globals);
            }
        }
    }

    pub fn reachable_procs(&self) -> BTreeSet<ProcId> {
        let mut globals = BTreeSet::new();

        if let Some(entry) = self.entry {
            self.reachable_procs_from(entry, &mut globals);
        }

        globals
    }
}

#[derive(Copy, Clone, Debug)]
pub enum OptMode {
    None,
    Size,
    Fast,
}

impl Default for OptMode {
    fn default() -> Self {
        Self::None
    }
}

impl FromStr for OptMode {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, &'static str> {
        match s {
            "none" => Ok(OptMode::None),
            "fast" => Ok(OptMode::Fast),
            "size" => Ok(OptMode::Size),
            _ => Err("Optimisation mode does not exist"),
        }
    }
}

impl fmt::Display for OptMode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            OptMode::None => write!(f, "none"),
            OptMode::Fast => write!(f, "fast"),
            OptMode::Size => write!(f, "size"),
        }
    }
}
