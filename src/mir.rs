use super::*;

pub enum Expr {
    Nat(u64),
}

pub struct Proc {
    body: Expr,
}

pub type ProcName = ArcIntern<(PkgId, Id<hir::Def>)>;

#[derive(Default)]
pub struct Program {
    procs: Store<Proc, ProcName>,
}

impl Program {
    fn lower_expr(&mut self, expr: &hir::Expr) -> Expr {
        match expr {
            hir::Expr::Nat(x) => Expr::Nat(*x),
            expr => todo!("{expr:?}"),
        }
    }

    pub fn lower(&mut self, build: &Build, entry: (PkgId, Id<hir::Def>)) -> Id<Proc> {
        let proc_name = ProcName::new(entry.clone());

        let proc = Proc {
            body: self.lower_expr(
                build.pkgs[&entry.0]
                    .1
                    .get()
                    .unwrap()
                    .defs
                    .get(entry.1)
                    .body
                    .as_ref()
                    .unwrap(),
            ),
        };
        self.procs.add(proc_name, proc).ok().unwrap()
    }
}
