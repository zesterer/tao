use super::*;

#[derive(Default)]
pub struct RemoveDeadProc;

impl Pass for RemoveDeadProc {
    fn apply(&mut self, ctx: &mut Context) {
        let reachable = ctx.reachable_procs();

        ctx.procs.procs.retain(|proc, _| reachable.contains(proc));
    }
}
