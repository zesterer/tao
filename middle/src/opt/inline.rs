use super::*;

/// Inline procedures where appropriate.
#[derive(Default)]
pub struct Inline;

impl Pass for Inline {
    fn apply(&mut self, ctx: &mut Context) {
        let proc_bodies = ctx.procs
            .iter()
            .map(|(id, proc)| (id, proc.body.clone()))
            .collect::<Vec<_>>();

        for (id, mut body) in proc_bodies {
            Inliner::run(ctx, &mut body);
            ctx.procs.get_mut(id).unwrap().body = body;
        }
    }
}

struct Inliner<'a> {
    ctx: &'a Context,
    calls: HashMap<ProcId, usize>,
}

impl<'a> Inliner<'a> {
    fn collect_calls(&mut self, expr: &Expr) {
        if let Expr::Global(proc_id) = expr {
            // Record call
            *self.calls.entry(*proc_id).or_default() += 1;
        }

        expr.for_children(|expr| self.collect_calls(expr));
    }

    fn do_inlining(&mut self, expr: &mut Expr) {
        if let Expr::Global(proc_id) = expr
            && let Some(calls) = self.calls.get_mut(proc_id)
            && *calls == 1 // Don't inline calls if there are >= 2 call sites in the current proc
            && let Some(proc) = self.ctx.procs.get(*proc_id)
            && !proc.is_recursive // Don't inline recursive calls
        {
            *calls -= 1; // There's now one less call!

            *expr = proc.body.inner().clone();
            expr.refresh_locals();

            // Record the number of calls in the now-inlined expression
            self.collect_calls(expr);
        }

        expr.for_children_mut(|expr| self.do_inlining(expr));
    }

    fn run(ctx: &'a Context, expr: &mut Expr) {
        let mut this = Self { ctx, calls: HashMap::new() };
        this.collect_calls(&expr);
        this.do_inlining(expr);
    }
}
