use super::*;

/// Inline procedures where appropriate.
#[derive(Default)]
pub struct Inline {
    // (total non-recursive calls, is recursive)
    calls: HashMap<ProcId, CallInfo>,
    call_stack: Vec<ProcId>,
    inline_indirectly_recursive: bool,
    inline_size_threshold: usize,
}

impl Pass for Inline {
    fn create(opt_mode: OptMode) -> Self {
        Self {
            calls: HashMap::default(),
            call_stack: Vec::new(),
            inline_indirectly_recursive: match opt_mode {
                OptMode::None => todo!(),
                OptMode::Fast => true,
                OptMode::Size => false,
            },
            inline_size_threshold: match opt_mode {
                OptMode::None => todo!(),
                OptMode::Fast => 512,
                OptMode::Size => 48,
            },
        }
    }

    fn apply(&mut self, ctx: &mut Context) {
        self.init(ctx);

        let mut proc_bodies = self.calls
            .keys()
            .map(|proc_id| (
                *proc_id,
                ctx.procs.get(*proc_id).unwrap().body.clone(),
                self.calls[proc_id],
            ))
            .collect::<Vec<_>>();
        // Perform inlining depth-first (procedures with a high depth are probably hotter)
        // Actually stable, procedure IDs are unique
        proc_bodies.sort_unstable_by_key(|(id, _, call_info)| (call_info.depth, *id));

        for (id, mut body, _) in proc_bodies.into_iter().rev() {
            let mut call_info = *self.calls.get(&id).unwrap();
            self.do_inlining(id, ctx, &mut body, &mut call_info);
            self.calls.insert(id, call_info);
            ctx.procs.get_mut(id).unwrap().body = body;
        }
    }
}

#[derive(Copy, Clone)]
struct CallInfo {
    depth: usize, // Maximum depth of the proc in the call graph
    size: usize, // Number of instructions in the proc
    indirectly_recursive: bool,
    directly_recursive: bool,
}

impl Inline {
    fn analyze_proc(&mut self, ctx: &Context, proc_id: ProcId) -> Option<CallInfo> {
        if let Some(call_info) = self.calls.get_mut(&proc_id) {
            call_info.depth = call_info.depth.max(self.call_stack.len());
            Some(*call_info) // We already calculated this proc on another branch
        } else if self.call_stack.contains(&proc_id) {
            None // Is recursive since we're already in the proc
        } else {
            let mut call_info = CallInfo {
                depth: self.call_stack.len(),
                size: 0,
                indirectly_recursive: false,
                directly_recursive: false,
            };

            // No information yet, go calculate it
            self.call_stack.push(proc_id);
            self.analyze_expr(ctx, proc_id, &ctx.procs.get(proc_id).unwrap().body, &mut call_info);
            self.call_stack.pop();

            assert!(self.calls.insert(proc_id, call_info).is_none());
            Some(call_info)
        }
    }

    fn analyze_expr(&mut self, ctx: &Context, proc_id: ProcId, expr: &Expr, call_info: &mut CallInfo) {
        if let Expr::Global(new_proc_id) = expr {
            if let Some(new_info) = self.analyze_proc(ctx, *new_proc_id) {
                call_info.indirectly_recursive |= new_info.indirectly_recursive;
            } else {
                call_info.directly_recursive = true;
            }
        }

        call_info.size += 1;

        expr.for_children(|expr| self.analyze_expr(ctx, proc_id, expr, call_info));
    }

    fn do_inlining(&mut self, proc_id: ProcId, ctx: &Context, expr: &mut Expr, call_info: &mut CallInfo) {
        if let Expr::Global(new_proc_id) = expr
            && let Some(new_call_info) = self.calls.get(new_proc_id)
            && let Some(proc) = ctx.procs.get(*new_proc_id)
            // Don't inline directly recursive functions
            && !new_call_info.directly_recursive
            && (!new_call_info.indirectly_recursive || self.inline_indirectly_recursive)
            && let new_size = (call_info.size + new_call_info.size).saturating_sub(1)
            // Only inline if the inlinee is smaller than the given threshold
            && new_size < self.inline_size_threshold
        {
            *expr = proc.body.inner().clone();
            expr.refresh_locals();
            call_info.size = new_size;
        }

        expr.for_children_mut(|expr| self.do_inlining(proc_id, ctx, expr, call_info));
    }

    fn init(&mut self, ctx: &Context) {
        self.calls.clear();

        if let Some(entry) = ctx.entry {
            self.analyze_proc(ctx, entry);
        }
    }
}
