use super::*;

/// Commutes nested branches (i.e: inline the outer match into the arms of the inner match).
///
/// Commuting nested branches makes it much easier for constant folding to by duplicating the context to remove
/// conditionality of the input. For example:
///
/// ```ignore
/// when (when xs is
///     | [x] => bar(x)
///     \ _ => False) is
/// | True => False
/// \ False => True
/// ```
///
/// becomes
///
/// ```ignore
/// when xs is
/// | [x] => when bar(x) is
///     | True => False
///     \ False => True
/// \ _ => when False is
///     | True => False
///     \ False => True
/// ```
#[derive(Default)]
pub struct CommuteBranches;

impl Pass for CommuteBranches {
    fn apply(&mut self, ctx: &mut Context) {
        fn visit(
            expr: &mut MirNode<Expr>,
        ) {
            expr.for_children_mut(|expr| visit(expr));

            let (expr, expr_meta) = expr.as_mut();
            if let Expr::Match(pred, arms) = expr
                && let Expr::Match(inner_pred, inner_arms) = &mut (**pred).clone()
            {
                *pred = inner_pred.clone();
                *arms = std::mem::take(inner_arms)
                    .into_iter()
                    .map(|(binding, inner_arm)| {
                        let meta = expr_meta.clone();
                        (binding, MirNode::new(Expr::Match(inner_arm, arms.clone()), meta))
                    })
                    .collect();
            }
        }

        let proc_bodies = ctx.procs
            .iter()
            .map(|(id, proc)| (id, proc.body.clone()))
            .collect::<Vec<_>>();

        for (_, proc) in ctx.procs.iter_mut() {
            visit(&mut proc.body);
        }
    }
}
