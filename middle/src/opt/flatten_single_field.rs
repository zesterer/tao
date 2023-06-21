use super::*;

/// Turns tuples with single fields into their inner value.
#[derive(Default)]
pub struct FlattenSingleField;

#[derive(Default)]
struct TupleMask { used_field: Option<usize> }

impl TupleMask {
    fn combine_with(self, binding: &Binding) -> Result<Self, ()> {
        if binding.name.is_some() {
            // If the whole tuple is bound, we can't produce a mask
            Err(())
        } else {
            match &binding.pat {
                Pat::Tuple(fields) => Ok(Self { used_field: fields
                    .iter()
                    .enumerate()
                    .try_fold(self.used_field, |used_field, (idx, field)| if field.is_dead() {
                        Ok(used_field)
                    } else if used_field.map_or(true, |used_field| used_field == idx) {
                        Ok(Some(idx))
                    } else {
                        Err(())
                    })? }),
                Pat::Wildcard => Ok(self),
                _ => Err(()),
            }
        }
    }
}

impl Pass for FlattenSingleField {
    fn apply(&mut self, ctx: &mut Context) {
        fn visit(
            expr: &mut MirNode<Expr>,
        ) {
            expr.for_children_mut(|expr| visit(expr));

            if let Expr::Match(pred, arms) = &mut **expr
                && let Expr::Tuple(preds) = &**pred
                && let Ok(TupleMask { used_field: Some(used_field) }) = arms
                    .iter()
                    .try_fold(TupleMask::default(), |mask, (binding, _)| mask.combine_with(binding))
                && preds.iter().enumerate().all(|(idx, p)| idx == used_field || !p.may_have_effect())
            {
                *pred = preds[used_field].clone();
                arms.iter_mut().for_each(|(binding, _)| *binding = match &binding.pat {
                    Pat::Tuple(fields) => fields[used_field].clone(),
                    Pat::Wildcard => MirNode::new(Binding::wildcard(None), pred.meta().clone()),
                    pat => panic!("Pattern was supposed to fit mask but got {pat:?}"),
                });
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
