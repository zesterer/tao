use super::*;

/// Turns tuples with single fields into their inner value.
#[derive(Default)]
pub struct FlattenSingleField;

impl Pass for FlattenSingleField {
    fn apply(&mut self, ctx: &mut Context) {
        ctx.visit(
            VisitOrder::First,
            |repr| {
                if let Repr::Tuple(fields) = repr {
                    if fields.len() == 1 {
                        *repr = fields.remove(0);
                    }
                }
            },
            |binding| {
                if binding.name.is_none() {
                    if let Pat::Tuple(fields) = &mut binding.pat {
                        if fields.len() == 1 {
                            *binding = fields.remove(0).into_inner();
                        }
                    }
                }
            },
            |expr| {
                if let Expr::Tuple(fields) = expr {
                    if fields.len() == 1 {
                        *expr = fields.remove(0).into_inner();
                    }
                }
            },
        );
    }
}
