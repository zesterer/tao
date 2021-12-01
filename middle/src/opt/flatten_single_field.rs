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
                if let Pat::Tuple(fields) = &mut binding.pat {
                    if fields.len() == 1 {
                        *binding = fields.remove(0).into_inner();
                    }
                }
            },
            |expr| {
                match expr {
                    Expr::Tuple(fields) if fields.len() == 1 => *expr = fields.remove(0).into_inner(),
                    Expr::Access(tuple, field) => if let Repr::Tuple(fields) = &tuple.meta().1 {
                        if fields.len() == 1 {
                            *expr = tuple.inner().clone();
                        }
                    } else {
                        unreachable!()
                    },
                    _ => {},
                }
            },
        );
    }
}
