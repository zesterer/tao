use super::*;

/// Fold constants into one-another, eagerly evaluating expressions at compile-time where possible. Additionally,
/// globals and locals will get const-folded if possible.
#[derive(Default)]
pub struct ConstFold;

impl Pass for ConstFold {
    fn apply(&mut self, ctx: &mut Context) {
        // Returns `true` if the branch *could* still match the partial value. If `false` is returned, there's no
        // saying what was or wasn't added to the stack.
        fn extract(ctx: &Context, binding: &mut Binding, partial: &Partial, locals: &mut Vec<(Local, Partial)>) -> bool {
            if let Some(name) = binding.name {
                locals.push((name, partial.clone()));
            }

            if let Partial::Unknown(_) = partial {
                // Extract child bindings
                binding.for_children_mut(|binding| { extract(ctx, binding, &Partial::Unknown(None), locals); });
                true // TODO: Can we be smarter?
            } else {
                match (&mut binding.pat, partial) {
                    (Pat::Wildcard, _) => true,
                    (Pat::Variant(variant, x), Partial::Sum(tag, y)) => if variant == tag {
                        extract(ctx, x, y, locals)
                    } else {
                        false
                    },
                    (Pat::Tuple(xs), Partial::Tuple(ys)) => {
                        assert_eq!(xs.len(), ys.len());
                        xs
                            .iter_mut()
                            .zip(ys.iter())
                            .all(|(x, y)| extract(ctx, x, y, locals))
                    },
                    (Pat::ListExact(xs), Partial::List(ys)) => if xs.len() != ys.len() {
                        false
                    } else {
                        xs
                            .iter_mut()
                            .zip(ys.iter())
                            .all(|(x, y)| extract(ctx, x, y, locals))
                    },
                    (Pat::ListFront(xs, tail), Partial::List(ys)) => if ys.len() < xs.len() {
                        false
                    } else {
                        let could_match_tail = if let Some(tail) = tail {
                            extract(ctx, tail, &Partial::List(ys[xs.len()..].to_vec()), locals)
                        } else {
                            true
                        };
                        could_match_tail && xs
                            .iter_mut()
                            .zip(ys.iter())
                            .all(|(x, y)| extract(ctx, x, y, locals))
                    },
                    p => todo!("{:?}", p),
                }
            }
        }

        fn eval(ctx: &Context, expr: &mut Expr, stack: &mut Vec<(Local, Partial)>) -> Partial {
            let partial = match expr {
                Expr::Literal(litr) => return litr.to_partial(), // Return directly, no need to set
                Expr::Local(local) => match stack
                    .iter()
                    .rev()
                    .find(|(name, _)| *name == *local)
                    .expect("local was not found on stack")
                    .1
                    .clone()
                {
                    // If the local is still accessible, we can just inline it directly
                    Partial::Unknown(Some(local)) if stack.iter().find(|(name, _)| *name == local).is_some() => {
                        *expr = Expr::Local(local);
                        return Partial::Unknown(Some(local))
                    },
                    partial => partial,
                },
                Expr::Global(proc_id, flags) => if flags.get().can_inline {
                    *expr = ctx.procs.get(*proc_id).unwrap().body.inner().clone();
                    expr.refresh_locals();
                    // Return directly, since we apply to itself
                    return eval(ctx, expr, &mut Vec::new())
                } else {
                    Partial::Unknown(None)
                },
                Expr::Match(pred, arms) => {
                    let pred = eval(ctx, pred, stack);
                    arms
                        .drain_filter(|(binding, arm)| {
                            let old_stack = stack.len();
                            let cull = if extract(ctx, binding, &pred, stack) {
                                eval(ctx, arm, stack);
                                false
                            } else {
                                true
                            };
                            stack.truncate(old_stack);

                            cull
                        })
                        .for_each(|_| {});
                    Partial::Unknown(None) // TODO: unify arms
                },
                Expr::Func(arg, body) => {
                    stack.push((*arg, Partial::Unknown(Some(*arg))));
                    eval(ctx, body, stack); // TODO: Turn into a `Partial::Func` if no captures
                    stack.pop();

                    Partial::Unknown(None)
                },
                Expr::Variant(variant, inner) => Partial::Sum(*variant, Box::new(eval(ctx, inner, stack))),
                Expr::Tuple(fields) => Partial::Tuple(fields
                    .iter_mut()
                    .map(|field| eval(ctx, field, stack))
                    .collect()),
                Expr::List(items) => Partial::List(items
                    .iter_mut()
                    .map(|item| eval(ctx, item, stack))
                    .collect()),
                Expr::Apply(f, arg) => {
                    eval(ctx, f, stack);
                    eval(ctx, arg, stack);

                    // Lower `(fn x => y)(z)` into `let x = z in y`
                    if let Expr::Func(param, body) = &mut **f {
                        *expr = Expr::Match(
                            arg.clone(),
                            vec![(MirNode::new(Binding::wildcard(*param), arg.meta().clone()), body.clone())],
                        );
                        return eval(ctx, expr, stack)
                    } else {
                        Partial::Unknown(None)
                    }
                },
                Expr::Intrinsic(intrinsic, args) => {
                    let args = args
                        .iter_mut()
                        .map(|arg| eval(ctx, arg, stack))
                        .collect::<Vec<_>>();
                    intrinsic.eval(ctx, &args)
                },
                Expr::Access(tuple, field) => {
                    let tuple = eval(ctx, tuple, stack);
                    if let Partial::Tuple(mut fields) = tuple {
                        fields.remove(*field)
                    } else {
                        Partial::Unknown(None)
                    }
                },
                Expr::AccessVariant(inner, variant) => {
                    let inner = eval(ctx, inner, stack);
                    if let Partial::Sum(tag, inner) = inner {
                        assert_eq!(*variant, tag);
                        *inner
                    } else {
                        Partial::Unknown(None)
                    }
                },
                e => todo!("{:?}", e),
            };

            if let Some(literal) = partial.to_literal() {
                *expr = Expr::Literal(literal);
            }

            partial
        }

        impl Intrinsic {
            pub fn eval(&self, ctx: &Context, args: &[Partial]) -> Partial {
                use {Intrinsic::*, mir::Const::*};

                macro_rules! op {
                    ($($X:ident($x:ident)),* => $O:ident($out:expr)) => {
                        {
                            let mut args = args.iter();
                            #[allow(unused_parens)]
                            match ($({ let $x = args.next().unwrap(); $x }),*) {
                                ($($X($x)),*) => $O($out),
                                _ => Unknown(None),
                            }
                        }
                    };
                }

                match self {
                    NegNat => op!(Nat(x) => Int(-(*x as i64))),
                    AddNat => op!(Nat(x), Nat(y) => Nat(x + y)),
                    MulNat => op!(Nat(x), Nat(y) => Nat(x * y)),
                    LessNat => op!(Nat(x), Nat(y) => Bool(x < y)),
                    MoreNat => op!(Nat(x), Nat(y) => Bool(x > y)),
                    MoreEqNat => op!(Nat(x), Nat(y) => Bool(x >= y)),
                    AddInt => op!(Int(x), Int(y) => Int(x + y)),
                    SubInt => op!(Int(x), Int(y) => Int(x - y)),
                    MulInt => op!(Int(x), Int(y) => Int(x * y)),
                    EqChar => op!(Char(x), Char(y) => Bool(x == y)),
                    Join(_) => op!(List(xs), List(ys) => List(xs.iter().chain(ys).cloned().collect())),
                    i => todo!("{:?}", i),
                    _ => Partial::Unknown(None),
                }
            }
        }

        let proc_bodies = ctx.procs
            .iter()
            .map(|(id, proc)| (id, proc.body.clone()))
            .collect::<Vec<_>>();

        for (id, mut body) in proc_bodies {
            // visit(&ctx, &mut body, &mut Vec::new());
            eval(&ctx, &mut body, &mut Vec::new());
            let requires = body.required_locals(None);
            debug_assert_eq!(requires.len(), 0, "Procedure requires locals {:?}\n\nOld = {}\n\n\nNew = {}\n", requires, ctx.procs.get_mut(id).unwrap().body.print(), body.print());
            ctx.procs.get_mut(id).unwrap().body = body;
        }
    }
}

impl Binding {
    fn try_extract_inner(&self, expr: Option<&Expr>, bindings: &mut Vec<(Local, Partial)>) {
        if let Some(name) = self.name {
            bindings.push((name, if let Some(Expr::Literal(literal)) = expr {
                literal.to_partial()
            } else if let Pat::Literal(literal) = &self.pat {
                literal.to_partial()
            } else {
                Partial::Unknown(None)
            }));
        }

        // TODO: add extraction for complex patterns
        match &self.pat {
            Pat::Wildcard => {},
            Pat::Literal(_) => {},
            Pat::Single(inner) => inner.try_extract_inner(None, bindings),
            Pat::Add(lhs, _) => lhs.try_extract_inner(None, bindings),
            Pat::Tuple(fields) => fields
                .iter()
                .for_each(|field| field.try_extract_inner(None, bindings)),
            Pat::ListExact(items) => items
                .iter()
                .for_each(|item| item.try_extract_inner(None, bindings)),
            Pat::ListFront(items, tail) => {
                items
                    .iter()
                    .for_each(|item| item.try_extract_inner(None, bindings));
                tail
                    .as_ref()
                    .map(|tail| tail.try_extract_inner(None, bindings));
            },
            Pat::Variant(_, inner) => inner.try_extract_inner(None, bindings),
            Pat::UnionVariant(_, inner) => inner.try_extract_inner(None, bindings),
        }
    }

    fn try_extract(&self, expr: &Expr) -> Vec<(Local, Partial)> {
        let mut bindings = Vec::new();
        self.try_extract_inner(Some(expr), &mut bindings);
        bindings
    }
}
