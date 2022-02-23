use super::*;

/// Fold constants into one-another, eagerly evaluating expressions at compile-time where possible. Additionally,
/// globals and locals will get const-folded if possible.
#[derive(Default)]
pub struct ConstFold;

impl Pass for ConstFold {
    fn apply(&mut self, ctx: &mut Context) {
        fn visit(
            mir: &Context,
            expr: &mut Expr,
            stack: &mut Vec<(Local, Partial)>,
            proc_stack: &mut Vec<ProcId>,
        ) {
            match expr {
                Expr::Literal(_) => {}, // Already constant!
                Expr::Global(proc_id, flags) => if flags.get().can_inline {
                    proc_stack.push(*proc_id);
                    *expr = mir.procs.get(*proc_id).unwrap().body.inner().clone();
                    expr.refresh_locals();
                    visit(mir, expr, &mut Vec::new(), proc_stack);
                    proc_stack.pop();
                },
                Expr::Local(local) => if let Some((_, partial)) = stack.iter().rev().find(|(name, _)| *name == *local) {
                    if let Some(literal) = partial.to_literal() {
                        *expr = Expr::Literal(literal.clone());
                    }
                },
                Expr::Intrinsic(op, args) => {
                    for arg in args.iter_mut() {
                        visit(mir, arg, stack, proc_stack);
                    }

                    let op = op.clone();
                    let x;
                    let y;
                    {
                        let mut args = args.iter().map(|e| &**e);
                        x = if let Some(Expr::Literal(c)) = args.next() { Some(c.clone()) } else { None };
                        y = if let Some(Expr::Literal(c)) = args.next() { Some(c.clone()) } else { None };
                    }

                    // Unary intrinsics
                    (|| {
                        let x = if let Some(c) = &x { c } else { return };

                        let res = match op {
                            Intrinsic::NotBool => Literal::Bool(!x.bool()),
                            _ => return,
                        };
                        *expr = Expr::Literal(res);
                    })();

                    // Binary intrinsics
                    (|| {
                        let x = if let Some(c) = &x { c } else { return };
                        let y = if let Some(c) = &y { c } else { return };

                        let res = match op {
                            Intrinsic::AddNat => Literal::Nat(x.nat() + y.nat()),
                            Intrinsic::AddInt => Literal::Int(x.int() + y.int()),
                            Intrinsic::SubNat => Literal::Int(x.nat() as i64 - y.nat() as i64),
                            Intrinsic::Join(_) => Literal::List({
                                let mut xs = x.list();
                                xs.append(&mut y.list());
                                xs
                            }),
                            // _ => return,
                            op => todo!("{:?}", op),
                        };
                        *expr = Expr::Literal(res);
                    })();
                },
                Expr::Match(pred, arms) => {
                    visit(mir, pred, stack, proc_stack);

                    // TODO: Should this allow space for something to 'maybe match'? Currently, all patterns must be
                    // constant but this might not always be the case.
                    fn matches(binding: &Binding, constant: &Literal) -> bool {
                        match &binding.pat {
                            Pat::Wildcard => true,
                            Pat::Literal(x) => x == constant,
                            Pat::Single(inner) => matches(inner, constant),
                            Pat::Add(lhs, rhs) => if let Literal::Nat(x) = constant {
                                if *x >= *rhs {
                                    matches(lhs, &Literal::Nat(*x - *rhs))
                                } else {
                                    false
                                }
                            } else {
                                unreachable!("Pat::Add must be matching a Nat")
                            },
                            Pat::Tuple(fields) => if let Literal::Tuple(const_fields) = constant {
                                fields
                                    .iter()
                                    .zip(const_fields.iter())
                                    .all(|(a, b)| matches(a, b))
                            } else {
                                unreachable!();
                            },
                            Pat::ListExact(items) => if let Literal::List(const_items) = constant {
                                const_items.len() == items.len() && items
                                    .iter()
                                    .zip(const_items.iter())
                                    .all(|(a, b)| matches(a, b))
                            } else {
                                unreachable!();
                            },
                            Pat::ListFront(items, tail) => if let Literal::List(const_items) = constant {
                                const_items.len() >= items.len() && items
                                    .iter()
                                    .zip(const_items.iter())
                                    .all(|(a, b)| matches(a, b))
                                && tail.as_ref().map_or(true, |tail| matches(tail, &Literal::List(const_items[items.len()..].to_vec())))
                            } else {
                                unreachable!();
                            },
                            Pat::Variant(a_variant, a) => if let Literal::Sum(b_variant, b) = constant {
                                a_variant == b_variant && matches(a, b)
                            } else {
                                unreachable!();
                            },
                            Pat::UnionVariant(a_id, a) => if let Literal::Union(b_id, b) = constant {
                                a_id == b_id && matches(a, b)
                            } else {
                                unreachable!();
                            },
                        }
                    }

                    // If the input expression is constant, remove all but the first arm because it must match
                    if let Expr::Literal(pred) = &**pred {
                        if let Some(arm) = arms.get(0) {
                            if matches(&arm.0, pred) {
                                *arms = vec![arms.remove(0)];
                            }
                        }
                    }

                    arms
                        .iter_mut()
                        .for_each(|(arm, body)| {
                            let old_stack = stack.len();
                            for (name, constant) in arm.try_extract(&pred) {
                                stack.push((name, constant));
                            }

                            visit(mir, body, stack, proc_stack);

                            stack.truncate(old_stack);
                        });
                },
                Expr::Tuple(fields) => {
                    fields
                        .iter_mut()
                        .for_each(|field| visit(mir, field, stack, proc_stack));

                    // If all fields of a tuple construction are constant, turn the tuple into a constant
                    if fields.iter().all(|field| matches!(&**field, Expr::Literal(_))) {
                        *expr = Expr::Literal(Literal::Tuple(std::mem::take(fields)
                            .into_iter()
                            .map(|field| match field.into_inner() {
                                Expr::Literal(c) => c,
                                _ => unreachable!(),
                            })
                            .collect()))
                    }
                },
                Expr::List(items) => {
                    items
                        .iter_mut()
                        .for_each(|item| visit(mir, item, stack, proc_stack));

                    // If all fields of a list construction are constant, turn the tuple into a constant
                    if items.iter().all(|item| matches!(&**item, Expr::Literal(_))) {
                        *expr = Expr::Literal(Literal::List(std::mem::take(items)
                            .into_iter()
                            .map(|item| match item.into_inner() {
                                Expr::Literal(c) => c,
                                _ => unreachable!(),
                            })
                            .collect()))
                    }
                },
                Expr::Access(tuple, field) => {
                    visit(mir, tuple, stack, proc_stack);

                    if let Expr::Literal(Literal::Tuple(fields)) = &mut **tuple {
                        *expr = Expr::Literal(fields.remove(*field));
                    }
                },
                Expr::Func(arg, body) => {
                    stack.push((*arg, Partial::Unknown(())));
                    visit(mir, body, stack, proc_stack);
                    stack.pop();
                },
                Expr::Apply(f, arg) => {
                    visit(mir, f, stack, proc_stack);
                    visit(mir, arg, stack, proc_stack);

                    if let Expr::Func(param, body) = &mut **f {
                        body.inline_local(*param, arg);
                        *expr = (**body).clone();
                    }
                },
                Expr::Variant(variant, inner) => {
                    visit(mir, inner, stack, proc_stack);

                    if let Expr::Literal(inner) = &mut **inner {
                        *expr = Expr::Literal(Literal::Sum(*variant, Box::new(inner.clone())));
                    }
                },
                Expr::AccessVariant(inner, variant) => {
                    visit(mir, inner, stack, proc_stack);

                    if let Expr::Literal(Literal::Sum(const_variant, inner)) = &mut **inner {
                        debug_assert!(const_variant == variant);
                        *expr = Expr::Literal((**inner).clone());
                    }
                },
                Expr::Debug(inner) => {
                    visit(mir, inner, stack, proc_stack);
                },
            }
        }

        let proc_bodies = ctx.procs
            .iter()
            .map(|(id, proc)| (id, proc.body.clone()))
            .collect::<Vec<_>>();

        for (id, mut body) in proc_bodies {
            let mut proc_stack = vec![id];
            visit(&ctx, &mut body, &mut Vec::new(), &mut proc_stack);
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
                Partial::Unknown(())
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
