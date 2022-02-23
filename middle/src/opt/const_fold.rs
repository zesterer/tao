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
            stack: &mut Vec<(Local, Option<Const>)>,
            proc_stack: &mut Vec<ProcId>,
        ) {
            match expr {
                Expr::Const(_) => {}, // Already constant!
                Expr::Global(proc_id, flags) => if flags.get().can_inline {
                    proc_stack.push(*proc_id);
                    *expr = mir.procs.get(*proc_id).unwrap().body.inner().clone();
                    expr.refresh_locals();
                    visit(mir, expr, &mut Vec::new(), proc_stack);
                    proc_stack.pop();
                },
                Expr::Local(local) => if let Some((_, constant)) = stack.iter().rev().find(|(name, _)| *name == *local) {
                    if let Some(constant) = constant {
                        *expr = Expr::Const(constant.clone());
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
                        x = if let Some(Expr::Const(c)) = args.next() { Some(c.clone()) } else { None };
                        y = if let Some(Expr::Const(c)) = args.next() { Some(c.clone()) } else { None };
                    }

                    // Unary intrinsics
                    (|| {
                        let x = if let Some(c) = &x { c } else { return };

                        let res = match op {
                            Intrinsic::NotBool => Const::Bool(!x.bool()),
                            _ => return,
                        };
                        *expr = Expr::Const(res);
                    })();

                    // Binary intrinsics
                    (|| {
                        let x = if let Some(c) = &x { c } else { return };
                        let y = if let Some(c) = &y { c } else { return };

                        let res = match op {
                            Intrinsic::AddNat => Const::Nat(x.nat() + y.nat()),
                            Intrinsic::AddInt => Const::Int(x.int() + y.int()),
                            Intrinsic::SubNat => Const::Int(x.nat() as i64 - y.nat() as i64),
                            Intrinsic::Join(_) => Const::List({
                                let mut xs = x.list();
                                xs.append(&mut y.list());
                                xs
                            }),
                            // _ => return,
                            op => todo!("{:?}", op),
                        };
                        *expr = Expr::Const(res);
                    })();
                },
                Expr::Match(pred, arms) => {
                    visit(mir, pred, stack, proc_stack);

                    // TODO: Should this allow space for something to 'maybe match'? Currently, all patterns must be
                    // constant but this might not always be the case.
                    fn matches(binding: &Binding, constant: &Const) -> bool {
                        match &binding.pat {
                            Pat::Wildcard => true,
                            Pat::Const(x) => x == constant,
                            Pat::Single(inner) => matches(inner, constant),
                            Pat::Add(lhs, rhs) => if let Const::Nat(x) = constant {
                                if *x >= *rhs {
                                    matches(lhs, &Const::Nat(*x - *rhs))
                                } else {
                                    false
                                }
                            } else {
                                unreachable!("Pat::Add must be matching a Nat")
                            },
                            Pat::Tuple(fields) => if let Const::Tuple(const_fields) = constant {
                                fields
                                    .iter()
                                    .zip(const_fields.iter())
                                    .all(|(a, b)| matches(a, b))
                            } else {
                                unreachable!();
                            },
                            Pat::ListExact(items) => if let Const::List(const_items) = constant {
                                const_items.len() == items.len() && items
                                    .iter()
                                    .zip(const_items.iter())
                                    .all(|(a, b)| matches(a, b))
                            } else {
                                unreachable!();
                            },
                            Pat::ListFront(items, tail) => if let Const::List(const_items) = constant {
                                const_items.len() >= items.len() && items
                                    .iter()
                                    .zip(const_items.iter())
                                    .all(|(a, b)| matches(a, b))
                                && tail.as_ref().map_or(true, |tail| matches(tail, &Const::List(const_items[items.len()..].to_vec())))
                            } else {
                                unreachable!();
                            },
                            Pat::Variant(a_variant, a) => if let Const::Sum(b_variant, b) = constant {
                                a_variant == b_variant && matches(a, b)
                            } else {
                                unreachable!();
                            },
                            Pat::UnionVariant(a_id, a) => if let Const::Union(b_id, b) = constant {
                                a_id == b_id && matches(a, b)
                            } else {
                                unreachable!();
                            },
                        }
                    }

                    // If the input expression is constant, remove all but the matching arm
                    if let Expr::Const(pred) = &**pred {
                        for i in 0..arms.len() {
                            if matches(&arms[i].0, pred) {
                                *arms = vec![arms.remove(i)];
                                break;
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
                    if fields.iter().all(|field| matches!(&**field, Expr::Const(_))) {
                        *expr = Expr::Const(Const::Tuple(std::mem::take(fields)
                            .into_iter()
                            .map(|field| match field.into_inner() {
                                Expr::Const(c) => c,
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
                    if items.iter().all(|item| matches!(&**item, Expr::Const(_))) {
                        *expr = Expr::Const(Const::List(std::mem::take(items)
                            .into_iter()
                            .map(|item| match item.into_inner() {
                                Expr::Const(c) => c,
                                _ => unreachable!(),
                            })
                            .collect()))
                    }
                },
                Expr::Access(tuple, field) => {
                    visit(mir, tuple, stack, proc_stack);

                    if let Expr::Const(Const::Tuple(fields)) = &mut **tuple {
                        *expr = Expr::Const(fields.remove(*field));
                    }
                },
                Expr::Func(arg, body) => {
                    stack.push((*arg, None));
                    visit(mir, body, stack, proc_stack);
                    stack.pop();
                },
                Expr::Apply(f, arg) => {
                    visit(mir, f, stack, proc_stack);
                    visit(mir, arg, stack, proc_stack);

                    // TODO: Inlining
                },
                Expr::Variant(variant, inner) => {
                    visit(mir, inner, stack, proc_stack);

                    // if let Expr::Const(inner) = &mut **inner {
                    //     *expr = Expr::Const(Const::Sum(*variant, Box::new(inner.clone())));
                    // }
                },
                Expr::AccessVariant(inner, variant) => {
                    visit(mir, inner, stack, proc_stack);

                    // if let Expr::Const(Const::Sum(const_variant, inner)) = &mut **inner {
                    //     debug_assert!(const_variant == variant);
                    //     *expr = Expr::Const((**inner).clone());
                    // }
                },
                Expr::UnionVariant(id, inner) => {
                    visit(mir, inner, stack, proc_stack);

                    // if let Expr::Const(inner) = &mut **inner {
                    //     *expr = Expr::Const(Const::Sum(*variant, Box::new(inner.clone())));
                    // }
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
    fn try_extract_inner(&self, expr: Option<&Expr>, bindings: &mut Vec<(Local, Option<Const>)>) {
        if let Some(name) = self.name {
            bindings.push((name, if let Some(Expr::Const(constant)) = expr {
                Some(constant.clone())
            } else if let Pat::Const(constant) = &self.pat {
                Some(constant.clone())
            } else {
                None
            }));
        }

        // TODO: add extraction for complex patterns
        match &self.pat {
            Pat::Wildcard => {},
            Pat::Const(_) => {},
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

    fn try_extract(&self, expr: &Expr) -> Vec<(Local, Option<Const>)> {
        let mut bindings = Vec::new();
        self.try_extract_inner(Some(expr), &mut bindings);
        bindings
    }
}
