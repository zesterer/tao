use super::*;

/// Remove bindings that are never used. Also, matches with a single arm that do not bind are flattened and arms that
/// follow irrefutable arms are removed.
#[derive(Default)]
pub struct RemoveUnusedBindings;

impl Pass for RemoveUnusedBindings {
    fn apply(&mut self, ctx: &mut Context) {
        fn visit(
            mir: &Context,
            expr: &mut Expr,
            stack: &mut Vec<(Ident, u64)>,
            proc_stack: &mut Vec<ProcId>,
        ) {
            match expr {
                Expr::Const(_) => {},
                Expr::Global(_, _) => {},
                Expr::Local(local) => if let Some((_, n)) = stack.iter_mut().rev().find(|(name, _)| *name == *local) {
                    // Increment uses
                    *n += 1;
                } else {
                    unreachable!()
                },
                Expr::Intrinsic(op, args) => {
                    for arg in args.iter_mut() {
                        visit(mir, arg, stack, proc_stack);
                    }
                },
                Expr::Match(pred, arms) => {
                    // Remove any arms that follow an irrefutable arm
                    for i in 0..arms.len() {
                        if !arms[i].0.is_refutable() {
                            arms.truncate(i + 1);
                            break;
                        }
                    }

                    arms
                        .iter_mut()
                        .for_each(|(arm, body)| {
                            let old_stack = stack.len();

                            stack.extend(arm.binding_names().into_iter().map(|name| (name, 0)));
                            visit(mir, body, stack, proc_stack);

                            fn remove_unused(binding: &mut Binding, stack: &mut Vec<(Ident, u64)>) {
                                if let Some(name) = binding.name {
                                    if stack.iter_mut().rev().find(|(n, _)| *n == name).unwrap().1 == 0 {
                                        binding.name = None;
                                    }
                                }

                                match &mut binding.pat {
                                    Pat::Wildcard => {},
                                    Pat::Const(_) => {},
                                    Pat::Single(inner) => remove_unused(inner, stack),
                                    Pat::Add(lhs, _) => remove_unused(lhs, stack),
                                    Pat::Tuple(fields) => fields
                                        .iter_mut()
                                        .for_each(|field| remove_unused(field, stack)),
                                    Pat::ListExact(items) => items
                                        .iter_mut()
                                        .for_each(|item| remove_unused(item, stack)),
                                    Pat::ListFront(items, tail) => {
                                        items
                                            .iter_mut()
                                            .for_each(|item| remove_unused(item, stack));
                                        tail
                                            .as_mut()
                                            .map(|tail| remove_unused(tail, stack));
                                    },
                                    Pat::Variant(_, inner) => remove_unused(inner, stack),
                                }
                            }

                            remove_unused(arm, stack);

                            stack.truncate(old_stack);
                        });

                    // Flatten matches with a single arm where the arm does not bind
                    if arms.len() == 1 && !arms.first().unwrap().0.binds() {
                        *expr = arms.remove(0).1.into_inner();
                    } else if arms.get(0).map_or(false, |(b, _)| matches!(&b.pat, Pat::Wildcard)) {
                        let (arm, mut body) = arms.remove(0);
                        if let Some(name) = arm.name {
                            body.inline_local(name, pred);
                        }
                        *expr = body.into_inner();
                    } else {
                        // Visit predicate last to avoid visiting it again if the match was removed
                        visit(mir, pred, stack, proc_stack);
                    }
                },
                Expr::Tuple(fields) => fields
                    .iter_mut()
                    .for_each(|field| visit(mir, field, stack, proc_stack)),
                Expr::List(items) => items
                    .iter_mut()
                    .for_each(|item| visit(mir, item, stack, proc_stack)),
                Expr::Access(expr, _) => visit(mir, expr, stack, proc_stack),
                Expr::Func(captures, arg, body) => {
                    let old_stack = stack.len();
                    for capture in captures.iter() {
                        stack.push((*capture, 0));
                    }

                    stack.push((*arg, 0));
                    visit(mir, body, stack, proc_stack);
                    stack.pop();

                    let mut new_captures = Vec::new();
                    for i in 0..captures.len() {
                        let (capture, uses) = stack[old_stack..][i];
                        if uses > 0 {
                            new_captures.push(capture);

                            if let Some((_, n)) = stack[..old_stack].iter_mut().rev().find(|(name, _)| *name == capture) {
                                // Increment uses
                                *n += 1;
                            } else {
                                unreachable!()
                            }
                        } else {
                            println!("Capture {} will be removed from {:?}", captures[i], body);
                        }
                    }
                    *captures = new_captures;

                    stack.truncate(old_stack);
                },
                Expr::Apply(f, arg) => {
                    visit(mir, f, stack, proc_stack);
                    visit(mir, arg, stack, proc_stack);
                },
                Expr::Variant(_, inner) => visit(mir, inner, stack, proc_stack),
                Expr::AccessVariant(inner, _) => visit(mir, inner, stack, proc_stack),
            }
        }

        let proc_bodies = ctx.procs
            .iter()
            .map(|(id, proc)| (id, proc.body.clone()))
            .collect::<Vec<_>>();

        for (id, mut body) in proc_bodies {
            let mut proc_stack = vec![id];
            visit(&ctx, &mut body, &mut Vec::new(), &mut proc_stack);
            ctx.procs.get_mut(id).unwrap().body = body;
        }
    }
}
