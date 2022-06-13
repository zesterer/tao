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
            stack: &mut Vec<(Local, u64)>,
            proc_stack: &mut Vec<ProcId>,
        ) {
            match expr {
                Expr::Local(local) => if let Some((_, n)) = stack.iter_mut().rev().find(|(name, _)| *name == *local) {
                    // Increment uses
                    *n += 1;
                } else {
                    panic!("Could not find local ${} in {:?}", local.0, stack);
                },
                Expr::Match(pred, arms) => {
                    visit(mir, pred, stack, proc_stack);

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

                            fn remove_unused(binding: &mut Binding, stack: &mut Vec<(Local, u64)>) {
                                if let Some(name) = binding.name {
                                    if stack.iter_mut().rev().find(|(n, _)| *n == name).unwrap().1 == 0 {
                                        binding.name = None;
                                    }
                                }

                                match &mut binding.pat {
                                    Pat::Wildcard => {},
                                    Pat::Literal(_) => {},
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
                                    Pat::Data(_, inner) => remove_unused(inner, stack),
                                }
                            }

                            remove_unused(arm, stack);

                            stack.truncate(old_stack);
                        });

                    // Visit predicate last to avoid visiting it again if the match was removed
                    visit(mir, pred, stack, proc_stack);

                    // Flatten matches with a single arm where the arm does not bind
                    if arms.len() == 1 && !arms.first().unwrap().0.binds() {
                        if !pred.may_have_effect() {
                            *expr = arms.remove(0).1.into_inner();
                        }
                    } else if arms.get(0).map_or(false, |(b, _)| matches!(&b.pat, Pat::Wildcard)) {
                        if !pred.may_have_effect() {
                            let (arm, mut body) = arms.remove(0);
                            if let Some(name) = arm.name {
                                body.inline_local(name, pred);
                            }
                            *expr = body.into_inner();
                        }
                    }
                },
                Expr::Func(arg, body) => {
                    stack.push((**arg, 0));
                    visit(mir, body, stack, proc_stack);
                    stack.pop();
                },
                Expr::Go(next, body, init) => {
                    visit(mir, init, stack, proc_stack);
                    stack.push((**next, 0));
                    visit(mir, body, stack, proc_stack);
                    stack.pop();
                },
                Expr::Handle { expr, eff: _, send, state, recv } => {
                    visit(mir, expr, stack, proc_stack);
                    let old_len = stack.len();
                    stack.push((**send, 0));
                    stack.push((**state, 0));
                    visit(mir, recv, stack, proc_stack);
                    stack.truncate(old_len);
                },
                _ => expr.for_children_mut(|expr| visit(mir, expr, stack, proc_stack)),
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
