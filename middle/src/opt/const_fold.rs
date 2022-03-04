use super::*;

/// Fold constants into one-another, eagerly evaluating expressions at compile-time where possible. Additionally,
/// globals and locals will get const-folded if possible.
#[derive(Default)]
pub struct ConstFold {
    // Is inlining permitted?
    pub inline: bool,
}

impl ConstFold {
    // Returns `true` if the branch *could* still match the partial value. If `false` is returned, there's no
    // saying what was or wasn't added to the stack.
    fn extract(&self, ctx: &Context, binding: &mut Binding, partial: &Partial, locals: &mut Vec<(Local, Partial)>) -> bool {
        if let Some(name) = binding.name {
            locals.push((name, partial.clone()));
        }

        if let Partial::Unknown(_) = partial {
            // Extract child bindings
            binding.for_children_mut(|binding| { self.extract(ctx, binding, &Partial::Unknown(None), locals); });
            true // TODO: Can we be smarter?
        } else {
            match (&mut binding.pat, partial) {
                (Pat::Wildcard, _) => true,
                (Pat::Literal(litr), partial) => if let Some(rhs) = partial.to_literal() {
                    litr == &rhs
                } else {
                    true
                },
                (Pat::Single(inner), partial) => self.extract(ctx, inner, partial, locals),
                (Pat::Variant(variant, x), Partial::Sum(tag, y)) => if variant == tag {
                    self.extract(ctx, x, y, locals)
                } else {
                    false
                },
                (Pat::Tuple(xs), Partial::Tuple(ys)) => {
                    debug_assert_eq!(xs.len(), ys.len());
                    xs
                        .iter_mut()
                        .zip(ys.iter())
                        .all(|(x, y)| self.extract(ctx, x, y, locals))
                },
                (Pat::ListExact(xs), Partial::List(ys)) => if xs.len() != ys.len() {
                    false
                } else {
                    xs
                        .iter_mut()
                        .zip(ys.iter())
                        .all(|(x, y)| self.extract(ctx, x, y, locals))
                },
                (Pat::ListFront(xs, tail), Partial::List(ys)) => if ys.len() < xs.len() {
                    false
                } else {
                    let could_match_tail = if let Some(tail) = tail {
                        self.extract(ctx, tail, &Partial::List(ys[xs.len()..].to_vec()), locals)
                    } else {
                        true
                    };
                    could_match_tail && xs
                        .iter_mut()
                        .zip(ys.iter())
                        .all(|(x, y)| self.extract(ctx, x, y, locals))
                },
                (Pat::Add(inner, n), partial) => if let Some(rhs) = partial.to_literal() {
                    let rhs = rhs.nat();
                    if rhs >= *n {
                        self.extract(ctx, inner, &Partial::Nat(rhs - *n), locals)
                    } else {
                        false
                    }
                } else {
                    true
                },
                (Pat::Data(a, inner), Partial::Data(b, partial)) => {
                    debug_assert_eq!(a, b);
                    self.extract(ctx, inner, partial, locals)
                },
                p => todo!("{:?}", p),
            }
        }
    }

    fn eval(&self, ctx: &Context, expr: &mut Expr, stack: &mut Vec<(Local, Partial)>) -> Partial {
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
            Expr::Global(proc_id, flags) => if flags.get().can_inline && self.inline {
                *expr = ctx.procs.get(*proc_id).unwrap().body.inner().clone();
                expr.refresh_locals();
                // Return directly, since we apply to itself
                return self.eval(ctx, expr, &mut Vec::new())
            } else {
                Partial::Unknown(None)
            },
            Expr::Match(pred, arms) => {
                let pred = self.eval(ctx, pred, stack);
                arms
                    .drain_filter(|(binding, arm)| {
                        let old_stack = stack.len();
                        let cull = if self.extract(ctx, binding, &pred, stack) {
                            self.eval(ctx, arm, stack);
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
                self.eval(ctx, body, stack); // TODO: Turn into a `Partial::Func` if no captures
                stack.pop();

                Partial::Unknown(None)
            },
            Expr::Go(next, body, init) => {
                self.eval(ctx, init, stack);

                stack.push((**next, Partial::Unknown(Some(**next))));
                self.eval(ctx, body, stack);
                stack.pop();

                Partial::Unknown(None)
            },
            Expr::Variant(variant, inner) => Partial::Sum(*variant, Box::new(self.eval(ctx, inner, stack))),
            Expr::Tuple(fields) => Partial::Tuple(fields
                .iter_mut()
                .map(|field| self.eval(ctx, field, stack))
                .collect()),
            Expr::List(items) => Partial::List(items
                .iter_mut()
                .map(|item| self.eval(ctx, item, stack))
                .collect()),
            Expr::Apply(f, arg) => {
                self.eval(ctx, f, stack);
                self.eval(ctx, arg, stack);

                // if let Expr::Func(param, body) = &mut **f {
                //     body.inline_local(*param, arg);
                //     *expr = (**body).clone();
                //     return self.eval(ctx, expr, stack)
                // } else
                // Lower `(fn x => y)(z)` into `let x = z in y`
                if let Expr::Func(param, body) = &mut **f {
                    *expr = Expr::Match(
                        arg.clone(),
                        vec![(MirNode::new(Binding::wildcard(*param), arg.meta().clone()), body.clone())],
                    );
                    return self.eval(ctx, expr, stack)
                } else {
                    Partial::Unknown(None)
                }
            },
            Expr::Intrinsic(intrinsic, args) => {
                let args = args
                    .iter_mut()
                    .map(|arg| self.eval(ctx, arg, stack))
                    .collect::<Vec<_>>();
                intrinsic.eval(ctx, &args)
            },
            Expr::Access(tuple, field) => {
                let tuple = self.eval(ctx, tuple, stack);
                if let Partial::Tuple(mut fields) = tuple {
                    fields.remove(*field)
                } else {
                    Partial::Unknown(None)
                }
            },
            Expr::AccessVariant(inner, variant) => {
                let inner = self.eval(ctx, inner, stack);
                if let Partial::Sum(tag, inner) = inner {
                    assert_eq!(*variant, tag);
                    *inner
                } else {
                    Partial::Unknown(None)
                }
            },
            Expr::Data(data, inner) => {
                let inner = self.eval(ctx, inner, stack);
                Partial::Data(*data, Box::new(inner))
            },
            e => todo!("{:?}", e),
        };

        if let Some(literal) = partial.to_literal() {
            *expr = Expr::Literal(literal);
        }

        partial
    }
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
            SubNat => op!(Nat(x), Nat(y) => Int(*x as i64 - *y as i64)),
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

impl Pass for ConstFold {
    fn apply(&mut self, ctx: &mut Context) {
        let proc_bodies = ctx.procs
            .iter()
            .map(|(id, proc)| (id, proc.body.clone()))
            .collect::<Vec<_>>();

        for (id, mut body) in proc_bodies {
            // visit(&ctx, &mut body, &mut Vec::new());
            self.eval(&ctx, &mut body, &mut Vec::new());
            let requires = body.required_locals(None);
            debug_assert_eq!(requires.len(), 0, "Procedure requires locals {:?}\n\nOld = {}\n\n\nNew = {}\n", requires, ctx.procs.get_mut(id).unwrap().body.print(), body.print());
            ctx.procs.get_mut(id).unwrap().body = body;
        }
    }
}
