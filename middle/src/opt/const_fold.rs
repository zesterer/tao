use super::*;

/// Fold constants into one-another, eagerly evaluating expressions at compile-time where possible. Additionally,
/// globals and locals will get const-folded if possible.
#[derive(Default)]
pub struct ConstFold {
    // Is inlining permitted?
    pub inline: bool,
}

impl ConstFold {
    // Returns `true` if the branch *could* still match the partial value and the partial value has inhabitants. If
    // `false` is returned, there's no saying what was or wasn't added to the stack.
    fn extract(&self, ctx: &Context, binding: &mut MirNode<Binding>, partial: &Partial, locals: &mut Vec<(Local, Partial)>) -> bool {
        if let Some(name) = binding.name {
            locals.push((name, partial.clone()));
        }

        if !binding.has_matches(ctx) {
            false
        } else if let Partial::Unknown(_) = partial {
            // Extract child bindings
            let mut matches = true;
            binding.for_children_mut(|binding| {
                // TODO: This will become unsound if or patterns are ever added because it assumes that all child
                // patterns are and patterns.
                matches &= self.extract(ctx, binding, &Partial::Unknown(None), locals);
            });
            matches
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
            Expr::Undefined => Partial::Never,
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
                let mut output = Partial::Never;
                arms
                    // Remove arms that cannot possibly match
                    .drain_filter(|(binding, arm)| {
                        let old_stack = stack.len();
                        let cull = if self.extract(ctx, binding, &pred, stack) {
                            let arm_output = self.eval(ctx, arm, stack);

                            // Combine outputs together in an attempt to unify their values
                            output = std::mem::replace(&mut output, Partial::Unknown(None)).or(arm_output);

                            false
                        } else {
                            // Arm could not possibly match, cull it
                            true
                        };
                        stack.truncate(old_stack);

                        cull
                    })
                    .for_each(|_| {});
                output
            },
            Expr::Func(arg, body) => {
                if ctx.reprs.has_inhabitants(arg.meta()) {
                    stack.push((**arg, Partial::Unknown(Some(**arg))));
                    self.eval(ctx, body, stack);
                    stack.pop();
                } else {
                    **body = Expr::Undefined;
                }
                Partial::Unknown(None) // TODO: Turn into a `Partial::Func` if no captures
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

                // Lower `(fn x => y)(z)` into `let x = z in y`
                if let Expr::Func(param, body) = &mut **f {
                    *expr = Expr::Match(
                        arg.clone(),
                        vec![(MirNode::new(Binding::wildcard(**param), arg.meta().clone()), body.clone())],
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
            Expr::AccessData(inner, data) => {
                let inner = self.eval(ctx, inner, stack);
                if let Partial::Data(id, inner) = inner {
                    assert_eq!(*data, id);
                    *inner
                } else {
                    Partial::Unknown(None)
                }
            },
            e => todo!("{:?}", e),
        };

        let partial = match partial {
            // If the partial output of this expression is a local found in the enclosing expression, just refer to it
            // directly.
            Partial::Unknown(local) if local
                .map_or(false, |local| stack
                    .iter()
                    .find(|(l, _)| *l == local)
                    .is_some()) => {
                *expr = Expr::Local(local.unwrap());
                partial
            },
            partial => {
                // Otherwise, if the partial is just a literal, use the literal directly.
                if let Some(literal) = partial.to_literal() {
                    *expr = Expr::Literal(literal);
                }
                partial
            },
        };

        partial
    }
}

impl Intrinsic {
    pub fn eval(&self, ctx: &Context, args: &[Partial]) -> Partial {
        use mir::Const::*;

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
            Intrinsic::NegNat => op!(Nat(x) => Int(-(*x as i64))),
            Intrinsic::AddNat => op!(Nat(x), Nat(y) => Nat(x + y)),
            Intrinsic::SubNat => op!(Nat(x), Nat(y) => Int(*x as i64 - *y as i64)),
            Intrinsic::MulNat => op!(Nat(x), Nat(y) => Nat(x * y)),
            Intrinsic::LessNat => op!(Nat(x), Nat(y) => Bool(x < y)),
            Intrinsic::MoreNat => op!(Nat(x), Nat(y) => Bool(x > y)),
            Intrinsic::MoreEqNat => op!(Nat(x), Nat(y) => Bool(x >= y)),
            Intrinsic::AddInt => op!(Int(x), Int(y) => Int(x + y)),
            Intrinsic::SubInt => op!(Int(x), Int(y) => Int(x - y)),
            Intrinsic::MulInt => op!(Int(x), Int(y) => Int(x * y)),
            Intrinsic::EqChar => op!(Char(x), Char(y) => Bool(x == y)),
            Intrinsic::Join(_) => op!(List(xs), List(ys) => List(xs.iter().chain(ys).cloned().collect())),
            Intrinsic::AndBool => op!(Bool(x), Bool(y) => Bool(*x && *y)),
            Intrinsic::Union(ty) => Partial::Union(*ty, Box::new(args[0].clone())),
            Intrinsic::Print => Partial::Unknown(None),
            Intrinsic::Input => Partial::Unknown(None),
            Intrinsic::UpdateField(idx) => Partial::Unknown(None), // TODO
            i => todo!("{:?}", i),
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
