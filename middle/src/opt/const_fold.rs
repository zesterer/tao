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
    // `false` is returned, there's no saying what was or wasn't added to the locals.
    fn extract(
        &self,
        ctx: &Context,
        binding: &mut MirNode<Binding>,
        partial: &Partial,
        locals: &mut Vec<(Local, Partial)>,
    ) -> bool {
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
                (Pat::Literal(litr), partial) => {
                    if let Some(rhs) = partial.to_literal() {
                        litr == &rhs
                    } else {
                        true
                    }
                }
                (Pat::Single(inner), partial) => self.extract(ctx, inner, partial, locals),
                (Pat::Variant(variant, x), Partial::Sum(tag, y)) => {
                    if variant == tag {
                        self.extract(ctx, x, y, locals)
                    } else {
                        false
                    }
                }
                (Pat::Tuple(xs), Partial::Tuple(ys)) => {
                    debug_assert_eq!(xs.len(), ys.len());
                    xs.iter_mut()
                        .zip(ys.iter())
                        .all(|(x, y)| self.extract(ctx, x, y, locals))
                }
                (Pat::ListExact(xs), Partial::List(ys)) => {
                    if xs.len() != ys.len() {
                        false
                    } else {
                        xs.iter_mut()
                            .zip(ys.iter())
                            .all(|(x, y)| self.extract(ctx, x, y, locals))
                    }
                }
                (Pat::ListFront(xs, tail), Partial::List(ys)) => {
                    if ys.len() < xs.len() {
                        false
                    } else {
                        let could_match_tail = if let Some(tail) = tail {
                            self.extract(ctx, tail, &Partial::List(ys[xs.len()..].to_vec()), locals)
                        } else {
                            true
                        };
                        could_match_tail
                            && xs
                                .iter_mut()
                                .zip(ys.iter())
                                .all(|(x, y)| self.extract(ctx, x, y, locals))
                    }
                }
                (Pat::Add(inner, n), partial) => {
                    if let Some(rhs) = partial.to_literal() {
                        let rhs = rhs.nat();
                        if rhs >= *n {
                            self.extract(ctx, inner, &Partial::Nat(rhs - *n), locals)
                        } else {
                            false
                        }
                    } else {
                        true
                    }
                }
                (Pat::Data(a, inner), Partial::Data(b, partial)) => {
                    debug_assert_eq!(a, b);
                    self.extract(ctx, inner, partial, locals)
                }
                (p, v) => todo!("Pattern:\n\n{:?}\n\nPartial:\n\n{:?}\n\n", p, v),
            }
        }
    }

    fn eval(&self, ctx: &Context, expr: &mut Expr, locals: &mut Vec<(Local, Partial)>) -> Partial {
        let partial = match expr {
            Expr::Undefined => Partial::Never,
            Expr::Literal(litr) => return litr.to_partial(), // Return directly, no need to set
            Expr::Local(local) => match locals
                .iter()
                .rev()
                .find(|(name, _)| *name == *local)
                .expect("local was not found on stack")
                .1
                .clone()
            {
                // If the local is still accessible, we can just inline it directly
                Partial::Unknown(Some(local))
                    if locals.iter().any(|(name, _)| *name == local) =>
                {
                    *expr = Expr::Local(local);
                    return Partial::Unknown(Some(local));
                }
                partial => partial,
            },
            Expr::Global(proc_id, flags) => {
                if flags.get().can_inline && self.inline {
                    *expr = ctx.procs.get(*proc_id).unwrap().body.inner().clone();
                    expr.refresh_locals();
                    // Return directly, since we apply to itself
                    return self.eval(ctx, expr, &mut Vec::new());
                } else {
                    Partial::Unknown(None)
                }
            }
            Expr::Match(pred, arms) => {
                let pred = self.eval(ctx, pred, locals);
                let mut output = Partial::Never;
                arms
                    // Remove arms that cannot possibly match
                    .drain_filter(|(binding, arm)| {
                        let old_locals = locals.len();
                        let cull = if self.extract(ctx, binding, &pred, locals) {
                            let arm_output = self.eval(ctx, arm, locals);

                            // Combine outputs together in an attempt to unify their values
                            output = std::mem::replace(&mut output, Partial::Unknown(None))
                                .or(arm_output);

                            false
                        } else {
                            // Arm could not possibly match, cull it
                            true
                        };
                        locals.truncate(old_locals);

                        cull
                    })
                    .for_each(|_| {});
                output
            }
            Expr::Func(arg, body) => {
                if ctx.reprs.has_inhabitants(arg.meta()) {
                    locals.push((**arg, Partial::Unknown(Some(**arg))));
                    self.eval(ctx, body, locals);
                    locals.pop();
                } else {
                    **body = Expr::Undefined;
                }
                Partial::Unknown(None) // TODO: Turn into a `Partial::Func` if no captures
            }
            Expr::Go(next, body, init) => {
                self.eval(ctx, init, locals);

                locals.push((**next, Partial::Unknown(Some(**next))));
                self.eval(ctx, body, locals);
                locals.pop();

                Partial::Unknown(None)
            }
            Expr::Variant(variant, inner) => {
                Partial::Sum(*variant, Box::new(self.eval(ctx, inner, locals)))
            }
            Expr::Tuple(fields) => Partial::Tuple(
                fields
                    .iter_mut()
                    .map(|field| self.eval(ctx, field, locals))
                    .collect(),
            ),
            Expr::List(items) => Partial::List(
                items
                    .iter_mut()
                    .map(|item| self.eval(ctx, item, locals))
                    .collect(),
            ),
            Expr::Apply(f, arg) => {
                self.eval(ctx, f, locals);
                self.eval(ctx, arg, locals);

                // Lower `(fn x => y)(z)` into `let x = z in y`
                if let Expr::Func(param, body) = &mut **f {
                    *expr = Expr::Match(
                        arg.clone(),
                        vec![(
                            MirNode::new(Binding::wildcard(**param), arg.meta().clone()),
                            body.clone(),
                        )],
                    );
                    return self.eval(ctx, expr, locals);
                } else {
                    Partial::Unknown(None)
                }
            }
            Expr::Intrinsic(intrinsic, args) => {
                let args = args
                    .iter_mut()
                    .map(|arg| self.eval(ctx, arg, locals))
                    .collect::<Vec<_>>();
                intrinsic.eval(ctx, &args)
            }
            Expr::Access(tuple, field) => {
                let tuple = self.eval(ctx, tuple, locals);
                if let Partial::Tuple(mut fields) = tuple {
                    fields.remove(*field)
                } else {
                    Partial::Unknown(None)
                }
            }
            Expr::AccessVariant(inner, variant) => {
                let inner = self.eval(ctx, inner, locals);
                if let Partial::Sum(tag, inner) = inner {
                    assert_eq!(*variant, tag);
                    *inner
                } else {
                    Partial::Unknown(None)
                }
            }
            Expr::Data(data, inner) => {
                let inner = self.eval(ctx, inner, locals);
                Partial::Data(*data, Box::new(inner))
            }
            Expr::AccessData(inner, data) => {
                let inner = self.eval(ctx, inner, locals);
                if let Partial::Data(id, inner) = inner {
                    assert_eq!(*data, id);
                    *inner
                } else {
                    Partial::Unknown(None)
                }
            }
            Expr::Basin(_, inner) => {
                self.eval(ctx, inner, locals);
                Partial::Unknown(None)
            }
            Expr::Handle { expr, handlers } => {
                self.eval(ctx, expr, locals);

                for Handler {
                    send, state, recv, ..
                } in handlers
                {
                    let old_len = locals.len();
                    locals.push((**send, Partial::Unknown(Some(**send))));
                    locals.push((**state, Partial::Unknown(Some(**state))));
                    self.eval(ctx, recv, locals);
                    locals.truncate(old_len);
                }

                Partial::Unknown(None)
            }
            e => todo!("{:?}", e),
        };

        match partial {
            // If the partial output of this expression is a local found in the enclosing expression, just refer to it
            // directly.
            Partial::Unknown(local)
                if local.map_or(false, |local| {
                    locals.iter().any(|(l, _)| *l == local)
                }) =>
            {
                if !expr.may_have_effect() {
                    *expr = Expr::Local(local.unwrap());
                }
                partial
            }
            partial => {
                // Otherwise, if the partial is just a literal, use the literal directly.
                if let Some(literal) = partial.to_literal() {
                    if !expr.may_have_effect() {
                        *expr = Expr::Literal(literal);
                    }
                }
                partial
            }
        }
    }
}

impl Intrinsic {
    pub fn eval(&self, ctx: &Context, args: &[Partial]) -> Partial {
        use mir::Const::{self, *};

        macro_rules! op {
            ($($X:ident($x:ident)),* => $out:expr) => {
                {
                    let mut args = args.iter();
                    #[allow(unused_parens)]
                    match ($({ let $x = args.next().unwrap(); $x }),*) {
                        ($($X($x)),*) => $out,
                        _ => Unknown(None),
                    }
                }
            };
        }

        let r#bool = |x| {
            Data(
                ctx.reprs.r#bool.expect("bool type must be known here"),
                Box::new(Sum(x as usize, Box::new(Tuple(Vec::new())))),
            )
        };

        match self {
            Intrinsic::NegNat => op!(Nat(x) => Int(-(*x as i64))),
            Intrinsic::NegInt => op!(Int(x) => Int(-*x)),
            Intrinsic::DisplayInt => {
                op!(Int(x) => List(x.to_string().chars().map(Const::Char).collect()))
            }
            Intrinsic::CodepointChar => op!(Char(c) => Nat(*c as u64)),
            Intrinsic::AddNat => op!(Nat(x), Nat(y) => Nat(x + y)),
            Intrinsic::SubNat => op!(Nat(x), Nat(y) => Int(*x as i64 - *y as i64)),
            Intrinsic::MulNat => op!(Nat(x), Nat(y) => Nat(x * y)),
            Intrinsic::LessNat => op!(Nat(x), Nat(y) => r#bool(x < y)),
            Intrinsic::MoreNat => op!(Nat(x), Nat(y) => r#bool(x > y)),
            Intrinsic::MoreEqNat => op!(Nat(x), Nat(y) => r#bool(x >= y)),
            Intrinsic::AddInt => op!(Int(x), Int(y) => Int(x + y)),
            Intrinsic::SubInt => op!(Int(x), Int(y) => Int(x - y)),
            Intrinsic::MulInt => op!(Int(x), Int(y) => Int(x * y)),
            Intrinsic::EqNat => op!(Nat(x), Nat(y) => r#bool(x == y)),
            Intrinsic::EqChar => op!(Char(x), Char(y) => r#bool(x == y)),
            Intrinsic::EqNat => op!(Nat(x), Nat(y) => r#bool(x == y)),
            Intrinsic::Join(_) => {
                op!(List(xs), List(ys) => List(xs.iter().chain(ys).cloned().collect()))
            }
            Intrinsic::Print => Partial::Unknown(None),
            Intrinsic::Input => Partial::Unknown(None),
            Intrinsic::Rand => Partial::Unknown(None),
            Intrinsic::UpdateField(_idx) => Partial::Unknown(None), // TODO
            Intrinsic::LenList => op!(List(xs) => Nat(xs.len() as u64)),
            Intrinsic::SkipList => {
                op!(List(xs), Nat(i) => List(xs.clone().split_off((*i as usize).min(xs.len()))))
            }
            Intrinsic::TrimList => op!(List(xs), Nat(i) => List({
                let mut xs = xs.clone();
                xs.truncate(*i as usize);
                xs
            })),
            Intrinsic::Suspend(_) => Partial::Unknown(None),
            Intrinsic::Propagate(_) => Partial::Unknown(None),
            i => todo!("{:?}", i),
        }
    }
}

impl Pass for ConstFold {
    fn apply(&mut self, ctx: &mut Context) {
        let proc_bodies = ctx
            .procs
            .iter()
            .map(|(id, proc)| (id, proc.body.clone()))
            .collect::<Vec<_>>();

        for (id, mut body) in proc_bodies {
            // visit(&ctx, &mut body, &mut Vec::new());
            self.eval(ctx, &mut body, &mut Vec::new());
            let requires = body.required_locals(None);
            debug_assert_eq!(
                requires.len(),
                0,
                "Procedure requires locals {:?}\n\nOld = {}\n\n\nNew = {}\n",
                requires,
                ctx.procs.get_mut(id).unwrap().body.print(),
                body.print()
            );
            ctx.procs.get_mut(id).unwrap().body = body;
        }
    }
}
