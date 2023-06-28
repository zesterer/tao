use super::*;

/// Fold constants into one-another, eagerly evaluating expressions at compile-time where possible.
#[derive(Default)]
pub struct ConstFold;

impl Pass for ConstFold {
    fn apply(&mut self, ctx: &mut Context) {
        let proc_bodies = ctx.procs
            .iter()
            .map(|(id, proc)| (id, proc.body.clone()))
            .collect::<Vec<_>>();

        for (id, mut body) in proc_bodies {
            // visit(&ctx, &mut body, &mut Vec::new());
            Evaluator { ctx, locals: Vec::new() }.eval(&mut body);
            let requires = body.required_locals(None);
            debug_assert_eq!(requires.len(), 0, "Procedure requires locals {:?}\n\nOld = {}\n\n\nNew = {}\n", requires, ctx.procs.get_mut(id).unwrap().body.print(), body.print());
            ctx.procs.get_mut(id).unwrap().body = body;
        }
    }
}

struct Evaluator<'a> {
    ctx: &'a Context,
    locals: Vec<(Local, Partial)>,
}

impl<'a> Evaluator<'a> {
    // Returns `true` if the branch *could* still match the partial value and the partial value has inhabitants. If
    // `false` is returned, there's no saying what was or wasn't added to the self.locals.
    fn extract(&mut self, binding: &mut MirNode<Binding>, partial: &Partial) -> bool {
        if let Some(name) = binding.name {
            self.locals.push((name, partial.clone()));
        }

        if !binding.has_matches(self.ctx) {
            false
        } else if let Partial::Unknown(_) = partial {
            // Extract child bindings
            let mut matches = true;
            binding.for_children_mut(|binding| {
                // TODO: This will become unsound if or patterns are ever added because it assumes that all child
                // patterns are and patterns.
                matches &= self.extract(binding, &Partial::Unknown(None));
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
                (Pat::Single(inner), partial) => self.extract(inner, partial),
                (Pat::Variant(variant, x), Partial::Sum(tag, y)) => if variant == tag {
                    self.extract(x, y)
                } else {
                    false
                },
                (Pat::Tuple(xs), Partial::Tuple(ys)) => {
                    debug_assert_eq!(xs.len(), ys.len());
                    xs
                        .iter_mut()
                        .zip(ys.iter())
                        .all(|(x, y)| self.extract(x, y))
                },
                (Pat::ListExact(xs), Partial::List(ys)) => if xs.len() != ys.len() {
                    false
                } else {
                    xs
                        .iter_mut()
                        .zip(ys.iter())
                        .all(|(x, y)| self.extract(x, y))
                },
                (Pat::ListFront(xs, tail), Partial::List(ys)) => if ys.len() < xs.len() {
                    false
                } else {
                    let could_match_tail = if let Some(tail) = tail {
                        self.extract(tail, &Partial::List(ys[xs.len()..].to_vec()))
                    } else {
                        true
                    };
                    could_match_tail && xs
                        .iter_mut()
                        .zip(ys.iter())
                        .all(|(x, y)| self.extract(x, y))
                },
                (Pat::Add(inner, n), partial) => if let Some(rhs) = partial.to_literal() {
                    let rhs = rhs.int();
                    if rhs >= *n as i64 {
                        self.extract(inner, &Partial::Int(rhs - *n as i64))
                    } else {
                        false
                    }
                } else {
                    true
                },
                (Pat::Data(a, inner), Partial::Data(b, partial)) => {
                    let a = &self.ctx.reprs.get(*a).repr;
                    let b = &self.ctx.reprs.get(*b).repr;
                    // TODO: Currently `T` is always assumed to coerce to `e ~ T` implicitly, so this is not a valid
                    // check. Really, this is a bug in the analysis stage: code that implicitly coerces in this way
                    // shouldn't be emitted to the middle-end.
                    // debug_assert_eq!(a, b, "{:?} does not match {:?}", a, b);
                    self.extract(inner, partial)
                },
                (p, v) => todo!("Pattern:\n\n{:?}\n\nPartial:\n\n{:?}\n\n", p, v),
            }
        }
    }

    fn eval(&mut self, expr: &mut Expr) -> Partial {
        let partial = match expr {
            Expr::Undefined => Partial::Never,
            Expr::Literal(litr) => return litr.to_partial(), // Return directly, no need to set
            Expr::Local(local) => match self.locals
                .iter()
                .rev()
                .find(|(name, _)| *name == *local)
                .expect("local was not found on stack")
                .1
                .clone()
            {
                // If the local is still accessible, we can just inline it directly
                Partial::Unknown(Some(local)) if self.locals.iter().find(|(name, _)| *name == local).is_some() => {
                    *expr = Expr::Local(local);
                    return Partial::Unknown(Some(local))
                },
                partial => partial,
            },
            Expr::Global(proc_id) => Partial::Unknown(None),
            Expr::Match(pred, arms) => {
                let pred = self.eval(pred);
                let mut output = Partial::Never;
                arms
                    // Remove arms that cannot possibly match
                    .drain_filter(|(binding, arm)| {
                        let old_locals = self.locals.len();
                        let cull = if self.extract(binding, &pred) {
                            let arm_output = self.eval(arm);

                            // Combine outputs together in an attempt to unify their values
                            output = std::mem::replace(&mut output, Partial::Unknown(None)).or(arm_output);

                            false
                        } else {
                            // Arm could not possibly match, cull it
                            true
                        };
                        self.locals.truncate(old_locals);

                        cull
                    })
                    .for_each(|_| {});
                output
            },
            Expr::Func(arg, body) => {
                if self.ctx.reprs.has_inhabitants(arg.meta()) {
                    self.locals.push((**arg, Partial::Unknown(Some(**arg))));
                    self.eval(body);
                    self.locals.pop();
                } else {
                    **body = Expr::Undefined;
                }
                Partial::Unknown(None) // TODO: Turn into a `Partial::Func` if no captures
            },
            Expr::Go(next, body, init) => {
                self.eval(init);

                self.locals.push((**next, Partial::Unknown(Some(**next))));
                self.eval(body);
                self.locals.pop();

                Partial::Unknown(None)
            },
            Expr::Variant(variant, inner) => Partial::Sum(*variant, Box::new(self.eval(inner))),
            Expr::Tuple(fields) => Partial::Tuple(fields
                .iter_mut()
                .map(|field| self.eval(field))
                .collect()),
            Expr::List(items) => Partial::List(items
                .iter_mut()
                .map(|item| self.eval(item))
                .collect()),
            Expr::Apply(f, arg) => {
                self.eval(f);
                self.eval(arg);

                // Lower `(fn x => y)(z)` into `let x = z in y`
                if let Expr::Func(param, body) = &mut **f {
                    *expr = Expr::Match(
                        arg.clone(),
                        vec![(MirNode::new(Binding::wildcard(**param), arg.meta().clone()), body.clone())],
                    );
                    return self.eval(expr)
                } else {
                    Partial::Unknown(None)
                }
            },
            Expr::Intrinsic(intrinsic, args) => {
                // Effect basins that get immediately propagated can be flattened
                if let Intrinsic::Propagate(_) = intrinsic
                    && let Expr::Basin(_, inner) = &*args[0]
                {
                    *expr = (**inner).clone();
                    return self.eval(expr);
                }

                let args = args
                    .iter_mut()
                    .map(|arg| self.eval(arg))
                    .collect::<Vec<_>>();
                intrinsic.eval(self.ctx, &args)
            },
            Expr::Access(tuple, field) => {
                let tuple = self.eval(tuple);
                if let Partial::Tuple(mut fields) = tuple {
                    fields.remove(*field)
                } else {
                    Partial::Unknown(None)
                }
            },
            Expr::AccessVariant(inner, variant) => {
                let inner = self.eval(inner);
                if let Partial::Sum(tag, inner) = inner {
                    assert_eq!(*variant, tag);
                    *inner
                } else {
                    Partial::Unknown(None)
                }
            },
            Expr::Data(data, inner) => {
                let inner = self.eval(inner);
                Partial::Data(*data, Box::new(inner))
            },
            Expr::AccessData(inner, data) => {
                let inner = self.eval(inner);
                if let Partial::Data(id, inner) = inner {
                    assert_eq!(*data, id);
                    *inner
                } else {
                    Partial::Unknown(None)
                }
            },
            Expr::Basin(_, inner) => {
                self.eval(inner);
                Partial::Unknown(None)
            },
            Expr::Handle { expr, handlers } => {
                self.eval(expr);

                for Handler { send, state, recv, .. } in handlers {
                    let old_len = self.locals.len();
                    self.locals.push((**send, Partial::Unknown(Some(**send))));
                    self.locals.push((**state, Partial::Unknown(Some(**state))));
                    self.eval(recv);
                    self.locals.truncate(old_len);
                }

                Partial::Unknown(None)
            },
            e => todo!("{:?}", e),
        };

        let partial = match partial {
            // If the partial output of this expression is a local found in the enclosing expression, just refer to it
            // directly.
            Partial::Unknown(local) if local
                .map_or(false, |local| self.locals
                    .iter()
                    .find(|(l, _)| *l == local)
                    .is_some()) => {
                if !expr.may_have_effect() {
                    *expr = Expr::Local(local.unwrap());
                }
                partial
            },
            partial => {
                // Otherwise, if the partial is just a literal, use the literal directly.
                if let Some(literal) = partial.to_literal() {
                    if !expr.may_have_effect() {
                        *expr = Expr::Literal(literal);
                    }
                }
                partial
            },
        };

        partial
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

        let r#bool = |x| Data(
            ctx.reprs.r#bool.expect("bool type must be known here"),
            Box::new(Sum(x as usize, Box::new(Tuple(Vec::new())))),
        );

        match self {
            Intrinsic::NegInt => op!(Int(x) => Int(-*x)),
            Intrinsic::DisplayInt => op!(Int(x) => List(x.to_string().chars().map(Const::Char).collect())),
            Intrinsic::CodepointChar => op!(Char(c) => Int(*c as i64)),
            Intrinsic::LessInt => op!(Int(x), Int(y) => r#bool(x < y)),
            Intrinsic::MoreInt => op!(Int(x), Int(y) => r#bool(x > y)),
            Intrinsic::MoreEqInt => op!(Int(x), Int(y) => r#bool(x >= y)),
            Intrinsic::AddInt => op!(Int(x), Int(y) => Int(x + y)),
            Intrinsic::SubInt => op!(Int(x), Int(y) => Int(x - y)),
            Intrinsic::MulInt => op!(Int(x), Int(y) => Int(x * y)),
            Intrinsic::EqInt => op!(Int(x), Int(y) => r#bool(x == y)),
            Intrinsic::EqChar => op!(Char(x), Char(y) => r#bool(x == y)),
            Intrinsic::Join(_) => op!(List(xs), List(ys) => List(xs.iter().chain(ys).cloned().collect())),
            Intrinsic::Print => Partial::Unknown(None),
            Intrinsic::Input => Partial::Unknown(None),
            Intrinsic::Rand => Partial::Unknown(None),
            Intrinsic::UpdateField(idx) => Partial::Unknown(None), // TODO
            Intrinsic::LenList => op!(List(xs) => Int(xs.len() as i64)),
            Intrinsic::SkipList => op!(List(xs), Int(i) => List(xs.clone().split_off((*i as usize).min(xs.len())))),
            Intrinsic::TrimList => op!(List(xs), Int(i) => List({
                let mut xs = xs.clone();
                xs.truncate(*i as usize);
                xs
            })),
            Intrinsic::Suspend(_) => Partial::Unknown(None),
            // TODO: This might become unsound if we choose to represent effect objects with `Partial`.
            Intrinsic::Propagate(_) => args[0].clone(),
            i => todo!("{:?}", i),
        }
    }
}
