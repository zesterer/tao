use super::*;
use std::any::{type_name, Any};

mod const_fold;
mod flatten_single_field;
mod remove_dead_proc;
mod remove_unused_bindings;

pub use {
    const_fold::ConstFold, flatten_single_field::FlattenSingleField,
    remove_dead_proc::RemoveDeadProc, remove_unused_bindings::RemoveUnusedBindings,
};

pub trait Pass: Any {
    fn apply(&mut self, ctx: &mut Context);

    fn run(&mut self, ctx: &mut Context, debug: bool) {
        self.apply(ctx);

        if debug {
            println!("\nMIR after {}:\n\n", type_name::<Self>());
            for (id, proc) in ctx.procs.iter() {
                println!("PROCEDURE {:?}\n\n{}\n", id, proc.body.print());
            }
            opt::check(ctx);
        }
    }
}

#[derive(Copy, Clone, PartialEq)]
pub enum VisitOrder {
    First,
    Last,
}

/*
impl Context {
    pub fn visit_inner(
        &mut self,
        order: VisitOrder,
        repr: &mut impl FnMut(&mut Repr),
        binding: &mut impl FnMut(&mut Binding),
        expr: &mut impl FnMut(&mut Expr),
    ) {
        self.procs
            .iter_mut()
            .for_each(|(_, proc)| proc.body.visit_inner(order, repr, binding, expr));

        self.reprs
            .iter_mut()
            .for_each(|r| r.repr.visit_inner(order, repr, binding, expr));

        self.reprs.datas
            .values_mut()
            .for_each(|r| r.as_mut().unwrap().repr.visit_inner(order, repr, binding, expr));
    }
    pub fn visit(
        &mut self,
        order: VisitOrder,
        mut repr: impl FnMut(&mut Repr),
        mut binding: impl FnMut(&mut Binding),
        mut expr: impl FnMut(&mut Expr),
    ) { self.visit_inner(order, &mut repr, &mut binding, &mut expr) }

    // pub fn visit(
    //     &mut self,
    //     mut visit_repr: impl FnMut(&Repr, &mut Context) -> Option<Repr>,
    //     mut visit_binding: impl FnMut(&Binding, &mut Context) -> Option<Repr>,
    //     mut visit_expr: impl FnMut(&Expr, &mut Context) -> Option<Repr>,
    // ) {
    //     self.visit_inner(&mut visit_repr, &mut visit_binding, &mut visit_expr
    // }
}
*/

/*
impl Repr {
    fn visit_inner(
        &mut self,
        order: VisitOrder,
        repr: &mut impl FnMut(&mut Repr),
        binding: &mut impl FnMut(&mut Binding),
        expr: &mut impl FnMut(&mut Expr),
    ) {
        if order == VisitOrder::First {
            repr(self);
        }

        match self {
            Repr::Prim(_) => {},
            Repr::List(item) => item.visit_inner(order, repr, binding, expr),
            Repr::Tuple(fields) => fields
                .iter_mut()
                .for_each(|field| field.visit_inner(order, repr, binding, expr)),
            Repr::Sum(variants) => variants
                .iter_mut()
                .for_each(|variant| variant.visit_inner(order, repr, binding, expr)),
            Repr::Data(data_id) => {},
            Repr::Func(i, o) => {
                i.visit_inner(order, repr, binding, expr);
                o.visit_inner(order, repr, binding, expr);
            },
        }

        if order == VisitOrder::Last {
            repr(self);
        }
    }
}
*/

impl Binding {
    pub fn for_children(&self, mut f: impl FnMut(&MirNode<Self>)) {
        match &self.pat {
            mir::Pat::Wildcard | mir::Pat::Literal(_) => {}
            mir::Pat::Single(inner) => f(inner),
            mir::Pat::Add(lhs, _) => f(lhs),
            mir::Pat::Tuple(fields) => fields.iter().for_each(f),
            mir::Pat::ListExact(items) => items.iter().for_each(f),
            mir::Pat::ListFront(items, tail) => {
                items.iter().for_each(&mut f);
                tail.as_ref().map(f);
            }
            mir::Pat::Variant(_, inner) => f(inner),
            mir::Pat::Data(_, inner) => f(inner),
        }
    }

    pub fn for_children_mut(&mut self, mut f: impl FnMut(&mut MirNode<Self>)) {
        match &mut self.pat {
            mir::Pat::Wildcard | mir::Pat::Literal(_) => {}
            mir::Pat::Single(inner) => f(inner),
            mir::Pat::Add(lhs, _) => f(lhs),
            mir::Pat::Tuple(fields) => fields.iter_mut().for_each(f),
            mir::Pat::ListExact(items) => items.iter_mut().for_each(f),
            mir::Pat::ListFront(items, tail) => {
                items.iter_mut().for_each(&mut f);
                tail.as_mut().map(f);
            }
            mir::Pat::Variant(_, inner) => f(inner),
            mir::Pat::Data(_, inner) => f(inner),
        }
    }

    /*
    fn visit_inner(
        self: &mut MirNode<Self>,
        order: VisitOrder,
        repr: &mut impl FnMut(&mut Repr),
        binding: &mut impl FnMut(&mut Binding),
        expr: &mut impl FnMut(&mut Expr),
    ) {
        if order == VisitOrder::First {
            binding(self);
        }

        self.meta_mut().visit_inner(order, repr, binding, expr);

        match &mut self.pat {
            mir::Pat::Wildcard | mir::Pat::Literal(_) => {},
            mir::Pat::Single(inner) => inner.visit_inner(order, repr, binding, expr),
            mir::Pat::Add(lhs, _) => lhs.visit_inner(order, repr, binding, expr),
            mir::Pat::Tuple(fields) => fields
                .iter_mut()
                .for_each(|field| field.visit_inner(order, repr, binding, expr)),
            mir::Pat::ListExact(items) => items
                .iter_mut()
                .for_each(|item| item.visit_inner(order, repr, binding, expr)),
            mir::Pat::ListFront(items, tail) => {
                items
                    .iter_mut()
                    .for_each(|item| item.visit_inner(order, repr, binding, expr));
                tail.as_mut().map(|tail| tail.visit_inner(order, repr, binding, expr));
            },
            mir::Pat::Variant(_, inner) => inner.visit_inner(order, repr, binding, expr),
            mir::Pat::Data(_, inner) => inner.visit_inner(order, repr, binding, expr),
        }

        if order == VisitOrder::Last {
            binding(self);
        }
    }
    */
}

impl Expr {
    pub fn for_children(&self, mut f: impl FnMut(&MirNode<Self>)) {
        match self {
            Expr::Undefined | Expr::Literal(_) | Expr::Local(_) | Expr::Global(_, _) => {}
            Expr::Intrinsic(_, args) => args.iter().for_each(f),
            Expr::Tuple(fields) => fields.iter().for_each(f),
            Expr::List(items) => items.iter().for_each(f),
            Expr::Match(pred, arms) => {
                f(pred);
                for (_, body) in arms {
                    f(body);
                }
            }
            Expr::Func(_, body) => f(body),
            Expr::Go(_, body, init) => {
                f(body);
                f(init);
            }
            Expr::Apply(func, arg) => {
                f(func);
                f(arg);
            }
            Expr::Access(record, _) => f(record),
            Expr::Variant(_, inner) => f(inner),
            Expr::AccessVariant(inner, _) => f(inner),
            Expr::Data(_, inner) => f(inner),
            Expr::AccessData(inner, _) => f(inner),
            Expr::Basin(_, inner) => f(inner),
            Expr::Handle { expr, handlers } => {
                f(expr);
                for Handler {
                    eff: _,
                    send: _,
                    state: _,
                    recv,
                } in handlers
                {
                    f(recv);
                }
            }
        }
    }

    pub fn for_children_mut(&mut self, mut f: impl FnMut(&mut MirNode<Self>)) {
        match self {
            Expr::Undefined | Expr::Literal(_) | Expr::Local(_) | Expr::Global(_, _) => {}
            Expr::Intrinsic(_, args) => args.iter_mut().for_each(f),
            Expr::Tuple(fields) => fields.iter_mut().for_each(f),
            Expr::List(items) => items.iter_mut().for_each(f),
            Expr::Match(pred, arms) => {
                f(pred);
                for (_, body) in arms {
                    f(body);
                }
            }
            Expr::Func(_, body) => f(body),
            Expr::Go(_, body, init) => {
                f(body);
                f(init);
            }
            Expr::Apply(func, arg) => {
                f(func);
                f(arg);
            }
            Expr::Access(record, _) => f(record),
            Expr::Variant(_, inner) => f(inner),
            Expr::AccessVariant(inner, _) => f(inner),
            Expr::Data(_, inner) => f(inner),
            Expr::AccessData(inner, _) => f(inner),
            Expr::Basin(_, inner) => f(inner),
            Expr::Handle { expr, handlers } => {
                f(expr);
                for Handler {
                    eff: _,
                    send: _,
                    state: _,
                    recv,
                } in handlers
                {
                    f(recv);
                }
            }
        }
    }

    pub fn inline_local(&mut self, name: Local, local_expr: &Self) {
        match self {
            Expr::Local(local) if *local == name => *self = local_expr.clone(),
            Expr::Match(pred, arms) => {
                pred.inline_local(name, local_expr);
                for (arm, body) in arms {
                    if !arm.binding_names().contains(&name) {
                        body.inline_local(name, local_expr);
                    }
                }
            }
            Expr::Func(arg, body) => {
                if **arg != name {
                    body.inline_local(name, local_expr);
                }
            }
            Expr::Handle { expr, handlers } => {
                expr.inline_local(name, local_expr);
                for Handler {
                    eff: _,
                    send,
                    state,
                    recv,
                } in handlers
                {
                    if **send != name && **state != name {
                        recv.inline_local(name, local_expr);
                    }
                }
            }
            _ => self.for_children_mut(|expr| expr.inline_local(name, local_expr)),
        }
    }

    /*
    fn visit_inner(
        self: &mut MirNode<Self>,
        order: VisitOrder,
        repr: &mut impl FnMut(&mut Repr),
        binding: &mut impl FnMut(&mut Binding),
        expr: &mut impl FnMut(&mut Expr),
    ) {
        if order == VisitOrder::First {
            expr(self);
        }

        self.meta_mut().visit_inner(order, repr, binding, expr);

        match &mut **self {
            Expr::Undefined | Expr::Literal(_) | Expr::Local(_) | Expr::Global(_, _) => {},
            Expr::Intrinsic(_, args) => args
                .iter_mut()
                .for_each(|arg| arg.visit_inner(order, repr, binding, expr)),
            Expr::Tuple(fields) => fields
                .iter_mut()
                .for_each(|field| field.visit_inner(order, repr, binding, expr)),
            Expr::List(items) => items
                .iter_mut()
                .for_each(|item| item.visit_inner(order, repr, binding, expr)),
            Expr::Match(pred, arms) => {
                pred.visit_inner(order, repr, binding, expr);
                for (b, body) in arms {
                    b.visit_inner(order, repr, binding, expr);
                    body.visit_inner(order, repr, binding, expr);
                }
            },
            Expr::Func(_, body) => {
                body.visit_inner(order, repr, binding, expr);
            },
            Expr::Go(_, body, init) => {
                body.visit_inner(order, repr, binding, expr);
                init.visit_inner(order, repr, binding, expr);
            },
            Expr::Apply(f, arg) => {
                f.visit_inner(order, repr, binding, expr);
                arg.visit_inner(order, repr, binding, expr);
            },
            Expr::Access(record, _) => record.visit_inner(order, repr, binding, expr),
            Expr::Variant(_, inner) => inner.visit_inner(order, repr, binding, expr),
            Expr::AccessVariant(inner, _) => inner.visit_inner(order, repr, binding, expr),
            Expr::Data(_, inner) => inner.visit_inner(order, repr, binding, expr),
            Expr::AccessData(inner, _) => inner.visit_inner(order, repr, binding, expr),
        }

        if order == VisitOrder::Last {
            expr(self);
        }
    }
    */
}

/// Ready a context for optimisation, making a variety of adjustments
pub fn prepare(ctx: &mut Context) {
    fn mark_loop_breakers(ctx: &Context, expr: &Expr, proc_stack: &mut Vec<ProcId>) {
        if let Expr::Global(proc, flags) = expr {
            if proc_stack.contains(proc) {
                flags.update(|mut flags| {
                    flags.can_inline = false;
                    flags
                });
            } else {
                proc_stack.push(*proc);
                mark_loop_breakers(ctx, &ctx.procs.get(*proc).unwrap().body, proc_stack);
                proc_stack.pop();
            }
        } else {
            expr.for_children(|c| mark_loop_breakers(ctx, c, proc_stack));
        }
    }

    for (id, proc) in ctx.procs.iter() {
        mark_loop_breakers(ctx, &proc.body, &mut vec![id]);
    }
}

/// Check the self-consistency of the MIR
pub fn check(ctx: &Context) {
    fn check_binding(
        binding: &Binding,
        repr: &Repr,
    ) {
        match (&binding.pat, repr) {
            (Pat::Data(a, _inner), Repr::Data(b)) if a == b => {} // TODO: Check inner
            (Pat::Wildcard, _) => {}
            (Pat::Tuple(a), Repr::Tuple(b)) if a.len() == b.len() => {}
            (Pat::Variant(idx, inner), Repr::Sum(variants)) if *idx < variants.len() => {
                check_binding(inner, &variants[*idx])
            }
            (Pat::Single(inner), _) => check_binding(inner, repr),
            (Pat::ListExact(_), Repr::List(_)) => {}
            (Pat::ListFront(_, _), Repr::List(_)) => {}
            (_, repr) => panic!(
                "Inconsistency between binding\n\n {:?}\n\nand repr {:?}",
                binding, repr
            ),
        }

        binding.for_children(|binding| check_binding(binding, binding.meta()));
    }

    fn check_expr(ctx: &Context, expr: &Expr, repr: &Repr, stack: &mut Vec<(Local, Repr)>) {
        match (expr, repr) {
            (Expr::Data(a, _inner), Repr::Data(b)) if a == b => {} // TODO: Check inner
            // TODO: Check literals elsewhere
            (Expr::Literal(Literal::Nat(_)), Repr::Prim(Prim::Nat)) => {}
            (Expr::Literal(Literal::List(_)), Repr::List(_)) => {}
            (Expr::Literal(Literal::Tuple(_)), Repr::Tuple(_)) => {}
            (Expr::Literal(Literal::Sum(_, _)), _) => {}
            (Expr::Literal(Literal::Data(a, _)), Repr::Data(b)) if a == b => {}
            (Expr::Global(_, _), _) => {} // TODO
            (Expr::Local(local), repr)
                if &stack
                    .iter()
                    .rev()
                    .find(|(name, _)| name == local)
                    .unwrap_or_else(|| panic!("Failed to find local ${} in scope", local.0))
                    .1
                    == repr => {}
            (Expr::Func(i, body), Repr::Func(i_repr, _)) => {
                stack.push((**i, (**i_repr).clone()));
                visit_expr(ctx, body, stack);
                stack.pop();
            }
            (Expr::Go(_, _body, _init), _) => {
                // TODO: Validate return body and return type
            }
            (Expr::Apply(f, _arg), _) => {
                assert!(matches!(f.meta(), Repr::Func(_, _)));
            }
            (Expr::Tuple(a), Repr::Tuple(b)) if a.len() == b.len() => {
                expr.for_children(|expr| visit_expr(ctx, expr, stack));
            }
            (Expr::List(items), Repr::List(b)) => {
                for item in items {
                    check_expr(ctx, item, b, stack);
                }
            }
            (Expr::Match(pred, arms), _repr) => {
                for (arm, body) in arms {
                    // TODO: visit binding
                    check_binding(arm.inner(), pred.meta());
                    let old_stack = stack.len();
                    stack.append(&mut arm.bindings());
                    check_expr(ctx, body, body.meta(), stack);
                    stack.truncate(old_stack);
                }
            }
            (Expr::Variant(idx, inner), Repr::Sum(variants)) if *idx < variants.len() => {
                check_expr(ctx, inner, &variants[*idx], stack);
                expr.for_children(|expr| visit_expr(ctx, expr, stack));
            }
            (expr, Repr::Data(data)) => {
                check_expr(ctx, expr, &ctx.reprs.get(*data).repr, stack);
            }
            (Expr::Intrinsic(_, _), _) => {}                 // TODO
            (Expr::Access(_, _), _) => {}                    // TODO
            (Expr::Basin(_, inner), Repr::Effect(_, o)) => {
                check_expr(ctx, inner, o, stack);
            } // TODO
            (Expr::Handle { expr, handlers: _ }, _r) if matches!(expr.meta(), Repr::Effect(_, _)) => {
                // TODO: Revisit this, might not be correct with subtyping
                // check_expr(ctx, expr, &Repr::Effect(vec![*eff], Box::new(r.clone())), stack);
            }
            // (Expr::Data(_, _, _), Repr::Func(_, _)) => {},
            (expr, repr) => panic!(
                "Inconsistency between expression\n\n {:?}\n\nand repr {:?}",
                expr, repr
            ),
        }
    }

    fn visit_expr(ctx: &Context, expr: &MirNode<Expr>, stack: &mut Vec<(Local, Repr)>) {
        check_expr(ctx, expr.inner(), expr.meta(), stack);
    }

    for (id, proc) in ctx.procs.iter() {
        println!("Checking {:?}", id);
        println!("{}", proc.body.print());
        assert_eq!(
            proc.body.required_locals(None).len(),
            0,
            "Procedure requires locals"
        );
        visit_expr(ctx, &proc.body, &mut Vec::new());
    }
}
