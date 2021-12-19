use super::*;
use std::any::{Any, type_name};

mod const_fold;
mod flatten_single_field;
mod remove_unused_bindings;

pub use {
    const_fold::ConstFold,
    flatten_single_field::FlattenSingleField,
    remove_unused_bindings::RemoveUnusedBindings,
};

pub trait Pass: Any {
    fn apply(&mut self, ctx: &mut Context);

    fn run(&mut self, ctx: &mut Context) {
        self.apply(ctx);
        opt::check(ctx);
        // println!("\nMIR after {}:\n\n{}", type_name::<Self>(), ctx.procs.get(ctx.entry.unwrap()).unwrap().body.print());
    }
}

#[derive(Copy, Clone, PartialEq)]
pub enum VisitOrder {
    First,
    Last,
}

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
            .for_each(|r| r.visit_inner(order, repr, binding, expr));
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
            Repr::Data(_, params) => params
                .iter_mut()
                .for_each(|param| param.visit_inner(order, repr, binding, expr)),
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

impl Binding {
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
            mir::Pat::Wildcard | mir::Pat::Const(_) => {},
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
        }

        if order == VisitOrder::Last {
            binding(self);
        }
    }
}

impl Expr {
    pub fn for_children(&self, mut f: impl FnMut(&MirNode<Self>)) {
        match self {
            Expr::Const(_) | Expr::Local(_) | Expr::Global(_, _) => {},
            Expr::Intrinsic(_, args) => args
                .iter()
                .for_each(|arg| f(arg)),
            Expr::Tuple(fields) => fields
                .iter()
                .for_each(|field| f(field)),
            Expr::List(items) => items
                .iter()
                .for_each(|item| f(item)),
            Expr::Match(pred, arms) => {
                f(pred);
                for (_, body) in arms {
                    f(body);
                }
            },
            Expr::Func(_, _, body) => {
                f(body);
            },
            Expr::Apply(func, arg) => {
                f(func);
                f(arg);
            },
            Expr::Access(record, _) => f(record),
            Expr::Variant(_, inner) => f(inner),
            Expr::AccessVariant(inner, _) => f(inner),
            Expr::Debug(inner) => f(inner),
        }
    }

    pub fn for_children_mut(&mut self, mut f: impl FnMut(&mut MirNode<Self>)) {
        match self {
            Expr::Const(_) | Expr::Local(_) | Expr::Global(_, _) => {},
            Expr::Intrinsic(_, args) => args
                .iter_mut()
                .for_each(|arg| f(arg)),
            Expr::Tuple(fields) => fields
                .iter_mut()
                .for_each(|field| f(field)),
            Expr::List(items) => items
                .iter_mut()
                .for_each(|item| f(item)),
            Expr::Match(pred, arms) => {
                f(pred);
                for (_, body) in arms {
                    f(body);
                }
            },
            Expr::Func(_, _, body) => {
                f(body);
            },
            Expr::Apply(func, arg) => {
                f(func);
                f(arg);
            },
            Expr::Access(record, _) => f(record),
            Expr::Variant(_, inner) => f(inner),
            Expr::AccessVariant(inner, _) => f(inner),
            Expr::Debug(inner) => f(inner),
        }
    }

    pub fn inline_local(&mut self, name: Ident, local_expr: &Self) {
        match self {
            Expr::Local(local) if *local == name => *self = local_expr.clone(),
            Expr::Match(pred, arms) => {
                pred.inline_local(name, local_expr);
                for (arm, body) in arms {
                    if !arm.binding_names().contains(&name) {
                        body.inline_local(name, local_expr);
                    }
                }
            },
            Expr::Func(captures, arg, body) => {
                if *arg != name {
                    body.inline_local(name, local_expr);
                }
            },
            _ => self.for_children_mut(|expr| expr.inline_local(name, local_expr)),
        }
    }

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
            Expr::Const(_) | Expr::Local(_) | Expr::Global(_, _) => {},
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
            Expr::Func(_, _, body) => {
                body.visit_inner(order, repr, binding, expr);
            },
            Expr::Apply(f, arg) => {
                f.visit_inner(order, repr, binding, expr);
                arg.visit_inner(order, repr, binding, expr);
            },
            Expr::Access(record, _) => record.visit_inner(order, repr, binding, expr),
            Expr::Variant(_, inner) => inner.visit_inner(order, repr, binding, expr),
            Expr::AccessVariant(inner, _) => inner.visit_inner(order, repr, binding, expr),
            Expr::Debug(inner) => inner.visit_inner(order, repr, binding, expr),
        }

        if order == VisitOrder::Last {
            expr(self);
        }
    }
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
    fn check_binding(ctx: &Context, binding: &Binding, repr: &Repr, stack: &mut Vec<(Ident, Repr)>) {
        match (&binding.pat, repr) {
            (Pat::Wildcard, _) => {},
            (Pat::Tuple(a), Repr::Tuple(b)) if a.len() == b.len() => {},
            (_, repr) => panic!("Inconsistency between binding\n\n {:?}\n\nand repr\n\n {:?}", binding, repr),
        }
    }

    fn check_expr(ctx: &Context, expr: &Expr, repr: &Repr, stack: &mut Vec<(Ident, Repr)>) {
        match (expr, repr) {
            (Expr::Local(local), repr) if &stack
                .iter()
                .rev()
                .find(|(name, _)| name == local)
                .unwrap_or_else(|| panic!("Failed to find local {} in scope", local)).1 == repr => {},
            (Expr::Func(_, _, _), Repr::Func(_, _)) => {},
            (Expr::Tuple(a), Repr::Tuple(b)) if a.len() == b.len() => {},
            (Expr::Match(pred, arms), repr) => {
                for (arm, body) in arms {
                    // TODO: visit binding
                    check_binding(ctx, arm.inner(), pred.meta(), stack);
                }
            },
            // (Expr::Data(_, _, _), Repr::Func(_, _)) => {},
            (expr, repr) => panic!("Inconsistency between expression\n\n {:?}\n\nand repr\n\n {:?}", expr, repr),
        }
    }

    fn visit_expr(ctx: &Context, expr: &MirNode<Expr>, stack: &mut Vec<(Ident, Repr)>) {
        check_expr(ctx, expr.inner(), expr.meta(), stack);

        expr.for_children(|expr| visit_expr(ctx, expr, stack));
    }

    // for (id, proc) in ctx.procs.iter() {
    //     visit_expr(ctx, &proc.body, &mut Vec::new());
    // }
}