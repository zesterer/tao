use super::*;
use std::any::{Any, type_name};

mod const_fold;
mod commute_branches;
mod inline;
mod flatten_single_field;
mod remove_dead_proc;
mod remove_identity_branches;
mod remove_unused_bindings;
mod simplify_arithmetic;

pub use {
    const_fold::ConstFold,
    commute_branches::CommuteBranches,
    inline::Inline,
    flatten_single_field::FlattenSingleField,
    remove_dead_proc::RemoveDeadProc,
    remove_identity_branches::RemoveIdentityBranches,
    remove_unused_bindings::RemoveUnusedBindings,
    simplify_arithmetic::SimplifyArithmetic,
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

impl Binding {
    pub fn for_children(&self, mut f: impl FnMut(&MirNode<Self>)) {
        match &self.pat {
            mir::Pat::Wildcard | mir::Pat::Literal(_) => {},
            mir::Pat::Single(inner) => f(inner),
            mir::Pat::Add(lhs, _) => f(lhs),
            mir::Pat::Tuple(fields) => fields
                .iter()
                .for_each(|field| f(field)),
            mir::Pat::ListExact(items) => items
                .iter()
                .for_each(|item| f(item)),
            mir::Pat::ListFront(items, tail) => {
                items
                    .iter()
                    .for_each(|item| f(item));
                tail.as_ref().map(|tail| f(tail));
            },
            mir::Pat::Variant(_, inner) => f(inner),
            mir::Pat::Data(_, inner) => f(inner),
        }
    }

    pub fn for_children_mut(&mut self, mut f: impl FnMut(&mut MirNode<Self>)) {
        match &mut self.pat {
            mir::Pat::Wildcard | mir::Pat::Literal(_) => {},
            mir::Pat::Single(inner) => f(inner),
            mir::Pat::Add(lhs, _) => f(lhs),
            mir::Pat::Tuple(fields) => fields
                .iter_mut()
                .for_each(|field| f(field)),
            mir::Pat::ListExact(items) => items
                .iter_mut()
                .for_each(|item| f(item)),
            mir::Pat::ListFront(items, tail) => {
                items
                    .iter_mut()
                    .for_each(|item| f(item));
                tail.as_mut().map(|tail| f(tail));
            },
            mir::Pat::Variant(_, inner) => f(inner),
            mir::Pat::Data(_, inner) => f(inner),
        }
    }
}

impl Expr {
    pub fn for_children(&self, mut f: impl FnMut(&MirNode<Self>)) {
        match self {
            Expr::Undefined | Expr::Literal(_) | Expr::Local(_) | Expr::Global(_) => {},
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
            Expr::Func(_, body) => f(body),
            Expr::Go(_, body, init) => {
                f(body);
                f(init);
            },
            Expr::Apply(func, arg) => {
                f(func);
                f(arg);
            },
            Expr::Access(record, _) => f(record),
            Expr::Variant(_, inner) => f(inner),
            Expr::AccessVariant(inner, _) => f(inner),
            Expr::Data(_, inner) => f(inner),
            Expr::AccessData(inner, _) => f(inner),
            Expr::Basin(_, inner) => f(inner),
            Expr::Handle { expr, handlers } => {
                f(expr);
                for Handler { eff, send, state, recv } in handlers {
                    f(recv);
                }
            },
        }
    }

    pub fn for_children_mut(&mut self, mut f: impl FnMut(&mut MirNode<Self>)) {
        match self {
            Expr::Undefined | Expr::Literal(_) | Expr::Local(_) | Expr::Global(_) => {},
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
            Expr::Func(_, body) => f(body),
            Expr::Go(_, body, init) => {
                f(body);
                f(init);
            },
            Expr::Apply(func, arg) => {
                f(func);
                f(arg);
            },
            Expr::Access(record, _) => f(record),
            Expr::Variant(_, inner) => f(inner),
            Expr::AccessVariant(inner, _) => f(inner),
            Expr::Data(_, inner) => f(inner),
            Expr::AccessData(inner, _) => f(inner),
            Expr::Basin(_, inner) => f(inner),
            Expr::Handle { expr, handlers } => {
                f(expr);
                for Handler { eff, send, state, recv } in handlers {
                    f(recv);
                }
            },
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
            },
            Expr::Func(arg, body) => {
                if **arg != name {
                    body.inline_local(name, local_expr);
                }
            },
            Expr::Handle { expr, handlers } => {
                expr.inline_local(name, local_expr);
                for Handler { eff, send, state, recv } in handlers {
                    if **send != name && **state != name {
                        recv.inline_local(name, local_expr);
                    }
                }
            },
            _ => self.for_children_mut(|expr| expr.inline_local(name, local_expr)),
        }
    }
}

/// Check the self-consistency of the MIR
pub fn check(ctx: &Context) {
    fn check_binding(ctx: &Context, binding: &Binding, repr: &Repr, stack: &mut Vec<(Local, Repr)>) {
        match (&binding.pat, repr) {
            (Pat::Data(a, inner), Repr::Data(b)) if a == b => {}, // TODO: Check inner
            (Pat::Wildcard, _) => {},
            (Pat::Literal(Literal::Int(_)), Repr::Prim(Prim::Int)) => {},
            (Pat::Literal(Literal::Char(_)), Repr::Prim(Prim::Char)) => {},
            (Pat::Add(a, _), Repr::Prim(Prim::Int)) => check_binding(ctx, a, repr, stack),
            (Pat::Tuple(a), Repr::Tuple(b)) if a.len() == b.len() => a.iter().zip(b.iter()).for_each(|(a, b)| check_binding(ctx, a, b, stack)),
            (Pat::Variant(idx, inner), Repr::Sum(variants)) if *idx < variants.len() => check_binding(ctx, inner, &variants[*idx], stack),
            (Pat::Single(inner), _) => check_binding(ctx, inner, repr, stack),
            (Pat::ListExact(_), Repr::List(_)) => {},
            (Pat::ListFront(_, _), Repr::List(_)) => {},
            (_, repr) => panic!("Inconsistency between binding\n\n {:?}\n\nand repr {:?}", binding, repr),
        }

        binding.for_children(|binding| check_binding(ctx, binding, binding.meta(), stack));
    }

    fn check_expr(ctx: &Context, expr: &Expr, repr: &Repr, stack: &mut Vec<(Local, Repr)>) {
        match (expr, repr) {
            (Expr::Data(a, inner), Repr::Data(b)) if a == b => {}, // TODO: Check inner
            // TODO: Check literals elsewhere
            (Expr::Literal(Literal::Int(_)), Repr::Prim(Prim::Int)) => {},
            (Expr::Literal(Literal::List(_)), Repr::List(_)) => {},
            (Expr::Literal(Literal::Tuple(_)), Repr::Tuple(_)) => {},
            (Expr::Literal(Literal::Sum(_, _)), _) => {},
            (Expr::Literal(Literal::Data(a, _)), Repr::Data(b)) if a == b => {},
            (Expr::Global(_), _) => {}, // TODO
            (Expr::Local(local), repr) => assert_eq!(&stack
                .iter()
                .rev()
                .find(|(name, _)| name == local)
                .unwrap_or_else(|| panic!("Failed to find local ${} in scope", local.0)).1, repr, "Local ${} does not match repr {:?}", local.0, repr),
            (Expr::Func(i, body), Repr::Func(i_repr, _)) => {
                stack.push((**i, (**i_repr).clone()));
                visit_expr(ctx, body, stack);
                stack.pop();
            },
            (Expr::Go(_, body, init), _) => {
                // TODO: Validate return body and return type
            },
            (Expr::Apply(f, arg), _) => {
                assert!(matches!(f.meta(), Repr::Func(_, _)));
            },
            (Expr::Tuple(a), Repr::Tuple(b)) if a.len() == b.len() => {
                expr.for_children(|expr| visit_expr(ctx, expr, stack));
            },
            (Expr::List(items), Repr::List(b)) => {
                for item in items {
                    check_expr(ctx, item, b, stack);
                }
            },
            (Expr::Match(pred, arms), repr) => {
                for (arm, body) in arms {
                    // TODO: visit binding
                    check_binding(ctx, arm.inner(), pred.meta(), stack);
                    let old_stack = stack.len();
                    stack.append(&mut arm.bindings());
                    check_expr(ctx, body, body.meta(), stack);
                    stack.truncate(old_stack);
                }
            },
            (Expr::Variant(idx, inner), Repr::Sum(variants)) if *idx < variants.len() => {
                check_expr(ctx, inner, &variants[*idx], stack);
                expr.for_children(|expr| visit_expr(ctx, expr, stack));
            },
            (expr, Repr::Data(data)) => {
                check_expr(ctx, expr, &ctx.reprs.get(*data).repr, stack);
            },
            (Expr::Variant(idx, inner), Repr::Data(_)) => {}, // TODO
            (Expr::Intrinsic(_, _), _) => {}, // TODO
            (Expr::Access(_, _), _) => {}, // TODO
            (Expr::Basin(_, inner), Repr::Effect(_, o)) => {
                check_expr(ctx, inner, o, stack);
            }, // TODO
            (Expr::Handle { expr, handlers }, r) if matches!(expr.meta(), Repr::Effect(_, _)) => {
                // TODO: Revisit this, might not be correct with subtyping
                // check_expr(ctx, expr, &Repr::Effect(vec![*eff], Box::new(r.clone())), stack);
            },
            // (Expr::Data(_, _, _), Repr::Func(_, _)) => {},
            (expr, repr) => panic!("Inconsistency between expression\n\n {:?}\n\nand repr {:?}", expr, repr),
        }
    }

    fn visit_expr(ctx: &Context, expr: &MirNode<Expr>, stack: &mut Vec<(Local, Repr)>) {
        check_expr(ctx, expr.inner(), expr.meta(), stack);
    }

    for (id, proc) in ctx.procs.iter() {
        println!("Checking {:?}", id);
        // println!("{}", proc.body.print());
        assert_eq!(proc.body.required_locals(None).len(), 0, "Procedure requires locals");
        visit_expr(ctx, &proc.body, &mut Vec::new());
    }
}
