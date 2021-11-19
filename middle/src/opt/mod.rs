use super::*;

mod const_fold;
mod flatten_single_field;
mod remove_unused_bindings;

pub use {
    const_fold::ConstFold,
    flatten_single_field::FlattenSingleField,
    remove_unused_bindings::RemoveUnusedBindings,
};

pub trait Pass {
    fn apply(&mut self, ctx: &mut Context);
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

        // TODO: Visit data types too
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
            mir::Pat::Tuple(fields) => fields
                .iter_mut()
                .for_each(|field| field.visit_inner(order, repr, binding, expr)),
        }

        if order == VisitOrder::Last {
            binding(self);
        }
    }
}

impl Expr {
    pub fn for_children_mut(&mut self, mut f: impl FnMut(&mut Self)) {
        match self {
            Expr::Const(_) | Expr::Local(_) | Expr::Global(_) => {},
            Expr::Intrinsic(_, args) => args
                .iter_mut()
                .for_each(|arg| f(arg)),
            Expr::Tuple(fields) => fields
                .iter_mut()
                .for_each(|field| f(field)),
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
            expr => todo!("{:?}", expr),
        }
    }

    pub fn inline_local(&mut self, name: Ident, local_expr: &Self) {
        match self {
            Expr::Local(local) if *local == name => *self = local_expr.clone(),
            _ => self.for_children_mut(|expr| match expr {
                Expr::Local(local) if *local == name => *expr = local_expr.clone(),
                Expr::Match(pred, arms) => {
                    pred.inline_local(name, local_expr);
                    for (arm, body) in arms {
                        if !arm.binding_names().contains(&name) {
                            body.inline_local(name, local_expr);
                        }
                    }
                },
                expr => expr.inline_local(name, local_expr),
            }),
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
            Expr::Const(_) | Expr::Local(_) | Expr::Global(_) => {},
            Expr::Intrinsic(_, args) => args
                .iter_mut()
                .for_each(|arg| arg.visit_inner(order, repr, binding, expr)),
            Expr::Tuple(fields) => fields
                .iter_mut()
                .for_each(|field| field.visit_inner(order, repr, binding, expr)),
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
            expr => todo!("{:?}", expr),
        }

        if order == VisitOrder::Last {
            expr(self);
        }
    }
}
