use super::*;

pub trait Reify: Sized {
    type Output;

    fn reify(self: InferNode<Self>, infer: &mut Checked) -> TyNode<Self::Output>;
}

impl Reify for hir::Binding<InferMeta> {
    type Output = hir::Binding<TyMeta>;

    fn reify(self: InferNode<Self>, infer: &mut Checked) -> TyNode<Self::Output> {
        let (span, ty) = *self.meta();

        let this = self.into_inner();
        TyNode::new(hir::Binding {
            pat: this.pat.map(|pat| match pat {
                hir::Pat::Error => hir::Pat::Error,
                hir::Pat::Wildcard => hir::Pat::Wildcard,
                hir::Pat::Literal(litr) => hir::Pat::Literal(litr),
                hir::Pat::Single(inner) => hir::Pat::Single(inner.reify(infer)),
                hir::Pat::Tuple(items) => hir::Pat::Tuple(items
                    .into_iter()
                    .map(|item| item.reify(infer))
                    .collect()),
                hir::Pat::Record(fields) => hir::Pat::Record(fields
                    .into_iter()
                    .map(|(name, field)| (name, field.reify(infer)))
                    .collect()),
                hir::Pat::ListExact(items) => hir::Pat::ListExact(items
                    .into_iter()
                    .map(|item| item.reify(infer))
                    .collect()),
                hir::Pat::ListFront(items, tail) => hir::Pat::ListFront(items
                    .into_iter()
                    .map(|item| item.reify(infer))
                    .collect(), tail.map(|tail| tail.reify(infer))),
                hir::Pat::Decons(data, inner) => hir::Pat::Decons(data, inner.reify(infer)),
            }),
            name: this.name,
        }, (span, infer.reify(ty)))
    }
}

impl Reify for hir::Expr<InferMeta> {
    type Output = hir::Expr<TyMeta>;

    fn reify(self: InferNode<Self>, infer: &mut Checked) -> TyNode<Self::Output> {
        let (span, ty) = *self.meta();

        let expr = match self.into_inner() {
            hir::Expr::Error => hir::Expr::Error,
            hir::Expr::Literal(litr) => hir::Expr::Literal(litr),
            hir::Expr::Local(local) => hir::Expr::Local(local),
            hir::Expr::Global(global, generic_tys) => hir::Expr::Global(global, generic_tys
                .into_iter()
                .map(|(span, ty)| (span, infer.reify(ty)))
                .collect()),
            hir::Expr::Tuple(items) => hir::Expr::Tuple(items
                .into_iter()
                .map(|item| item.reify(infer))
                .collect()),
            hir::Expr::List(items) => hir::Expr::List(items
                .into_iter()
                .map(|item| item.reify(infer))
                .collect()),
            hir::Expr::Record(fields) => hir::Expr::Record(fields
                .into_iter()
                .map(|(name, field)| (name, field.reify(infer)))
                .collect()),
            hir::Expr::Access(record, field_name) => hir::Expr::Access(record.reify(infer), field_name),
            hir::Expr::Unary(op, a) => hir::Expr::Unary(op, a.reify(infer)),
            hir::Expr::Binary(op, a, b) => hir::Expr::Binary(op, a.reify(infer), b.reify(infer)),
            hir::Expr::Match(pred, arms) => hir::Expr::Match(pred.reify(infer), arms
                .into_iter()
                .map(|(binding, arm)| (binding.reify(infer), arm.reify(infer)))
                .collect()),
            hir::Expr::Func(param, body) => hir::Expr::Func(TyNode::new(*param, (param.meta().0, infer.reify(param.meta().1))), body.reify(infer)),
            hir::Expr::Apply(f, param) => hir::Expr::Apply(f.reify(infer), param.reify(infer)),
            hir::Expr::Cons(name, a) => hir::Expr::Cons(name, a.reify(infer)),
        };

        TyNode::new(expr, (span, infer.reify(ty)))
    }
}
