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
        TyNode::new(
            hir::Binding {
                pat: this.pat.map(|pat| match pat {
                    hir::Pat::Error => hir::Pat::Error,
                    hir::Pat::Wildcard => hir::Pat::Wildcard,
                    hir::Pat::Literal(litr) => hir::Pat::Literal(litr),
                    hir::Pat::Single(inner) => hir::Pat::Single(inner.reify(infer)),
                    hir::Pat::Add(lhs, rhs) => hir::Pat::Add(lhs.reify(infer), rhs),
                    hir::Pat::Record(fields, is_tuple) => hir::Pat::Record(
                        fields
                            .into_iter()
                            .map(|(name, field)| (name, field.reify(infer)))
                            .collect(),
                        is_tuple,
                    ),
                    hir::Pat::ListExact(items) => hir::Pat::ListExact(
                        items.into_iter().map(|item| item.reify(infer)).collect(),
                    ),
                    hir::Pat::ListFront(items, tail) => hir::Pat::ListFront(
                        items.into_iter().map(|item| item.reify(infer)).collect(),
                        tail.map(|tail| tail.reify(infer)),
                    ),
                    hir::Pat::Decons(data, variant, inner) => {
                        hir::Pat::Decons(data, variant, inner.reify(infer))
                    }
                }),
                name: this.name,
            },
            (span, infer.reify(ty)),
        )
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
            hir::Expr::Global((global, gen_tys, gen_effs)) => hir::Expr::Global((
                global,
                gen_tys
                    .into_iter()
                    .map(|(span, ty)| (span, infer.reify(ty)))
                    .collect(),
                gen_effs
                    .into_iter()
                    .map(|eff| infer.reify_effect(eff))
                    .collect(),
            )),
            hir::Expr::List(items, tails) => hir::Expr::List(
                items.into_iter().map(|item| item.reify(infer)).collect(),
                tails.into_iter().map(|tail| tail.reify(infer)).collect(),
            ),
            hir::Expr::Record(fields, is_tuple) => hir::Expr::Record(
                fields
                    .into_iter()
                    .map(|(name, field)| (name, field.reify(infer)))
                    .collect(),
                is_tuple,
            ),
            hir::Expr::Access(record, field_name) => {
                hir::Expr::Access(record.reify(infer), field_name)
            }
            hir::Expr::Match(hidden_outer, pred, arms) => {
                let pred = pred.reify(infer);
                let arms = arms
                    .into_iter()
                    .map(|(binding, arm)| (binding.reify(infer), arm.reify(infer)))
                    .collect::<Vec<_>>();

                if let Err(example) =
                    exhaustivity(infer.ctx(), pred.meta().1, arms.iter().map(|(b, _)| b))
                {
                    infer
                        .ctx_mut()
                        .emit(Error::NotExhaustive(span, example, hidden_outer));
                }

                hir::Expr::Match(hidden_outer, pred, arms)
            }
            hir::Expr::Func(param, body) => hir::Expr::Func(
                TyNode::new(*param, (param.meta().0, infer.reify(param.meta().1))),
                body.reify(infer),
            ),
            hir::Expr::Apply(f, param) => hir::Expr::Apply(f.reify(infer), param.reify(infer)),
            hir::Expr::Cons(name, variant, a) => hir::Expr::Cons(name, variant, a.reify(infer)),
            hir::Expr::ClassAccess((ty_span, ty), class, field) => {
                hir::Expr::ClassAccess((ty_span, infer.reify(ty)), infer.reify_class(class), field)
            }
            hir::Expr::Intrinsic(name, args) => {
                hir::Expr::Intrinsic(name, args.into_iter().map(|arg| arg.reify(infer)).collect())
            }
            hir::Expr::Update(record, fields) => hir::Expr::Update(
                record.reify(infer),
                fields
                    .into_iter()
                    .map(|(name, field)| (name, field.reify(infer)))
                    .collect(),
            ),
            hir::Expr::Basin(eff, inner) => match infer.reify_effect(eff) {
                Some(eff) => hir::Expr::Basin(Some(eff), inner.reify(infer)),
                None => inner.reify(infer).into_inner(),
            },
            hir::Expr::Suspend(eff, inner) => {
                hir::Expr::Suspend(infer.reify_effect_inst(eff), inner.reify(infer))
            }
            hir::Expr::Handle { expr, handlers } => hir::Expr::Handle {
                expr: expr.reify(infer),
                handlers: handlers
                    .into_iter()
                    .map(
                        |hir::Handler {
                             eff,
                             send,
                             state,
                             recv,
                         }| hir::Handler {
                            eff: infer.reify_effect_inst(eff),
                            send: TyNode::new(*send, (send.meta().0, infer.reify(send.meta().1))),
                            state: state.map(|state| {
                                TyNode::new(*state, (state.meta().0, infer.reify(state.meta().1)))
                            }),
                            recv: recv.reify(infer),
                        },
                    )
                    .collect(),
            },
        };

        TyNode::new(expr, (span, infer.reify(ty)))
    }
}
