use crate::{
    hir::*,
    Error,
    util::{InferNode, TyNode, Span},
};

impl InferBinding {
    pub fn check(&self, ctx: &mut InferCtx) -> TyBinding {
        let pat = match self.pat.inner() {
            Pat::Wildcard => Pat::Wildcard,
            Pat::Literal(litr) => Pat::Literal(litr.clone()),
            _ => todo!(),
        };
        let binding = Binding { pat: SrcNode::new(pat, self.pat.span()), binding: self.binding.clone() };
        TyNode::new(binding, ctx.reconstruct(self.ty(), self.span()))
    }
}

impl InferExpr {
    pub fn check(&self, ctx: &mut InferCtx) -> TyExpr {
        let this = match self.inner() {
            Expr::Error => Expr::Error,
            Expr::Literal(litr) => Expr::Literal(litr.clone()),
            Expr::Local(name) => Expr::Local(*name),
            Expr::List(items) => Expr::List(items
                .iter()
                .map(|item| item.check(ctx))
                .collect()),
            Expr::Tuple(fields) => Expr::Tuple(fields
                .iter()
                .map(|item| item.check(ctx))
                .collect()),
            Expr::Match(pred, arms) => Expr::Match(pred.check(ctx), arms
                .iter()
                .map(|arm| MatchArm { binding: arm.binding.check(ctx), body: arm.body.check(ctx) })
                .collect()),
            expr => todo!("Implement {:?}", expr),
        };
        TyNode::new(this, ctx.reconstruct(self.ty(), self.span()))
    }
}
