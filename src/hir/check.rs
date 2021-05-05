use crate::{
    hir::*,
    Error,
    util::{InferNode, TyNode, Span},
};

impl InferBinding {
    pub fn check(&self, solved: &SolvedTys) -> TyBinding {
        let pat = match self.pat.inner() {
            Pat::Wildcard => Pat::Wildcard,
            Pat::Literal(litr) => Pat::Literal(litr.clone()),
            Pat::Tuple(fields) => Pat::Tuple(fields
                .iter()
                .map(|field| field.check(solved))
                .collect()),
            _ => todo!(),
        };
        let binding = Binding { pat: SrcNode::new(pat, self.pat.span()), binding: self.binding.clone() };
        TyNode::new(binding, solved.get(self.ty()))
    }
}

impl InferExpr {
    pub fn check(&self, solved: &SolvedTys) -> TyExpr {
        let this = match self.inner() {
            Expr::Error => Expr::Error,
            Expr::Literal(litr) => Expr::Literal(litr.clone()),
            Expr::Local(name) => Expr::Local(*name),
            Expr::Unary(op, a) => Expr::Unary(op.clone(), a.check(solved)),
            Expr::Binary(op, a, b) => Expr::Binary(op.clone(), a.check(solved), b.check(solved)),
            Expr::Coerce(expr) => Expr::Coerce(expr.check(solved)),
            Expr::List(items) => Expr::List(items
                .iter()
                .map(|item| item.check(solved))
                .collect()),
            Expr::Tuple(fields) => Expr::Tuple(fields
                .iter()
                .map(|field| field.check(solved))
                .collect()),
            Expr::Match(pred, arms) => Expr::Match(pred.check(solved), arms
                .iter()
                .map(|arm| MatchArm { binding: arm.binding.check(solved), body: arm.body.check(solved) })
                .collect()),
            Expr::Func(param, body) => Expr::Func(param.check(solved), body.check(solved)),
            Expr::Apply(func, arg) => Expr::Apply(func.check(solved), arg.check(solved)),
            Expr::Record(fields) => Expr::Record(fields
                .iter()
                .map(|(name, (name2, field))| (*name, (name2.clone(), field.check(solved))))
                .collect()),
            Expr::Access(a, field) => Expr::Access(a.check(solved), field.clone()),
            expr => todo!("Implement {:?}", expr),
        };
        TyNode::new(this, solved.get(self.ty()))
    }
}
