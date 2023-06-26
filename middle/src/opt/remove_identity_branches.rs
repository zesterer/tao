use super::*;

/// Eliminate branches that emit their predicate exactly in all arms.
#[derive(Default)]
pub struct RemoveIdentityBranches;

fn is_identity_literal(binding: &Binding, litr: &Literal) -> bool {
    match (&binding.pat, litr) {
        // Since `Never` can never exist, `litr` might as well be an identity coercion of `binding`
        (_, Literal::Never) => true,
        (Pat::Literal(x), y) => x == y,
        (Pat::Tuple(xs), Literal::Tuple(ys)) if xs.len() == ys.len() => xs.iter()
            .zip(ys.iter())
            .all(|(x, y)| is_identity_literal(x, y)),
        (Pat::ListExact(xs), Literal::List(ys)) if xs.len() == ys.len() => xs.iter()
            .zip(ys.iter())
            .all(|(x, y)| is_identity_literal(x, y)),
        (Pat::Variant(x_variant, x), Literal::Sum(y_variant, y)) => x_variant == y_variant && is_identity_literal(x, y),
        (Pat::Data(x_data, x), Literal::Data(y_data, y)) => x_data == y_data && is_identity_literal(x, y),
        _ => false,
    }
}

fn is_identity(binding: &Binding, expr: &Expr) -> bool {
    match (&binding.pat, expr) {
        (_, Expr::Local(local)) if binding.name == Some(*local) => true,
        // TODO: Remember `binding.name` so that it can be used by the inner call to catch `x ~ y => x`?
        (Pat::Single(binding), y) => is_identity(binding, y),
        (Pat::Add(x, n), y) => if let Expr::Intrinsic(Intrinsic::AddInt, args) = y
            && let [lhs, rhs] = args.as_slice()
            && let (other, Expr::Literal(m)) | (Expr::Literal(m), other) = (&**lhs, &**rhs)
            && is_identity(x, other) && Literal::Int(*n as i64) == *m
        {
            true
        } else {
            false
        },
        (Pat::Tuple(xs), Expr::Tuple(ys)) if xs.len() == ys.len() => xs.iter()
            .zip(ys.iter())
            .all(|(x, y)| is_identity(x, y)),
        (Pat::ListExact(xs), Expr::List(ys)) if xs.len() == ys.len() => xs.iter()
            .zip(ys.iter())
            .all(|(x, y)| is_identity(x, y)),
        (Pat::Variant(x_variant, x), Expr::Variant(y_variant, y)) => x_variant == y_variant && is_identity(x, y),
        (Pat::Data(x_data, x), Expr::Data(y_data, y)) => x_data == y_data && is_identity(x, y),
        (_, Expr::Literal(y)) => is_identity_literal(binding, y),
        _ => false,
    }
}

impl Pass for RemoveIdentityBranches {
    fn apply(&mut self, ctx: &mut Context) {
        fn visit(
            expr: &mut MirNode<Expr>,
        ) {
            expr.for_children_mut(|expr| visit(expr));

            if let Expr::Match(pred, arms) = &mut **expr
                && arms
                    .iter()
                    .all(|(binding, arm)| is_identity(binding, arm))
            {
                **expr = (**pred).clone();
            }
        }

        let proc_bodies = ctx.procs
            .iter()
            .map(|(id, proc)| (id, proc.body.clone()))
            .collect::<Vec<_>>();

        for (_, proc) in ctx.procs.iter_mut() {
            visit(&mut proc.body);
        }
    }
}
