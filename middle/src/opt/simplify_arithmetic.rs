use super::*;

/// Simplify arithmetic expressions where possible.
#[derive(Default)]
pub struct SimplifyArithmetic;

impl Pass for SimplifyArithmetic {
    fn apply(&mut self, ctx: &mut Context) {
        fn visit(
            expr: &mut Expr,
        ) {
            // Visit children first to maximally simplify the inner expression
            expr.for_children_mut(|expr| visit(expr));

            // Simplify `- - x` to `x`
            if let Expr::Intrinsic(Intrinsic::NegInt, args) = expr
                && let [arg] = args.as_slice()
                && let Expr::Intrinsic(Intrinsic::NegInt, args) = &**arg
                && let [arg] = args.as_slice()
            {
                *expr = (**arg).clone();
            }

            // Simplify `a + - b` to `a - b`
            if let Expr::Intrinsic(intr @ Intrinsic::AddInt, args) = expr
                && let [a, b] = args.as_mut_slice()
                && let Expr::Intrinsic(Intrinsic::NegInt, b2) = &**b
                && let [b2] = b2.as_slice()
            {
                *intr = Intrinsic::SubInt;
                *b = b2.clone();
            }

            // Simplify `a * 1` and `1 * a` to `a`
            if let Expr::Intrinsic(Intrinsic::MulInt, args) = expr
                && let [a, b] = args.as_slice()
                && let (x, Expr::Literal(Literal::Int(1)))
                    | (Expr::Literal(Literal::Int(1)), x) = (&**a, &**b)
            {
                *expr = x.clone();
            }

            // Simplify `a * 0` and `0 * a` to `a`
            if let Expr::Intrinsic(Intrinsic::MulInt, args) = expr
                && let [a, b] = args.as_slice()
                && let (x, Expr::Literal(Literal::Int(0)))
                    | (Expr::Literal(Literal::Int(0)), x) = (&**a, &**b)
            {
                *expr = Expr::Literal(Literal::Int(0));
            }

            // Simplify `a + 0` and `0 + a` to `a`
            if let Expr::Intrinsic(Intrinsic::AddInt, args) = expr
                && let [a, b] = args.as_slice()
                && let (x, Expr::Literal(Literal::Int(0)))
                    | (Expr::Literal(Literal::Int(0)), x) = (&**a, &**b)
            {
                *expr = x.clone();
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
