use super::*;

pub trait ToMir: Sized {
    type Output;

    fn to_mir(&self, ctx: &mut Context, hir: &HirContext) -> Self::Output;
}

fn prim_to_mir(prim: ty::Prim) -> repr::Prim {
    match prim {
        ty::Prim::Nat => repr::Prim::Nat,
        ty::Prim::Int => repr::Prim::Int,
        ty::Prim::Char => repr::Prim::Char,
        ty::Prim::Bool => repr::Prim::Bool,
        p => todo!("{:?}", p),
    }
}

impl ToMir for ty::TyId {
    type Output = ReprId;

    fn to_mir(&self, ctx: &mut Context, hir: &HirContext) -> Self::Output {
        let repr = match hir.tys.get(*self) {
            ty::Ty::Error => unreachable!("Error types should not exist during MIR compilation"),
            ty::Ty::Prim(prim) => Repr::Prim(prim_to_mir(prim)),
            ty::Ty::List(ty) => Repr::List(ty.to_mir(ctx, hir)),
            ty::Ty::Tuple(fields) => Repr::Tuple(fields
                .iter()
                .map(|field| field.to_mir(ctx, hir))
                .collect()),
            ty => todo!("{:?}", ty),
        };

        ctx.reprs.get_or_insert(repr)
    }
}

impl ToMir for hir::Literal {
    type Output = mir::Expr;

    fn to_mir(&self, ctx: &mut Context, hir: &HirContext) -> Self::Output {
        match &*self {
            hir::Literal::Nat(x) => mir::Expr::Literal(mir::Literal::Nat(*x)),
            hir::Literal::Str(s) => {
                let char_ty = ctx.reprs.get_or_insert(Repr::Prim(repr::Prim::Char));
                let chars = s
                    .chars()
                    .map(|c| MirNode::new(mir::Expr::Literal(mir::Literal::Char(c)), char_ty))
                    .collect::<Vec<_>>();
                let intrinsic = mir::Intrinsic::MakeList(char_ty, chars.len());
                mir::Expr::Intrinsic(intrinsic, chars)
            },
            hir::Literal::Bool(x) => mir::Expr::Literal(mir::Literal::Bool(*x)),
            l => todo!("{:?}", l),
        }
    }
}

impl ToMir for hir::TyBinding {
    type Output = MirNode<mir::Binding>;

    fn to_mir(&self, ctx: &mut Context, hir: &HirContext) -> Self::Output {
        let pat = match &*self.pat {
            hir::Pat::Wildcard => mir::Pat::Wildcard,
            hir::Pat::Literal(litr) => mir::Pat::Literal(litr.to_mir(ctx, hir)),
            hir::Pat::Tuple(fields) => mir::Pat::Tuple(fields
                .iter()
                .map(|field| field.to_mir(ctx, hir))
                .collect()),
            pat => todo!("{:?}", pat),
        };

        assert!(self.name.is_none());
        let binding = mir::Binding {
            pat,
            name: None,
        };

        MirNode::new(binding, self.meta().1.to_mir(ctx, hir))
    }
}

impl ToMir for hir::TyExpr {
    type Output = MirNode<mir::Expr>;

    fn to_mir(&self, ctx: &mut Context, hir: &HirContext) -> Self::Output {
        let expr = match &**self {
            hir::Expr::Literal(litr) => litr.to_mir(ctx, hir),
            hir::Expr::Unary(op, x) => {
                use ast::BinaryOp::*;
                use ty::{Ty::Prim, Prim::*};
                let intrinsic = match (**op, hir.tys.get(x.meta().1)) {
                    (Not, Prim(Bool)) => mir::Intrinsic::NotBool,
                    op => panic!("Invalid unary op in HIR: {:?}", op),
                };
                mir::Expr::Intrinsic(intrinsic, vec![x.to_mir(ctx, hir)])
            },
            hir::Expr::Binary(op, x, y) => {
                use ast::BinaryOp::*;
                use ty::{Ty::Prim, Prim::*};
                let intrinsic = match (**op, hir.tys.get(x.meta().1), hir.tys.get(y.meta().1)) {
                    (Add, Prim(Nat), Prim(Nat)) => mir::Intrinsic::AddNat,
                    (Add, Prim(Int), Prim(Int)) => mir::Intrinsic::AddInt,
                    (Sub, Prim(Nat), Prim(Nat)) => mir::Intrinsic::SubNat,
                    (Sub, Prim(Int), Prim(Int)) => mir::Intrinsic::SubInt,
                    op => panic!("Invalid binary op in HIR: {:?}", op),
                };
                mir::Expr::Intrinsic(intrinsic, vec![x.to_mir(ctx, hir), y.to_mir(ctx, hir)])
            },
            hir::Expr::Match(pred, arms) => {
                let arms = arms
                    .iter()
                    .map(|(binding, body)| (binding.to_mir(ctx, hir), body.to_mir(ctx, hir)))
                    .collect();
                mir::Expr::Match(pred.to_mir(ctx, hir), arms)
            },
            hir::Expr::Tuple(fields) => mir::Expr::MakeTuple(fields
                .iter()
                .map(|field| field.to_mir(ctx, hir))
                .collect()),
            expr => todo!("{:?}", expr),
        };

        MirNode::new(expr, self.meta().1.to_mir(ctx, hir))
    }
}

pub fn lower_def(ctx: &mut Context, hir: &HirContext, id: DefId, gen: Vec<ReprId>) -> ProcId {
    let def = hir.defs.get(id);

    let body = def
        .body
        .as_ref()
        .unwrap()
        .to_mir(ctx, hir);

    ctx.procs.insert(id, gen, Proc {
        body,
    })
}
