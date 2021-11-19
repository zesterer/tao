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
    type Output = Repr;

    fn to_mir(&self, ctx: &mut Context, hir: &HirContext) -> Self::Output {
        match hir.tys.get(*self) {
            ty::Ty::Error => unreachable!("Error types should not exist during MIR compilation"),
            ty::Ty::Prim(prim) => Repr::Prim(prim_to_mir(prim)),
            ty::Ty::List(ty) => Repr::List(Box::new(ty.to_mir(ctx, hir))),
            ty::Ty::Tuple(fields) => Repr::Tuple(fields
                .iter()
                .map(|field| field.to_mir(ctx, hir))
                .collect()),
            ty::Ty::Func(i, o) => Repr::Func(
                Box::new(i.to_mir(ctx, hir)),
                Box::new(o.to_mir(ctx, hir)),
            ),
            ty => todo!("{:?}", ty),
        }
    }
}

impl ToMir for hir::Literal {
    type Output = Const;

    fn to_mir(&self, ctx: &mut Context, hir: &HirContext) -> Self::Output {
        match &*self {
            hir::Literal::Nat(x) => mir::Const::Int(*x as i64), // TODO: Use Nat
            hir::Literal::Str(s) => mir::Const::Str(*s),
            hir::Literal::Bool(x) => mir::Const::Bool(*x),
            l => todo!("{:?}", l),
        }
    }
}

impl ToMir for hir::TyBinding {
    type Output = MirNode<mir::Binding>;

    fn to_mir(&self, ctx: &mut Context, hir: &HirContext) -> Self::Output {
        let pat = match &*self.pat {
            hir::Pat::Wildcard => mir::Pat::Wildcard,
            hir::Pat::Literal(litr) => mir::Pat::Const(litr.to_mir(ctx, hir)),
            hir::Pat::Tuple(fields) => mir::Pat::Tuple(fields
                .iter()
                .map(|field| field.to_mir(ctx, hir))
                .collect()),
            pat => todo!("{:?}", pat),
        };

        let binding = mir::Binding {
            pat,
            name: self.name.as_ref().map(|n| **n),
        };

        MirNode::new(binding, self.meta().1.to_mir(ctx, hir))
    }
}

impl ToMir for hir::TyExpr {
    type Output = MirNode<mir::Expr>;

    fn to_mir(&self, ctx: &mut Context, hir: &HirContext) -> Self::Output {
        let expr = match &**self {
            hir::Expr::Literal(litr) => mir::Expr::Const(litr.to_mir(ctx, hir)),
            hir::Expr::Local(local) => mir::Expr::Local(*local),
            hir::Expr::Global(def_id, args) => {
                let args = args
                    .iter()
                    .map(|(_, ty)| ty.to_mir(ctx, hir))
                    .collect();
                mir::Expr::Global(lower_def(ctx, hir, *def_id, args))
            },
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
                    (Mul, Prim(Nat), Prim(Nat)) => mir::Intrinsic::MulNat,
                    (Mul, Prim(Int), Prim(Int)) => mir::Intrinsic::MulInt,
                    (Less, Prim(Nat), Prim(Nat)) => mir::Intrinsic::LessNat,
                    (Less, Prim(Int), Prim(Int)) => mir::Intrinsic::LessInt,
                    (More, Prim(Nat), Prim(Nat)) => mir::Intrinsic::MoreNat,
                    (More, Prim(Int), Prim(Int)) => mir::Intrinsic::MoreInt,
                    (LessEq, Prim(Nat), Prim(Nat)) => mir::Intrinsic::LessEqNat,
                    (LessEq, Prim(Int), Prim(Int)) => mir::Intrinsic::LessEqInt,
                    (MoreEq, Prim(Nat), Prim(Nat)) => mir::Intrinsic::MoreEqNat,
                    (MoreEq, Prim(Int), Prim(Int)) => mir::Intrinsic::MoreEqInt,
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
            hir::Expr::Tuple(fields) => mir::Expr::Tuple(fields
                .iter()
                .map(|field| field.to_mir(ctx, hir))
                .collect()),
            hir::Expr::Func(arg, body) => {
                let body = body.to_mir(ctx, hir);
                mir::Expr::Func(body.required_locals(Some(**arg)), **arg, body)
            },
            hir::Expr::Apply(f, arg) => mir::Expr::Apply(f.to_mir(ctx, hir), arg.to_mir(ctx, hir)),
            expr => todo!("{:?}", expr),
        };

        MirNode::new(expr, self.meta().1.to_mir(ctx, hir))
    }
}

pub fn lower_def(ctx: &mut Context, hir: &HirContext, id: DefId, gen: Vec<Repr>) -> ProcId {
    let def = hir.defs.get(id);

    let id = ctx.procs.id_of(id, gen);

    // Instantiate proc if not already done
    if !ctx.procs.is_declared(id) {
        ctx.procs.declare(id);
        let proc = Proc {
            body: def
                .body
                .as_ref()
                .unwrap()
                .to_mir(ctx, hir),
        };
        ctx.procs.define(id, proc);
    }

    id
}
