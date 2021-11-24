use super::*;

pub trait ToMir: Sized {
    type Output;

    fn to_mir(&self, ctx: &mut Context, hir: &HirContext, gen_tys: &[Repr]) -> Self::Output;
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

    fn to_mir(&self, ctx: &mut Context, hir: &HirContext, gen_tys: &[Repr]) -> Self::Output {
        match hir.tys.get(*self) {
            ty::Ty::Error => unreachable!("Error types should not exist during MIR compilation"),
            ty::Ty::Prim(prim) => Repr::Prim(prim_to_mir(prim)),
            ty::Ty::List(ty) => Repr::List(Box::new(ty.to_mir(ctx, hir, gen_tys))),
            ty::Ty::Tuple(fields) => Repr::Tuple(fields
                .iter()
                .map(|field| field.to_mir(ctx, hir, gen_tys))
                .collect()),
            ty::Ty::Func(i, o) => Repr::Func(
                Box::new(i.to_mir(ctx, hir, gen_tys)),
                Box::new(o.to_mir(ctx, hir, gen_tys)),
            ),
            ty::Ty::Gen(idx, _) => gen_tys[idx].clone(),
            ty::Ty::Data(data, args) => {
                let args = args
                    .iter()
                    .map(|arg| arg.to_mir(ctx, hir, gen_tys))
                    .collect::<Vec<_>>();

                if ctx.reprs.declare(data, args.clone()) {
                    let variants = hir.datas
                        .get_data(data)
                        .cons
                        .iter()
                        .map(|(_, ty)| ty.to_mir(ctx, hir, &args))
                        .collect();
                    ctx.reprs.define(data, args.clone(), Repr::Sum(variants));
                }
                Repr::Data(data, args)
            },
            ty::Ty::Record(fields) => {
                let mut fields = fields
                    .iter()
                    .map(|(name, ty)| (*name, ty.to_mir(ctx, hir, gen_tys)))
                    .collect::<Vec<_>>();
                fields.sort_by_key(|(name, _)| name.as_ref());
                Repr::Tuple(fields.into_iter().map(|(_, ty)| ty).collect())
            },
        }
    }
}

impl ToMir for hir::Literal {
    type Output = Const;

    fn to_mir(&self, ctx: &mut Context, hir: &HirContext, gen_tys: &[Repr]) -> Self::Output {
        match &*self {
            hir::Literal::Nat(x) => mir::Const::Nat(*x),
            hir::Literal::Str(s) => mir::Const::Str(*s),
            hir::Literal::Bool(x) => mir::Const::Bool(*x),
            hir::Literal::Char(c) => mir::Const::Char(*c),
            l => todo!("{:?}", l),
        }
    }
}

impl ToMir for hir::TyBinding {
    type Output = MirNode<mir::Binding>;

    fn to_mir(&self, ctx: &mut Context, hir: &HirContext, gen_tys: &[Repr]) -> Self::Output {
        let pat = match &*self.pat {
            hir::Pat::Error => unreachable!(),
            hir::Pat::Wildcard => mir::Pat::Wildcard,
            hir::Pat::Literal(litr) => mir::Pat::Const(litr.to_mir(ctx, hir, gen_tys)),
            hir::Pat::Single(inner) => mir::Pat::Single(inner.to_mir(ctx, hir, gen_tys)),
            hir::Pat::Add(lhs, rhs) => mir::Pat::Add(lhs.to_mir(ctx, hir, gen_tys), **rhs),
            hir::Pat::Tuple(fields) => mir::Pat::Tuple(fields
                .iter()
                .map(|field| field.to_mir(ctx, hir, gen_tys))
                .collect()),
            hir::Pat::ListExact(items) => mir::Pat::ListExact(items
                .iter()
                .map(|item| item.to_mir(ctx, hir, gen_tys))
                .collect()),
            hir::Pat::ListFront(items, tail) => mir::Pat::ListFront(
                items
                    .iter()
                    .map(|item| item.to_mir(ctx, hir, gen_tys))
                    .collect(),
                tail.as_ref().map(|tail| tail.to_mir(ctx, hir, gen_tys)),
            ),
            hir::Pat::Decons(data, variant, inner) => {
                let variant = hir.datas
                    .get_data(**data)
                    .cons
                    .iter()
                    .enumerate()
                    .find(|(_, (name, _))| **name == *variant)
                    .unwrap()
                    .0;
                mir::Pat::Variant(variant, inner.to_mir(ctx, hir, gen_tys))
            },
            pat => todo!("{:?}", pat),
        };

        let binding = mir::Binding {
            pat,
            name: self.name.as_ref().map(|n| **n),
        };

        MirNode::new(binding, self.meta().1.to_mir(ctx, hir, gen_tys))
    }
}

impl ToMir for hir::TyExpr {
    type Output = MirNode<mir::Expr>;

    fn to_mir(&self, ctx: &mut Context, hir: &HirContext, gen_tys: &[Repr]) -> Self::Output {
        let expr = match &**self {
            hir::Expr::Error => unreachable!(),
            hir::Expr::Literal(litr) => mir::Expr::Const(litr.to_mir(ctx, hir, gen_tys)),
            hir::Expr::Local(local) => mir::Expr::Local(*local),
            hir::Expr::Global(def_id, args) => {
                let gen_tys = args
                    .iter()
                    .map(|(_, ty)| ty.to_mir(ctx, hir, gen_tys))
                    .collect();
                mir::Expr::Global(lower_def(ctx, hir, *def_id, gen_tys), Default::default())
            },
            hir::Expr::Unary(op, x) => {
                use ast::BinaryOp::*;
                use ty::{Ty::Prim, Prim::*};
                let intrinsic = match (**op, hir.tys.get(x.meta().1)) {
                    (Not, Prim(Bool)) => mir::Intrinsic::NotBool,
                    op => panic!("Invalid unary op in HIR: {:?}", op),
                };
                mir::Expr::Intrinsic(intrinsic, vec![x.to_mir(ctx, hir, gen_tys)])
            },
            hir::Expr::Binary(op, x, y) => {
                use ast::BinaryOp::*;
                use ty::{Ty::{Prim, List}, Prim::*};
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
                    (Join, List(x), List(y)) => mir::Intrinsic::Join(x.to_mir(ctx, hir, gen_tys)), // Assume x = y
                    op => panic!("Invalid binary op in HIR: {:?}", op),
                };
                mir::Expr::Intrinsic(intrinsic, vec![x.to_mir(ctx, hir, gen_tys), y.to_mir(ctx, hir, gen_tys)])
            },
            hir::Expr::Match(pred, arms) => {
                let arms = arms
                    .iter()
                    .map(|(binding, body)| (binding.to_mir(ctx, hir, gen_tys), body.to_mir(ctx, hir, gen_tys)))
                    .collect();
                mir::Expr::Match(pred.to_mir(ctx, hir, gen_tys), arms)
            },
            hir::Expr::Tuple(fields) => mir::Expr::Tuple(fields
                .iter()
                .map(|field| field.to_mir(ctx, hir, gen_tys))
                .collect()),
            hir::Expr::List(items) => mir::Expr::List(items
                .iter()
                .map(|item| item.to_mir(ctx, hir, gen_tys))
                .collect()),
            hir::Expr::ListFront(items, tail) => {
                let tail = tail.to_mir(ctx, hir, gen_tys);
                mir::Expr::Intrinsic(
                    mir::Intrinsic::Join(match tail.meta() {
                        Repr::List(item) => (**item).clone(),
                        _ => unreachable!(),
                    }),
                    vec![
                        MirNode::new(mir::Expr::List(items
                            .iter()
                            .map(|item| item.to_mir(ctx, hir, gen_tys))
                            .collect()), tail.meta().clone()),
                        tail,
                    ],
                )
            },
            hir::Expr::Func(arg, body) => {
                let body = body.to_mir(ctx, hir, gen_tys);
                mir::Expr::Func(body.required_locals(Some(**arg)), **arg, body)
            },
            hir::Expr::Apply(f, arg) => mir::Expr::Apply(f.to_mir(ctx, hir, gen_tys), arg.to_mir(ctx, hir, gen_tys)),
            hir::Expr::Cons(data, variant, inner) => {
                let variant = hir.datas
                    .get_data(**data)
                    .cons
                    .iter()
                    .enumerate()
                    .find(|(_, (name, _))| **name == *variant)
                    .unwrap()
                    .0;
                mir::Expr::Variant(variant, inner.to_mir(ctx, hir, gen_tys))
            },
            hir::Expr::Access(record, field) => {
                let (record_ty, _, indirections) = hir.follow_field_access(record.meta().1, **field).unwrap();
                let field_idx = if let ty::Ty::Record(fields) = hir.tys.get(record_ty) {
                    let mut fields = fields.iter().map(|(name, _)| *name).collect::<Vec<_>>();
                    fields.sort_by_key(|name| name.as_ref());
                    fields.iter().enumerate().find(|(_, name)| **name == **field).unwrap().0
                } else {
                    unreachable!();
                };
                let mut expr = record.to_mir(ctx, hir, gen_tys);
                // Perform indirections for field accesses
                for _ in 0..indirections {
                    let variant_repr = if let Repr::Data(data, params) = expr.meta() {
                        if let Repr::Sum(variants) = ctx.reprs.get(*data, params.clone()) {
                            variants[0].clone()
                        } else {
                            unreachable!()
                        }
                    } else {
                        unreachable!()
                    };
                    expr = MirNode::new(mir::Expr::AccessVariant(expr, 0), variant_repr);
                }

                mir::Expr::Access(expr, field_idx)
            },
            hir::Expr::Record(fields) => {
                let mut fields = fields
                    .iter()
                    .map(|(name, field)| (**name, field.to_mir(ctx, hir, gen_tys)))
                    .collect::<Vec<_>>();
                fields.sort_by_key(|(name, _)| name.as_ref());
                mir::Expr::Tuple(fields.into_iter().map(|(_, field)| field).collect())
            },
        };

        MirNode::new(expr, self.meta().1.to_mir(ctx, hir, gen_tys))
    }
}

pub fn lower_def(ctx: &mut Context, hir: &HirContext, id: DefId, gen_tys: Vec<Repr>) -> ProcId {
    let def = hir.defs.get(id);

    let id = ctx.procs.id_of(id, gen_tys.clone());

    // Instantiate proc if not already done
    if !ctx.procs.is_declared(id) {
        ctx.procs.declare(id);
        let proc = Proc {
            body: def
                .body
                .as_ref()
                .unwrap()
                .to_mir(ctx, hir, &gen_tys),
        };
        ctx.procs.define(id, proc);
    }

    id
}
