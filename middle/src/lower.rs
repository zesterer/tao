use super::*;

pub trait ToMir: Sized {
    type Output;

    fn to_mir(&self, ctx: &mut Context, hir: &HirContext, ty_insts: &TyInsts) -> Self::Output;
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

// Type instantiations for generic types
pub struct TyInsts<'a> {
    pub self_ty: Option<Repr>,
    pub gen: &'a [Repr],
}

impl ToMir for ty::TyId {
    type Output = Repr;

    fn to_mir(&self, ctx: &mut Context, hir: &HirContext, ty_insts: &TyInsts) -> Self::Output {
        match hir.tys.get(*self) {
            ty::Ty::Error(_) => unreachable!("Error types should not exist during MIR compilation"),
            ty::Ty::Prim(prim) => Repr::Prim(prim_to_mir(prim)),
            ty::Ty::List(ty) => Repr::List(Box::new(ty.to_mir(ctx, hir, ty_insts))),
            ty::Ty::Tuple(fields) => Repr::Tuple(fields
                .iter()
                .map(|field| field.to_mir(ctx, hir, ty_insts))
                .collect()),
            ty::Ty::Func(i, o) => Repr::Func(
                Box::new(i.to_mir(ctx, hir, ty_insts)),
                Box::new(o.to_mir(ctx, hir, ty_insts)),
            ),
            ty::Ty::Gen(idx, _) => ty_insts.gen[idx].clone(),
            ty::Ty::Data(data, args) => {
                let args = args
                    .iter()
                    .map(|arg| arg.to_mir(ctx, hir, ty_insts))
                    .collect::<Vec<_>>();

                if ctx.reprs.declare(data, args.clone()) {
                    let variants = hir.datas
                        .get_data(data)
                        .cons
                        .iter()
                        .map(|(_, ty)| ty.to_mir(ctx, hir, &TyInsts {
                            self_ty: None,
                            gen: &args,
                        }))
                        .collect();
                    ctx.reprs.define(data, args.clone(), Repr::Sum(variants));
                }
                Repr::Data(data, args)
            },
            ty::Ty::Record(fields) => {
                let mut fields = fields
                    .iter()
                    .map(|(name, ty)| (*name, ty.to_mir(ctx, hir, ty_insts)))
                    .collect::<Vec<_>>();
                fields.sort_by_key(|(name, _)| name.as_ref());
                Repr::Tuple(fields.into_iter().map(|(_, ty)| ty).collect())
            },
            ty::Ty::SelfType => todo!(),
        }
    }
}
