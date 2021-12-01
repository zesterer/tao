use super::*;
use std::{
    ops::Range,
    cmp::Ord,
    fmt,
};
use ranges::Ranges;

#[derive(Debug)]
pub enum AbstractPat {
    Wildcard,
    Bool([bool; 2]),
    Nat(Ranges<u64>),
    Char(char),
    Tuple(Vec<Self>),
    Variant(DataId, Ident, Box<Self>),
    ListExact(Vec<Self>), // Exactly N in size
    ListFront(Vec<Self>, Box<Self>), // At least N in size
    Gen(Ident),
}

impl AbstractPat {
    fn from_binding(ctx: &Context, binding: &TyBinding) -> Self {
        match &*binding.pat {
            hir::Pat::Wildcard => Self::Wildcard,
            hir::Pat::Literal(hir::Literal::Bool(x)) => Self::Bool([
                !x,
                *x,
            ]),
            hir::Pat::Literal(hir::Literal::Nat(x)) => Self::Nat({
                let mut range = Ranges::new();
                range.insert(*x..*x + 1);
                range
            }),
            hir::Pat::Single(inner) => Self::from_binding(ctx, inner),
            hir::Pat::Add(lhs, rhs) if matches!(&*lhs.pat, hir::Pat::Wildcard) => Self::Nat({
                let mut range = Ranges::new();
                range.insert(**rhs..);
                range
            }),
            hir::Pat::Tuple(fields) => AbstractPat::Tuple(fields
                .iter()
                .map(|field| AbstractPat::from_binding(ctx, field))
                .collect()),
            hir::Pat::Decons(data, cons, inner) => AbstractPat::Variant(**data, *cons, Box::new(AbstractPat::from_binding(ctx, inner))),
            hir::Pat::ListExact(items) => AbstractPat::ListExact(items
                .iter()
                .map(|item| AbstractPat::from_binding(ctx, item))
                .collect()),
            hir::Pat::ListFront(items, tail) => AbstractPat::ListFront(items
                .iter()
                .map(|item| AbstractPat::from_binding(ctx, item))
                .collect(), Box::new(tail.as_ref().map(|tail| AbstractPat::from_binding(ctx, tail)).unwrap_or(AbstractPat::Wildcard))),
            pat => todo!("{:?}", pat),
        }
    }

    fn is_refutable(&self, ctx: &Context) -> bool {
        match self {
            AbstractPat::Wildcard => false,
            AbstractPat::Nat(set) => !set.clone().invert().is_empty(),
            AbstractPat::Tuple(fields) => !fields
                .iter()
                .all(|field| !field.is_refutable(ctx)),
            AbstractPat::ListExact(_) => true,
            AbstractPat::ListFront(items, tail) => !items.is_empty() || tail.is_refutable(ctx),
            AbstractPat::Variant(data, _, _) => ctx.datas.get_data(*data).cons.len() > 1,
            pat => todo!("{:?}", pat),
        }
    }

    fn inexhaustive_pat<'a>(
        ctx: &Context,
        ty: TyId,
        mut filter: &mut dyn Iterator<Item = &'a AbstractPat>,
        get_gen_ty: Option<&dyn Fn(usize) -> Option<TyId>>,
    ) -> Option<ExamplePat> {
        let ty = match ctx.tys.get(ty) {
            Ty::Gen(idx, _) => {
                get_gen_ty.and_then(|get_gen_ty| get_gen_ty(idx)).unwrap_or(ty)
            },
            _ => ty,
        };

        match ctx.tys.get(ty) {
            Ty::Error => None,
            Ty::Prim(Prim::Nat) => {
                let mut covered = Ranges::new();
                for pat in filter {
                    match pat {
                        AbstractPat::Wildcard => return None,
                        AbstractPat::Nat(x) => covered = covered.clone().union(x.clone()),
                        _ => unreachable!(),
                    }
                }
                covered.clone().invert().into_iter().next().map(ExamplePrim::Nat).map(ExamplePat::Prim)
            },
            Ty::Prim(Prim::Bool) => {
                let (mut caught_f, mut caught_t) = (false, false);
                for pat in filter {
                    match pat {
                        AbstractPat::Wildcard => return None,
                        AbstractPat::Bool([f, t]) => {
                            caught_f |= f;
                            caught_t |= t;
                        },
                        _ => unreachable!(),
                    }
                }
                if !caught_f {
                    Some(ExamplePat::Prim(ExamplePrim::Bool(false)))
                } else if !caught_t {
                    Some(ExamplePat::Prim(ExamplePrim::Bool(true)))
                } else {
                    None
                }
            },
            Ty::Prim(Prim::Char) => {
                for pat in filter {
                    match pat {
                        AbstractPat::Wildcard => return None,
                        AbstractPat::Char(_) => {},
                        _ => unreachable!(),
                    }
                }
                Some(ExamplePat::Wildcard)
            },
            Ty::Prim(prim) => todo!("{:?}", prim),
            Ty::Tuple(fields) if fields.len() == 1 => {
                let mut inners = Vec::new();
                for pat in filter {
                    match pat {
                        AbstractPat::Wildcard => return None,
                        AbstractPat::Tuple(fields) => inners.push(&fields[0]),
                        _ => unreachable!(),
                    }
                }
                Self::inexhaustive_pat(ctx, fields[0], &mut inners.into_iter(), get_gen_ty)
                    .map(|inner| ExamplePat::Tuple(vec![inner]))
            },
            Ty::Record(fields) => {
                if (&mut filter).any(|pat| !pat.is_refutable(ctx)) {
                    None
                } else {
                    Some(ExamplePat::Record(fields
                        .iter()
                        .map(|(name, _)| (*name, ExamplePat::Wildcard))
                        .collect()))
                }
            },
            Ty::Tuple(fields) => {
                let filter = (&mut filter).collect::<Vec<_>>();

                if filter.iter().copied().any(|pat| !pat.is_refutable(ctx)) {
                    None
                } else {
                    let wildcard = AbstractPat::Wildcard;
                    let mut cols = vec![Vec::new(); fields.len()];
                    for pat in filter.iter().copied() {
                        match pat {
                            AbstractPat::Wildcard => (0..fields.len()).for_each(|i| cols[i].push(&wildcard)),
                            AbstractPat::Tuple(fields) => {
                                assert_eq!(fields.len(), cols.len());
                                for (i, pat) in fields.iter().enumerate() {
                                    cols[i].push(pat);
                                }
                            },
                            _ => unreachable!(),
                        }
                    }

                    let mut refutable = cols
                        .into_iter()
                        .enumerate()
                        .filter(|(_, col)| col.iter().any(|pat| pat.is_refutable(ctx)))
                        .collect::<Vec<_>>();

                    match refutable.len() {
                        0 => None,
                        1 => {
                            let (idx, col) = refutable.remove(0);
                            Self::inexhaustive_pat(ctx, fields[idx], &mut col.into_iter(), get_gen_ty)
                                .map(|pat| ExamplePat::Tuple(
                                    (0..idx).map(|_| ExamplePat::Wildcard)
                                        .chain(std::iter::once(pat))
                                        .chain((idx + 1..fields.len()).map(|_| ExamplePat::Wildcard))
                                        .collect()))
                        },
                        _ => Some(ExamplePat::Wildcard),
                    }
                }
            },
            Ty::Data(data, gen_tys) => {
                let mut variants = HashMap::<_, Vec<_>>::default();
                for pat in filter {
                    match pat {
                        AbstractPat::Wildcard => return None,
                        AbstractPat::Variant(_, x, inner) => variants
                            .entry(*x)
                            .or_default()
                            .push(&**inner),
                        _ => unreachable!(),
                    }
                }
                for (cons, cons_ty) in &ctx.datas.get_data(data).cons {
                    if let Some(variants) = variants.remove(&**cons) {
                        let get_gen_ty = |idx| match ctx.tys.get(gen_tys[idx]) {
                            Ty::Gen(idx, _) => get_gen_ty
                                .and_then(|get_gen_ty| get_gen_ty(idx)),
                            _ => Some(gen_tys[idx]),
                        };
                        if let Some(pat) = Self::inexhaustive_pat(ctx, *cons_ty, &mut variants.into_iter(), Some(&get_gen_ty)) {
                            return Some(ExamplePat::Variant(**cons, Box::new(pat)));
                        }
                    } else {
                        return Some(ExamplePat::Variant(**cons, Box::new(ExamplePat::Wildcard)));
                    }
                }
                None
            },
            Ty::Gen(idx, _) => {
                for pat in filter {
                    match pat {
                        AbstractPat::Wildcard => return None,
                        pat => {},
                    }
                }
                Some(ExamplePat::Wildcard)
            },
            Ty::List(item_ty) => {
                let mut covered_lens = Ranges::new();
                for pat in filter {
                    match pat {
                        AbstractPat::Wildcard => return None,
                        AbstractPat::ListExact(items) => {
                            if items.iter().all(|item| !item.is_refutable(ctx)) {
                                covered_lens.insert(items.len() as u64..items.len() as u64 + 1);
                            }
                        },
                        AbstractPat::ListFront(items, tail) => {
                            if items.iter().all(|item| !item.is_refutable(ctx)) && !tail.is_refutable(ctx) {
                                covered_lens.insert(items.len() as u64..);
                            }
                        },
                        _ => unreachable!(),
                    }
                }

                covered_lens.clone().invert().into_iter().next().map(|n| {
                    ExamplePat::List((0..n).map(|_| ExamplePat::Wildcard).collect())
                })
            },
            Ty::Func(_, _) => {
                for pat in filter {
                    match pat {
                        AbstractPat::Wildcard => return None,
                        _ => unreachable!(),
                    }
                }
                Some(ExamplePat::Wildcard)
            },
            ty => todo!("{:?}", ty),
        }
    }
}

#[derive(Clone, Debug)]
pub enum ExamplePrim {
    Bool(bool),
    Nat(u64),
}

impl fmt::Display for ExamplePrim {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Bool(x) => write!(f, "{}", x),
            Self::Nat(x) => write!(f, "{}", x),
        }
    }
}

#[derive(Clone, Debug)]
pub enum ExamplePat {
    Wildcard,
    Prim(ExamplePrim),
    Tuple(Vec<Self>),
    Record(Vec<(Ident, Self)>),
    List(Vec<Self>),
    Variant(Ident, Box<Self>),
}

impl fmt::Display for ExamplePat {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Wildcard => write!(f, "_"),
            Self::Prim(prim) => write!(f, "{}", prim),
            Self::Tuple(fields) => write!(f, "({})", fields.iter().map(|f| format!("{},", f)).collect::<Vec<_>>().join(" ")),
            Self::Record(fields) => write!(f, "{{ {} }}", fields.iter().map(|(name, f)| format!("{}: {},", name, f)).collect::<Vec<_>>().join(" ")),
            Self::List(items) => write!(f, "[{}]", items.iter().map(|i| format!("{}", i)).collect::<Vec<_>>().join(", ")),
            Self::Variant(name, inner) => write!(f, "{} {}", name, inner),
        }
    }
}

pub fn exhaustivity<'a>(ctx: &Context, ty: TyId, arms: impl IntoIterator<Item = &'a TyBinding>) -> Result<(), ExamplePat> {
    let arms = arms
        .into_iter()
        .map(|b| AbstractPat::from_binding(ctx, b))
        .collect::<Vec<_>>();

    if let Some(pat) = AbstractPat::inexhaustive_pat(ctx, ty, &mut arms.iter(), None) {
        Err(pat)
    } else {
        Ok(())
    }
}
