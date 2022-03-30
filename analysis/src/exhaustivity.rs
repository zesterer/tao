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
    Int(Ranges<i64>),
    Real(f64),
    Char(char),
    Tuple(Vec<Self>),
    Record(Vec<(Ident, Self)>),
    Variant(DataId, Ident, Box<Self>),
    ListExact(Vec<Self>), // Exactly N in size
    ListFront(Vec<Self>, Box<Self>), // At least N in size
    Gen(Ident),
}

impl AbstractPat {
    fn from_binding(ctx: &Context, binding: &TyBinding) -> Self {
        match &*binding.pat {
            hir::Pat::Error => Self::Wildcard,
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
            hir::Pat::Literal(hir::Literal::Int(x)) => Self::Int({
                let mut range = Ranges::new();
                range.insert(*x..*x + 1);
                range
            }),
            hir::Pat::Literal(hir::Literal::Char(c)) => Self::Char(*c),
            hir::Pat::Literal(hir::Literal::Str(x)) => Self::ListExact(x.chars().map(Self::Char).collect()),
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
            hir::Pat::Record(fields) => AbstractPat::Record(fields
                .iter()
                .map(|(name, field)| (*name, AbstractPat::from_binding(ctx, field)))
                .collect()),
            pat => todo!("{:?}", pat),
        }
    }

    fn is_refutable_basic(&self, ctx: &Context) -> bool {
        match self {
            AbstractPat::Wildcard => false,
            AbstractPat::Bool([t, f]) => !(*t && *f),
            AbstractPat::Nat(set) => !set.clone().invert().is_empty(),
            AbstractPat::Int(set) => !set.clone().invert().is_empty(),
            AbstractPat::Char(_) => true,
            AbstractPat::Tuple(fields) => !fields
                .iter()
                .all(|field| !field.is_refutable_basic(ctx)),
            AbstractPat::ListExact(_) => true,
            AbstractPat::ListFront(items, tail) => !items.is_empty() || tail.is_refutable_basic(ctx),
            AbstractPat::Variant(data, _, _) => ctx.datas.get_data(*data).cons.len() > 1,
            AbstractPat::Record(fields) => !fields
                .iter()
                .all(|(_, field)| !field.is_refutable_basic(ctx)),
            pat => todo!("{:?}", pat),
        }
    }

    fn is_refutable(&self, ctx: &Context, ty: TyId, get_gen_ty: Option<&dyn Fn(usize) -> Option<TyId>>) -> bool {
        Self::inexhaustive_pat(ctx, ty, &mut std::iter::once(self), get_gen_ty).is_some()
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
            Ty::Error(_) => None,
            Ty::Prim(Prim::Nat) => {
                let mut covered = Ranges::new();
                for pat in filter {
                    match pat {
                        AbstractPat::Wildcard => return None,
                        AbstractPat::Nat(x) => covered = covered.clone().union(x.clone()),
                        _ => return None, // Type mismatch, don't yield an error because one was already generated
                    }
                }
                covered.clone().invert().into_iter().next().map(ExamplePrim::Nat).map(ExamplePat::Prim)
            },
            Ty::Prim(Prim::Int) => {
                let mut covered = Ranges::new();
                for pat in filter {
                    match pat {
                        AbstractPat::Wildcard => return None,
                        AbstractPat::Int(x) => covered = covered.clone().union(x.clone()),
                        _ => return None, // Type mismatch, don't yield an error because one was already generated
                    }
                }
                covered.clone().invert().into_iter().next().map(ExamplePrim::Int).map(ExamplePat::Prim)
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
                        _ => return None, // Type mismatch, don't yield an error because one was already generated
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
                        _ => return None, // Type mismatch, don't yield an error because one was already generated
                    }
                }
                Some(ExamplePat::Wildcard)
            },
            Ty::Prim(Prim::Real) => {
                for pat in filter {
                    match pat {
                        AbstractPat::Wildcard => return None,
                        AbstractPat::Real(_) => {},
                        _ => return None, // Type mismatch, don't yield an error because one was already generated
                    }
                }
                Some(ExamplePat::Wildcard)
            },
            Ty::Prim(Prim::Universe) => {
                for pat in filter {
                    match pat {
                        AbstractPat::Wildcard => return None,
                        _ => return None, // Type mismatch, don't yield an error because one was already generated
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
                        _ => return None, // Type mismatch, don't yield an error because one was already generated
                    }
                }
                Self::inexhaustive_pat(ctx, fields[0], &mut inners.into_iter(), get_gen_ty)
                    .map(|inner| ExamplePat::Tuple(vec![inner]))
            },
            Ty::Record(fields) if fields.len() == 1 => {
                let mut inners = Vec::new();
                for pat in filter {
                    match pat {
                        AbstractPat::Wildcard => return None,
                        AbstractPat::Record(fields) => inners.push(&fields[0].1),
                        _ => return None, // Type mismatch, don't yield an error because one was already generated
                    }
                }
                Self::inexhaustive_pat(ctx, *fields.values().next().unwrap(), &mut inners.into_iter(), get_gen_ty)
                    .map(|inner| ExamplePat::Record(vec![(*fields.keys().next().unwrap(), inner)]))
            },
            Ty::Tuple(fields) => {
                let filter = (&mut filter).collect::<Vec<_>>();

                if filter.iter().copied().any(|pat| !pat.is_refutable_basic(ctx)) {
                    None
                } else {
                    let wildcard = AbstractPat::Wildcard;
                    let mut cols = vec![Vec::new(); fields.len()];
                    for pat in filter.iter().copied() {
                        match pat {
                            AbstractPat::Wildcard => (0..fields.len()).for_each(|i| cols[i].push((&wildcard, ty))),
                            AbstractPat::Tuple(pat_fields) => {
                                debug_assert_eq!(pat_fields.len(), cols.len());
                                for ((i, pat), ty) in pat_fields.iter().enumerate().zip(fields.iter()) {
                                    cols[i].push((pat, *ty));
                                }
                            },
                            _ => return None, // Type mismatch, don't yield an error because one was already generated
                        }
                    }

                    let mut refutable = cols
                        .into_iter()
                        .enumerate()
                        .filter(|(_, col)| col.iter().any(|(pat, ty)| pat.is_refutable(ctx, *ty, get_gen_ty)))
                        .collect::<Vec<_>>();

                    match refutable.len() {
                        0 => None,
                        1 => {
                            let (idx, col) = refutable.remove(0);
                            Self::inexhaustive_pat(ctx, fields[idx], &mut col.into_iter().map(|(pat, _)| pat), get_gen_ty)
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
            Ty::Record(fields) => {
                let filter = (&mut filter).collect::<Vec<_>>();

                if filter.iter().copied().any(|pat| !pat.is_refutable_basic(ctx)) {
                    None
                } else {
                    let wildcard = AbstractPat::Wildcard;
                    let mut cols = vec![Vec::new(); fields.len()];
                    for pat in filter.iter().copied() {
                        match pat {
                            AbstractPat::Wildcard => (0..fields.len()).for_each(|i| cols[i].push((&wildcard, ty))),
                            AbstractPat::Record(pat_fields) => {
                                debug_assert_eq!(fields.len(), cols.len());
                                for ((i, (_, pat)), (_, ty)) in pat_fields.iter().enumerate().zip(fields.iter()) {
                                    cols[i].push((pat, *ty));
                                }
                            },
                            _ => return None, // Type mismatch, don't yield an error because one was already generated
                        }
                    }

                    let mut refutable = cols
                        .into_iter()
                        .enumerate()
                        .filter(|(_, col)| col.iter().any(|(pat, ty)| pat.is_refutable(ctx, *ty, get_gen_ty)))
                        .collect::<Vec<_>>();

                    match refutable.len() {
                        0 => None,
                        1 => {
                            let (idx, col) = refutable.remove(0);
                            Self::inexhaustive_pat(ctx, *fields.values().nth(idx).unwrap(), &mut col.into_iter().map(|(pat, _)| pat), get_gen_ty)
                                .map(|pat| ExamplePat::Record(
                                    (0..idx).map(|idx| (*fields.keys().nth(idx).unwrap(), ExamplePat::Wildcard))
                                        .chain(std::iter::once((*fields.keys().nth(idx).unwrap(), pat)))
                                        .chain((idx + 1..fields.len()).map(|idx| (*fields.keys().nth(idx).unwrap(), ExamplePat::Wildcard)))
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
                        _ => return None, // Type mismatch, don't yield an error because one was already generated
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
                    // TODO: This is ugly, but checks for inhabitants of sub-patterns, allowing things like `let Just x = Just 5`
                    } else if ctx.tys.has_inhabitants(&ctx.datas, *cons_ty, &mut |id| {
                        ctx.tys.has_inhabitants(&ctx.datas, gen_tys[id], &mut |_| true)
                    }) {
                        return Some(ExamplePat::Variant(**cons, Box::new(ExamplePat::Wildcard)));
                    }
                }
                None
            },
            Ty::Gen(_, _) | Ty::Assoc(_, _, _) => {
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
                            if items.iter().all(|item| !item.is_refutable(ctx, item_ty, get_gen_ty)) {
                                covered_lens.insert(items.len() as u64..items.len() as u64 + 1);
                            }
                        },
                        AbstractPat::ListFront(items, tail) => {
                            if items.iter().all(|item| !item.is_refutable(ctx, item_ty, get_gen_ty)) && !tail.is_refutable(ctx, ty, get_gen_ty) {
                                covered_lens.insert(items.len() as u64..);
                            }
                        },
                        _ => return None, // Type mismatch, don't yield an error because one was already generated
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
                        _ => return None, // Type mismatch, don't yield an error because one was already generated
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
    Int(i64),
}

impl fmt::Display for ExamplePrim {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Bool(x) => write!(f, "{}", if *x { "True" } else { "False" }),
            Self::Nat(x) => write!(f, "{}", x),
            Self::Int(x) => write!(f, "{}", x),
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

impl ExamplePat {
    pub fn display<'a>(&'a self, ctx: &'a Context, hidden_outer: bool) -> impl fmt::Display + 'a {
        struct DisplayExamplePat<'a>(&'a ExamplePat, bool, &'a Context);

        impl<'a> fmt::Display for DisplayExamplePat<'a> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                let ctx = self.2;
                match &self.0 {
                    ExamplePat::Wildcard => write!(f, "_"),
                    ExamplePat::Prim(prim) => write!(f, "{}", prim),
                    ExamplePat::Tuple(fields) if self.1 => write!(f, "{}", fields
                        .iter()
                        .map(|f| format!("{}", DisplayExamplePat(f, false, ctx)))
                        .collect::<Vec<_>>()
                        .join(", ")),
                    ExamplePat::Tuple(fields) => write!(f, "({}{})", fields
                        .iter()
                        .map(|f| format!("{}", DisplayExamplePat(f, false, ctx)))
                        .collect::<Vec<_>>()
                        .join(", "), if fields.len() == 1 { "," } else { "" }),
                    ExamplePat::Record(fields) => write!(f, "{{ {} }}", fields.iter().map(|(name, f)| format!("{}: {},", name, DisplayExamplePat(f, false, ctx))).collect::<Vec<_>>().join(" ")),
                    ExamplePat::List(items) => write!(f, "[{}]", items.iter().map(|i| format!("{}", DisplayExamplePat(i, false, ctx))).collect::<Vec<_>>().join(", ")),
                    ExamplePat::Variant(name, inner) => write!(f, "{} {}", name, DisplayExamplePat(inner, false, ctx)),
                }
            }
        }

        DisplayExamplePat(self, hidden_outer, ctx)
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
