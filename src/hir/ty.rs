use super::{Ident, data::DataId, infer::{InferCtx, TyVar, TyInfo}, Span};
use crate::util::SrcNode;
use slab::Slab;
use std::{
    collections::HashMap,
    fmt,
};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Primitive {
    Nat,
    Int,
    Num,
    Bool,
    Char,
    Universe,
}

impl fmt::Display for Primitive {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Primitive::Nat => write!(f, "Nat"),
            Primitive::Int => write!(f, "Int"),
            Primitive::Num => write!(f, "Num"),
            Primitive::Char => write!(f, "Char"),
            Primitive::Bool => write!(f, "Bool"),
            Primitive::Universe => write!(f, "<Universe>"),
        }
    }
}

impl Primitive {
    pub fn subtype_of(self, other: Self) -> bool {
        self == other || self.numerical_rank()
            .zip(other.numerical_rank())
            .map_or(false, |(a, b)| a < b)
    }

    /// Return a number that determines how general a numerical primitive is.
    pub fn numerical_rank(&self) -> Option<usize> {
        Some(match self {
            Primitive::Nat => 0,
            Primitive::Int => 1,
            Primitive::Num => 2,
            _ => return None,
        })
    }

    pub fn supertype(self, other: Self) -> Option<Self> {
        Some(match self.numerical_rank().zip_with(other.numerical_rank(), |a, b| a.max(b))? {
            0 => Primitive::Nat,
            1 => Primitive::Int,
            2 => Primitive::Num,
            _ => unreachable!()
        })
    }

    pub fn subtype(self, other: Self) -> Option<Self> {
        Some(match self.numerical_rank().zip_with(other.numerical_rank(), |a, b| a.min(b))? {
            0 => Primitive::Nat,
            1 => Primitive::Int,
            2 => Primitive::Num,
            _ => unreachable!()
        })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Ty {
    /// An error occurred when performing inference with this type variable, generate no other errors.
    Error,
    Prim(Primitive),
    Generic(Ident, EnvId),
    List(TyId),
    Tuple(Vec<TyId>),
    Record(HashMap<Ident, TyId>),
    Func(TyId, TyId),
    Data(DataId, Vec<TyId>),
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct TyId(pub usize);

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct EnvId(pub usize);

impl fmt::Display for TyId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "${}", self.0)
    }
}

#[derive(Default)]
pub struct TyCtx {
    pub tys: Slab<Ty>,
}

impl TyCtx {
    pub fn insert(&mut self, ty: Ty) -> TyId {
        TyId(self.tys.insert(ty))
    }

    pub fn get(&self, id: TyId) -> &Ty {
        self.tys
            .get(id.0)
            .expect("Attempted to fetch type for invalid type ID")
    }

    pub fn display(&self, id: TyId) -> TyDisplay<'_> {
        TyDisplay {
            ctx: self,
            id,
        }
    }

    pub fn to_var(&self, id: TyId, infer: &mut InferCtx, span: Span, generics: &impl Fn(Ident) -> TyVar) -> TyVar {
        match self.get(id) {
            Ty::Error => infer.error_var(span),
            Ty::Prim(prim) => infer.info_var(TyInfo::Prim(*prim), span),
            Ty::Generic(ident, _) => generics(*ident),
            Ty::List(id) => self.to_var(*id, infer, span, generics),
            Ty::Tuple(items) => {
                let items = items
                    .iter()
                    .map(|id| self.to_var(*id, infer, span, generics))
                    .collect();
                infer.info_var(TyInfo::Tuple(items), span)
            },
            Ty::Record(fields) => {
                let fields = fields
                    .iter()
                    .map(|(name, id)| (*name, self.to_var(*id, infer, span, generics)))
                    .collect();
                infer.info_var(TyInfo::Record(fields), span)
            },
            Ty::Func(i, o) => {
                let i = self.to_var(*i, infer, span, generics);
                let o = self.to_var(*o, infer, span, generics);
                infer.info_var(TyInfo::Func(i, o), span)
            },
            Ty::Data(data_id, params) => {
                let params = params
                    .iter()
                    .map(|id| self.to_var(*id, infer, span, generics))
                    .collect();
                infer.info_var(TyInfo::Data(*data_id, params), span)
            },
        }
    }
}

pub struct TyDisplay<'a> {
    ctx: &'a TyCtx,
    id: TyId,
}

impl<'a> fmt::Display for TyDisplay<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.ctx.get(self.id) {
            Ty::Error => write!(f, "_"),
            Ty::Prim(p) => write!(f, "{}", p),
            Ty::Generic(ident, _) => write!(f, "{}", ident),
            Ty::List(inner) => write!(f, "[{}]", Self { ctx: self.ctx, id: *inner }),
            Ty::Tuple(fields) => write!(f, "({})", fields
                .iter()
                .map(|field| Self { ctx: self.ctx, id: *field }.to_string())
                .collect::<Vec<_>>()
                .join(", ")),
            Ty::Record(fields) => write!(f, "{{ {} }}", fields
                .iter()
                .map(|(ident, field)| format!("{}: {}", ident, Self { ctx: self.ctx, id: *field }))
                .collect::<Vec<_>>()
                .join(", ")),
            Ty::Func(i, o) => write!(f, "{} -> {}", Self { ctx: self.ctx, id: *i }, Self { ctx: self.ctx, id: *o }),
            _ => todo!(),
        }
    }
}
