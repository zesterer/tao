use super::{Ident, data::DataId};
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
    /// Return a number that determines how general a numerical primitive is.
    pub fn numerical_rank(&self) -> Option<usize> {
        match self {
            Primitive::Nat => Some(0),
            Primitive::Int => Some(1),
            Primitive::Num => Some(2),
            _ => None,
        }
    }
}

#[derive(Clone, Debug)]
pub enum Ty {
    /// An error occurred when performing inference with this type variable, generate no other errors.
    Error,
    Primitive(Primitive),
    Generic(Ident, EnvId),
    List(TyId),
    Tuple(Vec<TyId>),
    Record(HashMap<Ident, TyId>),
    Func(TyId, TyId),
    Data(SrcNode<DataId>, Vec<TyId>),
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
}

pub struct TyDisplay<'a> {
    ctx: &'a TyCtx,
    id: TyId,
}

impl<'a> fmt::Display for TyDisplay<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.ctx.get(self.id) {
            Ty::Error => write!(f, "_"),
            Ty::Primitive(p) => write!(f, "{}", p),
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
