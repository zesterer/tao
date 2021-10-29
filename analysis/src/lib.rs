#![feature(arbitrary_self_types)]

pub mod context;
pub mod data;
pub mod def;
pub mod error;
pub mod infer;
pub mod hir;
pub mod lower;
pub mod reify;
pub mod ty;

pub use crate::{
    context::Context,
    data::{Datas, Data, DataId, Alias, AliasId},
    def::{Defs, Def, DefId},
    error::Error,
    hir::{InferExpr, InferBinding, TyExpr, TyBinding},
    infer::{Infer, Checked, TyVar, TyInfo, InferNode, InferMeta},
    lower::{Scope, ToHir},
    reify::Reify,
    ty::{Types, TyId, GenScope, GenScopeId, Prim, Ty, TyNode, TyMeta},
};

use tao_syntax::{
    Node,
    Span,
    SrcNode,
    SrcId,
    ast::{self, Ident},
};
use hashbrown::{HashMap, HashSet};
use std::fmt;
