#![feature(arbitrary_self_types, cell_update)]

pub mod error;
pub mod proc;
pub mod mir;
pub mod opt;
pub mod repr;
pub mod lower;
pub mod context;

pub use crate::{
    error::Error,
    opt::Pass,
    proc::{ProcId, Proc, Procs},
    mir::{MirNode, Pat, Binding, Expr, Const, Intrinsic, Local},
    repr::{Repr, Reprs, Prim},
    context::Context,
};
pub use tao_analysis::Ident;

use tao_syntax::{
    Node,
    Span,
    SrcId,
    SrcNode,
    ast,
};
use tao_analysis::{
    hir,
    TyNode,
    ty,
    DefId,
    Context as HirContext,
    data::DataId,
    ConDefId,
    ConContext,
    ConExpr,
    ConBinding,
    ConTy,
    ConTyId,
    ConDataId,
};
use hashbrown::HashMap;
use internment::Intern;
