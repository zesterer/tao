#![feature(arbitrary_self_types, cell_update, never_type, drain_filter, let_else)]

pub mod context;
pub mod error;
pub mod lower;
pub mod mir;
pub mod opt;
pub mod proc;
pub mod repr;

pub use crate::{
    context::{Context, OptMode},
    error::Error,
    mir::{Binding, Expr, Handler, Intrinsic, Literal, Local, MirNode, Partial, Pat},
    opt::Pass,
    proc::{Proc, ProcId, Procs},
    repr::{Data, Prim, Repr, Reprs},
};
pub use tao_analysis::Ident;

use hashbrown::{HashSet};

use std::collections::{BTreeMap, BTreeSet};
use tao_analysis::{
    hir, ty, ConBinding, ConContext, ConDataId, ConEffectId, ConExpr,
    ConProcId, ConTy, ConTyId, Context as HirContext,
};
use tao_syntax::{Node, Span, SrcId, SrcNode};

pub type EffectId = ConEffectId;
