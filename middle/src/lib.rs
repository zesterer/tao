#![feature(arbitrary_self_types)]

pub mod error;
pub mod proc;
pub mod mir;
pub mod repr;
pub mod lower;
pub mod context;

pub use crate::{
    error::Error,
    proc::{ProcId, Proc, Procs},
    mir::MirNode,
    repr::{ReprId, Repr, Reprs},
    context::Context,
};

use tao_syntax::{
    Node,
    Span,
    SrcId,
    SrcNode,
    ast::{self, Ident},
};
use tao_analysis::{
    hir,
    TyNode,
    ty,
    DefId,
    Context as HirContext,
};
use hashbrown::HashMap;
use internment::Intern;
