#![feature(arbitrary_self_types, option_zip, bool_to_option, never_type)]

pub mod class;
pub mod concrete;
pub mod context;
pub mod data;
pub mod def;
pub mod error;
pub mod exhaustivity;
pub mod infer;
pub mod hir;
pub mod lower;
pub mod reify;
pub mod ty;

pub use crate::{
    class::{ClassId, Class, Classes, ClassItem, Member, MemberItem},
    concrete::{ConContext, ConTyId, ConTy, ConNode, ConMeta, ConDefId, ConDataId},
    context::Context,
    data::{Datas, Data, DataId, Alias, AliasId},
    def::{Defs, Def, DefId},
    error::Error,
    exhaustivity::{exhaustivity, ExamplePat},
    hir::{InferExpr, InferBinding, TyExpr, TyBinding, ConBinding, ConExpr, Intrinsic},
    infer::{Infer, Checked, TyVar, TyInfo, InferNode, InferMeta, InferError, EqInfo, ClassVar, NumLitr},
    lower::{Scope, ToHir},
    reify::Reify,
    ty::{Types, TyId, GenScope, GenScopeId, Prim, Ty, TyNode, TyMeta, ErrorReason, Obligation},
};
pub use tao_syntax::ast::Ident;

use tao_syntax::{
    Node,
    Span,
    SrcNode,
    SrcId,
    ast,
};
use hashbrown::{HashMap, HashSet};
use internment::Intern;
use std::collections::BTreeMap;
use std::fmt;
