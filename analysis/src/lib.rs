#![feature(arbitrary_self_types, option_zip, never_type, let_else, drain_filter, generic_associated_types, let_chains)]

pub mod class;
pub mod concrete;
pub mod context;
pub mod data;
pub mod def;
pub mod effect;
pub mod error;
pub mod exhaustivity;
pub mod infer;
pub mod hir;
pub mod lower;
pub mod reify;
pub mod ty;

pub use crate::{
    class::{ClassId, Class, Classes, ClassAssoc, ClassField, Member, MemberId, MemberItem},
    concrete::{ConContext, ConTyId, ConTy, ConNode, ConMeta, ConProc, ConProcId, ConDataId, ConEffectId},
    context::Context,
    data::{Datas, Data, DataId, Alias, AliasId},
    def::{Defs, Def, DefId},
    effect::{Effects, EffectDecl, EffectDeclId, EffectAlias, EffectAliasId},
    error::Error,
    exhaustivity::{exhaustivity, ExamplePat},
    hir::{InferExpr, InferBinding, TyExpr, TyBinding, ConBinding, ConExpr, Intrinsic, Meta},
    infer::{Infer, Checked, TyVar, TyInfo, InferNode, InferMeta, InferError, EqInfo, ClassVar, ClassInfo, EffectVar, EffectInfo, EffectInstInfo, EffectInstVar, covariant, contravariant, invariant},
    lower::{Scope, ToHir, TypeLowerCfg},
    reify::Reify,
    ty::{Types, TyId, GenScope, GenScopeId, Prim, Ty, TyNode, TyMeta, ErrorReason, ImpliedMember, TyImpliedMember, InferImpliedMember, ImpliedItems, InferImpliedItems, Effect, EffectId, EffectInst},
};
pub use tao_syntax::ast::Ident;
pub use tao_util::index::{Id, Index};

use tao_syntax::{
    Node,
    Span,
    SrcNode,
    SrcId,
    ast,
};
use hashbrown::{HashMap, HashSet};
use internment::Intern;
use std::{
    fmt,
    marker::PhantomData,
    collections::BTreeMap,
};
