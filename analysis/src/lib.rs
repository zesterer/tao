#![feature(arbitrary_self_types, option_zip, never_type, let_else, let_chains)]

#![allow(clippy::result_unit_err)]
#![allow(clippy::type_complexity)]

pub mod class;
pub mod concrete;
pub mod context;
pub mod data;
#[cfg(feature = "debug")]
pub mod debug;
pub mod def;
pub mod effect;
pub mod error;
pub mod exhaustivity;
pub mod hir;
pub mod infer;
pub mod lower;
pub mod reify;
pub mod ty;

pub use crate::{
    class::{Class, ClassAssoc, ClassField, ClassId, Classes, Member, MemberId, MemberItem},
    concrete::{
        ConContext, ConDataId, ConEffectId, ConMeta, ConNode, ConProc, ConProcId, ConTy, ConTyId,
    },
    context::Context,
    data::{Alias, AliasId, Data, DataId, Datas},
    def::{Def, DefId, Defs},
    effect::{EffectAlias, EffectAliasId, EffectDecl, EffectDeclId, Effects},
    error::Error,
    exhaustivity::{exhaustivity, ExamplePat},
    hir::{ConBinding, ConExpr, InferBinding, InferExpr, Intrinsic, Meta, TyBinding, TyExpr},
    infer::{
        contravariant, covariant, invariant, Checked, ClassInfo, ClassVar, EffectInfo,
        EffectInstInfo, EffectInstVar, EffectVar, EqInfo, Infer, InferError, InferMeta, InferNode,
        TyInfo, TyVar,
    },
    lower::{Scope, ToHir, TypeLowerCfg},
    reify::Reify,
    ty::{
        Effect, EffectId, EffectInst, ErrorReason, GenScope, GenScopeId, ImpliedItems,
        ImpliedMember, InferImpliedItems, InferImpliedMember, Prim, Ty, TyId, TyImpliedMember,
        TyMeta, TyNode, Types,
    },
};
pub use tao_syntax::ast::Ident;
pub use tao_util::index::{Id, Index};

use hashbrown::{HashMap, HashSet};
use internment::Intern;
use std::{collections::BTreeMap, fmt};
use tao_syntax::{ast, Node, Span, SrcId, SrcNode};
