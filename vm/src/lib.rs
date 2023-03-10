// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

pub mod code;
pub mod exec;
pub mod lower;

pub use crate::{
    code::{Instr, Program, Addr},
    exec::{exec, Value, Env},
};
use tao_middle::{
    mir,
    Context as MirContext,
    MirNode,
    ProcId,
    repr,
    Ident,
    EffectId,
};
use hashbrown::HashMap;
