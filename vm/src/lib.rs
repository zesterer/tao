pub mod code;
pub mod exec;
pub mod lower;

pub use crate::{
    code::{Instr, Program, Addr},
    exec::{exec, Value, Env, Stdio},
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
