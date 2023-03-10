use tao_util::index::{Id, Index};

#[derive(Copy, Clone)]
pub struct Reg(usize);

#[derive(Clone)]
pub enum Repr {
    Nat,
    Bool,
    Universe,
    List(Box<Self>),
    Tuple(Vec<Self>),
    Sum(Vec<Self>),
    Box(Id<Repr>),
    Func(Box<Self>, Box<Self>),
}

#[derive(Copy, Clone)]
pub enum Intrinsic {
    Box,
    Unbox,
}

#[derive(Copy, Clone)]
pub enum Imm {
    Nat(u64),
}

#[derive(Clone)]
pub enum Op {
    Imm(Imm),
    Call(Id<Func>, Vec<Reg>),
    Apply(Reg, Reg),
    Tuple(Vec<Reg>),
    AccessField(Reg, usize),
    AccessVariant(Reg, usize),
    VariantIdx(Reg),
    Intrinsic(Intrinsic, Vec<Reg>),
}

#[derive(Copy, Clone)]
pub enum Pat {
    Wildcard,
    Imm(Imm),
}

#[derive(Copy, Clone)]
pub enum Target {
    Return,
    Block(Id<Block>),
}

#[derive(Clone)]
pub struct Branch {
    pub pred: Reg,
    /// (matching pattern, parameters passed to target, branch destination)
    pub arms: Vec<(Pat, Vec<Reg>, Target)>,
}

#[derive(Clone)]
pub struct Block {
    pub args: Vec<Repr>,
    pub ops: Vec<(Repr, Reg, Op)>,
    pub branch: Branch,
}

#[derive(Copy, Clone)]
pub struct BlockId(usize);

#[derive(Clone)]
pub struct Func {
    pub entry: Id<Block>,
    pub ret: Vec<Repr>,
    pub blocks: Vec<Block>,
}

#[derive(Clone)]
pub struct Program {
    datas: Index<Repr>,
    funcs: Index<Func>,
    entry: Id<Func>,
}
