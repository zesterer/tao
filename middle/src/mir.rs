use super::*;

pub type MirMeta = ReprId;
pub type MirNode<T> = Node<T, MirMeta>;

// TODO: Keep track of scope, perhaps?
#[derive(Copy, Clone, Debug)]
pub struct LocalId(usize);

#[derive(Copy, Clone, Debug)]
pub struct GlobalId(usize);

#[derive(Debug)]
pub enum Literal {
    Nat(u64),
    Char(char),
    Bool(bool),
}

#[derive(Debug)]
pub enum Intrinsic {
    MakeList(ReprId, usize),
    NotBool,
    AddNat,
    AddInt,
    SubNat,
    SubInt,
}

#[derive(Debug)]
pub enum Pat {
    Wildcard,
    Literal(Expr), // Expression is evaluated and then compared
    Tuple(Vec<MirNode<Binding>>),
}

#[derive(Debug)]
pub struct Binding {
    pub pat: Pat,
    pub name: Option<LocalId>,
}

#[derive(Debug)]
pub enum Expr {
    Literal(Literal),
    Local(LocalId),
    Global(GlobalId),

    Intrinsic(Intrinsic, Vec<MirNode<Self>>),
    Match(MirNode<Self>, Vec<(MirNode<Binding>, MirNode<Self>)>),
    Apply(MirNode<Self>, MirNode<Self>),
    Access(MirNode<Self>, usize),

    MakeTuple(Vec<MirNode<Self>>),
    MakeFunc(MirNode<Binding>, MirNode<Self>),
    MakeVariant(usize, MirNode<Self>),
}
