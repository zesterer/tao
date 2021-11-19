use super::*;

pub type MirMeta = Repr;
pub type MirNode<T> = Node<T, MirMeta>;

// TODO: Keep track of scope, perhaps?
#[derive(Copy, Clone, Debug)]
pub struct LocalId(usize);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Const {
    Nat(u64),
    Int(i64),
    Char(char),
    Bool(bool),
    Str(Intern<String>),
    Tuple(Vec<Self>),
}

impl Const {
    pub fn nat(&self) -> u64 { if let Const::Nat(x) = self { *x } else { panic!() } }
    pub fn int(&self) -> i64 { if let Const::Int(x) = self { *x } else { panic!() } }
    pub fn bool(&self) -> bool { if let Const::Bool(x) = self { *x } else { panic!() } }
}

#[derive(Clone, Debug)]
pub enum Intrinsic {
    MakeList(usize),
    NotBool,
    AddNat,
    AddInt,
    SubNat,
    SubInt,
    MulNat,
    MulInt,
    LessNat,
    LessInt,
    MoreNat,
    MoreInt,
    LessEqNat,
    LessEqInt,
    MoreEqNat,
    MoreEqInt,
}

#[derive(Clone, Debug)]
pub enum Pat {
    Wildcard,
    Const(Const), // Expression is evaluated and then compared
    Tuple(Vec<MirNode<Binding>>),
}

#[derive(Clone, Debug)]
pub struct Binding {
    pub pat: Pat,
    pub name: Option<Ident>,
}

impl Binding {
    pub fn is_refutable(&self) -> bool {
        match &self.pat {
            Pat::Wildcard => false,
            Pat::Const(_) => true,
            Pat::Tuple(fields) => fields
                .iter()
                .any(|field| field.is_refutable()),
        }
    }

    fn visit_bindings(&self, mut bind: &mut impl FnMut(Ident)) {
        self.name.map(&mut bind);
        match &self.pat {
            Pat::Wildcard => {},
            Pat::Const(_) => {},
            Pat::Tuple(fields) => fields
                .iter()
                .for_each(|field| field.visit_bindings(bind)),
        }
    }

    pub fn binding_names(&self) -> Vec<Ident> {
        let mut names = Vec::new();
        self.visit_bindings(&mut |name| names.push(name));
        names
    }

    pub fn binds(&self) -> bool {
        let mut binds = false;
        self.visit_bindings(&mut |_| binds = true);
        binds
    }
}

#[derive(Clone, Debug)]
pub enum Expr {
    Const(Const),
    Local(Ident),
    Global(ProcId),

    Intrinsic(Intrinsic, Vec<MirNode<Self>>),
    Match(MirNode<Self>, Vec<(MirNode<Binding>, MirNode<Self>)>),

    Func(Vec<Ident>, Ident, MirNode<Self>),
    Apply(MirNode<Self>, MirNode<Self>),

    Tuple(Vec<MirNode<Self>>),
    Access(MirNode<Self>, usize),

    Variant(usize, MirNode<Self>),
}

impl Expr {
    fn required_locals_inner(&self, stack: &mut Vec<Ident>, required: &mut Vec<Ident>) {
        match self {
            Expr::Const(_) => {},
            Expr::Local(local) => {
                if !stack.contains(local) {
                    required.push(*local);
                }
            },
            Expr::Global(_) => {},
            Expr::Intrinsic(_, args) => args
                .iter()
                .for_each(|arg| arg.required_locals_inner(stack, required)),
            Expr::Match(pred, arms) => {
                pred.required_locals_inner(stack, required);
                for (arm, body) in arms {
                    let old_stack = stack.len();
                    stack.append(&mut arm.binding_names());

                    body.required_locals_inner(stack, required);

                    stack.truncate(old_stack);
                }
            },
            Expr::Func(captures, arg, body) => {
                for capture in captures {
                    if !stack.contains(capture) {
                        required.push(*capture);
                    }
                }

                let old_stack = stack.len();
                stack.extend(captures.iter().copied());
                stack.push(*arg);

                body.required_locals_inner(stack, required);

                stack.truncate(old_stack);
            },
            Expr::Apply(f, arg) => {
                f.required_locals_inner(stack, required);
                arg.required_locals_inner(stack, required);
            },
            Expr::Tuple(fields) => fields
                .iter()
                .for_each(|field| field.required_locals_inner(stack, required)),
            Expr::Access(tuple, _) => tuple.required_locals_inner(stack, required),
            Expr::Variant(_, inner) => inner.required_locals_inner(stack, required),
        }
    }

    pub fn required_locals(&self, already_has: impl IntoIterator<Item = Ident>) -> Vec<Ident> {
        let mut required = Vec::new();
        self.required_locals_inner(&mut already_has.into_iter().collect(), &mut required);
        required
    }
}
