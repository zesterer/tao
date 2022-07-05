//! The MIR is *not* correct (in the context of Tao's abstract machine) by construction. See the 'SAFETY' notes.

use super::*;
use std::{
    cell::Cell,
    fmt,
};

pub type EffectId = ConEffectId;

pub type MirMeta = Repr;
pub type MirNode<T> = Node<T, MirMeta>;

// TODO: Keep track of scope, perhaps?
#[derive(Copy, Clone, Debug)]
pub struct LocalId(usize);

#[derive(Clone, Debug, PartialEq)]
pub enum Const<U> {
    // Value has no possible inhabitants, even if the type of the value has inhabitants. For example, `x` in
    // `let x : Char = match never in` may have an inferred never value, even though `Char` has inhabitants.
    Never,
    // Value could not be inferred at compile-time
    Unknown(U),
    Nat(u64),
    Int(i64),
    Real(f64),
    Char(char),
    Tuple(Vec<Self>),
    List(Vec<Self>),
    Sum(usize, Box<Self>),
    Data(ConDataId, Box<Self>),
}

pub type Literal = Const<!>;
pub type Partial = Const<Option<Local>>;

impl<U: fmt::Debug + Clone> Const<U> {
    pub fn nat(&self) -> u64 { if let Const::Nat(x) = self { *x } else { panic!("{:?}", self) } }
    pub fn int(&self) -> i64 { if let Const::Int(x) = self { *x } else { panic!("{:?}", self) } }
    pub fn list(&self) -> Vec<Self> { if let Const::List(x) = self { x.clone() } else { panic!("{:?}", self) } }
}

impl Partial {
    pub fn to_literal(&self) -> Option<Literal> {
        match self {
            Self::Never => Some(Literal::Never),
            Self::Unknown(_) => None,
            Self::Nat(x) => Some(Literal::Nat(*x)),
            Self::Int(x) => Some(Literal::Int(*x)),
            Self::Real(x) => Some(Literal::Real(*x)),
            Self::Char(c) => Some(Literal::Char(*c)),
            Self::Tuple(fields) => Some(Literal::Tuple(fields
                .iter()
                .map(|field| field.to_literal())
                .collect::<Option<_>>()?)),
            Self::List(items) => Some(Literal::List(items
                .iter()
                .map(|item| item.to_literal())
                .collect::<Option<_>>()?)),
            Self::Sum(v, inner) => Some(Literal::Sum(*v, Box::new(inner.to_literal()?))),
            Self::Data(data, inner) => Some(Literal::Data(*data, Box::new(inner.to_literal()?))),
        }
    }

    pub fn or(self, other: Self) -> Self {
        match (self, other) {
            (x, Self::Never) => x,
            (Self::Never, y) => y,

            (Self::Unknown(a), Self::Unknown(b)) if a == b => Self::Unknown(a),
            (Self::Unknown(_), _) | (_, Self::Unknown(_)) => Self::Unknown(None),

            (Self::Nat(x), Self::Nat(y)) if x == y => Self::Nat(x),
            (Self::Int(x), Self::Int(y)) if x == y => Self::Int(x),
            (Self::Real(x), Self::Real(y)) if x == y => Self::Real(x),
            (Self::Char(x), Self::Char(y)) if x == y => Self::Char(x),
            (Self::Tuple(xs), Self::Tuple(ys)) => Self::Tuple(xs
                .into_iter()
                .zip(ys)
                .map(|(x, y)| x.or(y))
                .collect()),
            (Self::List(xs), Self::List(ys)) if xs.len() == ys.len() => Self::List(xs
                .into_iter()
                .zip(ys)
                .map(|(x, y)| x.or(y))
                .collect()),
            (Self::Sum(a, x), Self::Sum(b, y)) if a == b => Self::Sum(a, Box::new(x.or(*y))),
            (Self::Data(a, x), Self::Data(b, y)) if a == b => Self::Data(a, Box::new(x.or(*y))),
            _ => Self::Unknown(None),
        }
    }
}

impl Literal {
    pub fn to_partial(&self) -> Partial {
        match self {
            Self::Never => Partial::Never,
            Self::Unknown(x) => *x,
            Self::Nat(x) => Partial::Nat(*x),
            Self::Int(x) => Partial::Int(*x),
            Self::Real(x) => Partial::Real(*x),
            Self::Char(c) => Partial::Char(*c),
            Self::Tuple(fields) => Partial::Tuple(fields
                .iter()
                .map(|field| field.to_partial())
                .collect()),
            Self::List(items) => Partial::List(items
                .iter()
                .map(|item| item.to_partial())
                .collect()),
            Self::Sum(v, inner) => Partial::Sum(*v, Box::new(inner.to_partial())),
            Self::Data(data, inner) => Partial::Data(*data, Box::new(inner.to_partial())),
        }
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Never => write!(f, "!"),
            Self::Unknown(x) => *x,
            Self::Nat(x) => write!(f, "{}", x),
            Self::Int(x) => write!(f, "{}", x),
            Self::Real(x) => write!(f, "{}", x),
            Self::Char('\t') => write!(f, "'\\t'"),
            Self::Char('\n') => write!(f, "'\\n'"),
            Self::Char(c) => write!(f, "'{}'", c),
            Self::Tuple(xs) => write!(f, "({})", xs.iter().map(|x| format!("{},", x)).collect::<Vec<_>>().join(" ")),
            Self::List(xs) => write!(f, "[{}]", xs.iter().map(|x| format!("{}", x)).collect::<Vec<_>>().join(", ")),
            Self::Sum(v, x) => write!(f, "#{} {}", v, x),
            Self::Data(data, x) => write!(f, "{:?} {}", data, x),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Intrinsic {
    Debug,
    MakeList(Repr),
    NegNat,
    NegInt,
    NegReal,
    DisplayInt,
    CodepointChar,
    AddNat,
    AddInt,
    SubNat,
    SubInt,
    MulNat,
    MulInt,
    EqNat,
    EqInt,
    EqChar,
    NotEqNat,
    NotEqInt,
    NotEqChar,
    LessNat,
    LessInt,
    MoreNat,
    MoreInt,
    LessEqNat,
    LessEqInt,
    MoreEqNat,
    MoreEqInt,
    Join(Repr),
    Print,
    Input,
    UpdateField(usize),
    LenList,
    SkipList,
    TrimList,
    Suspend(EffectId),
    Propagate(Vec<EffectId>),
}

#[derive(Clone, Debug)]
pub enum Pat {
    Wildcard,
    Literal(Literal), // Expression is evaluated and then compared
    Single(MirNode<Binding>),
    Add(MirNode<Binding>, u64),
    Tuple(Vec<MirNode<Binding>>),
    ListExact(Vec<MirNode<Binding>>),
    ListFront(Vec<MirNode<Binding>>, Option<MirNode<Binding>>),
    Variant(usize, MirNode<Binding>),
    Data(ConDataId, MirNode<Binding>),
}

// Uniquely refer to locals *without* shadowing
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Local(pub usize);

impl Local {
    pub fn new() -> Self {
        use core::sync::atomic::{AtomicUsize, Ordering};
        static ID: AtomicUsize = AtomicUsize::new(0);
        Self(ID.fetch_add(1, Ordering::Relaxed))
    }
}

#[derive(Clone, Debug)]
pub struct Binding {
    pub pat: Pat,
    pub name: Option<Local>,
}

impl Binding {
    pub fn wildcard(name: impl Into<Option<Local>>) -> Self {
        Self {
            pat: Pat::Wildcard,
            name: name.into(),
        }
    }

    pub fn is_refutable(&self) -> bool {
        match &self.pat {
            Pat::Wildcard => false,
            Pat::Literal(c) => match c {
                Const::Tuple(fields) if fields.is_empty() => false,
                _ => true,
            },
            Pat::Single(inner) => inner.is_refutable(),
            Pat::Add(lhs, rhs) => *rhs > 0 || lhs.is_refutable(),
            Pat::Tuple(fields) => fields
                .iter()
                .any(|field| field.is_refutable()),
            Pat::ListExact(_) => true,
            Pat::ListFront(items, tail) => items.len() > 0 || tail.as_ref().map_or(false, |tail| tail.is_refutable()),
            Pat::Variant(_, _) => true, // TODO: Check number of variants
            Pat::Data(_, inner) => inner.is_refutable(),
        }
    }

    fn visit_bindings(self: &MirNode<Self>, mut bind: &mut impl FnMut(Local, &Repr)) {
        self.name.map(|name| bind(name, self.meta()));
        match &self.pat {
            Pat::Wildcard => {},
            Pat::Literal(_) => {},
            Pat::Single(inner) => inner.visit_bindings(bind),
            Pat::Add(lhs, _) => lhs.visit_bindings(bind),
            Pat::Tuple(fields) => fields
                .iter()
                .for_each(|field| field.visit_bindings(bind)),
            Pat::ListExact(items) => items
                .iter()
                .for_each(|item| item.visit_bindings(bind)),
            Pat::ListFront(items, tail) => {
                items
                    .iter()
                    .for_each(|item| item.visit_bindings(bind));
                tail.as_ref().map(|tail| tail.visit_bindings(bind));
            },
            Pat::Variant(_, inner) => inner.visit_bindings(bind),
            Pat::Data(_, inner) => inner.visit_bindings(bind),
        }
    }

    pub fn binding_names(self: &MirNode<Self>) -> Vec<Local> {
        let mut names = Vec::new();
        self.visit_bindings(&mut |name, _| names.push(name));
        names
    }

    pub fn bindings(self: &MirNode<Self>) -> Vec<(Local, Repr)> {
        let mut names = Vec::new();
        self.visit_bindings(&mut |name, repr| names.push((name, repr.clone())));
        names
    }

    pub fn binds(self: &MirNode<Self>) -> bool {
        let mut binds = false;
        self.visit_bindings(&mut |_, _| binds = true);
        binds
    }

    fn refresh_locals_inner(&mut self, stack: &mut Vec<(Local, Local)>) {
        if let Some(name) = self.name {
            let new_name = stack.iter().rev().find(|(old, _)| *old == name).expect("No such local").1;
            self.name = Some(new_name);
        }
        self.for_children_mut(|expr| expr.refresh_locals_inner(stack));
    }

    /// Returns `true` is might be inhabitant values that can match this pattern.
    pub fn has_matches(self: &MirNode<Self>, ctx: &Context) -> bool {
        if !ctx.reprs.has_inhabitants(self.meta()) {
            false
        } else {
            match &self.pat {
                Pat::Wildcard => true,
                Pat::Literal(_) => true,
                Pat::Single(inner) => inner.has_matches(ctx),
                Pat::Add(inner, _) => inner.has_matches(ctx),
                Pat::Tuple(xs) => xs
                    .iter()
                    .all(|x| x.has_matches(ctx)),
                Pat::ListExact(xs) => xs
                    .iter()
                    .all(|x| x.has_matches(ctx)),
                Pat::ListFront(xs, tail) => xs
                    .iter()
                    .all(|x| x.has_matches(ctx)) && tail
                        .as_ref()
                        .map_or(true, |tail| tail.has_matches(ctx)),
                Pat::Variant(_, inner) => inner.has_matches(ctx),
                Pat::Data(_, inner) => inner.has_matches(ctx),
            }
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub struct GlobalFlags {
    /// Determines whether a global reference may be inlined. By default this is `true`, but inlining is not permitted
    /// for recursive definitions.
    pub can_inline: bool,
}

impl Default for GlobalFlags {
    fn default() -> Self {
        Self {
            can_inline: true,
        }
    }
}

#[derive(Clone, Debug)]
pub enum Expr {
    /// SAFETY: this node must never be evaluated at run-time
    Undefined,
    Literal(Literal),
    /// SAFETY: The local *must* be available in the enclosing scope of the expression.
    Local(Local),
    Global(ProcId, Cell<GlobalFlags>),

    Intrinsic(Intrinsic, Vec<MirNode<Self>>),
    /// SAFETY: All possible *inhabitant* (i.e: values that can actually be generated at run-time) predicate values
    /// must be matched by at least one arm. If this is not the case, the compiler's output is undefined.
    Match(MirNode<Self>, Vec<(MirNode<Binding>, MirNode<Self>)>),

    Func(MirNode<Local>, MirNode<Self>),
    Apply(MirNode<Self>, MirNode<Self>),

    // Tail recursion
    Go(MirNode<Local>, MirNode<Self>, MirNode<Self>),

    Tuple(Vec<MirNode<Self>>),
    Access(MirNode<Self>, usize),
    List(Vec<MirNode<Self>>),

    Variant(usize, MirNode<Self>),
    AccessVariant(MirNode<Self>, usize), // Unsafely assume the value is a specific variant
    Data(ConDataId, MirNode<Self>),
    AccessData(MirNode<Self>, ConDataId),

    Basin(Vec<EffectId>, MirNode<Self>),
    Handle {
        expr: MirNode<Self>,
        handlers: Vec<Handler>,
    },
}

#[derive(Clone, Debug)]
pub struct Handler {
    pub eff: EffectId,
    pub send: MirNode<Local>,
    pub state: MirNode<Local>,
    pub recv: MirNode<Expr>,
}

impl Expr {
    pub fn required_globals(&self) -> HashSet<ProcId> {
        let mut globals = HashSet::new();
        self.required_globals_inner(&mut globals);
        globals
    }

    fn required_globals_inner(&self, globals: &mut HashSet<ProcId>) {
        if let Expr::Global(proc, _) = self {
            globals.insert(*proc);
        }

        self.for_children(|expr| expr.required_globals_inner(globals));
    }

    pub fn refresh_locals(&mut self) {
        let required = self.required_locals(None);
        // debug_assert_eq!(required.len(), 0, "Cannot refresh locals for an expression\n\n{}\n\nthat captures (required = {:?})", self.print(), required);
        self.refresh_locals_inner(&mut Vec::new());
    }

    fn refresh_locals_inner(&mut self, stack: &mut Vec<(Local, Local)>) {
        match self {
            Expr::Local(local) => {
                let new_local = stack.iter().rev().find(|(old, _)| old == local)
                    .unwrap_or_else(|| panic!("No such local ${} in {:?}", local.0, stack)).1;
                *local = new_local;
            },
            Expr::Match(pred, arms) => {
                pred.refresh_locals_inner(stack);
                for (binding, arm) in arms {
                    let old_stack = stack.len();
                    binding.visit_bindings(&mut |name, _| stack.push((name, Local::new())));

                    binding.refresh_locals_inner(stack);
                    arm.refresh_locals_inner(stack);
                    stack.truncate(old_stack);
                }
            },
            Expr::Func(arg, body) => {
                let new_arg = Local::new();
                stack.push((**arg, new_arg));
                body.refresh_locals_inner(stack);
                stack.pop();
                **arg = new_arg;
            },
            Expr::Go(next, body, init) => {
                init.refresh_locals_inner(stack);

                let new_init = Local::new();
                stack.push((**next, new_init));
                body.refresh_locals_inner(stack);
                stack.pop();
                **next = new_init;
            },
            Expr::Handle { expr, handlers } => {
                expr.refresh_locals_inner(stack);

                for Handler { eff, send, state, recv } in handlers {
                    let new_send = Local::new();
                    let new_state = Local::new();
                    let old_len = stack.len();
                    stack.push((**send, new_send));
                    stack.push((**state, new_state));
                    recv.refresh_locals_inner(stack);
                    stack.truncate(old_len);
                    **send = new_send;
                    **state = new_state;
                }
            },
            _ => self.for_children_mut(|expr| expr.refresh_locals_inner(stack)),
        }
    }

    fn required_locals_inner(&self, stack: &mut Vec<Local>, required: &mut Vec<Local>) {
        match self {
            Expr::Undefined => {},
            Expr::Literal(_) => {},
            Expr::Local(local) => {
                if !stack.contains(local) {
                    required.push(*local);
                }
            },
            Expr::Global(_, _) => {},
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
            Expr::Func(arg, body) => {
                stack.push(**arg);
                body.required_locals_inner(stack, required);
                stack.pop();
            },
            Expr::Go(next, body, init) => {
                init.required_locals_inner(stack, required);
                stack.push(**next);
                body.required_locals_inner(stack, required);
                stack.pop();
            },
            Expr::Apply(f, arg) => {
                f.required_locals_inner(stack, required);
                arg.required_locals_inner(stack, required);
            },
            Expr::Tuple(fields) => fields
                .iter()
                .for_each(|field| field.required_locals_inner(stack, required)),
            Expr::List(items) => items
                .iter()
                .for_each(|item| item.required_locals_inner(stack, required)),
            Expr::Access(tuple, _) => tuple.required_locals_inner(stack, required),
            Expr::Variant(_, inner) => inner.required_locals_inner(stack, required),
            Expr::AccessVariant(inner, _) => inner.required_locals_inner(stack, required),
            Expr::Data(_, inner) => inner.required_locals_inner(stack, required),
            Expr::AccessData(inner, _) => inner.required_locals_inner(stack, required),
            Expr::Basin(_, inner) => inner.required_locals_inner(stack, required),
            Expr::Handle { expr, handlers } => {
                expr.required_locals_inner(stack, required);
                for Handler { eff, send, state, recv } in handlers {
                    let old_len = stack.len();
                    stack.push(**send);
                    stack.push(**state);
                    recv.required_locals_inner(stack, required);
                    stack.truncate(old_len);
                }
            },
        }
    }

    pub fn required_locals(&self, already_has: impl IntoIterator<Item = Local>) -> Vec<Local> {
        let mut required = Vec::new();
        self.required_locals_inner(&mut already_has.into_iter().collect(), &mut required);
        required
    }

    // If an expression has no side effect, it can be optimised away if its result is unused
    pub fn may_have_effect(&self) -> bool {
        match self {
            Expr::Undefined => false,
            Expr::Literal(_) => false,
            Expr::Local(local) => false,
            Expr::Global(_, _) => false,
            Expr::Intrinsic(Intrinsic::Propagate(_), _) => true,
            Expr::Intrinsic(_, args) => args
                .iter()
                .any(|arg| arg.may_have_effect()),
            Expr::Match(pred, arms) => pred.may_have_effect() || arms
                .iter()
                .any(|(_, body)| body.may_have_effect()),
            Expr::Func(_, _) => false,
            Expr::Go(next, body, init) => init.may_have_effect() || body.may_have_effect(),
            Expr::Apply(f, arg) => f.may_have_effect() || arg.may_have_effect(),
            Expr::Tuple(fields) => fields
                .iter()
                .any(|field| field.may_have_effect()),
            Expr::List(items) => items
                .iter()
                .any(|item| item.may_have_effect()),
            Expr::Access(tuple, _) => tuple.may_have_effect(),
            Expr::Variant(_, inner) => inner.may_have_effect(),
            Expr::AccessVariant(inner, _) => inner.may_have_effect(),
            Expr::Data(_, inner) => inner.may_have_effect(),
            Expr::AccessData(inner, _) => inner.may_have_effect(),
            Expr::Basin(_, inner) => false,
            Expr::Handle { expr, handlers } => expr.may_have_effect() || handlers
                .iter()
                .any(|Handler { recv, .. }| recv.may_have_effect()),
        }
    }

    pub fn print<'a>(self: &'a MirNode<Self>) -> impl fmt::Display + 'a {
        struct DisplayBinding<'a>(&'a MirNode<Binding>, usize);

        impl<'a> fmt::Display for DisplayBinding<'a> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                if let Some(name) = self.0.name {
                    write!(f, "${}", name.0)?;
                    if let Pat::Wildcard = &self.0.pat {
                        return Ok(());
                    } else {
                        write!(f, " ~ ")?;
                    }
                }
                match &self.0.pat {
                    Pat::Wildcard => write!(f, "_"),
                    Pat::Literal(c) => write!(f, "{}", c),
                    Pat::Single(inner) => write!(f, "{}", DisplayBinding(inner, self.1)),
                    Pat::Variant(variant, inner) => write!(f, "#{} {}", variant, DisplayBinding(inner, self.1)),
                    Pat::ListExact(items) => write!(f, "[{}]", items.iter().map(|i| format!("{},", DisplayBinding(i, self.1 + 1))).collect::<Vec<_>>().join(" ")),
                    Pat::ListFront(items, tail) => write!(
                        f,
                        "[{} .. {}]",
                        items.iter().map(|i| format!("{},", DisplayBinding(i, self.1))).collect::<Vec<_>>().join(" "),
                        tail.as_ref().map(|tail| format!("{}", DisplayBinding(tail, self.1))).unwrap_or_default(),
                    ),
                    Pat::Tuple(fields) => write!(f, "({})", fields.iter().map(|f| format!("{},", DisplayBinding(f, self.1))).collect::<Vec<_>>().join(" ")),
                    Pat::Add(inner, n) => write!(f, "{} + {}", DisplayBinding(inner, self.1), n),
                    Pat::Data(data, inner) => write!(f, "{:?} {}", data, DisplayBinding(inner, self.1)),
                    pat => todo!("{:?}", pat),
                }
            }
        }

        struct DisplayExpr<'a>(&'a MirNode<Expr>, usize, bool);

        impl<'a> fmt::Display for DisplayExpr<'a> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                use Intrinsic::*;
                if self.2 {
                    write!(f, "{}", "    ".repeat(self.1))?;
                }
                match &**self.0 {
                    Expr::Local(local) => write!(f, "${}", local.0),
                    Expr::Global(global, _) => write!(f, "{:?}", global),
                    Expr::Literal(c) => write!(f, "{}", c),
                    Expr::Func(arg, body) => write!(f, "fn ${} =>\n{}", arg.0, DisplayExpr(body, self.1 + 1, true)),
                    Expr::Go(next, body, init) => write!(f, "go(${} => {}, {})", next.0, DisplayExpr(body, self.1, false), DisplayExpr(init, self.1, false)),
                    Expr::Apply(func, arg) => write!(f, "({})({})", DisplayExpr(func, self.1, false), DisplayExpr(arg, self.1, false)),
                    Expr::Variant(variant, inner) => write!(f, "#{} {}", variant, DisplayExpr(inner, self.1, false)),
                    Expr::Tuple(fields) => write!(f, "({})", fields.iter().map(|f| format!("{},", DisplayExpr(f, self.1, false))).collect::<Vec<_>>().join(" ")),
                    Expr::List(items) => write!(f, "[{}]", items.iter().map(|i| format!("{}", DisplayExpr(i, self.1 + 1, false))).collect::<Vec<_>>().join(", ")),
                    Expr::Intrinsic(NegNat | NegInt | NegReal, args) => write!(f, "- {}", DisplayExpr(&args[0], self.1, false)),
                    Expr::Intrinsic(DisplayInt, args) => write!(f, "@display_int({})", DisplayExpr(&args[0], self.1, false)),
                    Expr::Intrinsic(CodepointChar, args) => write!(f, "@codepoint_char({})", DisplayExpr(&args[0], self.1, false)),
                    Expr::Intrinsic(EqChar | EqNat | EqInt, args) => write!(f, "{} = {}", DisplayExpr(&args[0], self.1, false), DisplayExpr(&args[1], self.1, false)),
                    Expr::Intrinsic(AddNat | AddInt, args) => write!(f, "{} + {}", DisplayExpr(&args[0], self.1, false), DisplayExpr(&args[1], self.1, false)),
                    Expr::Intrinsic(SubNat | SubInt, args) => write!(f, "{} - {}", DisplayExpr(&args[0], self.1, false), DisplayExpr(&args[1], self.1, false)),
                    Expr::Intrinsic(MulNat | MulInt, args) => write!(f, "{} * {}", DisplayExpr(&args[0], self.1, false), DisplayExpr(&args[1], self.1, false)),
                    Expr::Intrinsic(LessNat, args) => write!(f, "{} < {}", DisplayExpr(&args[0], self.1, false), DisplayExpr(&args[1], self.1, false)),
                    Expr::Intrinsic(MoreNat, args) => write!(f, "{} > {}", DisplayExpr(&args[0], self.1, false), DisplayExpr(&args[1], self.1, false)),
                    Expr::Intrinsic(MoreEqNat, args) => write!(f, "{} >= {}", DisplayExpr(&args[0], self.1, false), DisplayExpr(&args[1], self.1, false)),
                    Expr::Intrinsic(LessEqNat, args) => write!(f, "{} <= {}", DisplayExpr(&args[0], self.1, false), DisplayExpr(&args[1], self.1, false)),
                    Expr::Intrinsic(Join(_), args) => write!(f, "{} ++ {}", DisplayExpr(&args[0], self.1, false), DisplayExpr(&args[1], self.1, false)),
                    Expr::Intrinsic(Print, args) => write!(f, "@print({}, {})", DisplayExpr(&args[0], self.1, false), DisplayExpr(&args[1], self.1, false)),
                    Expr::Intrinsic(Input, args) => write!(f, "@input({})", DisplayExpr(&args[0], self.1, false)),
                    Expr::Intrinsic(UpdateField(idx), args) => write!(f, "@update_field<{}>({}, {})", idx, DisplayExpr(&args[0], self.1, false), DisplayExpr(&args[1], self.1, false)),
                    Expr::Intrinsic(LenList, args) => write!(f, "@len_list({})", DisplayExpr(&args[0], self.1, false)),
                    Expr::Intrinsic(SkipList, args) => write!(f, "@skip_list({}, {})", DisplayExpr(&args[0], self.1, false), DisplayExpr(&args[1], self.1, false)),
                    Expr::Intrinsic(TrimList, args) => write!(f, "@trim_list({}, {})", DisplayExpr(&args[0], self.1, false), DisplayExpr(&args[1], self.1, false)),
                    Expr::Intrinsic(Suspend(_), args) => write!(f, "@suspend({})", DisplayExpr(&args[0], self.1, false)),
                    Expr::Intrinsic(Propagate(_), args) => write!(f, "{}!", DisplayExpr(&args[0], self.1, false)),
                    Expr::Match(pred, arms) if arms.len() == 1 => {
                        let (arm, body) = &arms[0];
                        write!(f, "let {} = {} in\n{}", DisplayBinding(arm, self.1 + 1), DisplayExpr(pred, self.1, false), DisplayExpr(body, self.1, true))
                    },
                    Expr::Match(pred, arms) => {
                        write!(f, "match {} in", DisplayExpr(pred, self.1 + 1, false))?;
                        for (i, (arm, body)) in arms.iter().enumerate() {
                            let start = if i + 1 == arms.len() { '\\' } else { '|' };
                            write!(f, "\n{}{} {} => {}", "    ".repeat(self.1 + 1), start, DisplayBinding(arm, self.1 + 1), DisplayExpr(body, self.1 + 1, false))?;
                        }
                        if arms.len() == 0 {
                            write!(f, " (no arms)")?;
                        }
                        Ok(())
                    },
                    Expr::Basin(eff, inner) => write!(f, "{{\n{}\n{}}}", DisplayExpr(inner, self.1 + 1, true), "    ".repeat(self.1)),
                    Expr::Handle { expr, handlers } => write!(
                        f,
                        "{}\n{}",
                        DisplayExpr(expr, self.1, false),
                        handlers
                            .iter()
                            .map(|Handler { eff, send, state, recv }| format!(
                                "{}handle {:?} with ${}, ${} =>\n{}",
                                "    ".repeat(self.1 + 1),
                                eff,
                                send.0,
                                state.0,
                                DisplayExpr(recv, self.1 + 1, true),
                            ))
                            .collect::<Vec<_>>()
                            .join("\n"),
                    ),
                    Expr::Access(inner, field) => write!(f, "({}).{}", DisplayExpr(inner, self.1, false), field),
                    Expr::AccessVariant(inner, variant) => write!(f, "({}).#{}", DisplayExpr(inner, self.1, false), variant),
                    Expr::Data(data, inner) => write!(f, "{:?} {}", data, DisplayExpr(inner, self.1, false)),
                    Expr::AccessData(inner, data) => write!(f, "{}.#{:?}", DisplayExpr(inner, self.1, false), data),
                    // _ => write!(f, "<TODO>"),
                    expr => todo!("{:?}", expr),
                }
            }
        }

        DisplayExpr(self, 0, true)
    }
}
