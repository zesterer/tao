use super::*;

pub use ast::Literal as Literal;

pub trait Meta {
    type Ty;
    type Data: Clone + fmt::Debug;
    type Class: fmt::Debug;
    type Global: fmt::Debug;
    type Effect: fmt::Debug;
    type EffectInst;
}

impl Meta for InferMeta {
    type Ty = TyVar;
    type Data = SrcNode<DataId>;
    type Class = ClassVar;
    type Global = (DefId, Vec<Self>, Vec<EffectVar>);
    type Effect = EffectVar;
    type EffectInst = EffectInstVar;
}

impl Meta for TyMeta {
    type Ty = TyId;
    type Data = SrcNode<DataId>;
    type Class = Result<(ClassId, Vec<TyId>, Vec<Option<EffectId>>), ()>; // Required because we don't have proper error classes yet
    type Global = (DefId, Vec<Self>, Vec<Option<EffectId>>);
    type Effect = Option<EffectId>;
    type EffectInst = Result<EffectInst, ()>;
}

impl Meta for ConMeta {
    type Ty = ConTyId;
    type Data = ConDataId;
    type Class = !;
    type Global = ConProcId;
    type Effect = Vec<ConEffectId>;
    type EffectInst = ConEffectId;
}

#[derive(Clone, Debug)]
pub enum Pat<M: Meta> {
    Error,
    Wildcard,
    Literal(Literal),
    Single(Node<Binding<M>, M>),
    Add(Node<Binding<M>, M>, SrcNode<u64>),
    // (_, is_tuple)
    Record(BTreeMap<Ident, Node<Binding<M>, M>>, bool),
    ListExact(Vec<Node<Binding<M>, M>>),
    ListFront(Vec<Node<Binding<M>, M>>, Option<Node<Binding<M>, M>>),
    Decons(M::Data, Ident, Node<Binding<M>, M>),
}

impl<M: Meta> Pat<M> {
    pub fn tuple(i: impl IntoIterator<Item = Node<Binding<M>, M>>) -> Self {
        Self::Record(i
            .into_iter()
            .enumerate()
            .map(|(i, field)| (Ident::new(format!("{}", i)), field))
            .collect(), true)
    }
}

#[derive(Clone, Debug)]
pub struct Binding<M: Meta> {
    pub pat: SrcNode<Pat<M>>,
    pub name: Option<SrcNode<Ident>>,
}

pub type InferBinding = InferNode<Binding<InferMeta>>;
pub type TyBinding = TyNode<Binding<TyMeta>>;
pub type ConBinding = ConNode<Binding<ConMeta>>;

impl<M: Meta> Binding<M> {
    pub fn from_pat(pat: SrcNode<Pat<M>>) -> Self {
        Self { pat, name: None }
    }

    pub fn wildcard(name: SrcNode<Ident>) -> Self {
        Self { pat: SrcNode::new(hir::Pat::Wildcard, name.span()), name: Some(name) }
    }

    pub fn unit(span: Span) -> Self {
        Self { pat: SrcNode::new(hir::Pat::tuple([]), span), name: None }
    }
}

impl Binding<InferMeta> {
    pub fn get_binding_tys(self: &InferNode<Self>) -> Vec<(SrcNode<Ident>, TyVar)> {
        let mut bindings = Vec::new();
        self.visit_bindings_inner(&mut |name, (_, ty)| bindings.push((name.clone(), *ty)));
        bindings
    }
}

impl<M: Meta> Binding<M> {
    fn visit_bindings_inner(self: &Node<Self, M>, visit: &mut impl FnMut(&SrcNode<Ident>, &M)) {
        // TODO: Check for duplicates!
        if let Some(name) = &self.name { visit(name, self.meta()); };
        match &*self.pat {
            Pat::Error => {},
            Pat::Wildcard => {},
            Pat::Literal(_) => {},
            Pat::Single(inner) => inner.visit_bindings_inner(visit),
            Pat::Add(lhs, _) => lhs.visit_bindings_inner(visit),
            Pat::Record(fields, _) => fields
                .values()
                .for_each(|field| field.visit_bindings_inner(visit)),
            Pat::ListExact(items) => items
                .iter()
                .for_each(|item| item.visit_bindings_inner(visit)),
            Pat::ListFront(items, tail) => {
                items
                    .iter()
                    .for_each(|item| item.visit_bindings_inner(visit));
                if let Some(tail) = tail { tail.visit_bindings_inner(visit); }
            },
            Pat::Decons(_, _, inner) => inner.visit_bindings_inner(visit),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Intrinsic {
    TypeName,
    NegNat,
    NegInt,
    NegReal,
    DisplayInt,
    CodepointChar,
    EqChar,
    EqNat,
    LessNat,
    AddNat,
    AddInt,
    MulNat,
    MulInt,
    Go,
    Print,
    Input,
    Rand,
    LenList,
    SkipList,
    TrimList,
    JoinList,
    Propagate,
    Dispatch,
}

#[derive(Debug)]
pub enum Expr<M: Meta> {
    Error,
    Literal(Literal),
    // TODO: replace with `Item` when scoping is added
    Local(Ident),
    Global(M::Global),
    List(Vec<Node<Self, M>>, Vec<Node<Self, M>>),
    // (_, is_tuple)
    Record(BTreeMap<SrcNode<Ident>, Node<Self, M>>, bool),
    Access(Node<Self, M>, SrcNode<Ident>),
    // hidden_outer
    Match(bool, Node<Self, M>, Vec<(Node<Binding<M>, M>, Node<Self, M>)>),
    Func(Node<Ident, M>, Node<Self, M>),
    Apply(Node<Self, M>, Node<Self, M>),
    Cons(M::Data, Ident, Node<Self, M>),

    // member, class, field
    ClassAccess(M, M::Class, SrcNode<Ident>),

    Intrinsic(SrcNode<Intrinsic>, Vec<Node<Self, M>>),
    Update(Node<Self, M>, Vec<(SrcNode<Ident>, Node<Self, M>)>),

    // Blocks propagation of effects, collecting them
    // i.e: `@{ foo?; bar?; x }` gets type `foo + bar ~ X`
    Basin(M::Effect, Node<Self, M>),
    Suspend(M::EffectInst, Node<Self, M>),
    Handle {
        expr: Node<Self, M>,
        handlers: Vec<Handler<M>>,
    },
}

#[derive(Debug)]
pub struct Handler<M: Meta> {
    pub eff: M::EffectInst,
    pub send: Node<Ident, M>,
    pub state: Option<Node<Ident, M>>,
    pub recv: Node<Expr<M>, M>,
}

pub type InferExpr = InferNode<Expr<InferMeta>>;
pub type TyExpr = TyNode<Expr<TyMeta>>;
pub type ConExpr = ConNode<Expr<ConMeta>>;

impl Expr<InferMeta> {
    pub fn tuple(i: impl IntoIterator<Item = InferExpr>) -> Self {
        Self::Record(i
            .into_iter()
            .enumerate()
            .map(|(i, field)| (SrcNode::new(Ident::new(format!("{}", i)), field.meta().0), field))
            .collect(), true)
    }
}

impl<M: Meta> Expr<M> {
    pub fn for_children(&self, mut f: impl FnMut(&Node<Self, M>)) {
        match &self {
            Expr::Literal(_)
            | Expr::Error
            | Expr::Local(_)
            | Expr::ClassAccess(_, _, _)
            | Expr::Global(_) => {},
            Expr::List(items, tails) => {
                items
                    .iter()
                    .for_each(|item| f(item));
                tails
                    .iter()
                    .for_each(|tail| f(tail));
            },
            Expr::Record(fields, _) => fields
                .iter()
                .for_each(|(_, field)| f(field)),
            Expr::Access(record, _) => f(&record),
            Expr::Match(_, pred, arms) => {
                f(&pred);
                arms
                    .iter()
                    .for_each(|(_, arm)| f(arm));
            },
            Expr::Func(_, body) => f(&body),
            Expr::Apply(func, arg) => {
                f(&func);
                f(&arg);
            },
            Expr::Cons(_, _, inner) => f(&inner),
            Expr::Intrinsic(_, args) => args
                .iter()
                .for_each(|arg| f(arg)),
            Expr::Update(record, fields) => {
                f(&record);
                fields
                    .iter()
                    .for_each(|(_, field)| f(field));
            },
            Expr::Basin(_, inner) => f(&inner),
            Expr::Suspend(_, inner) => f(&inner),
            Expr::Handle { expr, handlers } => {
                f(&expr);
                handlers
                    .iter()
                    .for_each(|handler| f(&handler.recv));
            },
        }
    }
}

impl Expr<ConMeta> {
    fn required_locals_inner(&self, stack: &mut Vec<Ident>, required: &mut Vec<Ident>) {
        match self {
            Expr::Literal(_) | Expr::Error => {},
            Expr::Local(local) => {
                if !stack.contains(local) {
                    required.push(*local);
                }
            },
            Expr::Global(_) => {},
            Expr::Intrinsic(_, args) => args
                .iter()
                .for_each(|arg| arg.required_locals_inner(stack, required)),
            Expr::Match(_, pred, arms) => {
                pred.required_locals_inner(stack, required);
                for (arm, body) in arms {
                    let old_stack = stack.len();
                    arm.visit_bindings_inner(&mut |name, _| stack.push(**name));
                    body.required_locals_inner(stack, required);
                    stack.truncate(old_stack);
                }
            },
            Expr::Func(arg, body) => {
                stack.push(**arg);
                body.required_locals_inner(stack, required);
                stack.pop();
            },
            Expr::Apply(f, arg) => {
                f.required_locals_inner(stack, required);
                arg.required_locals_inner(stack, required);
            },
            Expr::Record(fields, _) => fields
                .iter()
                .for_each(|(_, field)| field.required_locals_inner(stack, required)),
            Expr::List(items, tails) => {
                items
                    .iter()
                    .for_each(|item| item.required_locals_inner(stack, required));
                tails
                    .iter()
                    .for_each(|tail| tail.required_locals_inner(stack, required));
            },
            Expr::Access(tuple, _) => tuple.required_locals_inner(stack, required),
            Expr::Cons(_, _, inner) => inner.required_locals_inner(stack, required),
            Expr::Access(inner, _) => inner.required_locals_inner(stack, required),
            Expr::ClassAccess(_, _, _) => {},
            Expr::Update(record, fields) => {
                record.required_locals_inner(stack, required);
                fields
                    .iter()
                    .for_each(|(_, field)| field.required_locals_inner(stack, required));
            },
            Expr::Basin(_, inner) => inner.required_locals_inner(stack, required),
            Expr::Suspend(_, inner) => inner.required_locals_inner(stack, required),
            Expr::Handle { expr, handlers } => {
                expr.required_locals_inner(stack, required);
                for Handler { send, recv, state, .. } in handlers {
                    let old_len = stack.len();
                    stack.push(**send);
                    if let Some(state) = state { stack.push(**state); }
                    recv.required_locals_inner(stack, required);
                    stack.truncate(old_len);
                }
            },
        }
    }

    pub fn required_locals(&self, already_has: impl IntoIterator<Item = Ident>) -> Vec<Ident> {
        let mut required = Vec::new();
        self.required_locals_inner(&mut already_has.into_iter().collect(), &mut required);
        required
    }
}
