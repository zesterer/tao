use super::*;
use std::collections::VecDeque;

pub type InferMeta = (Span, TyVar);
pub type InferNode<T> = Node<T, InferMeta>;

#[derive(Clone, Debug, PartialEq)]
pub enum TyInfo {
    Ref(TyVar),
    Error,
    Unknown,
    Prim(ty::Prim),
    List(TyVar),
    Tuple(Vec<TyVar>),
    Record(HashMap<Ident, TyVar>),
    Func(TyVar, TyVar),
    Data(DataId, Vec<TyVar>),
    Gen(Ident, GenScopeId),
}

#[derive(Debug)]
pub enum InferError {
    Mismatch(TyVar, TyVar),
    CannotInfer(TyVar),
    NoSuchField(TyVar, SrcNode<Ident>),
    InvalidUnaryOp(SrcNode<ast::UnaryOp>, TyVar),
    InvalidBinaryOp(SrcNode<ast::BinaryOp>, TyVar, TyVar),
}

#[derive(Clone)]
enum Constraint {
    // (record, field_name, field)
    Access(TyVar, SrcNode<Ident>, TyVar),
    Unary(SrcNode<ast::UnaryOp>, TyVar, TyVar),
    Binary(SrcNode<ast::BinaryOp>, TyVar, TyVar, TyVar),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct TyVar(usize);

pub struct Infer<'a> {
    ctx: &'a mut Context,
    gen_scope: Option<GenScopeId>,
    vars: Vec<(Span, TyInfo)>,
    constraints: VecDeque<Constraint>,
    errors: Vec<InferError>,
}

impl<'a> Infer<'a> {
    pub fn new(ctx: &'a mut Context, gen_scope: Option<GenScopeId>) -> Self {
        Self {
            ctx,
            gen_scope,
            vars: Vec::new(),
            constraints: VecDeque::new(),
            errors: Vec::new(),
        }
    }

    pub fn ctx(&self) -> &Context { self.ctx }
    pub fn ctx_mut(&mut self) -> &mut Context { self.ctx }

    pub fn gen_scope(&self) -> Option<GenScopeId> {
        self.gen_scope
    }

    fn iter(&self) -> impl Iterator<Item = (TyVar, TyInfo)> + '_ {
        (0..self.vars.len())
            .map(|i| (TyVar(i), self.vars[i].1.clone()))
    }

    fn follow(&self, ty: TyVar) -> TyVar {
        match &self.vars[ty.0].1 {
            TyInfo::Ref(x) => self.follow(*x),
            _ => ty,
        }
    }

    fn set_info(&mut self, ty: TyVar, info: TyInfo) {
        match self.vars[ty.0].1.clone() {
            TyInfo::Ref(x) => self.set_info(x, info),
            _ => self.vars[ty.0].1 = info,
        }
    }

    fn info(&self, ty: TyVar) -> TyInfo {
        self.vars[ty.0].1.clone()
    }

    fn span(&self, ty: TyVar) -> Span {
        self.vars[ty.0].0
    }

    fn follow_info(&self, ty: TyVar) -> TyInfo {
        match &self.vars[ty.0].1 {
            TyInfo::Ref(x) => self.follow_info(*x),
            info => info.clone(),
        }
    }

    pub fn insert(&mut self, span: Span, info: TyInfo) -> TyVar {
        let id = TyVar(self.vars.len());
        self.vars.push((span, info));
        id
    }

    pub fn instantiate(&mut self, ty: TyId, f: &impl Fn(Ident, &Context) -> TyVar) -> TyVar {
        let info = match self.ctx.tys.get(ty) {
            Ty::Error => TyInfo::Error,
            Ty::Prim(prim) => TyInfo::Prim(prim),
            Ty::List(item) => TyInfo::List(self.instantiate(item, f)),
            Ty::Tuple(items) => TyInfo::Tuple(items
                .into_iter()
                .map(|item| self.instantiate(item, f))
                .collect()),
            Ty::Record(fields) => TyInfo::Record(fields
                .into_iter()
                .map(|(name, field)| (name, self.instantiate(field, f)))
                .collect()),
            Ty::Func(i, o) => TyInfo::Func(self.instantiate(i, f), self.instantiate(o, f)),
            Ty::Data(data, params) => TyInfo::Data(data, params
                .into_iter()
                .map(|param| self.instantiate(param, f))
                .collect()),
            Ty::Gen(name, scope) => TyInfo::Ref(f(name, self.ctx)), // TODO: Check scope is valid for recursive scopes
        };
        self.insert(self.ctx.tys.get_span(ty), info)
    }

    pub fn unknown(&mut self, span: Span) -> TyVar {
        self.insert(span, TyInfo::Unknown)
    }

    // pub fn usage(&mut self, span: Span, ty: TyVar) -> TyVar {
    //     self.insert(span, TyInfo::Ref(ty))
    // }

    pub fn make_access(&mut self, record: TyVar, field_name: SrcNode<Ident>, field: TyVar) {
        self.constraints.push_back(Constraint::Access(record, field_name, field));
    }

    pub fn make_unary(&mut self, op: SrcNode<ast::UnaryOp>, a: TyVar, output: TyVar) {
        self.constraints.push_back(Constraint::Unary(op, a, output));
    }

    pub fn make_binary(&mut self, op: SrcNode<ast::BinaryOp>, a: TyVar, b: TyVar, output: TyVar) {
        self.constraints.push_back(Constraint::Binary(op, a, b, output));
    }

    pub fn make_eq(&mut self, x: TyVar, y: TyVar) {
        if x == y { return } // If the vars are equal, we have no need to check equivalence
        match (self.info(x), self.info(y)) {
            // Follow references
            (TyInfo::Ref(x), _) => self.make_eq(x, y),
            (_, TyInfo::Ref(y)) => self.make_eq(x, y),
            (TyInfo::Error, _) => self.set_info(y, TyInfo::Ref(x)),
            (_, TyInfo::Error) => self.set_info(x, TyInfo::Ref(y)),
            (_, TyInfo::Unknown) => self.set_info(y, TyInfo::Ref(x)),
            (TyInfo::Unknown, _) => self.set_info(x, TyInfo::Ref(y)),
            (TyInfo::Prim(x), TyInfo::Prim(y)) if x == y => {},
            (TyInfo::List(x), TyInfo::List(y)) => self.make_eq(x, y),
            (TyInfo::Tuple(xs), TyInfo::Tuple(ys)) if xs.len() == ys.len() => xs
                .into_iter()
                .zip(ys.into_iter())
                .for_each(|(x, y)| self.make_eq(x, y)),
            (TyInfo::Record(xs), TyInfo::Record(ys)) if xs.len() == ys.len() && xs
                .keys()
                .all(|x| ys.contains_key(x)) => xs
                    .into_iter()
                    .for_each(|(x, x_ty)| self.make_eq(x_ty, ys[&x])),
            (TyInfo::Func(x_i, x_o), TyInfo::Func(y_i, y_o)) => {
                self.make_eq(x_i, y_i);
                self.make_eq(x_o, y_o);
            },
            (TyInfo::Data(x_data, xs), TyInfo::Data(y_data, ys)) if x_data == y_data &&
                xs.len() == ys.len() => xs
                    .into_iter()
                    .zip(ys.into_iter())
                    .for_each(|(x, y)| self.make_eq(x, y)),
            (TyInfo::Gen(a, a_scope), TyInfo::Gen(b, b_scope)) if a == a && a_scope == b_scope => {},
            (_, _) => {
                // self.set_info(x, TyInfo::Error);
                // self.set_info(y, TyInfo::Ref(x));
                self.errors.push(InferError::Mismatch(x, y));
            },
        }
    }

    fn resolve(&mut self, c: Constraint) -> Option<Result<(), InferError>> {
        use ast::{UnaryOp::*, BinaryOp::*};
        match c {
            Constraint::Access(record, field_name, field) => match self.follow_info(record) {
                TyInfo::Error => {
                    self.set_info(field, TyInfo::Error);
                    // Trying to access a field on an error type counts as success because we don't want to emit more
                    // errors than necessary.
                    Some(true)
                },
                TyInfo::Unknown => None,
                TyInfo::Record(fields) => if let Some(field_ty) = fields.get(&field_name) {
                    self.make_eq(*field_ty, field);
                    Some(true)
                } else {
                    Some(false)
                },
                _ => Some(false),
            }
                .map(|success| if success {
                    Ok(())
                } else {
                    self.set_info(field, TyInfo::Error);
                    Err(InferError::NoSuchField(record, field_name.clone()))
                }),
            Constraint::Unary(op, a, output) => match (&*op, self.follow_info(a)) {
                (_, TyInfo::Error) => Some(Ok(TyInfo::Error)),
                (_, TyInfo::Unknown) => None,
                (Neg, TyInfo::Prim(Prim::Num)) => Some(Ok(TyInfo::Prim(Prim::Num))),
                (Neg, TyInfo::Prim(Prim::Nat)) => Some(Ok(TyInfo::Prim(Prim::Int))),
                (Neg, TyInfo::Prim(Prim::Int)) => Some(Ok(TyInfo::Prim(Prim::Int))),
                (Not, TyInfo::Prim(Prim::Bool)) => Some(Ok(TyInfo::Prim(Prim::Bool))),
                _ => {
                    self.set_info(output, TyInfo::Error);
                    Some(Err(InferError::InvalidUnaryOp(op.clone(), a)))
                },
            }
                .map(|info| info.map(|info| {
                    // TODO: Use correct span
                    let result_ty = self.insert(op.span(), info);
                    self.make_eq(output, result_ty);
                })),
            Constraint::Binary(op, a, b, output) => match (&*op, self.follow_info(a), self.follow_info(b)) {
                (_, _, TyInfo::Error) => Some(Ok(TyInfo::Error)),
                (_, TyInfo::Error, _) => Some(Ok(TyInfo::Error)),
                (_, _, TyInfo::Unknown) => None,
                (_, TyInfo::Unknown, _) => None,
                (_, TyInfo::Prim(prim_a), TyInfo::Prim(prim_b)) => {
                    use ty::Prim::*;
                    lazy_static::lazy_static! {
                        static ref PRIM_BINARY_IMPLS: HashMap<(ast::BinaryOp, ty::Prim, ty::Prim), ty::Prim> = [
                            // Bool
                            ((Eq, Bool, Bool), Bool),
                            ((NotEq, Bool, Bool), Bool),
                            ((And, Bool, Bool), Bool),
                            ((Or, Bool, Bool), Bool),
                            ((Xor, Bool, Bool), Bool),

                            // Nat
                            ((Add, Nat, Nat), Nat),
                            ((Sub, Nat, Nat), Int),
                            ((Mul, Nat, Nat), Nat),
                            ((Rem, Nat, Nat), Nat),
                            ((Div, Nat, Nat), Num),
                            ((Eq, Nat, Nat), Bool),
                            ((NotEq, Nat, Nat), Bool),
                            ((Less, Nat, Nat), Bool),
                            ((LessEq, Nat, Nat), Bool),
                            ((More, Nat, Nat), Bool),
                            ((MoreEq, Nat, Nat), Bool),

                            // Int
                            ((Add, Int, Int), Int),
                            ((Sub, Int, Int), Int),
                            ((Mul, Int, Int), Int),
                            ((Div, Int, Int), Num),

                            // TODO: Others
                        ]
                            .into_iter()
                            .collect();
                    }

                    if let Some(out) = PRIM_BINARY_IMPLS.get(&(*op, prim_a, prim_b)) {
                        Some(Ok(TyInfo::Prim(*out)))
                    } else {
                        self.set_info(output, TyInfo::Error);
                        Some(Err(InferError::InvalidBinaryOp(op.clone(), a, b)))
                    }
                },
                (Join, TyInfo::List(a), TyInfo::List(b)) if self.follow_info(a) == self.follow_info(b) => Some(Ok(TyInfo::List(a))),
                _ => {
                    self.set_info(output, TyInfo::Error);
                    Some(Err(InferError::InvalidBinaryOp(op.clone(), a, b)))
                },
            }
                .map(|info| info.map(|info| {
                    // TODO: Use correct span
                    let result_ty = self.insert(self.span(output), info);
                    self.make_eq(output, result_ty);
                })),
        }
    }

    fn resolve_constraints(&mut self) {
        let mut tries = self.constraints.len();
        while let (Some(c), true) = (self.constraints.pop_front(), tries > 0) {
            tries -= 1;
            match self.resolve(c.clone()) {
                // Constraint resolved
                Some(res) => {
                    // Record any errors while resolving the constraint
                    if let Err(e) = res {
                        self.errors.push(e);
                    }
                    tries = self.constraints.len();
                },
                None => self.constraints.push_back(c), // Still unresolved...
            }
        }
    }

    pub fn into_checked(mut self) -> (Checked<'a>, Vec<Error>) {
        self.resolve_constraints();

        let mut errors = std::mem::take(&mut self.errors);

        // Report errors for types that cannot be inferred
        for (ty, info) in self.iter() {
            if matches!(info, TyInfo::Unknown) {
                errors.push(InferError::CannotInfer(ty));
            }
        }

        let mut checked = Checked { infer: self };

        let errors = errors
            .into_iter()
            .map(|error| match error {
                InferError::Mismatch(a, b) => Error::Mismatch(checked.reify(a), checked.reify(b)),
                InferError::CannotInfer(a) => Error::CannotInfer(checked.reify(a)),
                InferError::NoSuchField(a, field) => Error::NoSuchField(checked.reify(a), field),
                InferError::InvalidUnaryOp(op, a) => Error::InvalidUnaryOp(op, checked.reify(a), checked.infer.span(a)),
                InferError::InvalidBinaryOp(op, a, b) => Error::InvalidBinaryOp(op, checked.reify(a), checked.infer.span(a), checked.reify(b), checked.infer.span(b)),
            })
            .collect();

        (checked, errors)
    }
}

pub struct Checked<'a> { infer: Infer<'a> }

impl<'a> Checked<'a> {
    fn reify_inner(&mut self, var: TyVar) -> TyId {
        let ty = match self.infer.info(var) {
            // Follow references
            TyInfo::Ref(x) => return self.reify_inner(x),
            // Unknown types are treated as errors from here on out
            TyInfo::Error | TyInfo::Unknown => Ty::Error,
            TyInfo::Prim(prim) => Ty::Prim(prim),
            TyInfo::List(item) => Ty::List(self.reify_inner(item)),
            TyInfo::Tuple(items) => Ty::Tuple(items
                .into_iter()
                .map(|item| self.reify_inner(item))
                .collect()),
            TyInfo::Record(fields) => Ty::Record(fields
                .into_iter()
                .map(|(name, field)| (name, self.reify_inner(field)))
                .collect()),
            TyInfo::Func(i, o) => Ty::Func(self.reify_inner(i), self.reify_inner(o)),
            TyInfo::Data(data, args) => Ty::Data(data, args
                .into_iter()
                .map(|arg| self.reify_inner(arg))
                .collect()),
            TyInfo::Gen(name, scope) => Ty::Gen(name, scope),
        };
        self.infer.ctx.tys.insert(self.infer.span(var), ty)
    }

    pub fn reify(&mut self, ty: TyVar) -> TyId {
        self.reify_inner(ty)
    }
}
