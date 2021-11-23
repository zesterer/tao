use super::*;
use std::collections::VecDeque;

pub type InferMeta = (Span, TyVar);
pub type InferNode<T> = Node<T, InferMeta>;

#[derive(Clone, Debug, PartialEq)]
pub enum TyInfo {
    Ref(TyVar),
    Error,
    Unknown(Option<Span>), // With optional instantiation origin
    Prim(ty::Prim),
    List(TyVar),
    Tuple(Vec<TyVar>),
    Record(HashMap<Ident, TyVar>),
    Func(TyVar, TyVar),
    Data(DataId, Vec<TyVar>),
    Gen(usize, GenScopeId, Span),
}

#[derive(Debug)]
pub enum InferError {
    Mismatch(TyVar, TyVar),
    CannotInfer(TyVar, Option<Span>), // With optional instantiation origin
    Recursive(TyVar),
    NoSuchField(TyVar, SrcNode<Ident>),
    InvalidUnaryOp(SrcNode<ast::UnaryOp>, TyVar),
    InvalidBinaryOp(SrcNode<ast::BinaryOp>, TyVar, TyVar),
    RecursiveAlias(AliasId, TyVar, Span),
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

    pub fn instantiate(&mut self, ty: TyId, f: &impl Fn(usize, GenScopeId, &Context) -> TyVar) -> TyVar {
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
            Ty::Gen(index, scope) => TyInfo::Ref(f(index, scope, self.ctx)), // TODO: Check scope is valid for recursive scopes
        };
        self.insert(self.ctx.tys.get_span(ty), info)
    }

    pub fn unknown(&mut self, span: Span) -> TyVar {
        self.insert(span, TyInfo::Unknown(None))
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

    pub fn emit(&mut self, err: InferError) {
        self.errors.push(err);
    }

    // Returns true if `x` occurs in `y`.
    fn occurs_in(&self, x: TyVar, y: TyVar) -> bool {
        match self.info(y) {
            TyInfo::Unknown(_) | TyInfo::Error | TyInfo::Prim(_) | TyInfo::Gen(_, _, _) => false,
            TyInfo::Ref(y) => x == y || self.occurs_in(x, y),
            TyInfo::List(item) => x == item || self.occurs_in(x, item),
            TyInfo::Func(i, o) => x == i || x == o || self.occurs_in(x, i) || self.occurs_in(x, o),
            TyInfo::Tuple(ys) => ys
                .into_iter()
                .any(|y| x == y || self.occurs_in(x, y)),
            TyInfo::Record(ys) => ys
                .into_iter()
                .any(|(_, y)| x == y || self.occurs_in(x, y)),
            TyInfo::Data(_, ys) => ys
                .into_iter()
                .any(|y| x == y || self.occurs_in(x, y)),
        }
    }

    pub fn make_eq(&mut self, x: TyVar, y: TyVar) {
        if let Err((a, b)) = self.make_eq_inner(x, y) {
            // self.set_info(a, TyInfo::Error);
            // self.set_info(b, TyInfo::Error);
            self.errors.push(InferError::Mismatch(a, b));
        }
    }

    pub fn make_eq_inner(&mut self, x: TyVar, y: TyVar) -> Result<(), (TyVar, TyVar)> {
        if x == y { return Ok(()) } // If the vars are equal, we have no need to check equivalence
        match (self.info(x), self.info(y)) {
            // Follow references
            (TyInfo::Ref(x), _) => self.make_eq_inner(x, y),
            (_, TyInfo::Ref(y)) => self.make_eq_inner(x, y),

            // Unify unknown or erronoeus types
            (TyInfo::Unknown(_), _) => if self.occurs_in(x, y) {
                self.errors.push(InferError::Recursive(y));
                Ok(self.set_info(x, TyInfo::Error)) // TODO: Not actually ok
            } else {
                Ok(self.set_info(x, TyInfo::Ref(y)))
            },
            (_, TyInfo::Error) => Ok(self.set_info(x, TyInfo::Ref(y))),
            (TyInfo::Error, _) | (_, TyInfo::Unknown(_)) => self.make_eq_inner(y, x),

            (TyInfo::Prim(x), TyInfo::Prim(y)) if x == y => Ok(()),
            (TyInfo::List(x), TyInfo::List(y)) => self.make_eq_inner(x, y),
            (TyInfo::Tuple(xs), TyInfo::Tuple(ys)) if xs.len() == ys.len() => xs
                .into_iter()
                .zip(ys.into_iter())
                .try_for_each(|(x, y)| self.make_eq_inner(x, y)),
            (TyInfo::Record(xs), TyInfo::Record(ys)) if xs.len() == ys.len() && xs
                .keys()
                .all(|x| ys.contains_key(x)) => xs
                    .into_iter()
                    .try_for_each(|(x, x_ty)| self.make_eq_inner(x_ty, ys[&x])),
            (TyInfo::Func(x_i, x_o), TyInfo::Func(y_i, y_o)) => {
                self.make_eq_inner(x_i, y_i)?;
                self.make_eq_inner(x_o, y_o)?;
                Ok(())
            },
            (TyInfo::Data(x_data, xs), TyInfo::Data(y_data, ys)) if x_data == y_data &&
                xs.len() == ys.len() => xs
                    .into_iter()
                    .zip(ys.into_iter())
                    .try_for_each(|(x, y)| self.make_eq_inner(x, y)),
            (TyInfo::Gen(a, a_scope, _), TyInfo::Gen(b, b_scope, _)) if a == a && a_scope == b_scope => Ok(()),
            (_, _) => Err((x, y)),//self.errors.push(InferError::Mismatch(x, y)),
        }
    }

    /// Reinstantiate a type variable, replacing any known generic types with new unknown ones
    // TODO: Is this a good way to resolve the problem of type inference of recursive definitions in the presence of
    // polymorphism?
    pub fn try_reinstantiate(&mut self, span: Span, ty: TyVar) -> TyVar {
        match self.info(ty) {
            TyInfo::Ref(x) => self.try_reinstantiate(span, x),
            TyInfo::Unknown(_) | TyInfo::Error | TyInfo::Prim(_) => ty,
            TyInfo::List(item) => {
                let item = self.try_reinstantiate(span, item);
                self.insert(self.span(ty), TyInfo::List(item))
            },
            TyInfo::Func(i, o) => {
                let i = self.try_reinstantiate(span, i);
                let o = self.try_reinstantiate(span, o);
                self.insert(self.span(ty), TyInfo::Func(i, o))
            },
            // TODO: Reinstantiate type parameters with fresh type variables, but without creating inference problems
            TyInfo::Gen(x, _, origin) => ty,//self.insert(span, TyInfo::Unknown(Some(origin))),
            info => todo!("{:?}", info),
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
                TyInfo::Unknown(_) => None,
                TyInfo::Record(fields) => if let Some(field_ty) = fields.get(&field_name) {
                    self.make_eq(*field_ty, field);
                    Some(true)
                } else {
                    Some(false)
                },
                // Field access through a data type
                TyInfo::Data(data, params) => {
                    // TODO: Use `self.ctx.follow_field_access(...)` but work out how to instantiate type parameters
                    // throughout the chain.
                    let data = self.ctx.datas.get_data(data);
                    // Field access on data only works for single-variant, record datatypes
                    if let (Some((_, ty)), true) = (data.cons.iter().next(), data.cons.len() == 1) {
                        if let Ty::Record(fields) = self.ctx.tys.get(*ty) {
                            if let Some(field_ty) = fields.get(&field_name) {
                                let field_ty = self.instantiate(*field_ty, &|index, _, _| params[index]);
                                self.make_eq(field_ty, field);
                                Some(true)
                            } else {
                                Some(false)
                            }
                        } else {
                            Some(false)
                        }
                    } else {
                        Some(false)
                    }
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
                (_, TyInfo::Unknown(_)) => None,
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
                (_, _, TyInfo::Unknown(_)) => None,
                (_, TyInfo::Unknown(_), _) => None,
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
                            ((Eq, Int, Int), Bool),
                            ((NotEq, Int, Int), Bool),
                            ((Less, Int, Int), Bool),
                            ((LessEq, Int, Int), Bool),
                            ((More, Int, Int), Bool),
                            ((MoreEq, Int, Int), Bool),

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
            if let TyInfo::Unknown(origin) = info {
                errors.push(InferError::CannotInfer(ty, origin));
            }
        }

        let mut checked = Checked {
            cache: HashMap::default(),
            infer: self,
        };

        let errors = errors
            .into_iter()
            .map(|error| match error {
                InferError::Mismatch(a, b) => Error::Mismatch(checked.reify(a), checked.reify(b)),
                InferError::CannotInfer(a, origin) => Error::CannotInfer(checked.reify(a), origin),
                InferError::Recursive(a) => Error::Recursive(checked.infer.span(a)),
                InferError::NoSuchField(a, field) => Error::NoSuchField(checked.reify(a), field),
                InferError::InvalidUnaryOp(op, a) => Error::InvalidUnaryOp(op, checked.reify(a), checked.infer.span(a)),
                InferError::InvalidBinaryOp(op, a, b) => Error::InvalidBinaryOp(op, checked.reify(a), checked.infer.span(a), checked.reify(b), checked.infer.span(b)),
                InferError::RecursiveAlias(alias, a, span) => Error::RecursiveAlias(alias, checked.reify(a), span),
            })
            .collect();

        (checked, errors)
    }
}

pub struct Checked<'a> {
    cache: HashMap<TyVar, TyId>,
    infer: Infer<'a>,
}

impl<'a> Checked<'a> {
    fn reify_inner(&mut self, var: TyVar) -> TyId {
        if let Some(ty) = self.cache.get(&var) {
            return *ty;
        }

        let ty = match self.infer.info(var) {
            // Follow references
            TyInfo::Ref(x) => return self.reify_inner(x),
            // Unknown types are treated as errors from here on out
            TyInfo::Error | TyInfo::Unknown(_) => Ty::Error,
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
            TyInfo::Gen(name, scope, _) => Ty::Gen(name, scope),
        };
        self.infer.ctx.tys.insert(self.infer.span(var), ty)
    }

    pub fn reify(&mut self, var: TyVar) -> TyId {
        let ty = self.reify_inner(var);
        self.cache.insert(var, ty);
        ty
    }
}
