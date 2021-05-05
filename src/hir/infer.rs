use super::{Span, Primitive, Ctx, Ty, TyId, UnaryOp, BinaryOp, Ident, data::DataId};
use crate::{Error, ErrorCode};
use std::{fmt, cmp::PartialEq};
use hashbrown::{HashSet, HashMap};

#[derive(Copy, Clone, Debug)]
pub enum EquateReason {
    Other,
    Conditional,
    List,
}

#[derive(Debug)]
enum TyError {
    /// Two type variables could not be equated to one-another because they are different types.
    CannotEquate(TyVar, TyVar, Span, EquateReason),
    /// A type variable could not flowed into another because it is not a subtype of the other. In other words, this
    /// error implies that instances of the former are not valid instances of the latter.
    CannotFlow(TyVar, TyVar, Span),
    /// The type of a type variable could not be inferred from available information.
    CannotInfer(TyVar, HashSet<TyVar>, HashSet<TyVar>),
    /// The type expands to one of infinite size (i.e: is self-referential). To resolve this, a data type is best used.
    InfiniteType(TyVar),
    /// The type solver has reached its maximum iteration limit and will no longer proceed.
    SolverLimitReached,
    /// No subtype relationship between types.
    NoSubtype(TyVar, TyVar, Span),
    /// No supertype relationship between types.
    NoSupertype(TyVar, TyVar, Span),
    /// Cannot resolve a unary operation upon a type variable.
    CannotResolveUnary(TyVar, UnaryOp, TyVar, Span),
    /// Cannot resolve a binary operation upon type variables.
    CannotResolveBinary(TyVar, TyVar, BinaryOp, TyVar, Span),
    /// Cannot resolve a field access operation upon a type variable.
    CannotResolveField(TyVar, Ident, TyVar, Span),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct TyVar(usize);

#[derive(Debug)]
pub enum TyInfo {
    Error,
    Unknown {
        subs: HashSet<TyVar>,
        sups: HashSet<TyVar>,
    },
    Prim(Primitive),
    Func(TyVar, TyVar),
    List(TyVar),
    Tuple(Vec<TyVar>),
    Record(HashMap<Ident, TyVar>),
    Data(DataId, Vec<TyVar>),
}

#[derive(Clone, Debug)]
pub enum Constraint {
    Eq(TyVar, TyVar, Span, EquateReason),
    Flow(TyVar, TyVar, Span),
    Unary { a: TyVar, op: UnaryOp, out: TyVar, span: Span },
    Binary { a: TyVar, b: TyVar, op: BinaryOp, out: TyVar, span: Span },
    Field { a: TyVar, field: Ident, out: TyVar, span: Span },
}

#[derive(Debug)]
pub enum Entry {
    Ref(TyVar),
    Info(TyInfo),
}

pub struct InferCtx<'a> {
    vars: Vec<Entry>,
    errors: Vec<bool>,
    spans: Vec<Span>,
    constraints: Vec<Constraint>,
    ctx: &'a mut Ctx,
}

impl<'a> InferCtx<'a> {
    pub fn from_ctx(ctx: &'a mut Ctx) -> Self {
        Self {
            vars: Vec::new(),
            errors: Vec::new(),
            spans: Vec::new(),
            constraints: Vec::new(),
            ctx,
        }
    }

    pub fn print_constraints(&self) {
        for c in &self.constraints {
            println!("{:?}", c);
        }
    }

    pub fn emit_error(&mut self, error: Error) {
        self.ctx.emit_error(error);
    }

    fn var_and_info_inner(&self, var: TyVar, until: &[TyVar]) -> (TyVar, &TyInfo) {
        static FAIL: TyInfo = TyInfo::Error;

        match &self.vars[var.0] {
            &Entry::Ref(x) if until.contains(&x) => {
                (x, &FAIL)
            },
            &Entry::Ref(x) => self.var_and_info_inner(x, until),
            Entry::Info(info) => (var, info),
        }
    }

    fn var_and_info(&self, var: TyVar) -> (TyVar, &TyInfo) {
        self.var_and_info_inner(var, &[var])
    }

    pub fn info(&self, var: TyVar) -> &TyInfo {
        self.var_and_info(var).1
    }

    pub fn is_known(&self, var: TyVar) -> bool {
        match self.info(var) {
            TyInfo::Error | TyInfo::Unknown { .. } => false,
            _ => true,
        }
    }

    /// Create a new type variable with the provided type information.
    pub fn info_var(&mut self, info: TyInfo, span: Span) -> TyVar {
        let id = self.vars.len();
        debug_assert_eq!(id, self.vars.len());
        debug_assert_eq!(id, self.errors.len());
        debug_assert_eq!(id, self.spans.len());
        self.vars.push(Entry::Info(info));
        self.errors.push(false);
        self.spans.push(span);
        TyVar(id)
    }

    /// Retrieve the span of a type variable.
    pub fn span(&self, var: TyVar) -> Span {
        self.spans[var.0]
    }

    /// Create a new type variable, pre-primed as an error.
    pub fn error_var(&mut self, span: Span) -> TyVar {
        self.info_var(TyInfo::Error, span)
    }

    /// Create a new free type variable (i.e: a type variable with no type hint).
    pub fn free_var(&mut self, span: Span) -> TyVar {
        self.info_var(TyInfo::Unknown { subs: HashSet::new(), sups: HashSet::new() }, span)
    }

    /// Force one type variable to be equal to another.
    pub fn unify_eq_reason(&mut self, a: TyVar, b: TyVar, span: Span, reason: EquateReason) {
        self.constraints.push(Constraint::Eq(a, b, span, reason));
    }

    /// Force one type variable to be equal to another.
    pub fn unify_eq(&mut self, a: TyVar, b: TyVar, span: Span) {
        self.unify_eq_reason(a, b, span, EquateReason::Other);
    }

    /// Force one type variable to flow to another (i.e: the first type must be a subtype of the second).
    pub fn unify_flow(&mut self, a: TyVar, b: TyVar, span: Span) {
        self.constraints.push(Constraint::Flow(a, b, span));
    }

    pub fn unary(&mut self, a: TyVar, op: UnaryOp, out: TyVar, span: Span) {
        self.constraints.push(Constraint::Unary { a, op, out, span });
    }

    pub fn binary(&mut self, a: TyVar, b: TyVar, op: BinaryOp, out: TyVar, span: Span) {
        self.constraints.push(Constraint::Binary { a, b, op, out, span });
    }

    pub fn field(&mut self, a: TyVar, field: Ident, out: TyVar, span: Span) {
        self.constraints.push(Constraint::Field { a, field, out, span });
    }

    fn occurs_in(&self, var: TyVar, tree: TyVar) -> bool {
        use TyInfo::*;
        if tree == var {
            true
        } else {
            match self.var_and_info_inner(tree, &[tree, var]).1 {
                &Func(i, o) => self.occurs_in(var, i) || self.occurs_in(var, i),
                &List(i) => self.occurs_in(var, i),
                Tuple(xs) => xs.iter().any(|&x| self.occurs_in(var, x)),
                _ => false,
            }
        }
    }

    /// Attempt to solve an equality constraint.
    fn solve_eq(
        &mut self,
        a: TyVar,
        b: TyVar,
        span: Span,
        reason: EquateReason,
        mut add_constraint: impl FnMut(Constraint),
    ) -> Option<Result<(), TyError>> {
        // println!("Equating {:?} :: {:?}", (a, b), (self.info(a), self.info(b)));

        use TyInfo::*;
        let ((a, a_info), (b, b_info)) = (self.var_and_info(a), self.var_and_info(b));
        match (a_info, b_info) {
            // We lack information for one of the types. This doesn't mean we can solve the equality, but it does mean
            // that we've not yet discovered a contradiction, so we simply assume that the equality holds until a
            // contradition is discovered elsewhere.
            _ if self.errors[a.0] || self.errors[b.0] => Some(Ok(())),
            (Error, _) | (_, Error) => Some(Ok(())),
            (_, Unknown { subs, sups }) => Some(Ok({
                for &sub in subs.iter() { add_constraint(Constraint::Flow(sub, a, span)); }
                for &sup in sups.iter() { add_constraint(Constraint::Flow(a, sup, span)); }
                self.spans[a.0] = self.spans[a.0].or(self.spans[b.0]);
                self.spans[b.0] = self.spans[b.0].or(self.spans[a.0]);
                if !self.occurs_in(b, a) {
                    self.vars[b.0] = Entry::Ref(a);
                }
            })),
            (Unknown { .. }, _) => self.solve_eq(b, a, span, reason, add_constraint),
            // Everything else is pretty self-explanatory
            (Prim(a), Prim(b)) if a == b => Some(Ok(())),
            (&Func(ai, ao), &Func(bi, bo)) => {
                add_constraint(Constraint::Eq(ai, bi, span, reason));
                add_constraint(Constraint::Eq(ao, bo, span, reason));
                Some(Ok(()))
            },
            (&List(a), &List(b)) => Some(Ok(add_constraint(Constraint::Eq(a, b, span, reason)))),
            (Tuple(a), Tuple(b)) if a.len() == b.len() => Some(Ok(a
                .iter()
                .zip(b.iter())
                .for_each(|(&a, &b)| add_constraint(Constraint::Eq(a, b, span, reason))))),
            (Record(a), Record(b)) if a.keys().all(|a| b.contains_key(a)) && b.keys().all(|b| a.contains_key(b)) => {
                for (a, a_var) in a {
                    add_constraint(Constraint::Eq(*a_var, b[a], span, reason));
                }
                Some(Ok(()))
            },
            (Data(a_data, a), Data(b_data, b)) if a_data == b_data && a.len() == b.len() => Some(Ok(a
                .iter()
                .zip(b.iter())
                .for_each(|(&a, &b)| add_constraint(Constraint::Eq(a, b, span, reason))))),
            _ => {
                self.errors[a.0] = true;
                self.errors[b.0] = true;
                Some(Err(TyError::CannotEquate(a, b, span, reason)))
            },
        }
    }

    /// Attempt to solve a flow constraint.
    fn solve_flow(
        &mut self,
        a: TyVar,
        b: TyVar,
        span: Span,
        mut add_constraint: impl FnMut(Constraint),
    ) -> Option<Result<(), TyError>> {
        //println!("Flowing {:?} :: {:?}", (a, b), (self.info(a), self.info(b)));

        use TyInfo::*;
        let ((a, a_info), (b, b_info)) = (self.var_and_info(a), self.var_and_info(b));
        match (a_info, b_info) {
            // Errors always resolve because we don't want to produce phantom errors
            _ if self.errors[a.0] || self.errors[b.0] => Some(Ok(())),
            (Error, _) | (_, Error) => Some(Ok(())),
            // If we don't have enough information about these types to determine whether this subtyping relationship
            // is valid yet. Instead, we just update the sub/super type constraints.
            (Unknown { sups: a_sups, subs: a_subs }, Unknown { sups: b_sups, subs: b_subs }) => {
                let mut a_subs = a_subs.clone();
                let mut b_sups = b_sups.clone();
                // TODO: Is this needed?
                if let Entry::Info(Unknown { sups, .. }) = &mut self.vars[a.0] { sups.insert(b); sups.extend(b_sups.into_iter()); }
                if let Entry::Info(Unknown { subs, .. }) = &mut self.vars[b.0] { subs.insert(a); subs.extend(a_subs.into_iter()); }
                None
            },
            (Unknown { .. }, _) | (_, Unknown { .. }) => {
                if let Entry::Info(Unknown { sups, .. }) = &mut self.vars[a.0] { sups.insert(b); }
                if let Entry::Info(Unknown { subs, .. }) = &mut self.vars[b.0] { subs.insert(a); }
                None
            },
            // Primitives may flow into one-another if the former is less general than the latter (e.g `Nat` -> `Int`).
            (&Prim(a), &Prim(b)) if a.subtype_of(b) => Some(Ok(())),
            // Function outputs are as expected, but function parameters are 'contravariant', meaning that the
            // subtyping relationship is reversed. Consider a function A of type Nat -> Nat and a function B of type
            // Int -> Int. We wish to flow A into B. For the return type, this is fine. All natural numbers are valid
            // integers. But what about the parameters? Subtyping relationships imply that the subtype may be
            // substituted for the super type, but substituting A for B weakens the capability of the function: it goes
            // from accepting all integers as input to only accepting natural numbers. To solve this problem, we have
            // to invert the subtyping relationship for the input parameter. This is known as contravariance.
            (&Func(ai, ao), &Func(bi, bo)) => {
                add_constraint(Constraint::Flow(bi, ai, span)); // Function parameters are contravariant
                add_constraint(Constraint::Flow(ao, bo, span));
                Some(Ok(()))
            },
            // Everything else is pretty self-explanatory
            (&List(a), &List(b)) => Some(Ok(add_constraint(Constraint::Flow(a, b, span)))),
            (Tuple(a), Tuple(b)) if a.len() == b.len() => Some(Ok(a
                    .iter()
                    .zip(b.iter())
                    .for_each(|(&a, &b)| add_constraint(Constraint::Flow(a, b, span))))),
            (Record(a), Record(b)) if b.keys().all(|b| a.contains_key(b)) => Some(Ok(b
                    .iter()
                    .for_each(|(b, b_var)| add_constraint(Constraint::Flow(a[b], *b_var, span))))),
            // TODO: Query global subtyping state to figure out what constraints need to be applied to the type to
            // satisfy subtyping rules (this sounds hard). Until this is implemented, we just force an equality
            // relationship between the data IDs: it's trivial to check and it'll work for most cases we care about for
            // now. The only disadvantage is that it doesn't allow us to specify subtyping relationships between
            // different datatypes, but that's probably not something we care about at this stage.
            // TODO: This might is not sound. Consider a data type:
            //
            // Foo A
            //
            // This subtyping rule permits `Foo Nat` to coerce into `Foo Num`. However, what if the definition of `Foo`
            // looks like this?
            //
            // data Foo A B = A -> Num;
            //
            // It's not valid to coerce `Nat -> Num` to `Num -> Num`. To resolve this, we need some
            // automatically-inferred notion of a type's variance.
            // See: https://rustc-dev-guide.rust-lang.org/variance.html
            /*
            (Data(a_data, a), Data(b_data, b)) if a_data == b_data && a.len() == b.len() => Some(Ok(a
                .iter()
                .zip(b.iter())
                .for_each(|(&a, &b)| add_constraint(Constraint::Flow(a, b, span))))),
            */
            // Because the above is unsound, just enforce equality even though this is a flow relationship. The real
            // solution is to figure out how to do variance with datatypes properly.
            (Data(a_data, a), Data(b_data, b)) if a_data == b_data && a.len() == b.len() => Some(Ok(a
                .iter()
                .zip(b.iter())
                .for_each(|(&a, &b)| add_constraint(Constraint::Eq(a, b, span, EquateReason::Other))))),
            _ => {
                self.errors[a.0] = true;
                self.errors[b.0] = true;
                Some(Err(TyError::CannotFlow(a, b, span)))
            },
        }
    }

    /// Attempt to solve a unary operation constraint.
    fn solve_unary(
        &mut self,
        a: TyVar,
        op: UnaryOp,
        out: TyVar,
        span: Span,
        mut add_constraint: impl FnMut(Constraint),
    ) -> Option<Result<(), TyError>> {
        // println!("Unary {} => {:?} :: {:?}", op, (a, out), (self.info(a), self.info(out)));

        use TyInfo::*;
        use Primitive::*;
        let (a, a_info) = self.var_and_info(a);
        match (a_info, op)  {
            (Error, _) => Some(Ok(())),
            (Unknown { .. }, _) => None,
            (Prim(Nat), UnaryOp::Neg) => Some(Ok(add_constraint(Constraint::Eq(self.info_var(TyInfo::Prim(Primitive::Int), self.span(out)), out, span, EquateReason::Other)))),
            (Prim(Int), UnaryOp::Neg) => Some(Ok(add_constraint(Constraint::Eq(a, out, span, EquateReason::Other)))),
            (Prim(Num), UnaryOp::Neg) => Some(Ok(add_constraint(Constraint::Eq(a, out, span, EquateReason::Other)))),
            (Prim(Bool), UnaryOp::Not) => Some(Ok(add_constraint(Constraint::Eq(a, out, span, EquateReason::Other)))),
            _ => {
                self.errors[a.0] = true;
                self.errors[out.0] = true;
                Some(Err(TyError::CannotResolveUnary(a, op, out, span)))
            },
        }
    }

    /// Attempt to solve a binary operation constraint.
    fn solve_binary(
        &mut self,
        a: TyVar,
        b: TyVar,
        op: BinaryOp,
        out: TyVar,
        span: Span,
        mut add_constraint: impl FnMut(Constraint),
    ) -> Option<Result<(), TyError>> {
        //println!("Flowing {:?} :: {:?}", (a, b), (self.info(a), self.info(b)));

        use TyInfo::*;
        use Primitive::*;
        let ((a, a_info), (b, b_info)) = (self.var_and_info(a), self.var_and_info(b));
        match (a_info, op, b_info) {
            (Error, _, _) | (_, _, Error) => {
                self.errors[a.0] = true;
                self.errors[b.0] = true;
                self.errors[out.0] = true;
                Some(Ok(()))
            },
            (Unknown { .. }, _, _) | (_, _, Unknown { .. }) => None,
            // Arithmetic
            (
                Prim(a),
                BinaryOp::Add | BinaryOp::Sub
                | BinaryOp::Mul | BinaryOp::Div,
                Prim(b),
            ) if a.numerical_rank().is_some() && b.numerical_rank().is_some() => {
                let sup = a.supertype(*b).unwrap(); // Arithmetic produces the super type of operands
                let sup = if op == BinaryOp::Sub { // Subtraction operations can result in negatives, so must be at least Int
                    sup.supertype(Primitive::Int).unwrap()
                } else {
                    sup
                };
                let sup = if op == BinaryOp::Div { // Division operations can result in fractions, so must be at least Num
                    sup.supertype(Primitive::Num).unwrap()
                } else {
                    sup
                };
                Some(Ok(add_constraint(Constraint::Eq(self.info_var(TyInfo::Prim(sup), self.span(out)), out, span, EquateReason::Other))))
            },
            // Numerical comparison
            (
                Prim(a),
                BinaryOp::Eq | BinaryOp::NotEq
                | BinaryOp::Less | BinaryOp::LessEq
                | BinaryOp::More | BinaryOp::MoreEq,
                Prim(b),
            ) if a.numerical_rank().is_some() && b.numerical_rank().is_some()
                => Some(Ok(add_constraint(Constraint::Eq(self.info_var(TyInfo::Prim(Primitive::Bool), span), out, span, EquateReason::Other)))),
            // Binary comparison
            (Prim(Bool), BinaryOp::Eq, Prim(Bool))
            | (Prim(Bool), BinaryOp::NotEq, Prim(Bool))
            | (Prim(Bool), BinaryOp::And, Prim(Bool))
            | (Prim(Bool), BinaryOp::Or, Prim(Bool))
                => Some(Ok(add_constraint(Constraint::Eq(self.info_var(TyInfo::Prim(Primitive::Bool), span), out, span, EquateReason::Other)))),
            // List joins
            (&List(a), BinaryOp::Join, &List(b)) => {
                let item = self.free_var(span);
                // add_constraint(Constraint::Eq(a, b, span, EquateReason::List));
                add_constraint(Constraint::Flow(a, item, span));
                add_constraint(Constraint::Flow(b, item, span));
                add_constraint(Constraint::Eq(self.info_var(TyInfo::List(item), span), out, span, EquateReason::List));
                Some(Ok(()))
            },
            _ => {
                self.errors[a.0] = true;
                self.errors[b.0] = true;
                self.errors[out.0] = true;
                Some(Err(TyError::CannotResolveBinary(a, b, op, out, span)))
            },
        }
    }

    /// Attempt to solve a field access constraint.
    fn solve_field(
        &mut self,
        a: TyVar,
        field: Ident,
        out: TyVar,
        span: Span,
        mut add_constraint: impl FnMut(Constraint),
    ) -> Option<Result<(), TyError>> {
        // println!("Unary {} => {:?} :: {:?}", op, (a, out), (self.info(a), self.info(out)));

        use TyInfo::*;
        use Primitive::*;
        let (a, a_info) = self.var_and_info(a);

        match a_info {
            Error => Some(Ok(())),
            Unknown { .. } => None,
            Record(fields) if fields.contains_key(&field) => {
                add_constraint(Constraint::Eq(out, fields[&field], span, EquateReason::Other));
                Some(Ok(()))
            },
            _ => {
                self.errors[a.0] = true;
                self.errors[out.0] = true;
                Some(Err(TyError::CannotResolveField(a, field, out, span)))
            },
        }
    }

    fn vars_eq(&self, a: TyVar, b: TyVar) -> bool {
        self.var_and_info(a).0 == self.var_and_info(b).0
    }

    // Ok(var) => found a general type
    // Err(None) => Found an unknown type, can't usefully proceed but not an error
    // Err(Some(a, b)) => Failed to find compatible supertype for a and b
    fn find_general_type(&mut self, head: TyVar, mut tail: impl Iterator<Item = TyVar>, span: Span, mut add_constraint: impl FnMut(Constraint), assume_unknown: bool) -> Result<TyVar, Option<(TyVar, TyVar)>> {
        use TyInfo::*;
        tail.try_fold(head, |a_var, b_var| {
            Ok(match (self.info(a_var), self.info(b_var)) {
                _ if a_var == b_var => a_var,
                // Errors are considered most general
                (Error, _) => a_var,
                (_, Error) => b_var,
                (Unknown { .. }, _) if assume_unknown => b_var,
                (_, Unknown { .. }) if assume_unknown => a_var,
                // No useful progress can be made if either is unknown
                (Unknown { .. }, _) | (_, TyInfo::Unknown { .. }) => return Err(None),
                (Prim(a), Prim(b)) => if a.subtype_of(*b) {
                    b_var
                } else if b.subtype_of(*a) {
                    a_var
                } else {
                    return Err(Some((a_var, b_var)));
                },
                (&Func(ai, ao), &Func(bi, bo)) => {
                    let new_i = if !self.vars_eq(ai, bi) {
                        let new_i = self.free_var(span);
                        add_constraint(Constraint::Flow(new_i, ai, span)); // Contravariant
                        add_constraint(Constraint::Flow(new_i, bi, span)); // Contravariant
                        new_i
                    } else {
                        ai
                    };
                    let new_o = if !self.vars_eq(ao, bo) {
                        let new_o = self.free_var(span);
                        add_constraint(Constraint::Flow(ao, new_o, span)); // Contravariant
                        add_constraint(Constraint::Flow(bo, new_o, span)); // Contravariant
                        new_o
                    } else {
                        ao
                    };
                    self.info_var(TyInfo::Func(new_i, new_o), span)
                },
                (&List(a), &List(b)) => {
                    if !self.vars_eq(a, b) {
                        let new_item = self.free_var(span);
                        add_constraint(Constraint::Flow(a, new_item, span));
                        add_constraint(Constraint::Flow(b, new_item, span));
                        self.info_var(TyInfo::List(new_item), span)
                    } else {
                        a_var
                    }
                },
                (Tuple(a), Tuple(b)) if a.len() == b.len() => {
                    let a = a.clone();
                    let b = b.clone();
                    let mut fields = (0..a.len())
                        .map(|_| self.free_var(span))
                        .collect::<Vec<_>>();
                    for (i, a) in a.into_iter().enumerate() {
                        add_constraint(Constraint::Flow(a, fields[i], span));
                    }
                    for (i, b) in b.into_iter().enumerate() {
                        add_constraint(Constraint::Flow(b, fields[i], span));
                    }
                    self.info_var(TyInfo::Tuple(fields), span)
                },
                (Record(a), Record(b)) => {
                    let fields = a
                        .iter()
                        .map(|(name, var)| (*name, *var))
                        .filter(|(a, _)| b.contains_key(a))
                        .collect();
                    for (name, &var) in &fields {
                        add_constraint(Constraint::Flow(a[name], var, span));
                        add_constraint(Constraint::Flow(b[name], var, span));
                    }
                    self.info_var(TyInfo::Record(fields), span)
                },
                // TODO: Others
                _ => {
                    return Err(Some((a_var, b_var)))
                    // add_constraint(Constraint::Eq(a_var, b_var, span, EquateReason::Other));
                    // a_var
                },
            })
        })
    }

    // Ok(var) => found a common type
    // Err(None) => Found an unknown type, can't usefully proceed but not an error
    // Err(Some(a, b)) => Failed to find compatible subtype for a and b
    fn find_common_type(&mut self, head: TyVar, mut tail: impl Iterator<Item = TyVar>, span: Span, mut add_constraint: impl FnMut(Constraint), assume_unknown: bool) -> Result<TyVar, Option<(TyVar, TyVar)>> {
        use TyInfo::*;
        tail.try_fold(head, |a_var, b_var| {
            Ok(match (self.info(a_var), self.info(b_var)) {
                _ if a_var == b_var => a_var,
                // Errors are considered least general
                (Error, _) => a_var,
                (_, Error) => b_var,
                (Unknown { .. }, _) if assume_unknown => b_var,
                (_, Unknown { .. }) if assume_unknown => a_var,
                // No useful progress can be made if either is unknown
                (Unknown { .. }, _) | (_, Unknown { .. }) => return Err(None),
                (Prim(a), Prim(b)) => if a.subtype_of(*b) {
                    a_var
                } else if b.subtype_of(*a) {
                    b_var
                } else {
                    return Err(Some((a_var, b_var)));
                },
                (&Func(ai, ao), &Func(bi, bo)) => {
                    let new_i = if !self.vars_eq(ai, bi) {
                        let new_i = self.free_var(span);
                        add_constraint(Constraint::Flow(ai, new_i, span)); // Contravariant
                        add_constraint(Constraint::Flow(bi, new_i, span)); // Contravariant
                        new_i
                    } else {
                        ai
                    };
                    let new_o = if !self.vars_eq(ao, bo) {
                        let new_o = self.free_var(span);
                        add_constraint(Constraint::Flow(new_o, ao, span)); // Contravariant
                        add_constraint(Constraint::Flow(new_o, bo, span)); // Contravariant
                        new_o
                    } else {
                        ao
                    };
                    self.info_var(TyInfo::Func(new_i, new_o), span)
                },
                (&List(a), &List(b)) => {
                    if !self.vars_eq(a, b) {
                        let new_item = self.free_var(span);
                        add_constraint(Constraint::Flow(new_item, a, span));
                        add_constraint(Constraint::Flow(new_item, b, span));
                        self.info_var(TyInfo::List(new_item), span)
                    } else {
                        a_var
                    }
                },
                (Tuple(a), Tuple(b)) if a.len() == b.len() => {
                    let a = a.clone();
                    let b = b.clone();
                    let mut fields = (0..a.len())
                        .map(|_| self.free_var(span))
                        .collect::<Vec<_>>();
                    for (i, a) in a.into_iter().enumerate() {
                        add_constraint(Constraint::Flow(fields[i], a, span));
                    }
                    for (i, b) in b.into_iter().enumerate() {
                        add_constraint(Constraint::Flow(fields[i], b, span));
                    }
                    self.info_var(TyInfo::Tuple(fields), span)
                },
                (Record(a), Record(b)) => {
                    // TODO: Don't clone
                    let a = a.clone();
                    let b = b.clone();
                    let fields = a
                        .keys()
                        .chain(b.keys())
                        .map(|name| (*name, self.free_var(span)))
                        .collect::<HashMap<_, _>>();

                    for (name, &var) in a.iter().chain(b.iter()) {
                        add_constraint(Constraint::Flow(fields[name], var, span));
                    }

                    self.info_var(TyInfo::Record(fields), span)
                },
                // TODO: Others
                _ => {
                    return Err(Some((a_var, b_var)))
                    // add_constraint(Constraint::Eq(a_var, b_var, span, EquateReason::Other));
                    // a_var
                },
            })
        })
    }

    /// Attempt to derive a type from the subtype and super type constraints placed upon it.
    fn try_derive_from_sub(
        &mut self,
        var: TyVar,
        mut add_constraint: impl FnMut(Constraint),
        assume_unknown: bool,
    ) -> Option<Result<(), TyError>> {
        let (var, info) = self.var_and_info(var);

        if self.errors[var.0] {
            return None;
        }

        use TyInfo::*;
        match info {
            Unknown { subs, sups } => {
                let subs = subs.clone();

                let span = self.span(var);

                let (sub_var, sub_constraints) = {
                    let mut sub_constraints = Vec::new();
                    let mut add_constraint = |c| sub_constraints.push(c);
                    let mut subs_iter = subs.clone().into_iter();
                    (match subs_iter.next().map(|head| self.find_general_type(head, subs_iter, span, &mut add_constraint, assume_unknown)).map(|v| v.and_then(|v| if self.is_known(v) { Ok(v) } else { Err(None) })) {
                        None => None,
                        Some(Ok(most_general)) => Some(most_general),
                        Some(Err(None)) => None,
                        Some(Err(Some((a, b)))) => {
                            let a = self.var_and_info(a).0;
                            let b = self.var_and_info(b).0;
                            let var = self.var_and_info(var).0;
                            if !self.errors[a.0] && !self.errors[b.0] {
                                self.errors[var.0] = true;
                                self.errors[a.0] = true;
                                self.errors[b.0] = true;
                                return Some(Err(TyError::NoSupertype(a, b, span)))
                            } else {
                                None
                            }
                        },
                    }, sub_constraints)
                };

                if let Some(new_var) = sub_var {
                    if !self.errors[new_var.0] && !self.errors[var.0] {
                        // println!("Assumption made! {} == {}", self.display(var, &[], None), self.display(new_var, &[], None));
                        // println!("Subs({}) = {:?}", subs.len(), subs.iter().map(|s| self.display(*s, &[], None).to_string()).collect::<Vec<_>>());
                        add_constraint(Constraint::Eq(var, new_var, self.span(var), EquateReason::Other));

                        for c in sub_constraints {
                            add_constraint(c);
                        }

                        Some(Ok(()))
                    } else {
                        None
                    }
                } else {
                    None
                }
            },
            _ => None,
        }
    }

    /// Attempt to derive a type from the subtype and super type constraints placed upon it.
    fn try_derive_from_sup(
        &mut self,
        var: TyVar,
        mut add_constraint: impl FnMut(Constraint),
        assume_unknown: bool,
    ) -> Option<Result<(), TyError>> {
        let (var, info) = self.var_and_info(var);

        if self.errors[var.0] {
            return None;
        }

        use TyInfo::*;
        match info {
            Unknown { subs, sups } => {
                let sups = sups.clone();

                let span = self.span(var);

                let (sup_var, sup_constraints) = {
                    let mut sup_constraints = Vec::new();
                    let mut add_constraint = |c| sup_constraints.push(c);
                    // TODO: Is inferring from supertypes okay?
                    let mut sups_iter = sups.clone().into_iter();
                    (match sups_iter.next().map(|head| self.find_common_type(head, sups_iter, span, &mut add_constraint, assume_unknown)).map(|v| v.and_then(|v| if self.is_known(v) { Ok(v) } else { Err(None) })) {
                        None => None,
                        Some(Ok(least_general)) => Some(least_general),
                        Some(Err(None)) => None,
                        Some(Err(Some((a, b)))) => {
                            let a = self.var_and_info(a).0;
                            let b = self.var_and_info(b).0;
                            let var = self.var_and_info(var).0;
                            if !self.errors[a.0] && !self.errors[b.0] {
                                self.errors[var.0] = true;
                                self.errors[a.0] = true;
                                self.errors[b.0] = true;
                                return Some(Err(TyError::NoSubtype(a, b, span)))
                            } else {
                                None
                            }
                        },
                    }, sup_constraints)
                };

                if let Some(new_var) = sup_var {
                    if !self.errors[var.0] && !self.errors[new_var.0] {
                        add_constraint(Constraint::Eq(var, new_var, self.span(var), EquateReason::Other));

                        for c in sup_constraints {
                            add_constraint(c);
                        }

                        Some(Ok(()))
                    } else {
                        None
                    }
                } else {
                    None
                }
            },
            _ => None,
        }
    }

    /// Reconstruct a type from a type variable.
    fn reconstruct(
        &mut self,
        cannot_infer: &mut HashMap<TyVar, (HashSet<TyVar>, HashSet<TyVar>)>,
        infinite_tys: &mut HashSet<TyVar>,
        seen: &mut Vec<TyVar>,
        var: TyVar,
    ) -> TyId {
        let seen_len = seen.len();
        let ty = if seen.contains(&var) {
            infinite_tys.insert(var);
            Ty::Error
        } else {
            seen.push(var);

            use TyInfo::*;
            let (var, info) = self.var_and_info(var);
            match info {
                Unknown { subs, sups } => {
                    if !self.errors[var.0] && !subs.iter().any(|s| cannot_infer.contains_key(s)) && !sups.iter().any(|s| cannot_infer.contains_key(s)) {
                        cannot_infer.insert(self.var_and_info(var).0, (subs.clone(), sups.clone()));
                    }
                    Ty::Error
                },
                Error => Ty::Error,
                &Prim(p) => Ty::Prim(p),
                &Func(i, o) => Ty::Func(
                    self.reconstruct(cannot_infer, infinite_tys, seen, i),
                    self.reconstruct(cannot_infer, infinite_tys, seen, o),
                ),
                &List(item) => Ty::List(self.reconstruct(cannot_infer, infinite_tys, seen, item)),
                Tuple(fields) => Ty::Tuple(fields
                    .clone()
                    .into_iter()
                    .map(|field| self.reconstruct(cannot_infer, infinite_tys, seen, field))
                    .collect()),
                Record(fields) => Ty::Record(fields
                    .clone()
                    .into_iter()
                    .map(|(name, field)| (name, self.reconstruct(cannot_infer, infinite_tys, seen, field)))
                    .collect()),
                Data(id, params) => Ty::Data(*id, params
                    .clone()
                    .into_iter()
                    .map(|param| self.reconstruct(cannot_infer, infinite_tys, seen, param))
                    .collect()),
            }
        };

        seen.truncate(seen_len);

        self.ctx.ty.insert(ty)
    }

    /// Attempt to solve all the constraints provided to this context.
    fn solve_inner(&mut self) -> (SolvedTys, Vec<TyError>) {
        const ITER_LIMIT: usize = 1_000_000;

        let mut errors = Vec::new();
        let mut eq_constraints = Vec::new();
        let mut flow_constraints = Vec::new();
        let mut op_constraints = Vec::new();
        let mut stale_constraints = Vec::new();

        // for c in &self.constraints {
        //     println!("{:?}", c);
        // }

        // Insert constraints according to their solve order
        for c in std::mem::take(&mut self.constraints) {
            match &c {
                Constraint::Eq(_, _, _, _) => eq_constraints.push(c),
                Constraint::Flow(_, _, _) => flow_constraints.push(c),
                Constraint::Unary { .. } => op_constraints.push(c),
                Constraint::Binary { .. } => op_constraints.push(c),
                Constraint::Field { .. } => op_constraints.push(c),
            }
        }

        // Keep solving until either no constraints are left to solve, or no further progress can be made
        let mut i = 0;
        loop {
            if i == ITER_LIMIT {
                errors.push(TyError::SolverLimitReached);
                break;
            } else {
                i += 1;
            }

            let next_constraint = eq_constraints.pop()
                .or_else(|| flow_constraints.pop())
                .or_else(|| op_constraints.pop());

            let mut add_constraint = |c| match &c {
                Constraint::Eq(_, _, _, _) => eq_constraints.push(c),
                Constraint::Flow(_, _, _) => flow_constraints.push(c),
                Constraint::Unary { .. } => op_constraints.push(c),
                Constraint::Binary { .. } => op_constraints.push(c),
                Constraint::Field { .. } => op_constraints.push(c),
            };

            if let Some(c) = next_constraint {
                //println!("{:?}", c);
                match match &c {
                    &Constraint::Eq(a, b, span, reason) => self.solve_eq(a, b, span, reason, &mut add_constraint),
                    &Constraint::Flow(a, b, span) => self.solve_flow(a, b, span, &mut add_constraint),
                    &Constraint::Unary { a, op, out, span } => self.solve_unary(a, op, out, span, &mut add_constraint),
                    &Constraint::Binary { a, b, op, out, span } => self.solve_binary(a, b, op, out, span, &mut add_constraint),
                    &Constraint::Field { a, field, out, span } => self.solve_field(a, field, out, span, &mut add_constraint),
                } {
                    // Successfully proving something means that we might be able to prove other things as a result, so
                    // consider all proofs to no longer be stale. TODO: Be smarter about this. We don't want to be
                    // reconsidering unsolvable things.
                    Some(Ok(())) => std::mem::take(&mut stale_constraints)
                        .into_iter()
                        .for_each(|c| match &c {
                            Constraint::Eq(_, _, _, _) => eq_constraints.push(c),
                            Constraint::Flow(_, _, _) => flow_constraints.push(c),
                            Constraint::Unary { .. } => op_constraints.push(c),
                            Constraint::Binary { .. } => op_constraints.push(c),
                            Constraint::Field { .. } => op_constraints.push(c),
                        }),
                    Some(Err(e)) => errors.push(e),
                    // Failing to gain new information about a proof adds that proof back into the list of things to
                    // be proved, thereby increasing the number of stale proofs.
                    None => stale_constraints.push(c),
                }
            } else if (0..self.vars.len())
                .into_iter()
                .map(TyVar)
                // Attempt to find the type of a var from its subtypes (i.e: the things that flow into it)
                .any(|var| match self.try_derive_from_sub(var, &mut add_constraint, false) {
                    Some(Ok(())) => true,
                    Some(Err(e)) => { errors.push(e); self.errors[var.0] = true; false },
                    None => false,
                })
            {
                // Reconsider all proofs if we managed to derive any types
                // TODO: Maybe don't invalidate all proofs? This seems a bit silly.
                std::mem::take(&mut stale_constraints)
                    .into_iter()
                    .for_each(|c| match &c {
                        Constraint::Eq(_, _, _, _) => eq_constraints.push(c),
                        Constraint::Flow(_, _, _) => flow_constraints.push(c),
                        Constraint::Unary { .. } => op_constraints.push(c),
                        Constraint::Binary { .. } => op_constraints.push(c),
                        Constraint::Field { .. } => op_constraints.push(c),
                    });
            } else if (0..self.vars.len())
                .into_iter()
                .map(TyVar)
                // Attempt to find the type of a var from its supertypes (i.e: the things that it flows into)
                .any(|var| match self.try_derive_from_sup(var, &mut add_constraint, false) {
                    Some(Ok(())) => true,
                    Some(Err(e)) => { errors.push(e); self.errors[var.0] = true; false },
                    None => false,
                })
            {
                // Reconsider all proofs if we managed to derive any types
                // TODO: Maybe don't invalidate all proofs? This seems a bit silly.
                std::mem::take(&mut stale_constraints)
                    .into_iter()
                    .for_each(|c| match &c {
                        Constraint::Eq(_, _, _, _) => eq_constraints.push(c),
                        Constraint::Flow(_, _, _) => flow_constraints.push(c),
                        Constraint::Unary { .. } => op_constraints.push(c),
                        Constraint::Binary { .. } => op_constraints.push(c),
                        Constraint::Field { .. } => op_constraints.push(c),
                    });
            } else if (0..self.vars.len())
                .into_iter()
                .map(TyVar)
                // Attempt to find the type of a var from its subtypes (i.e: the things that flow into it)
                .any(|var| match self.try_derive_from_sub(var, &mut add_constraint, true) {
                    Some(Ok(())) => true,
                    Some(Err(e)) => { errors.push(e); self.errors[var.0] = true; false },
                    None => false,
                })
            {
                // Reconsider all proofs if we managed to derive any types
                // TODO: Maybe don't invalidate all proofs? This seems a bit silly.
                std::mem::take(&mut stale_constraints)
                    .into_iter()
                    .for_each(|c| match &c {
                        Constraint::Eq(_, _, _, _) => eq_constraints.push(c),
                        Constraint::Flow(_, _, _) => flow_constraints.push(c),
                        Constraint::Unary { .. } => op_constraints.push(c),
                        Constraint::Binary { .. } => op_constraints.push(c),
                        Constraint::Field { .. } => op_constraints.push(c),
                    });
            } else if (0..self.vars.len())
                .into_iter()
                .map(TyVar)
                // Attempt to find the type of a var from its supertypes (i.e: the things that it flows into)
                .any(|var| match self.try_derive_from_sup(var, &mut add_constraint, true) {
                    Some(Ok(())) => true,
                    Some(Err(e)) => { errors.push(e); self.errors[var.0] = true; false },
                    None => false,
                })
            {
                // Reconsider all proofs if we managed to derive any types
                // TODO: Maybe don't invalidate all proofs? This seems a bit silly.
                std::mem::take(&mut stale_constraints)
                    .into_iter()
                    .for_each(|c| match &c {
                        Constraint::Eq(_, _, _, _) => eq_constraints.push(c),
                        Constraint::Flow(_, _, _) => flow_constraints.push(c),
                        Constraint::Unary { .. } => op_constraints.push(c),
                        Constraint::Binary { .. } => op_constraints.push(c),
                        Constraint::Field { .. } => op_constraints.push(c),
                    });
            } else {
                break;
            }
        }

        let mut infinite_tys = HashSet::new();
        let mut failed_to_infer = HashMap::new();
        let solved_tys = SolvedTys {
            tys: (0..self.vars.len())
                .into_iter()
                .map(TyVar)
                .map(|var| self.reconstruct(&mut failed_to_infer, &mut infinite_tys, &mut Vec::new(), var))
                .collect(),
        };
        errors.extend(infinite_tys.into_iter().map(TyError::InfiniteType));

        // Don't emit inference errors if there are other errors in this context already (TODO: don't be so coarse)
        if errors.is_empty() {
            errors.extend(failed_to_infer.into_iter().map(|(var, (subs, sups))| TyError::CannotInfer(var, subs, sups)));
        }

        // for (i, v) in self.vars.iter().enumerate() {
        //     println!("{:?} => {:?}", TyVar(i), v);
        // }

        (solved_tys, errors)
    }

    /// Attempt to solve all the constraints provided to this context.
    pub fn solve(mut self) -> (SolvedTys, Vec<Error>) {
        let (solved_tys, errors) = self.solve_inner();
        let errors = errors
            .into_iter()
            .map(|error| match error {
                TyError::CannotEquate(a, b, span, reason) => Error::new(
                    ErrorCode::TypeMismatch,
                    span,
                    format!("Type mismatch between `{}` and `{}`", self.display(a, &[a], Some(1)), self.display(b, &[b], Some(1))),
                )
                    .with_secondary(span, match reason {
                        EquateReason::Conditional => Some(format!("Condition predicates must be of type `Bool`")),
                        EquateReason::List => Some(format!("List elements must all be the same type")),
                        EquateReason::Other => Some(format!("The types are required to be equal here")),
                    })
                    .with_primary(self.span(a), Some(format!("{}", self.display(a, &[a], Some(1)))))
                    .with_primary(self.span(b), Some(format!("{}", self.display(b, &[b], Some(1)))))
                    /*
                    .do_if(
                        self.least_general_super([a, b].iter().copied()).is_ok(),
                        |e| e.with_note(format!("These types appear to have a common subtype, perhaps a coercion is required?")),
                    )
                    */,
                TyError::CannotFlow(a, b, span) => Error::new(
                    ErrorCode::TypeIncompatibility,
                    span,
                    format!("Type coercion `{}` to `{}` is invalid", self.display(a, &[a], Some(1)), self.display(b, &[b], Some(1))),
                )
                    .with_secondary(span, Some(format!("The type is required to coerce here")))
                    .with_primary(self.span(a), Some(format!("{}", self.display(a, &[a], Some(1)))))
                    .with_primary(self.span(b), Some(format!("{}", self.display(b, &[b], Some(1))))),
                TyError::CannotInfer(a, subs, sups) => {
                    let subs_iter = subs
                        .iter()
                        .filter(|&&sub| self.is_known(sub));
                    let sups_iter = sups
                        .iter()
                        .filter(|&&sup| self.is_known(sup));
                    Error::new(
                        ErrorCode::TypeInferenceFailure,
                        self.span(a),
                        format!("Cannot infer ambiguous type `{}`", self.display(a, &[a], None)),
                    )
                        .with_primary(self.span(a), Some(format!("{}", self.display(a, &[a], None))))
                        .with_note(format!("Consider adding a type hint"))
                        .do_if(subs_iter.clone().count() > 0, |e| e.with_note(format!("Type must a super type of {}", subs_iter
                            .map(|&sub| format!("{}", self.display(sub, &[sub], None)))
                            .collect::<Vec<_>>()
                            .join(", "))))
                        .do_if(sups_iter.clone().count() > 0, |e| e.with_note(format!("Type must coerce to {}", sups_iter
                            .map(|&sup| format!("{}", self.display(sup, &[sup], None)))
                            .collect::<Vec<_>>()
                            .join(", "))))
                },
                TyError::InfiniteType(a) => Error::new(
                    ErrorCode::TypeInfinite,
                    self.span(a),
                    format!("Type `{}` is self-referential and expands infinitely", self.display(a, &[a], None)),
                )
                    .with_primary(self.span(a), Some(format!("This type is self-referential")))
                    .with_note(format!("Consider using a self-referential data type instead")),
                TyError::SolverLimitReached => Error::new(
                    ErrorCode::SolverLimitReached,
                    Span::none(),
                    format!("Type solver iteration limit reached"),
                ),
                TyError::NoSubtype(a, b, span) | TyError::NoSupertype(a, b, span) => Error::new(
                    ErrorCode::TypeIncompatibility,
                    self.span(a),
                    format!("Types `{}` and `{}` are not compatible", self.display(a, &[a], None), self.display(b, &[b], None)),
                )
                    .with_primary(self.span(a), Some(format!("{}", self.display(a, &[a], None))))
                    .with_primary(self.span(b), Some(format!("{}", self.display(b, &[b], None))))
                    .with_secondary(span, Some(format!("Compatibility is required here"))),
                TyError::CannotResolveUnary(a, op, out, span) => Error::new(
                    ErrorCode::InvalidUnaryOp,
                    span,
                    format!("Cannot resolve {} `{}` == `{}`", op, self.display(a, &[a], None), self.display(out, &[out], None)),
                )
                    .with_primary(span, Some(format!("Invalid operation")))
                    .with_primary(self.span(a), Some(format!("{}", self.display(a, &[a], None)))),
                TyError::CannotResolveBinary(a, b, op, out, span) => Error::new(
                    ErrorCode::InvalidBinaryOp,
                    span,
                    format!("Cannot resolve `{}` {} `{}` == `{}`", self.display(a, &[a], None), op, self.display(b, &[b], None), self.display(out, &[out], None)),
                )
                    .with_primary(span, Some(format!("Invalid operation")))
                    .with_primary(self.span(a), Some(format!("{}", self.display(a, &[a], None))))
                    .with_primary(self.span(b), Some(format!("{}", self.display(b, &[b], None)))),
                TyError::CannotResolveField(a, field, out, span) => Error::new(
                    ErrorCode::InvalidUnaryOp,
                    span,
                    format!("No such field `{}` on type `{}`", field, self.display(a, &[a], None)),
                )
                    .with_primary(span, Some(format!("No such field")))
                    .do_if(self.is_known(out), |err| err.with_note(format!("Expected field with type `{}`", self.display(out, &[out], None)))),
            })
            .collect();

        (solved_tys, errors)
    }

    fn display<'b>(&'b self, var: TyVar, excluding: &'b [TyVar], max_depth: Option<usize>) -> TyInfoDisplay<'b> {
        TyInfoDisplay {
            ctx: self,
            var,
            excluding,
            max_depth,
            depth: 0,
        }
    }
}

pub struct SolvedTys {
    tys: Vec<TyId>,
}

impl SolvedTys {
    pub fn get(&self, var: TyVar) -> TyId { self.tys[var.0] }
}

#[derive(Copy, Clone)]
struct TyInfoDisplay<'a> {
    ctx: &'a InferCtx<'a>,
    var: TyVar,
    excluding: &'a [TyVar],
    max_depth: Option<usize>,
    depth: usize,
}

impl<'a> TyInfoDisplay<'a> {
    fn with(self, var: TyVar) -> Self {
        Self {
            depth: self.depth + 1,
            var,
            ..self
        }
    }
}

impl<'a> fmt::Display for TyInfoDisplay<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use TyInfo::*;

        if Some(self.depth) == self.max_depth {
            write!(f, "_")
        } else if self.excluding.contains(&self.var) && self.depth > 0 {
            write!(f, "...")
        } else {
            match self.ctx.info(self.var) {
                Unknown { .. } => write!(f, "?"),
                Error => write!(f, "!"),
                Prim(p) => write!(f, "{}", p),
                &Func(i, o) => write!(f, "{} -> {}", self.with(i), self.with(o)),
                &List(item) => write!(f, "[{}]", self.with(item)),
                Tuple(fields) => write!(f, "({})", fields
                    .iter()
                    .map(|field| format!("{}", self.with(*field)))
                    .collect::<Vec<_>>()
                    .join(", ")),
                Record(fields) => write!(f, "{{ {} }}", fields
                    .iter()
                    .map(|(name, field)| format!("{}: {}", name, self.with(*field)))
                    .collect::<Vec<_>>()
                    .join(", ")),
                // TODO: Formatting for data IDs
                Data(id, params) => write!(f, "{:?} {}", id, params
                    .iter()
                    .map(|param| format!("{}", self.with(*param)))
                    .collect::<Vec<_>>()
                    .join(" ")),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[cfg(test)]
    impl PartialEq for TyError {
        fn eq(&self, other: &Self) -> bool {
            use TyError::*;
            match (self, other) {
                (CannotEquate(a, b, _, _), CannotEquate(x, y, _, _)) if a == x && b == y => true,
                (CannotFlow(a, b, _), CannotFlow(x, y, _)) if a == x && b == y => true,
                (CannotInfer(a, _, _), CannotInfer(x, _, _)) if a == x => true,
                (InfiniteType(a), InfiniteType(x)) if a == x => true,
                _ => false,
            }
        }
    }

    #[test]
    fn basic() {
        let mut ctx = Ctx::default();
        let mut infer_ctx = InferCtx::from_ctx(&mut ctx);

        let x0 = infer_ctx.free_var(Span::none());
        let x1 = infer_ctx.info_var(TyInfo::Prim(Primitive::Int), Span::none());
        let x_tuple = infer_ctx.info_var(TyInfo::Tuple(vec![x0, x1]), Span::none());
        let list_x = infer_ctx.info_var(TyInfo::List(x_tuple), Span::none());

        let y0 = infer_ctx.info_var(TyInfo::Prim(Primitive::Nat), Span::none());
        let y1 = infer_ctx.free_var(Span::none());
        let y_tuple = infer_ctx.info_var(TyInfo::Tuple(vec![y0, y1]), Span::none());
        let list_y = infer_ctx.info_var(TyInfo::List(y_tuple), Span::none());

        // [(?X, Int)] = [(Nat, ?Y)]
        infer_ctx.unify_eq(list_x, list_y, Span::none());

        let (solved_tys, errors) = infer_ctx.solve_inner();

        assert_eq!(errors, Vec::new());
        assert_eq!(ctx.ty.get(solved_tys.get(x0)), &Ty::Prim(Primitive::Nat));
        assert_eq!(ctx.ty.get(solved_tys.get(y1)), &Ty::Prim(Primitive::Int));
    }

    #[test]
    fn contradiction() {
        let mut ctx = Ctx::default();
        let mut infer_ctx = InferCtx::from_ctx(&mut ctx);

        let x = infer_ctx.info_var(TyInfo::Prim(Primitive::Int), Span::none());
        let y = infer_ctx.info_var(TyInfo::Prim(Primitive::Nat), Span::none());
        infer_ctx.unify_eq(x, y, Span::none());

        let (solved_tys, errors) = infer_ctx.solve_inner();

        assert_eq!(errors, vec![TyError::CannotEquate(x, y, Span::none(), EquateReason::Other)]);
    }

    #[test]
    fn flow_good() {
        let mut ctx = Ctx::default();
        let mut infer_ctx = InferCtx::from_ctx(&mut ctx);

        let x = infer_ctx.info_var(TyInfo::Prim(Primitive::Nat), Span::none());
        let y = infer_ctx.info_var(TyInfo::Prim(Primitive::Int), Span::none());
        infer_ctx.unify_flow(x, y, Span::none());

        let (solved_tys, errors) = infer_ctx.solve_inner();

        assert_eq!(errors, Vec::new());
    }

    #[test]
    fn flow_bad() {
        let mut ctx = Ctx::default();
        let mut infer_ctx = InferCtx::from_ctx(&mut ctx);

        let x = infer_ctx.info_var(TyInfo::Prim(Primitive::Int), Span::none());
        let y = infer_ctx.info_var(TyInfo::Prim(Primitive::Nat), Span::none());
        infer_ctx.unify_flow(x, y, Span::none());

        let (solved_tys, errors) = infer_ctx.solve_inner();

        assert_eq!(errors, vec![TyError::CannotFlow(x, y, Span::none())]);
    }

    #[test]
    fn infinite() {
        let mut ctx = Ctx::default();
        let mut infer_ctx = InferCtx::from_ctx(&mut ctx);

        let inner = infer_ctx.free_var(Span::none());
        let outer = infer_ctx.info_var(TyInfo::List(inner), Span::none());
        infer_ctx.unify_eq(inner, outer, Span::none());

        let (solved_tys, errors) = infer_ctx.solve_inner();

        assert_eq!(errors, vec![TyError::InfiniteType(inner)]);
        assert!(matches!(ctx.ty.get(solved_tys.get(inner)), Ty::List(_)));
    }
}
