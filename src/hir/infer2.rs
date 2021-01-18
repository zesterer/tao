use super::{Span, Primitive, Ctx, Ty, TyId};
use crate::{Error, ErrorCode};
use std::{fmt, cmp::PartialEq, collections::{HashSet, HashMap}};

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
    // No subtype/super type relationship between types.
    Incompatible(TyVar, TyVar),
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
    Primitive(Primitive),
    Func(TyVar, TyVar),
    List(TyVar),
    Tuple(Vec<TyVar>),
}

#[derive(Clone, Debug)]
pub enum Constraint {
    Eq(TyVar, TyVar, Span, EquateReason),
    Flow(TyVar, TyVar, Span),
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
            (Primitive(a), Primitive(b)) if a == b => Some(Ok(())),
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
            (Unknown { .. }, _) | (_, Unknown { .. }) => {
                if let Entry::Info(Unknown { sups, .. }) = &mut self.vars[a.0] { sups.insert(b); }
                if let Entry::Info(Unknown { subs, .. }) = &mut self.vars[b.0] { subs.insert(a); }
                None
            },
            // Primitives may flow into one-another if the former is less general than the latter (e.g `Nat` -> `Int`).
            (&Primitive(a), &Primitive(b)) if a.subtype_of(b) => Some(Ok(())),
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
            _ => {
                self.errors[a.0] = true;
                self.errors[b.0] = true;
                Some(Err(TyError::CannotFlow(a, b, span)))
            },
        }
    }

    fn least_general_super(&mut self, mut vars: impl Iterator<Item = TyVar>) -> Result<TyVar, Result<(TyVar, TyVar), bool>> {
        let a = vars.next().ok_or(Err(false))?;
        use TyInfo::*;
        vars.try_fold(a, |a, b| {
            // if self.occurs_in(a, b) || self.occurs_in(b, a) {
            //     return Err(None);
            // }
            match (self.info(a), self.info(b)) {
                (Error, _) | (_, Error) => Ok(a),
                (Unknown { .. }, _) | (_, Unknown { .. }) => Err(Err(true)),
                (&Primitive(pa), &Primitive(pb)) => if pa.subtype_of(pb) {
                    Ok(b)
                } else if pb.subtype_of(pa) {
                    Ok(a)
                } else {
                    Err(Ok((a, b)))
                },
                (&Func(ai, ao), &Func(bi, bo)) => {
                    let i = if self.least_general_super([ai, bi].into_iter().copied())? == ai { bi } else { ai };
                    let o = if self.least_general_super([ao, bo].into_iter().copied())? == ao { ao } else { bo };
                    Ok(self.info_var(TyInfo::Func(i, o), Span::none()))
                },
                (&List(ai), &List(bi)) => Ok(if self.least_general_super([ai, bi].into_iter().copied())? == ai { a } else { b }),
                _ => Err(Ok((a, b))),
            }
        })
    }

    fn most_general_sub(&mut self, mut vars: impl Iterator<Item = TyVar>) -> Result<TyVar, Result<(TyVar, TyVar), bool>> {
        let a = vars.next().ok_or(Err(false))?;
        use TyInfo::*;
        vars.try_fold(a, |a, b| {
            // if self.occurs_in(a, b) || self.occurs_in(b, a) {
            //     return Err(None);
            // }
            match (self.info(a), self.info(b)) {
                (Error, _) | (_, Error) => Ok(a),
                (Unknown { .. }, _) | (_, Unknown { .. }) => Err(Err(true)),
                (&Primitive(pa), &Primitive(pb)) => if pa.subtype_of(pb) {
                    Ok(b)
                } else if pb.subtype_of(pa) {
                    Ok(a)
                } else {
                    Err(Ok((a, b)))
                },
                (&Func(ai, ao), &Func(bi, bo)) => {
                    let i = if self.most_general_sub([ai, bi].into_iter().copied())? == ai { bi } else { ai };
                    let o = if self.most_general_sub([ao, bo].into_iter().copied())? == ao { ao } else { bo };
                    Ok(self.info_var(TyInfo::Func(i, o), Span::none()))
                },
                (&List(ai), &List(bi)) => Ok(if self.most_general_sub([ai, bi].into_iter().copied())? == ai { a } else { b }),
                _ => Err(Ok((a, b))),
            }
        })
    }

    /// Attempt to derive a type from the subtype and super type constraints placed upon it.
    fn try_derive_from_sub_sup(&mut self, var: TyVar, mut add_constraint: impl FnMut(Constraint)) -> Option<Result<(), TyError>> {
        let (var, info) = self.var_and_info(var);

        if self.errors[var.0] {
            return None;
        }

        use TyInfo::*;
        match info {
            Unknown { subs, sups } => {
                let subs = subs.clone();
                let sups = sups.clone();
                match self.least_general_super(subs.into_iter()) {
                    Ok(lgs) => match self.most_general_sub(sups.into_iter()) {
                        Ok(mgs) => Some(Ok({
                            add_constraint(Constraint::Eq(var, lgs, self.span(var), EquateReason::Other));
                            add_constraint(Constraint::Flow(lgs, var, self.span(var)));
                            add_constraint(Constraint::Flow(var, mgs, self.span(var)));
                        })),
                        Err(Ok((a, b))) => {
                            self.errors[a.0] = true;
                            self.errors[b.0] = true;
                            self.errors[var.0] = true;
                            Some(Err(TyError::Incompatible(a, b)))
                        },
                        Err(Err(false)) => Some(Ok({
                            add_constraint(Constraint::Eq(var, lgs, self.span(var), EquateReason::Other));
                            add_constraint(Constraint::Flow(lgs, var, self.span(var)));
                        })),
                        Err(Err(true)) => None, // Ambiguity, an uninferred supertype
                    },
                    Err(Ok((a, b))) => {
                        self.errors[a.0] = true;
                        self.errors[b.0] = true;
                        self.errors[var.0] = true;
                        Some(Err(TyError::Incompatible(a, b)))
                    },
                    Err(Err(false)) => match self.most_general_sub(sups.into_iter()) {
                        Ok(mgs) => Some(Ok({
                            add_constraint(Constraint::Eq(var, mgs, self.span(var), EquateReason::Other));
                            add_constraint(Constraint::Flow(var, mgs, self.span(var)));
                        })),
                        Err(Ok((a, b))) => {
                            self.errors[a.0] = true;
                            self.errors[b.0] = true;
                            self.errors[var.0] = true;
                            Some(Err(TyError::Incompatible(a, b)))
                        },
                        Err(Err(false)) => None, // No sub or super typing info at all
                        Err(Err(true)) => None, // Ambiguity, an uninferred supertype
                    },
                    Err(Err(true)) => None, // Ambiguity, an uninferred subtype
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
                    if !self.errors[var.0] {
                        cannot_infer.insert(var, (subs.clone(), sups.clone()));
                    }
                    Ty::Error
                },
                Error => Ty::Error,
                &Primitive(p) => Ty::Primitive(p),
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
            }
        };

        seen.truncate(seen_len);

        self.ctx.ty.insert(ty)
    }

    /// Attempt to solve all the constraints provided to this context.
    fn solve_inner(&mut self) -> (SolvedTys, Vec<TyError>) {
        const ITER_LIMIT: usize = 100_000;

        let mut errors = Vec::new();
        let mut eq_constraints = Vec::new();
        let mut flow_constraints = Vec::new();
        let mut stale_constraints = Vec::new();

        // for c in &self.constraints {
        //     println!("{:?}", c);
        // }

        // Insert constraints according to their solve order
        for c in std::mem::take(&mut self.constraints) {
            match &c {
                Constraint::Eq(_, _, _, _) => eq_constraints.push(c),
                Constraint::Flow(_, _, _) => flow_constraints.push(c),
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

            let next_constraint = eq_constraints.pop().or_else(|| flow_constraints.pop());

            let mut add_constraint = |c| match &c {
                Constraint::Eq(_, _, _, _) => eq_constraints.push(c),
                Constraint::Flow(_, _, _) => flow_constraints.push(c),
            };

            if let Some(c) = next_constraint {
                //println!("{:?}", c);
                match match &c {
                    &Constraint::Eq(a, b, span, reason) => self.solve_eq(a, b, span, reason, &mut add_constraint),
                    &Constraint::Flow(a, b, span) => self.solve_flow(a, b, span, &mut add_constraint),
                } {
                    // Successfully proving something means that we might be able to prove other things as a result, so
                    // consider all proofs to no longer be stale.
                    Some(Ok(())) => std::mem::take(&mut stale_constraints)
                        .into_iter()
                        .for_each(|c| match &c {
                            Constraint::Eq(_, _, _, _) => eq_constraints.push(c),
                            Constraint::Flow(_, _, _) => flow_constraints.push(c),
                        }),
                    Some(Err(e)) => errors.push(e),
                    // Failing to gain new information about a proof adds that proof back into the list of things to
                    // be proved, thereby increasing the number of stale proofs.
                    None => stale_constraints.push(c),
                }
            } else if (0..self.vars.len())
                .into_iter()
                .map(TyVar)
                .any(|var| match self.try_derive_from_sub_sup(var, &mut add_constraint) {
                    Some(Ok(())) => true,
                    Some(Err(e)) => { errors.push(e); self.errors[var.0] = true; false },
                    None => false,
                })
            {
                // TODO: Maybe don't invalidate all proofs? This seems a bit silly.
                // Reconsider all proofs if we managed to derive any types
                std::mem::take(&mut stale_constraints)
                    .into_iter()
                    .for_each(|c| match &c {
                        Constraint::Eq(_, _, _, _) => eq_constraints.push(c),
                        Constraint::Flow(_, _, _) => flow_constraints.push(c),
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
        errors.extend(failed_to_infer.into_iter().map(|(var, (subs, sups))| TyError::CannotInfer(var, subs, sups)));

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
                    format!("Type mismatch between `{}` and `{}`", self.display(a, &[a], None), self.display(b, &[b], None)),
                )
                    .with_secondary(span, match reason {
                        EquateReason::Conditional => Some(format!("Condition predicates must be of type `Bool`")),
                        EquateReason::List => Some(format!("List elements must all be the same type")),
                        EquateReason::Other => Some(format!("The types are required to be equal here")),
                    })
                    .with_primary(self.span(a), Some(format!("{}", self.display(a, &[a], None))))
                    .with_primary(self.span(b), Some(format!("{}", self.display(b, &[b], None))))
                    .do_if(
                        self.least_general_super([a, b].into_iter().copied()).is_ok(),
                        |e| e.with_note(format!("These types appear to have a common subtype, perhaps a coercion is required?")),
                    ),
                TyError::CannotFlow(a, b, span) => Error::new(
                    ErrorCode::TypeIncompatibility,
                    span,
                    format!("Type coercion `{}` to `{}` is invalid", self.display(a, &[a], None), self.display(b, &[b], None)),
                )
                    .with_secondary(span, Some(format!("The type is required to coerce here")))
                    .with_primary(self.span(a), Some(format!("{}", self.display(a, &[a], None))))
                    .with_primary(self.span(b), Some(format!("{}", self.display(b, &[b], None)))),
                TyError::CannotInfer(a, subs, sups) => Error::new(
                    ErrorCode::TypeInferenceFailure,
                    self.span(a),
                    format!("Cannot infer ambiguous type `{}`", self.display(a, &[a], None)),
                )
                    .with_primary(self.span(a), Some(format!("{}", self.display(a, &[a], None))))
                    .with_note(format!("Consider adding a type hint"))
                    .do_if(subs.len() > 0, |e| e.with_note(format!("Type must a super type of {}", subs
                        .iter()
                        .map(|&sub| format!("{}", self.display(sub, &[sub], None)))
                        .collect::<Vec<_>>()
                        .join(", "))))
                    .do_if(sups.len() > 0, |e| e.with_note(format!("Type must coerce to {}", sups
                        .iter()
                        .map(|&sup| format!("{}", self.display(sup, &[sup], None)))
                        .collect::<Vec<_>>()
                        .join(", ")))),
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
                TyError::Incompatible(a, b) => Error::new(
                    ErrorCode::TypeIncompatibility,
                    self.span(a),
                    format!("Types `{}` and `{}` do not have a common subtype", self.display(a, &[a], None), self.display(b, &[b], None)),
                )
                    .with_primary(self.span(a), Some(format!("{}", self.display(a, &[a], None))))
                    .with_primary(self.span(b), Some(format!("{}", self.display(b, &[b], None)))),
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
            Ok(())
        } else if self.excluding.contains(&self.var) && self.depth > 0 {
            write!(f, "...")
        } else {
            match self.ctx.info(self.var) {
                Unknown { .. } => write!(f, "?"),
                Error => write!(f, "!"),
                Primitive(p) => write!(f, "{}", p),
                &Func(i, o) => write!(f, "{} -> {}", self.with(i), self.with(o)),
                &List(item) => write!(f, "[{}]", self.with(item)),
                Tuple(fields) => write!(f, "({})", fields
                    .iter()
                    .map(|field| format!("{}", self.with(*field)))
                    .collect::<Vec<_>>()
                    .join(", ")),
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
        let x1 = infer_ctx.info_var(TyInfo::Primitive(Primitive::Int), Span::none());
        let x_tuple = infer_ctx.info_var(TyInfo::Tuple(vec![x0, x1]), Span::none());
        let list_x = infer_ctx.info_var(TyInfo::List(x_tuple), Span::none());

        let y0 = infer_ctx.info_var(TyInfo::Primitive(Primitive::Nat), Span::none());
        let y1 = infer_ctx.free_var(Span::none());
        let y_tuple = infer_ctx.info_var(TyInfo::Tuple(vec![y0, y1]), Span::none());
        let list_y = infer_ctx.info_var(TyInfo::List(y_tuple), Span::none());

        // [(?X, Int)] = [(Nat, ?Y)]
        infer_ctx.unify_eq(list_x, list_y, Span::none());

        let (solved_tys, errors) = infer_ctx.solve_inner();

        assert_eq!(errors, Vec::new());
        assert_eq!(ctx.ty.get(solved_tys.get(x0)), &Ty::Primitive(Primitive::Nat));
        assert_eq!(ctx.ty.get(solved_tys.get(y1)), &Ty::Primitive(Primitive::Int));
    }

    #[test]
    fn contradiction() {
        let mut ctx = Ctx::default();
        let mut infer_ctx = InferCtx::from_ctx(&mut ctx);

        let x = infer_ctx.info_var(TyInfo::Primitive(Primitive::Int), Span::none());
        let y = infer_ctx.info_var(TyInfo::Primitive(Primitive::Nat), Span::none());
        infer_ctx.unify_eq(x, y, Span::none());

        let (solved_tys, errors) = infer_ctx.solve_inner();

        assert_eq!(errors, vec![TyError::CannotEquate(x, y, Span::none(), EquateReason::Other)]);
    }

    #[test]
    fn flow_good() {
        let mut ctx = Ctx::default();
        let mut infer_ctx = InferCtx::from_ctx(&mut ctx);

        let x = infer_ctx.info_var(TyInfo::Primitive(Primitive::Nat), Span::none());
        let y = infer_ctx.info_var(TyInfo::Primitive(Primitive::Int), Span::none());
        infer_ctx.unify_flow(x, y, Span::none());

        let (solved_tys, errors) = infer_ctx.solve_inner();

        assert_eq!(errors, Vec::new());
    }

    #[test]
    fn flow_bad() {
        let mut ctx = Ctx::default();
        let mut infer_ctx = InferCtx::from_ctx(&mut ctx);

        let x = infer_ctx.info_var(TyInfo::Primitive(Primitive::Int), Span::none());
        let y = infer_ctx.info_var(TyInfo::Primitive(Primitive::Nat), Span::none());
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
