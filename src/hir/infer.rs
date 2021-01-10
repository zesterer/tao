use crate::{Error, ErrorCode, util::Span};
use super::*;
use slab::Slab;
use std::{fmt, collections::HashMap};

#[derive(Clone)]
pub enum TyInfo {
    /// An unknown type, but with a list of suber type constraints.
    Unknown {
        subs: Vec<TyVar>,
        sups: Vec<TyVar>,
    },
    /// An error occurred when performing inference with this type variable, generate no other errors.
    Error,
    Primitive(Primitive),
    List(TyVar),
    Tuple(Vec<TyVar>),
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct TyVar(pub usize);

impl fmt::Display for TyVar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "?{}", self.0)
    }
}

pub struct InferCtx<'a> {
    ctx: &'a mut Ctx,
    vars: Slab<(TyInfo, Span)>,
    ty_cache: HashMap<TyVar, TyId>,
}

pub enum SubtypeError {
    /// A silent error: a previously reported error term was found
    Silent,
    CannotInfer,
    NoGeneralType(TyVar, TyVar),
}

impl<'a> InferCtx<'a> {
    pub fn new(ctx: &'a mut Ctx) -> Self {
        Self {
            ctx,
            vars: Slab::new(),
            ty_cache: HashMap::new(),
        }
    }

    pub fn emit_error(&mut self, error: Error) {
        self.ctx.emit_error(error);
    }

    pub fn info(&self, var: TyVar) -> &TyInfo {
        &self.vars
            .get(var.0)
            .expect("Attempted to fetch type information for invalid type variable")
            .0
    }

    pub fn info_mut(&mut self, var: TyVar) -> &mut TyInfo {
        &mut self.vars
            .get_mut(var.0)
            .expect("Attempted to fetch type information for invalid type variable")
            .0
    }

    pub fn span(&self, var: TyVar) -> Span {
        self.vars
            .get(var.0)
            .expect("Attempted to fetch span for invalid type variable")
            .1
    }

    /// Create a new type variable with the given information.
    pub fn insert(&mut self, info: TyInfo, span: Span) -> TyVar {
        TyVar(self.vars.insert((info, span)))
    }

    /// Create a new type variable with no information (i.e: a 'free' variable).
    pub fn insert_free(&mut self, span: Span) -> TyVar {
        self.insert(TyInfo::Unknown { subs: Vec::new(), sups: Vec::new() }, span)
    }

    pub fn insert_error(&mut self, span: Span) -> TyVar {
        self.insert(TyInfo::Error, span)
    }

    /// Attempt to find the least general type that is a super type of the listed types
    pub fn least_general_of(&mut self, subs: &[TyVar], span: Span) -> Result<TyVar, SubtypeError> {
        impl<'a> InferCtx<'a> {
            fn least_general(&mut self, a: TyVar, b: TyVar, span: Span) -> Result<TyVar, SubtypeError> {
                Ok(match (self.info(a).clone(), self.info(b).clone()) {
                    (TyInfo::Error, _) | (_, TyInfo::Error) => return Err(SubtypeError::Silent),
                    (_, TyInfo::Unknown { subs, .. }) => {
                        let b = self.least_general_of(&subs, span)?;
                        self.least_general(a, b, span)?
                    },
                    (TyInfo::Unknown { subs, .. }, _) => {
                        let a = self.least_general_of(&subs, span)?;
                        self.least_general(a, b, span)?
                    },
                    (TyInfo::Primitive(a_prim), TyInfo::Primitive(b_prim)) => {
                        if a_prim == b_prim {
                            a
                        } else if a_prim
                            .numerical_rank()
                            .ok_or(SubtypeError::NoGeneralType(a, b))? > b_prim
                            .numerical_rank()
                            .ok_or(SubtypeError::NoGeneralType(a, b))?
                        {
                            a
                        } else {
                            b
                        }
                    },
                    (TyInfo::List(a_inner), TyInfo::List(b_inner)) => {
                        let inner = self.least_general(a_inner, b_inner, span)?;
                        self.insert(TyInfo::List(inner), span)
                    },
                    (TyInfo::Tuple(a_fields), TyInfo::Tuple(b_fields)) => {
                        let fields = a_fields
                            .iter()
                            .zip(b_fields.iter())
                            .map(|(a, b)| self.least_general(*a, *b, span))
                            .collect::<Result<_, _>>()?;
                        self.insert(TyInfo::Tuple(fields), span)
                    },
                    _ => return Err(SubtypeError::NoGeneralType(a, b)),
                })
            }
        }

        match subs {
            [] => Err(SubtypeError::CannotInfer),
            [a, b @ ..] => b.iter().fold(Ok(*a), |a, b| self.least_general(a?, *b, span)),
        }
    }

    pub fn unify_subtype(&mut self, sub: TyVar, sup: TyVar, span: Span) {
        match (self.info(sub), self.info(sup)) {
            (TyInfo::Unknown { .. }, TyInfo::Unknown { .. }) => {
                if let TyInfo::Unknown { subs, .. } = self.info_mut(sup) {
                    subs.push(sub);
                }
                if let TyInfo::Unknown { sups, .. } = self.info_mut(sub) {
                    sups.push(sup);
                }
            },
            (_, TyInfo::Unknown { .. }) => match self.info_mut(sup) {
                TyInfo::Unknown { subs, .. } => subs.push(sub),
                _ => unreachable!(),
            },
            (TyInfo::Unknown { .. }, _) => match self.info_mut(sub) {
                TyInfo::Unknown { sups, .. } => sups.push(sup),
                _ => unreachable!(),
            },
            (&TyInfo::List(a), &TyInfo::List(b)) => self.unify_subtype(a, b, span),
            (TyInfo::Primitive(a), TyInfo::Primitive(b)) if a == b => {},
            (TyInfo::Primitive(a), TyInfo::Primitive(b)) if a.numerical_rank().zip(b.numerical_rank()).map_or(false, |(a, b)| a <= b) => {},
            _ => {
                // TODO: unify both TyVars as TyInfo::Error?
                let sub_str = self.display(sub).to_string();
                let sup_str = self.display(sup).to_string();
                self.ctx.emit_error(Error::new(ErrorCode::TypeMismatch, span, format!("Cannot resolve `{}` as `{}`", sub_str, sup_str))
                    .with_primary(self.span(sub), Some(format!("{}", sub_str)))
                    .with_primary(self.span(sup), Some(format!("{}", sup_str))));
            },
        }
    }

    pub fn reconstruct(&mut self, var: TyVar, span: Span) -> TyId {
        if let Some(id) = self.ty_cache.get(&var) {
            *id
        } else {
            let ty = match self.info(var) {
                TyInfo::Unknown { subs, sups } => {
                    let subs = subs.clone();
                    let sups = sups.clone();
                    match self.least_general_of(&subs, span) {
                        Ok(var) => {
                            let id = self.reconstruct(var, span);
                            self.ty_cache.insert(var, id);
                            return id;
                        },
                        Err(SubtypeError::Silent) => Ty::Error,
                        Err(SubtypeError::CannotInfer) => /*if let [sup] = sups.as_slice() { // TODO: is special-casing one super type a good idea?
                            let id = self.reconstruct(*sup, span);
                            self.ty_cache.insert(var, id);
                            return id;
                        } else*/ {
                            self.ctx.emit_error(Error::new(ErrorCode::CannotInferType, span, format!("Cannot infer type `{}`", var))
                                .with_primary(self.span(var), None)
                                .with_note(format!("Type must fulfil the following super types: {}", sups
                                    .iter()
                                    .map(|sup| format!("{}", self.display(*sup)))
                                    .collect::<Vec<_>>()
                                    .join(", "))));
                            Ty::Error
                        },
                        Err(SubtypeError::NoGeneralType(a, b)) => {
                            let err = Error::new(ErrorCode::CannotInferType, span, format!("Cannot find appropriate super type"))
                                .with_primary(span, None)
                                .with_note(format!("Super type must fulfil the following subtypes: {}", subs
                                    .iter()
                                    .map(|sub| format!("{}", self.display(*sub)))
                                    .collect::<Vec<_>>()
                                    .join(", ")));
                            self.ctx.emit_error(err);
                            Ty::Error
                        },
                    }
                },
                TyInfo::Error => Ty::Error,
                TyInfo::Primitive(prim) => Ty::Primitive(*prim),
                &TyInfo::List(inner) => Ty::List(self.reconstruct(inner, span)),
                TyInfo::Tuple(fields) => Ty::Tuple(fields
                    .clone()
                    .iter()
                    .map(|field| self.reconstruct(*field, span))
                    .collect()),
            };
            let id = self.ctx.ty.insert(ty);
            self.ty_cache.insert(var, id);
            id
        }
    }

    pub fn display(&self, var: TyVar) -> TyInfoDisplay<'_> {
        TyInfoDisplay {
            ctx: self,
            var,
        }
    }
}

pub struct TyInfoDisplay<'a> {
    ctx: &'a InferCtx<'a>,
    var: TyVar,
}

impl<'a> fmt::Display for TyInfoDisplay<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.ctx.info(self.var) {
            TyInfo::Unknown { .. } => write!(f, "_"),
            TyInfo::Error => write!(f, "_"),
            TyInfo::Primitive(p) => write!(f, "{}", p),
            // TyInfo::Generic(ident, _) => write!(f, "{}", ident),
            TyInfo::List(inner) => write!(f, "[{}]", Self { ctx: self.ctx, var: *inner }),
            TyInfo::Tuple(fields) => write!(f, "({})", fields
                .iter()
                .map(|field| Self { ctx: self.ctx, var: *field }.to_string())
                .collect::<Vec<_>>()
                .join(", ")),
            // TyInfo::Record(fields) => write!(f, "{{ {} }}", fields
            //     .iter()
            //     .map(|(ident, field)| format!("{}: {}", ident, Self { ctx: self.ctx, var: *field }))
            //     .collect::<Vec<_>>()
            //     .join(", ")),
            // TyInfo::Func(i, o) => write!(f, "{} -> {}", Self { ctx: self.ctx, var: *i }, Self { ctx: self.ctx, var: *o }),
            _ => todo!(),
        }
    }
}
