use crate::{Error, ErrorCode, util::Span};
use super::*;
use slab::Slab;
use std::{fmt, collections::{HashMap, HashSet}};

#[derive(Clone, Debug)]
pub enum TyInfo {
    /// An unknown type, but with a list of suber type constraints.
    Unknown {
        subs: Vec<TyVar>,
        sups: Vec<TyVar>,
    },
    Ref(TyVar),
    /// An error occurred when performing inference with this type variable, generate no other errors.
    Error,
    Primitive(Primitive),
    List(TyVar),
    Tuple(Vec<TyVar>),
    Func(TyVar, TyVar),
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
    least_general_cache: HashMap<TyVar, Result<TyVar, SubtypeError>>,

    // >>> NEW >>>
    reachability: Reachability,
    nodes: Vec<TyNode>,
    // <<< NEW <<<
}

#[derive(Copy, Clone)]
pub enum SubtypeError {
    /// A silent error: a previously reported error term was found
    Silent,
    CannotInfer,
    NoGeneralType(TyVar, TyVar),
}

// >>> NEW >>>

#[derive(Clone, Debug)]
pub enum ValInfo {
    Error,
    Primitive(Primitive),
    Func(TyUse, TyVal),
    List(TyVal),
    Tuple(Vec<TyVal>),
}

#[derive(Clone, Debug)]
pub enum UseInfo {
    Error,
    Primitive(Primitive),
    Func(TyVal, TyUse),
    List(TyUse),
    Tuple(Vec<TyUse>),
}

#[derive(Debug)]
pub enum TyNode {
    Var,
    Val(ValInfo, Span),
    Use(UseInfo, Span),
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct TyVal(usize);

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct TyUse(usize);

pub enum TyError {
    CannotResolve(TyVal, TyUse),
}

#[derive(Default)]
struct OrderedSet<T> {
    v: Vec<T>,
    s: HashSet<T>,
}

impl<T: Eq + std::hash::Hash + Clone> OrderedSet<T> {
    fn insert(&mut self, value: T) -> bool {
        if self.s.insert(value.clone()) {
            self.v.push(value);
            true
        } else {
            false
        }
    }
}

#[derive(Default)]
struct Reachability {
    upsets: Vec<OrderedSet<usize>>,
    downsets: Vec<OrderedSet<usize>>,
}

impl Reachability {
    fn add_node(&mut self, id: usize) {
        assert_eq!(id, self.upsets.len());
        assert_eq!(id, self.downsets.len());
        self.upsets.push(OrderedSet::default());
        self.downsets.push(OrderedSet::default());
    }

    fn add_edge(&mut self, a: usize, b: usize, mut edge: impl FnMut(usize, usize)) {
        let mut edges = vec![(a, b)];

        while let Some((a, b)) = edges.pop() {
            if self.downsets[a].insert(b) {
                self.upsets[b].insert(a);

                edge(a, b);

                for &a in self.upsets[a].s.iter() {
                    edges.push((a, b));
                }
                for &b in self.upsets[b].s.iter() {
                    edges.push((a, b));
                }
            }
        }
    }
}

// <<< NEW <<<

impl<'a> InferCtx<'a> {
    pub fn new(ctx: &'a mut Ctx) -> Self {
        Self {
            ctx,
            vars: Slab::new(),
            ty_cache: HashMap::new(),
            least_general_cache: HashMap::new(),

            // >>> NEW >>>
            reachability: Reachability::default(),
            nodes: Vec::new(),
            // <<< NEW <<<
        }
    }

    // >>> NEW >>>

    pub fn error_ty(&mut self, span: Span) -> TyId {
        self.ctx.ty.insert(Ty::Error)
    }

    fn add_node(&mut self, node: TyNode) -> usize {
        let id = self.nodes.len();
        self.nodes.push(node);
        id
    }

    fn make_val(&mut self, v: ValInfo, span: Span) -> TyVal {
        let id = self.add_node(TyNode::Val(v, span));
        self.reachability.add_node(id);
        TyVal(id)
    }

    fn make_use(&mut self, u: UseInfo, span: Span) -> TyUse {
        let id = self.add_node(TyNode::Use(u, span));
        self.reachability.add_node(id);
        TyUse(id)
    }

    pub fn make_var(&mut self) -> (TyVal, TyUse) {
        let id = self.add_node(TyNode::Var);
        self.reachability.add_node(id);
        (TyVal(id), TyUse(id))
    }

    pub fn val_error(&mut self, span: Span) -> TyVal { self.make_val(ValInfo::Error, span) }

    pub fn val_primitive(&mut self, p: Primitive, span: Span) -> TyVal { self.make_val(ValInfo::Primitive(p), span) }
    pub fn use_primitive(&mut self, p: Primitive, span: Span) -> TyUse { self.make_use(UseInfo::Primitive(p), span) }

    pub fn val_func(&mut self, i: TyUse, o: TyVal, span: Span) -> TyVal { self.make_val(ValInfo::Func(i, o), span) }
    pub fn use_func(&mut self, i: TyVal, o: TyUse, span: Span) -> TyUse { self.make_use(UseInfo::Func(i, o), span) }

    pub fn val_list(&mut self, inner: TyVal, span: Span) -> TyVal { self.make_val(ValInfo::List(inner), span) }
    pub fn use_list(&mut self, inner: TyUse, span: Span) -> TyUse { self.make_use(UseInfo::List(inner), span) }

    pub fn val_tuple(&mut self, fields: Vec<TyVal>, span: Span) -> TyVal { self.make_val(ValInfo::Tuple(fields), span) }
    pub fn use_tuple(&mut self, fields: Vec<TyUse>, span: Span) -> TyUse { self.make_use(UseInfo::Tuple(fields), span) }

    fn flow_inner(
        &self,
        (v, v_info): (TyVal, &ValInfo),
        (u, u_info): (TyUse, &UseInfo),
        mut constraint: impl FnMut(TyVal, TyUse),
    ) -> Result<(), TyError> {
        match (v_info, u_info) {
            // Errors are always fine, we assume an error has already been reported when they were produced
            (&ValInfo::Error, _) => Ok(()),
            (_, &UseInfo::Error) => Ok(()),
            (&ValInfo::Primitive(v), &UseInfo::Primitive(u)) if u.subtype_of(v) => Ok(()),
            (&ValInfo::Func(vi, vo), &UseInfo::Func(ui, uo)) => {
                constraint(vo, uo);
                constraint(ui, vi); // Contravariance
                Ok(())
            },
            (&ValInfo::List(v), &UseInfo::List(u)) => {
                constraint(v, u);
                Ok(())
            },
            (ValInfo::Tuple(vs), UseInfo::Tuple(us)) => {
                for (&v, &u) in vs.iter().zip(us.iter()) {
                    constraint(v, u);
                }
                Ok(())
            },
            _ => Err(TyError::CannotResolve(v, u)),
        }
    }

    pub fn flow(&mut self, v: TyVal, u: TyUse, span: Span) {
        let mut constraints = vec![(v, u)];
        let mut edges_to_check = Vec::new();

        while let Some((v, u)) = constraints.pop() {
            self.reachability.add_edge(v.0, u.0, |v, u| edges_to_check.push((v, u)));

            println!("Processing constraint...");

            while let Some((v, u)) = edges_to_check.pop() {
                println!("Processing edge...");
                if let (TyNode::Val(v_info, v_span), TyNode::Use(u_info, u_span)) = (&self.nodes[v], &self.nodes[u]) {
                    println!("Processing flow: {:?} => {:?}", v_info, u_info);

                    if let Err(e) = self.flow_inner(
                        (TyVal(v), v_info),
                        (TyUse(u), u_info),
                        |v, u| constraints.push((v, u)),
                    ) {
                        if let TyError::CannotResolve(v, u) = e {
                            let err = Error::new(
                                ErrorCode::TypeMismatch,
                                span,
                                format!("Cannot resolve `{:?}` as `{:?}`", v_info, u_info),
                            )
                                .with_primary(*v_span, Some(format!("{:?}", v_span)))
                                .with_primary(*u_span, Some(format!("{:?}", u_info)));
                            self.emit_error(err);
                        }

                        // On failure, consider these nodes to be
                        self.nodes[v] = TyNode::Val(ValInfo::Error, Span::none());
                        self.nodes[u] = TyNode::Use(UseInfo::Error, Span::none());

                        return;
                    }
                } else {
                    println!("Not processing {:?}", (&self.nodes[v], &self.nodes[u]));
                }
            }
        }

        assert!(constraints.is_empty() && edges_to_check.is_empty());
    }

    // <<< NEW <<<

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
    pub fn least_general_of(&mut self, subs: &[TyVar], span: Span, ignore: &mut Vec<TyVar>) -> Result<TyVar, SubtypeError> {
        println!("Recurse4");
        impl<'a> InferCtx<'a> {
            fn least_general(&mut self, a: TyVar, b: TyVar, span: Span, ignore: &mut Vec<TyVar>) -> Result<TyVar, SubtypeError> {
                Ok(match (self.info(a).clone(), self.info(b).clone()) {
                    (TyInfo::Ref(a), _) => self.least_general(a, b, span, ignore)?,
                    (_, TyInfo::Ref(b)) => self.least_general(a, b, span, ignore)?,
                    (TyInfo::Error, _) | (_, TyInfo::Error) => return Err(SubtypeError::Silent),
                    (TyInfo::Unknown { subs, .. }, _) => {
                        let a = match self.least_general_cache.get(&a) {
                            Some(a) => a.clone(),
                            None => {
                                let old_len = ignore.len();
                                ignore.push(b);
                                let lg = self.least_general_of(&subs, span, ignore);
                                ignore.truncate(old_len);
                                self.least_general_cache.insert(a, lg);
                                lg
                            },
                        }?;
                        self.least_general(a, b, span, ignore)?
                    },
                    (_, TyInfo::Unknown { subs, .. }) => self.least_general(b, a, span, ignore)?,
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
                        let inner = self.least_general(a_inner, b_inner, span, &mut Vec::new())?;
                        self.insert(TyInfo::List(inner), span)
                    },
                    (TyInfo::Tuple(a_fields), TyInfo::Tuple(b_fields)) if a_fields.len() == b_fields.len() => {
                        let fields = a_fields
                            .iter()
                            .zip(b_fields.iter())
                            .map(|(a, b)| self.least_general(*a, *b, span, &mut Vec::new()))
                            .collect::<Result<_, _>>()?;
                        self.insert(TyInfo::Tuple(fields), span)
                    },
                    (TyInfo::Func(a_i, a_o), TyInfo::Func(b_i, b_o)) => {
                        let i = self.most_general(a_i, b_i, span)?;
                        let o = self.least_general(a_o, b_o, span, &mut Vec::new())?;
                        self.insert(TyInfo::Func(i, o), span)
                    },
                    _ => return Err(SubtypeError::NoGeneralType(a, b)),
                })
            }
        }

        let ignore2 = ignore.clone();
        let mut subs = subs.iter().filter(|v| if !ignore2.contains(v) {
            true
        } else {
            println!("Ignoring {:?}", v);
            false
        });

        println!("Start");
        let r = match subs.next() {
            None => Err(SubtypeError::CannotInfer),
            Some(a) => subs.fold(Ok(*a), |a, b| self.least_general(a?, *b, span, ignore)),
        };
        println!("End");
        r
    }

    /// Attempt to find the most general type that is a super type of the listed types
    pub fn most_general_of(&mut self, subs: &[TyVar], span: Span) -> Result<TyVar, SubtypeError> {
        println!("Recurse3");
        impl<'a> InferCtx<'a> {
            fn most_general(&mut self, a: TyVar, b: TyVar, span: Span) -> Result<TyVar, SubtypeError> {
                Ok(match (self.info(a).clone(), self.info(b).clone()) {
                    (TyInfo::Ref(a), _) => self.most_general(a, b, span)?,
                    (_, TyInfo::Ref(b)) => self.most_general(a, b, span)?,
                    (TyInfo::Error, _) | (_, TyInfo::Error) => return Err(SubtypeError::Silent),
                    (_, TyInfo::Unknown { subs, .. }) => {
                        let b = self.most_general_of(&subs, span)?;
                        self.most_general(a, b, span)?
                    },
                    (TyInfo::Unknown { subs, .. }, _) => {
                        let a = self.most_general_of(&subs, span)?;
                        self.most_general(a, b, span)?
                    },
                    (TyInfo::Primitive(a_prim), TyInfo::Primitive(b_prim)) => {
                        if a_prim == b_prim {
                            a
                        } else if a_prim
                            .numerical_rank()
                            .ok_or(SubtypeError::NoGeneralType(a, b))? < b_prim
                            .numerical_rank()
                            .ok_or(SubtypeError::NoGeneralType(a, b))?
                        {
                            a
                        } else {
                            b
                        }
                    },
                    (TyInfo::List(a_inner), TyInfo::List(b_inner)) => {
                        let inner = self.most_general(a_inner, b_inner, span)?;
                        self.insert(TyInfo::List(inner), span)
                    },
                    (TyInfo::Tuple(a_fields), TyInfo::Tuple(b_fields)) if a_fields.len() == b_fields.len() => {
                        let fields = a_fields
                            .iter()
                            .zip(b_fields.iter())
                            .map(|(a, b)| self.most_general(*a, *b, span))
                            .collect::<Result<_, _>>()?;
                        self.insert(TyInfo::Tuple(fields), span)
                    },
                    (TyInfo::Func(a_i, a_o), TyInfo::Func(b_i, b_o)) => {
                        let i = self.most_general(a_i, b_i, span)?;
                        let o = self.most_general(a_o, b_o, span)?;
                        self.insert(TyInfo::Func(i, o), span)
                    },
                    _ => return Err(SubtypeError::NoGeneralType(a, b)),
                })
            }
        }

        match subs {
            [] => Err(SubtypeError::CannotInfer),
            [a, b @ ..] => b.iter().fold(Ok(*a), |a, b| self.most_general(a?, *b, span)),
        }
    }

    pub fn unify_subtype(&mut self, sub: TyVar, sup: TyVar, span: Span) -> Option<()> {
        println!("Recurse2");
        match (self.info(sub), self.info(sup)) {
            (&TyInfo::Ref(sub), _) => self.unify_subtype(sub, sup, span)?,
            (_, &TyInfo::Ref(sup)) => self.unify_subtype(sub, sup, span)?,
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
            (&TyInfo::List(a), &TyInfo::List(b)) => self.unify_subtype(a, b, span)?,
            (TyInfo::Tuple(a), TyInfo::Tuple(b)) if a.len() == b.len() => a
                .clone()
                .iter()
                .zip(b.clone().iter())
                .map(|(a, b)| self.unify_subtype(*a, *b, span))
                .collect::<Option<_>>()?,
            (TyInfo::Primitive(a), TyInfo::Primitive(b)) if a == b => {},
            (TyInfo::Primitive(a), TyInfo::Primitive(b)) if a.numerical_rank().zip(b.numerical_rank()).map_or(false, |(a, b)| a <= b) => {},
            (&TyInfo::Func(a_i, a_o), &TyInfo::Func(b_i, b_o)) => {
                self.unify_subtype(b_i, a_i, span)?; // Contravariant
                self.unify_subtype(a_o, b_o, span)?;
            },
            _ => {
                // TODO: unify both TyVars as TyInfo::Error?
                let sub_str = self.display(sub).to_string();
                let sup_str = self.display(sup).to_string();
                self.ctx.emit_error(Error::new(ErrorCode::TypeMismatch, span, format!("Cannot resolve `{}` as `{}`", sub_str, sup_str))
                    .with_primary(self.span(sub), Some(format!("{}", sub_str)))
                    .with_primary(self.span(sup), Some(format!("{}", sup_str))));
                return None;
            },
        }

        Some(())
    }

    pub fn unify_eq(&mut self, a: TyVar, b: TyVar, span: Span) {
        match (self.info(a), self.info(b)) {
            (&TyInfo::Ref(a), _) => self.unify_eq(a, b, span),
            (_, &TyInfo::Ref(b)) => self.unify_eq(a, b, span),
            (TyInfo::Unknown { .. }, _) => *self.info_mut(a) = TyInfo::Ref(b),
            (_, TyInfo::Unknown { .. }) => self.unify_eq(b, a, span),
            (&TyInfo::List(a), &TyInfo::List(b)) => self.unify_eq(a, b, span),
            (TyInfo::Tuple(a), TyInfo::Tuple(b)) if a.len() == b.len() => a
                .clone()
                .iter()
                .zip(b.clone().iter())
                .for_each(|(a, b)| self.unify_eq(*a, *b, span)),
            (TyInfo::Primitive(a), TyInfo::Primitive(b)) if a == b => {},
            (&TyInfo::Func(a_i, a_o), &TyInfo::Func(b_i, b_o)) => {
                self.unify_eq(b_i, a_i, span);
                self.unify_eq(a_o, b_o, span);
            },
            _ => {
                let a_str = self.display(a).to_string();
                let b_str = self.display(b).to_string();
                self.ctx.emit_error(Error::new(ErrorCode::TypeMismatch, span, format!("Cannot resolve `{}` = `{}`", a_str, b_str))
                    .with_primary(self.span(a), Some(format!("{}", a_str)))
                    .with_primary(self.span(b), Some(format!("{}", b_str))));
            },
        }
    }

    pub fn reconstruct(&mut self, var: TyVar, span: Span) -> TyId {
        println!("Recurse8");
        if let Some(id) = self.ty_cache.get(&var) {
            *id
        } else {
            println!("HERE!: {} is {:?}", var, self.info(var));
            let ty = match self.info(var) {
                &TyInfo::Ref(var) => {
                    let id = self.reconstruct(var, span);
                    self.ty_cache.insert(var, id);
                    return id;
                },
                TyInfo::Unknown { subs, sups } => {
                    let subs = subs.clone();
                    let sups = sups.clone();
                    match self.least_general_of(&subs, span, &mut Vec::new()) {
                        Ok(var) => {
                            if sups.iter().any(|sup| self.unify_subtype(var, *sup, span).is_none()) {
                                Ty::Error
                            } else {
                                let id = self.reconstruct(var, span);
                                self.ty_cache.insert(var, id);
                                return id;
                            }
                        },
                        Err(SubtypeError::Silent) => Ty::Error,
                        Err(SubtypeError::CannotInfer) => /*if let [sup] = sups.as_slice() { // TODO: is special-casing one super type a good idea?
                            let id = self.reconstruct(*sup, span);
                            self.ty_cache.insert(var, id);
                            return id;
                        } else*/ {
                            self.ctx.emit_error(Error::new(ErrorCode::CannotInferType, span, format!("Cannot infer type `{}`", self.display(var)))
                                .with_primary(self.span(var), None)
                                .with_note(format!("Type must fulfil the following super types: {}", sups
                                    .iter()
                                    .map(|sup| format!("{}", self.display(*sup)))
                                    .collect::<Vec<_>>()
                                    .join(", "))));
                            Ty::Error
                        },
                        Err(SubtypeError::NoGeneralType(a, b)) => {
                            let a_str = format!("{}", self.display(a));
                            let b_str = format!("{}", self.display(b));
                            let err = Error::new(ErrorCode::CannotInferType, span, format!("Types `{}` and `{}` are not compatible", a_str, b_str))
                                .with_note(format!("No type exists for which `{}` and `{}` are subtypes", a_str, b_str))
                                .with_primary(span, None)
                                .with_secondary(self.span(a), Some(a_str))
                                .with_secondary(self.span(b), Some(b_str));
                                // .with_note(format!("Super type must fulfil the following subtypes: {}", subs
                                //     .iter()
                                //     .map(|sub| format!("{}", self.display(*sub)))
                                //     .collect::<Vec<_>>()
                                //     .join(", ")));
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
                &TyInfo::Func(i, o) => Ty::Func(self.reconstruct(i, span), self.reconstruct(o, span)),
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
        println!("Recurse7");
        match self.ctx.info(self.var) {
            &TyInfo::Ref(var) => write!(f, "{}", Self { ctx: self.ctx, var }),
            TyInfo::Unknown { .. } => write!(f, "?"),
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
