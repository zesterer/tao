use std::{
    fmt,
    collections::HashMap,
};
use internment::LocalIntern;
use crate::{
    error::Error,
    src::Span,
    node::SrcNode,
    ast::{UnaryOp, BinaryOp},
    ty::{Type, Primitive},
};
use super::data::{DataCtx, DataId};

type Ident = LocalIntern<String>;

pub type TypeId = usize;
pub type ConstraintId = usize;

#[derive(Clone, Debug)]
pub enum TypeInfo {
    Unknown(Option<Type>), // Optionally instantiated from
    Ref(TypeId),
    Primitive(Primitive),
    List(TypeId),
    Tuple(Vec<TypeId>),
    Record(Vec<(SrcNode<Ident>, TypeId)>),
    Func(TypeId, TypeId),
    GenParam(Ident),
    Data(DataId, Vec<TypeId>),
}

#[derive(Clone, Debug)]
pub enum Constraint {
    Unary {
        out: TypeId,
        op: SrcNode<UnaryOp>,
        a: TypeId,
    },
    Binary {
        out: TypeId,
        op: SrcNode<BinaryOp>,
        a: TypeId,
        b: TypeId,
    },
    Access {
        out: TypeId,
        record: TypeId,
        field: SrcNode<Ident>,
    },
}

#[derive(Debug)]
pub struct InferCtx<'a> {
    data_ctx: &'a DataCtx,
    parent: Option<&'a Self>,

    id_counter: TypeId,
    types: HashMap<TypeId, TypeInfo>,
    spans: HashMap<TypeId, Span>,

    // Generics that are valid in this InferCtx scope
    // TODO: Should this be going in here?
    generics: HashMap<Ident, TypeId>,

    constraint_id_counter: ConstraintId,
    constraints: HashMap<ConstraintId, Constraint>,
}

impl<'a> InferCtx<'a> {
    pub fn from_data_ctx(data_ctx: &'a DataCtx) -> InferCtx<'a> {
        InferCtx {
            data_ctx,
            parent: None,

            id_counter: 0,
            types: HashMap::default(),
            spans: HashMap::default(),

            generics: HashMap::default(),

            constraint_id_counter: 0,
            constraints: HashMap::default(),
        }
    }

    pub fn scoped(&self) -> InferCtx {
        InferCtx {
            data_ctx: self.data_ctx,
            parent: Some(self),

            id_counter: self.id_counter,
            types: HashMap::default(),
            spans: HashMap::default(),

            generics: HashMap::default(),

            constraint_id_counter: self.constraint_id_counter,
            constraints: HashMap::default(),
        }
    }

    fn new_id(&mut self) -> TypeId {
        self.id_counter += 1;
        self.id_counter
    }

    pub fn data_ctx(&self) -> &'a DataCtx {
        self.data_ctx
    }

    pub fn get(&self, id: TypeId) -> TypeInfo {
        self.types
            .get(&id)
            .cloned()
            .or_else(|| self.parent.map(|p| p.get(id)))
            .unwrap()
    }

    fn get_base(&self, id: TypeId) -> TypeId {
        match self.get(id) {
            TypeInfo::Ref(id) => self.get_base(id),
            _ => id,
        }
    }

    pub fn span(&self, id: TypeId) -> Span {
        //let id = self.get_base(id);
        self.spans
            .get(&id)
            .cloned()
            .or_else(|| self.parent.map(|p| p.span(id)))
            .unwrap()
    }

    // Generics

    pub fn insert_generic(&mut self, name: Ident, span: Span) {
        let ty_id = self.insert(TypeInfo::GenParam(name), span);
        self.generics.insert(name, ty_id);
    }

    pub fn generic(&self, name: Ident) -> Option<TypeId> {
        self.generics
            .get(&name)
            .copied()
            .or_else(|| self.parent.and_then(|p| p.generic(name)))
    }

    // Return true if linking inferred new information
    fn link(&mut self, a: TypeId, b: TypeId) {
        if self.get_base(a) != self.get_base(b) {
            self.types.insert(a, TypeInfo::Ref(b));
        }
    }

    pub fn display_type_info(&self, id: TypeId) -> impl fmt::Display + '_ {
        #[derive(Copy, Clone)]
        struct TypeInfoDisplay<'a> {
            ctx: &'a InferCtx<'a>,
            id: TypeId,
            trailing: bool,
        }

        impl<'a> TypeInfoDisplay<'a> {
            fn with_id(mut self, id: TypeId, trailing: bool) -> Self {
                self.id = id;
                self.trailing = trailing;
                self
            }
        }

        impl<'a> fmt::Display for TypeInfoDisplay<'a> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                match self.ctx.get(self.id) {
                    TypeInfo::Unknown(ty) => if let Some(ty) = ty {
                        write!(f, "{}", ty)
                    } else {
                        write!(f, "?")
                    },
                    TypeInfo::Primitive(prim) => write!(f, "{}", prim),
                    TypeInfo::Ref(id) => self.with_id(id, self.trailing).fmt(f),
                    TypeInfo::List(id) => write!(f, "[{}]", self.with_id(id, true)),
                    TypeInfo::Tuple(ids) => {
                        write!(f, "(")?;
                        write!(f, "{}", ids
                            .iter()
                            .map(|id| format!("{}", self.with_id(*id, true)))
                            .collect::<Vec<_>>()
                            .join(", "))?;
                        if ids.len() == 1 {
                            write!(f, ",")?;
                        }
                        write!(f, ")")?;
                        Ok(())
                    },
                    TypeInfo::Record(fields) => {
                        write!(f, "{{{}", if fields.len() > 0 { " " } else { "" })?;
                        write!(f, "{}", fields
                            .iter()
                            .map(|(name, ty)| format!("{}: {}", name.as_str(), self.with_id(*ty, true)))
                            .collect::<Vec<_>>()
                            .join(", "))?;
                        write!(f, "{}}}", if fields.len() > 0 { " " } else { "" })?;
                        Ok(())
                    },
                    TypeInfo::Func(i, o) if self.trailing => write!(f, "{} -> {}", self.with_id(i, false), self.with_id(o, true)),
                    TypeInfo::Func(i, o) => write!(f, "({} -> {})", self.with_id(i, false), self.with_id(o, true)),
                    TypeInfo::GenParam(ident) => write!(f, "{}", ident),
                    TypeInfo::Data(data, params) => {
                        write!(f, "{}", self.ctx.data_ctx().get_data_name(data))?;
                        for param in params.iter() {
                            write!(f, " {}", self.with_id(*param, false))?;
                        }
                        Ok(())
                    },
                }
            }
        }

        TypeInfoDisplay {
            ctx: self,
            id,
            trailing: true,
        }
    }

    fn unify_inner(&mut self, iter: usize, a: TypeId, b: TypeId) -> Result<(), (TypeId, TypeId)> {
        const MAX_UNIFICATION_DEPTH: usize = 1024;
        if iter > MAX_UNIFICATION_DEPTH {
            panic!("Maximum unification depth reached (this error should not occur without extremely large types)");
        }

        use TypeInfo::*;
        match (self.get(a), self.get(b)) {
            (Ref(a), _) => self.unify_inner(iter + 1, a, b),
            (_, Ref(b)) => self.unify_inner(iter + 1, a, b),
            (Unknown(Some(_)), Unknown(_)) => Ok(self.link(a, b)),
            (Unknown(_), Unknown(Some(_))) => Ok(self.link(b, a)),
            (Unknown(_), _) => Ok(self.link(a, b)),
            (_, Unknown(_)) => Ok(self.link(b, a)), // TODO: does ordering matter?
            (Primitive(a), Primitive(b)) if a == b => Ok(()),
            (List(a), List(b)) => self.unify_inner(iter + 1, a, b),
            (Tuple(a), Tuple(b)) if a.len() == b.len() => a
                .into_iter()
                .zip(b.into_iter())
                .try_for_each(|(a, b)| self.unify_inner(iter + 1, a, b)),
            (Record(a_fields), Record(b_fields)) if a_fields.len() == b_fields.len() => a_fields
                .into_iter()
                .zip(b_fields.into_iter())
                .try_for_each(|((a_name, a_field), (b_name, b_field))| if *a_name == *b_name {
                    self.unify_inner(iter + 1, a_field, b_field)
                } else {
                    Err((a, b))
                }),
            (Func(ai, ao), Func(bi, bo)) => self.unify_inner(iter + 1, ai, bi)
                .and_then(|()| self.unify_inner(iter + 1, ao, bo)),
            (GenParam(a), GenParam(b)) if a == b => Ok(()),
            (Data(a, a_params), Data(b, b_params)) if a == b => a_params
                .into_iter()
                .zip(b_params.into_iter())
                .try_for_each(|(a, b)| self.unify_inner(iter + 1, a, b)),
            (_, _) => Err((a, b)),
        }
    }

    // Returns true if new information was inferred
    pub fn unify(&mut self, a: TypeId, b: TypeId) -> Result<(), Error> {
        self.unify_inner(0, a, b).map_err(|(x, y)| {
            Error::custom(format!(
                "Type mismatch between {} and {}",
                self.display_type_info(x),
                self.display_type_info(y),
            ))
                .with_span(self.span(x))
                .with_span(self.span(y))
                .with_secondary_span(self.span(a))
                .with_secondary_span(self.span(b))
        })
    }

    pub fn insert(&mut self, ty: impl Into<TypeInfo>, span: Span) -> TypeId {
        let id = self.new_id();
        self.types.insert(id, ty.into());
        self.spans.insert(id, span);
        id
    }

    pub fn instantiate_ty_inner(&mut self, get_generic: &impl Fn(Ident) -> Option<TypeId>, ty: &SrcNode<Type>) -> TypeId {
        let info = match &**ty {
            Type::GenParam(ident) => TypeInfo::Ref(get_generic(*ident)
                .expect("Generic type without matching generic parameter found in concrete type")),
            Type::Primitive(prim) => TypeInfo::Primitive(prim.clone()),
            Type::List(item) => TypeInfo::List(self.instantiate_ty_inner(get_generic, item)),
            Type::Tuple(items) => TypeInfo::Tuple(items
                .iter()
                .map(|item| self.instantiate_ty_inner(get_generic, item))
                .collect()),
            Type::Record(fields) => TypeInfo::Record(fields
                .iter()
                .map(|(name, field)| (name.clone(), self.instantiate_ty_inner(get_generic, field)))
                .collect()),
            Type::Func(i, o) => TypeInfo::Func(
                self.instantiate_ty_inner(get_generic, i),
                self.instantiate_ty_inner(get_generic, o),
            ),
            Type::Data(data, params) => TypeInfo::Data(**data, params
                .iter()
                .map(|param| self.instantiate_ty_inner(get_generic, param))
                .collect()),
        };

        self.insert(info, ty.span())
    }

    pub fn instantiate_ty(&mut self, generics: &[SrcNode<Ident>], ty: &SrcNode<Type>, span: Span) -> (TypeId, Vec<(SrcNode<Ident>, TypeId)>) {
        // Turn generics into types the `InferCtx` can understand
        let generic_type_ids = generics
            .iter()
            .map(|g| (g.clone(), self.insert(TypeInfo::Unknown(Some(Type::GenParam(**g))), span)))
            .collect::<Vec<_>>();
        let get_generic = |ident| generic_type_ids
            .iter()
            .find(|(name, _)| **name == ident)
            .map(|(_, id)| *id);

        (self.instantiate_ty_inner(&get_generic, ty), generic_type_ids)
    }

    pub fn add_constraint(&mut self, constraint: Constraint) -> ConstraintId {
        self.constraint_id_counter += 1;
        self.constraints.insert(self.constraint_id_counter, constraint);
        self.constraint_id_counter
    }

    // Attempt to infer type information for a constraint, return true if the constraint is solved
    fn solve_inner(&mut self, constraint: Constraint) -> Result<bool, Error> {
        match constraint {
            Constraint::Unary { out, op, a } => {
                let matchers: [fn(_, _, _, _) -> _; 2] = [
                    // -Int => Int
                    |this: &Self, out, op, a| {
                        let mut this = this.scoped();
                        let num = this.insert(TypeInfo::Primitive(Primitive::Number), Span::none());
                        if
                            this.unify(num, out).is_ok()
                            && op == UnaryOp::Neg
                            && this.unify(num, a).is_ok()
                        {
                            Some((TypeInfo::Primitive(Primitive::Number), TypeInfo::Primitive(Primitive::Number)))
                        } else {
                            None
                        }
                    },
                    // !Bool => Bool
                    |this: &Self, out, op, a| {
                        let mut this = this.scoped();
                        let boolean = this.insert(TypeInfo::Primitive(Primitive::Boolean), Span::none());
                        if
                            this.unify(boolean, out).is_ok()
                            && op == UnaryOp::Not
                            && this.unify(boolean, a).is_ok()
                        {
                            Some((TypeInfo::Primitive(Primitive::Boolean), TypeInfo::Primitive(Primitive::Boolean)))
                        } else {
                            None
                        }
                    },
                ];

                let mut matches = matchers
                    .iter()
                    .filter_map(|matcher| matcher(self, out, *op, a))
                    .collect::<Vec<_>>();

                if matches.len() == 0 {
                    Err(Error::custom(format!(
                        "Cannot resolve {} {} as {}",
                        *op,
                        self.display_type_info(a),
                        self.display_type_info(out),
                    ))
                        .with_span(op.span())
                        .with_span(self.span(a)))
                } else if matches.len() > 1 {
                    // Still ambiguous, so we can't infer anything
                    Ok(false)
                } else {
                    let (out_info, a_info) = matches.remove(0);

                    let out_id = self.insert(out_info, self.span(out));
                    let a_id = self.insert(a_info, self.span(a));

                    self.unify(out, out_id)?;
                    self.unify(a, a_id)?;

                    // Constraint solved
                    Ok(true)
                }
            },
            Constraint::Binary { out, op, a, b } => {
                let matchers: [fn(_, _, _, _, _) -> Option<fn(_, _, _) -> _>; 5] = [
                    // Int op Int => Int
                    |this: &Self, out, op, a, b| {
                        let mut this = this.scoped();
                        let num = this.insert(TypeInfo::Primitive(Primitive::Number), Span::none());

                        if
                            this.unify(num, out).is_ok()
                            && [BinaryOp::Add, BinaryOp::Sub, BinaryOp::Mul, BinaryOp::Div, BinaryOp::Rem].contains(&op)
                            && this.unify(num, a).is_ok()
                            && this.unify(num, b).is_ok()
                        {
                            Some(|this: &mut Self, a, b| (TypeInfo::Primitive(Primitive::Number), TypeInfo::Primitive(Primitive::Number), TypeInfo::Primitive(Primitive::Number)))
                        } else {
                            None
                        }
                    },
                    // Int op Int => Bool
                    |this: &Self, out, op, a, b| {
                        let mut this = this.scoped();
                        let num = this.insert(TypeInfo::Primitive(Primitive::Number), Span::none());
                        let boolean = this.insert(TypeInfo::Primitive(Primitive::Boolean), Span::none());
                        if
                            this.unify(boolean, out).is_ok()
                            && [BinaryOp::Eq, BinaryOp::NotEq, BinaryOp::Less, BinaryOp::More, BinaryOp::LessEq, BinaryOp::MoreEq].contains(&op)
                            && this.unify(num, a).is_ok()
                            && this.unify(num, b).is_ok()
                        {
                            Some(|this: &mut Self, a, b| (TypeInfo::Primitive(Primitive::Boolean), TypeInfo::Primitive(Primitive::Number), TypeInfo::Primitive(Primitive::Number)))
                        } else {
                            None
                        }
                    },
                    // Bool op Bool => Bool
                    |this: &Self, out, op, a, b| {
                        let mut this = this.scoped();
                        let boolean = this.insert(TypeInfo::Primitive(Primitive::Boolean), Span::none());
                        if
                            this.unify(boolean, out).is_ok()
                            && [BinaryOp::Eq, BinaryOp::NotEq, BinaryOp::And, BinaryOp::Or].contains(&op)
                            && this.unify(boolean, a).is_ok()
                            && this.unify(boolean, b).is_ok()
                        {
                            Some(|this: &mut Self, a, b| (TypeInfo::Primitive(Primitive::Boolean), TypeInfo::Primitive(Primitive::Boolean), TypeInfo::Primitive(Primitive::Boolean)))
                        } else {
                            None
                        }
                    },
                    // Char op Char => Bool
                    |this: &Self, out, op, a, b| {
                        let mut this = this.scoped();
                        let boolean = this.insert(TypeInfo::Primitive(Primitive::Boolean), Span::none());
                        let character = this.insert(TypeInfo::Primitive(Primitive::Char), Span::none());
                        if
                            this.unify(boolean, out).is_ok()
                            && [BinaryOp::Eq, BinaryOp::NotEq].contains(&op)
                            && this.unify(character, a).is_ok()
                            && this.unify(character, b).is_ok()
                        {
                            Some(|this: &mut Self, a, b| (TypeInfo::Primitive(Primitive::Boolean), TypeInfo::Primitive(Primitive::Char), TypeInfo::Primitive(Primitive::Char)))
                        } else {
                            None
                        }
                    },
                    // [A] ++ [A] => [A]
                    |this: &Self, out, op, a, b| {
                        let mut this = this.scoped();
                        let item_ty = this.insert(TypeInfo::Unknown(None), Span::none()); // A free type term for the list item type
                        let list_ty = this.insert(TypeInfo::List(item_ty), Span::none());

                        if this.unify(list_ty, out).is_ok()
                            && [BinaryOp::Join].contains(&op)
                            && this.unify(list_ty, a).is_ok()
                            && this.unify(list_ty, b).is_ok()
                        {
                            Some(|this: &mut Self, a, b| {
                                let item_ty = this.insert(TypeInfo::Unknown(None), Span::none()); // A free type term for the list item type
                                let list_ty = this.insert(TypeInfo::List(item_ty), this.span(a));

                                (TypeInfo::Ref(list_ty), TypeInfo::Ref(list_ty), TypeInfo::Ref(list_ty))
                            })
                        } else {
                            None
                        }
                    },
                ];

                let mut matches = matchers
                    .iter()
                    .filter_map(|matcher| {
                        matcher(self, out, *op, a, b)
                    })
                    .collect::<Vec<_>>();

                if matches.len() == 0 {
                    Err(Error::custom(format!(
                        "Cannot resolve {} {} {} as {}",
                        self.display_type_info(a),
                        *op,
                        self.display_type_info(b),
                        self.display_type_info(out),
                    ))
                        .with_span(op.span())
                        .with_span(self.span(a))
                        .with_span(self.span(b)))
                } else if matches.len() > 1 {
                    // Still ambiguous, so we can't infer anything
                    Ok(false)
                } else {
                    let (out_info, a_info, b_info) = (matches.remove(0))(self, a, b);

                    let out_id = self.insert(out_info, self.span(out));
                    let a_id = self.insert(a_info, self.span(a));
                    let b_id = self.insert(b_info, self.span(b));

                    self.unify(out, out_id)?;
                    self.unify(a, a_id)?;
                    self.unify(b, b_id)?;

                    // Constraint is solved
                    Ok(true)
                }
            },
            Constraint::Access { out, record, field } => {
                match self.get(self.get_base(record)) {
                    TypeInfo::Unknown(_) => Ok(false), // Can't infer yet
                    TypeInfo::Record(fields) => if let Some((_, ty)) = fields
                        .iter()
                        .find(|(name, _)| **name == *field)
                    {
                        self.unify(out, *ty)?;
                        Ok(true)
                    } else {
                        Err(Error::custom(format!(
                            "No such field '{}' on record {}",
                            **field,
                            self.display_type_info(record),
                        ))
                            .with_span(field.span())
                            .with_span(self.span(record)))
                    },
                    _ => Err(Error::custom(format!(
                        "Type {} does not support field access",
                        self.display_type_info(record),
                    ))
                        .with_span(field.span())
                        .with_span(self.span(record))),
                }
            },
        }
    }

    pub fn solve_all(&mut self) -> Result<(), Error> {
        'solver: loop {
            let constraints = self.constraints.keys().copied().collect::<Vec<_>>();

            // All constraints have been resolved
            if constraints.len() == 0 {
                break Ok(());
            }

            for c in constraints {
                if self.solve_inner(self.constraints[&c].clone())? {
                    self.constraints.remove(&c);
                    continue 'solver;
                }
            }

            break Ok(());
        }
    }

    fn reconstruct_inner(
        &self,
        iter: usize,
        id: TypeId,
    ) -> Result<SrcNode<Type>, ReconstructError> {
        const MAX_RECONSTRUCTION_DEPTH: usize = 1024;
        if iter > MAX_RECONSTRUCTION_DEPTH {
            return Err(ReconstructError::Recursive);
        }

        use TypeInfo::*;
        let ty = match self.get(id) {
            Unknown(ty) => return Err(ReconstructError::Unknown(id)),
            Ref(id) => self.reconstruct_inner(iter + 1, id)?.into_inner(),
            GenParam(name) => Type::GenParam(name),
            Primitive(prim) => Type::Primitive(prim),
            List(a) => Type::List(self.reconstruct_inner(iter + 1, a)?),
            Tuple(items) => Type::Tuple(items
                .into_iter()
                .map(|item| self.reconstruct_inner(iter + 1, item))
                .collect::<Result<_, _>>()?),
            Record(fields) => Type::Record(fields
                .into_iter()
                .map(|(name, field)| Ok((name, self.reconstruct_inner(iter + 1, field)?)))
                .collect::<Result<_, _>>()?),
            Func(a, b) => Type::Func(self.reconstruct_inner(iter + 1, a)?, self.reconstruct_inner(iter + 1, b)?),
            // TODO: This actually gets us the wrong span. We want the span for the data itself,
            // not the whole type including params
            Data(data, params) => Type::Data(SrcNode::new(data, self.span(id)), params
                .into_iter()
                .map(|param| self.reconstruct_inner(iter + 1, param))
                .collect::<Result<_, _>>()?),
        };

        Ok(SrcNode::new(ty, self.span(id)))
    }

    pub fn reconstruct(&self, id: TypeId, span: Span) -> Result<SrcNode<Type>, Error> {
        self.reconstruct_inner(0, id).map_err(|err| match err {
            ReconstructError::Recursive => Error::custom(format!("Recursive type"))
                .with_span(self.span(id)),
            ReconstructError::Unknown(a) => {
                let msg = match self.get(self.get_base(id)) {
                    TypeInfo::Unknown(_) => format!("Cannot infer type"),
                    _ => format!("Cannot infer type {} in {}", self.display_type_info(a), self.display_type_info(id)),
                };
                Error::custom(msg)
                    .with_span(span)
                    .with_secondary_span(self.span(id))
                    .with_hint(format!("Specify all missing types"))
            },
        })
    }
}

enum ReconstructError {
    Unknown(TypeId),
    Recursive,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic() {
        let mut ctx = InferCtx::default();

        // Create some types
        let number = Primitive::Number;
        let boolean = Primitive::Boolean;

        let a = ctx.insert(TypeInfo::Primitive(number), Span::none());
        let b = ctx.insert(TypeInfo::Unknown(None), Span::none());
        let c = ctx.insert(TypeInfo::Func(a, b), Span::none());
        let d = ctx.insert(TypeInfo::Primitive(boolean), Span::none());
        ctx.unify(b, d);

        assert_eq!(
            ctx.reconstruct(c, Span::none()).unwrap().into_inner(),
            Type::Func(
                SrcNode::new(Type::Primitive(number), Span::none()),
                SrcNode::new(Type::Primitive(boolean), Span::none()),
            ),
        );
    }

    #[test]
    fn list() {
        let mut ctx = InferCtx::default();

        // Create some types
        let number = Primitive::Number;

        let a = ctx.insert(TypeInfo::Unknown(None), Span::none());
        let b = ctx.insert(TypeInfo::Unknown(None), Span::none());
        let c = ctx.insert(TypeInfo::Primitive(number), Span::none());
        let d = ctx.insert(TypeInfo::List(a), Span::none());
        ctx.unify(a, b);
        ctx.unify(b, c);

        assert_eq!(
            ctx.reconstruct(d, Span::none()).unwrap().into_inner(),
            Type::List(SrcNode::new(Type::Primitive(number), Span::none())),
        );
    }
}
