use std::{
    fmt,
    collections::HashMap,
};
use internment::LocalIntern;
use crate::{
    error::Error,
    src::SrcRegion,
    node2::SrcNode,
    scope::Scope,
};

type Ident = LocalIntern<String>;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Primitive {
    Boolean,
    Number,
    String,
}

impl fmt::Display for Primitive {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Primitive::Boolean => write!(f, "Bool"),
            Primitive::Number => write!(f, "Num"),
            Primitive::String => write!(f, "Str"),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Type {
    Primitive(Primitive),
    Data(DataId),
    List(SrcNode<Self>),
    Tuple(Vec<SrcNode<Self>>),
    Func(SrcNode<Self>, SrcNode<Self>),
    GenParam(Ident),
}

impl Type {
    pub fn visit(self: &SrcNode<Self>, f: &mut impl FnMut(&SrcNode<Self>)) {
        let iter = match &**self {
            Type::Primitive(_) => {},
            Type::Data(_) => {},
            Type::List(item) => item.visit(f),
            Type::Tuple(items) => items
                .iter()
                .for_each(|item| item.visit(f)),
            Type::Func(i, o) => {
                i.visit(f);
                o.visit(f);
            },
            Type::GenParam(_) => {},
        };

        f(self);
    }

    pub fn visit_mut(self: &mut SrcNode<Self>, f: &mut impl FnMut(&mut SrcNode<Self>)) {
        let iter = match &mut **self {
            Type::Primitive(_) => {},
            Type::Data(_) => {},
            Type::List(item) => item.visit_mut(f),
            Type::Tuple(items) => items
                .iter_mut()
                .for_each(|item| item.visit_mut(f)),
            Type::Func(i, o) => {
                i.visit_mut(f);
                o.visit_mut(f);
            },
            Type::GenParam(_) => {},
        };

        f(self);
    }

    fn substitute_generics(self: &mut SrcNode<Self>, name: Ident, new_ty: SrcNode<Self>) {
        self.visit_mut(&mut |ty| {
            if let Type::GenParam(gen) = &**ty {
                if *gen == name {
                    *ty = new_ty.clone();
                }
            }
        });
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Primitive(prim) => write!(f, "{}", prim),
            Type::Data(_) => write!(f, "TODO"),
            Type::List(item) => write!(f, "[{}]", **item),
            Type::Tuple(items) => {
                write!(f, "(")?;
                items
                    .iter()
                    .map(|item| write!(f, "{}, ", **item))
                    .collect::<Result<_, _>>()?;
                write!(f, ")")?;
                Ok(())
            },
            Type::Func(i, o) => write!(f, "({} -> {})", **i, **o),
            Type::GenParam(ident) => write!(f, "{}", ident),
        }
    }
}

// Generics

pub type GenericId = usize;

// Data types

pub type DataId = usize;

pub enum DataType {
    Primitive,
    Sum(Vec<Type>, HashMap<Ident, Type>),
    Tuple(Vec<Type>),
    Record(HashMap<Ident, Type>),
}

#[derive(Default)]
pub struct DataCtx {
    id_counter: DataId,
    decls: HashMap<DataId, (Vec<Ident>, DataType)>,
}

impl DataCtx {
    fn new_id(&mut self) -> DataId {
        self.id_counter += 1;
        self.id_counter
    }

    pub fn declare(&mut self) -> DataId {
        self.new_id()
    }

    pub fn define(&mut self, id: DataId, params: Vec<Ident>, data: DataType) {
        self.decls.insert(id, (params, data));
    }

    pub fn insert(&mut self, params: Vec<Ident>, data: DataType) -> DataId {
        let id = self.declare();
        self.define(id, params, data);
        id
    }
}

// Trait resolution

pub type TraitId = usize;

pub enum TypeConstraint {
    MustImpl(TraitId, Vec<Type>),
}

pub struct Trait {
    pub params: Vec<Ident>,
    pub constraints: Vec<TypeConstraint>,
    pub associated_types: HashMap<Ident, Vec<TypeConstraint>>,
    pub constants: HashMap<Ident, Type>,
}

pub struct Impl {
    params: Vec<Ident>,
    tr: (TraitId, Vec<Type>),
    target: Type,
    associated_types: HashMap<Ident, Type>,
}

#[derive(Default)]
pub struct TraitCtx {
    id_counter: TraitId,
    traits: HashMap<TraitId, Trait>,
    impls: Vec<Impl>,
}

impl TraitCtx {
    fn new_id(&mut self) -> TraitId {
        self.id_counter += 1;
        self.id_counter
    }

    pub fn insert_trait(&mut self, tr: Trait) -> TypeId {
        let id = self.new_id();
        self.traits.insert(id, tr);
        id
    }

    pub fn insert_impl(&mut self, imp: Impl) {
        self.impls.push(imp);
    }
}

// Type Engine

#[derive(Default)]
pub struct TypeEngine {
    data_ctx: DataCtx,
    trait_ctx: TraitCtx,
}

impl TypeEngine {
    pub fn with_core(mut self) -> (Self, Core) {
        let neg = self.trait_ctx.insert_trait(Trait {
            params: Vec::new(),
            constraints: Vec::new(),
            associated_types: std::iter::once((Ident::new("Out".to_string()), Vec::new())).collect(),
            constants: HashMap::default(),
        });

        let add = self.trait_ctx.insert_trait(Trait {
            params: vec![Ident::new("Rhs".to_string())],
            constraints: Vec::new(),
            associated_types: std::iter::once((Ident::new("Out".to_string()), Vec::new())).collect(),
            constants: HashMap::default(),
        });

        /*
        macro_rules! make_impl {
            (<$(params:tt),*> $tr:tt for $ty:tt {$(body_item:tt)*}) => {

            };
        }

        make_impl! {
            <> Add for Num {
                type Out = Num;
            }
        }
        */

        // -Num
        self.trait_ctx.insert_impl(Impl {
            params: Vec::new(),
            tr: (neg, Vec::new()),
            target: Type::Primitive(Primitive::Number),
            associated_types: std::iter::once((Ident::new("Out".to_string()), Type::Primitive(Primitive::Number))).collect(),
        });

        // Num + Num
        self.trait_ctx.insert_impl(Impl {
            params: Vec::new(),
            tr: (add, vec![Type::Primitive(Primitive::Number)]),
            target: Type::Primitive(Primitive::Number),
            associated_types: std::iter::once((Ident::new("Out".to_string()), Type::Primitive(Primitive::Number))).collect(),
        });

        (self, Core {
            traits: CoreTraits {
                neg,
                add,
            },
        })
    }
}

pub struct CoreTraits {
    pub neg: TraitId,
    pub add: TraitId,
}

pub struct Core {
    pub traits: CoreTraits,
}

// Inference

pub type TypeId = usize;

#[derive(Clone, Debug)]
pub enum TypeInfo {
    Unknown,
    Ref(TypeId),
    Primitive(Primitive),
    List(TypeId),
    Tuple(Vec<TypeId>),
    Func(TypeId, TypeId),
    Data(DataId),
    GenParam(Ident),
    Associated(TraitId, Vec<TypeId>, TypeId, Ident), // e.g: (Add<Int>, Int, "Output") = Int
}

impl From<DataId> for TypeInfo {
    fn from(id: DataId) -> Self {
        TypeInfo::Data(id)
    }
}

#[derive(Default, Debug)]
pub struct InferCtx {
    id_counter: TypeId,
    types: HashMap<TypeId, TypeInfo>,
    regions: HashMap<TypeId, SrcRegion>,
}

impl InferCtx {
    fn new_id(&mut self) -> TypeId {
        self.id_counter += 1;
        self.id_counter
    }

    pub fn get(&self, id: TypeId) -> TypeInfo {
        self.types[&id].clone()
    }

    pub fn region(&self, id: TypeId) -> SrcRegion {
        self.regions[&id].clone()
        // Follow the chain of references
        // match self.get(id) {
        //     TypeInfo::Ref(a) => self.region(a),
        //     _ => self.regions[&id].clone(),
        // }
    }

    fn link(&mut self, a: TypeId, b: TypeId) {
        *self.types
            .get_mut(&a)
            .unwrap() = TypeInfo::Ref(b);
    }

    pub fn display_type_info(&self, id: TypeId) -> impl fmt::Display + '_ {
        #[derive(Copy, Clone)]
        struct TypeInfoDisplay<'a> {
            ctx: &'a InferCtx,
            id: TypeId,
        }

        impl<'a> TypeInfoDisplay<'a> {
            fn with_id(mut self, id: TypeId) -> Self {
                self.id = id;
                self
            }
        }

        impl<'a> fmt::Display for TypeInfoDisplay<'a> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                match self.ctx.get(self.id) {
                    TypeInfo::Unknown => write!(f, "_"),
                    TypeInfo::Primitive(prim) => write!(f, "{}", prim),
                    TypeInfo::Ref(id) => self.with_id(id).fmt(f),
                    TypeInfo::List(id) => write!(f, "[{}]", self.with_id(id)),
                    TypeInfo::Tuple(ids) => {
                        write!(f, "(")?;
                        ids
                            .iter()
                            .map(|id| write!(f, "{}, ", self.with_id(*id)))
                            .collect::<Result<_, _>>()?;
                        write!(f, ")")?;
                        Ok(())
                    },
                    TypeInfo::Func(i, o) => write!(f, "({} -> {})", self.with_id(i), self.with_id(o)),
                    TypeInfo::Data(_) => write!(f, "TODO"),
                    TypeInfo::GenParam(ident) => write!(f, "{}", ident),
                    TypeInfo::Associated(tr, params, a, assoc) => {
                        write!(f, "<{} as Trait<", self.with_id(a))?;
                        params
                            .iter()
                            .map(|id| write!(f, "{}, ", self.with_id(*id)))
                            .collect::<Result<Vec<_>, _>>()?;
                        write!(f, ">>::{}", assoc)?;
                        Ok(())
                    },
                }
            }
        }

        TypeInfoDisplay {
            ctx: self,
            id
        }
    }

    fn unify_inner(&mut self, iter: usize, a: TypeId, b: TypeId) -> Result<(), Error> {
        const MAX_UNIFICATION_DEPTH: usize = 1024;
        if iter > MAX_UNIFICATION_DEPTH {
            panic!("Maximum unification depth reached (this error should not occur without extremely large types)");
        }

        use TypeInfo::*;
        match (self.get(a), self.get(b)) {
            (Ref(a), _) => self.unify_inner(iter, a, b),
            (_, Ref(_)) => self.unify_inner(iter, b, a),
            (Unknown, _) => {
                self.link(a, b);
                Ok(())
            },
            (_, Unknown) => self.unify_inner(iter, b, a), // TODO: does ordering matter?
            (GenParam(a), GenParam(b)) if a == b => Ok(()),
            (Primitive(a), Primitive(b)) if a == b => Ok(()),
            (Data(a), Data(b)) if a == b => Ok(()),
            (List(a), List(b)) => self.unify_inner(iter + 1, a, b),
            (Tuple(a), Tuple(b)) if a.len() == b.len() => a
                .into_iter()
                .zip(b.into_iter())
                .try_fold((), |_, (a, b)| self.unify_inner(iter + 1, a, b)),
            (Func(ai, ao), Func(bi, bo)) => self.unify_inner(iter + 1, ai, bi)
                .and_then(|_| self.unify_inner(iter + 1, ao, bo)),
            (x, y) => {
                Err(Error::custom(format!(
                    "Type mismatch between {} and {}",
                    self.display_type_info(a),
                    self.display_type_info(b),
                ))
                    .with_region(self.region(a))
                    .with_region(self.region(b)))
            },
        }
    }

    pub fn unify(&mut self, a: TypeId, b: TypeId) -> Result<(), Error> {
        self.unify_inner(0, a, b)
    }

    pub fn insert(&mut self, ty: impl Into<TypeInfo>, region: SrcRegion) -> TypeId {
        let id = self.new_id();
        self.types.insert(id, ty.into());
        self.regions.insert(id, region);
        id
    }

    pub fn insert_ty_inner(&mut self, get_generic: &impl Fn(Ident) -> Option<TypeInfo>, ty: &SrcNode<Type>) -> TypeId {
        let info = match &**ty {
            Type::GenParam(ident) => get_generic(*ident)
                .expect("Generic type without matching generic parameter found in concrete type"),
            Type::Primitive(prim) => TypeInfo::Primitive(prim.clone()),
            Type::Data(data) => TypeInfo::Data(*data),
            Type::List(item) => TypeInfo::List(self.insert_ty_inner(get_generic, item)),
            Type::Tuple(items) => TypeInfo::Tuple(
                items.iter().map(|item| self.insert_ty_inner(get_generic, item)).collect(),
            ),
            Type::Func(i, o) => TypeInfo::Func(
                self.insert_ty_inner(get_generic, i),
                self.insert_ty_inner(get_generic, o),
            ),
        };

        self.insert(info, ty.region())
    }

    pub fn insert_ty(&mut self, generics: &[SrcNode<Ident>], ty: &SrcNode<Type>) -> TypeId {
        // Turn generics into types the `InferCtx` can understand
        let generic_type_ids = generics
            .iter()
            .map(|g| (**g, self.insert(TypeInfo::Unknown, g.region())))
            .collect::<Vec<_>>();
        let get_generic = |ident| generic_type_ids
            .iter()
            .find(|(name, _)| *name == ident)
            .map(|(_, id)| TypeInfo::Ref(*id));

        self.insert_ty_inner(&get_generic, ty)
    }

    fn reconstruct_inner(
        &self,
        iter: usize,
        id: TypeId,
    ) -> Result<SrcNode<Type>, Error> {
        const MAX_RECONSTRUCTION_DEPTH: usize = 1024;
        if iter > MAX_RECONSTRUCTION_DEPTH {
            return Err(Error::custom(format!("Recursive type"))
                .with_region(self.region(id)));
        }

        let ty = match self.get(id) {
            TypeInfo::Unknown => {
                return Err(Error::custom(format!("Cannot infer type"))
                    .with_region(self.region(id)));
            },
            TypeInfo::Ref(id) => self.reconstruct_inner(iter + 1, id)?.into_inner(),
            TypeInfo::GenParam(name) => Type::GenParam(name),
            TypeInfo::Primitive(prim) => Type::Primitive(prim),
            TypeInfo::Data(data) => Type::Data(data),
            TypeInfo::List(a) => Type::List(self.reconstruct_inner(iter + 1, a)?),
            TypeInfo::Tuple(a) => Type::Tuple(a.into_iter().map(|a| self.reconstruct_inner(iter + 1, a)).collect::<Result<_, _>>()?),
            TypeInfo::Func(a, b) => Type::Func(self.reconstruct_inner(iter + 1, a)?, self.reconstruct_inner(iter + 1, b)?),
            TypeInfo::Associated(_, _, _, _) => todo!("Query trait engine to determine type of associated type"),
        };

        Ok(SrcNode::new(ty, self.region(id)))
    }

    pub fn reconstruct(&self, id: TypeId) -> Result<SrcNode<Type>, Error> {
        self.reconstruct_inner(0, id)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic() {
        let mut ctx = InferCtx::default();

        // Create some types
        let number: DataId = 0;
        let boolean: DataId = 1;

        let a = ctx.insert(number, SrcRegion::none());
        let b = ctx.insert(TypeInfo::Unknown, SrcRegion::none());
        let c = ctx.insert(TypeInfo::Func(a, b), SrcRegion::none());
        let d = ctx.insert(boolean, SrcRegion::none());
        ctx.unify(b, d);

        assert_eq!(
            ctx.reconstruct(c).unwrap().into_inner(),
            Type::Func(
                SrcNode::new(Type::Data(number), SrcRegion::none()),
                SrcNode::new(Type::Data(boolean), SrcRegion::none()),
            ),
        );
    }

    #[test]
    fn list() {
        let mut ctx = InferCtx::default();

        // Create some types
        let number: DataId = 0;

        let a = ctx.insert(TypeInfo::Unknown, SrcRegion::none());
        let b = ctx.insert(TypeInfo::Unknown, SrcRegion::none());
        let c = ctx.insert(number, SrcRegion::none());
        let d = ctx.insert(TypeInfo::List(a), SrcRegion::none());
        ctx.unify(a, b);
        ctx.unify(b, c);

        assert_eq!(
            ctx.reconstruct(d).unwrap().into_inner(),
            Type::List(SrcNode::new(Type::Data(number), SrcRegion::none())),
        );
    }
}
