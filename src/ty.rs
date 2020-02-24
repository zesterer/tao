use std::collections::HashMap;
use internment::LocalIntern;
use crate::{
    error::Error,
    src::SrcRegion,
    node2::SrcNode,
};

type Ident = LocalIntern<String>;

#[derive(Clone, Debug, PartialEq)]
pub enum Atom {
    Num,
    Bool,
}

#[derive(PartialEq, Debug)]
pub enum Type {
    Named(DataId, Vec<SrcNode<Self>>), // parameters
    List(SrcNode<Self>),
    Tuple(Vec<SrcNode<Self>>),
    Func(SrcNode<Self>, SrcNode<Self>),
}

// Generics

pub type GenericId = usize;

// Data types

pub type DataId = usize;

#[derive(Clone, Debug)]
pub enum InnerDataType {
    Local(GenericId),
    Global(DataId, Vec<InnerDataType>),
}

impl From<DataId> for InnerDataType {
    fn from(id: DataId) -> Self {
        InnerDataType::Global(id, Vec::new())
    }
}

pub struct GenericParam {
    id: GenericId,
    constraints: Vec<TypeConstraint>,
}

impl GenericParam {
    pub fn unconstrained(id: GenericId) -> Self {
        Self {
            id,
            constraints: Vec::new(),
        }
    }
}

pub enum DataType {
    Primitive,
    Sum(Vec<GenericParam>, HashMap<Ident, InnerDataType>),
    Tuple(Vec<GenericParam>, Vec<InnerDataType>),
    Record(Vec<GenericParam>, HashMap<Ident, InnerDataType>),
}

#[derive(Default)]
pub struct DataCtx {
    id_counter: DataId,
    decls: HashMap<DataId, DataType>,
}

impl DataCtx {
    fn new_id(&mut self) -> DataId {
        self.id_counter += 1;
        self.id_counter
    }

    pub fn declare(&mut self) -> DataId {
        self.new_id()
    }

    pub fn define(&mut self, id: DataId, data: DataType) {
        self.decls.insert(id, data);
    }

    pub fn insert(&mut self, data: DataType) -> DataId {
        let id = self.declare();
        self.define(id, data);
        id
    }
}

// Trait resolution

pub type TraitId = usize;

pub enum TypeConstraint {
    MustImpl(TraitId, Vec<InnerDataType>),
}

pub struct Trait {
    pub params: Vec<GenericParam>,
    pub constraints: Vec<TypeConstraint>,
    pub associated_types: HashMap<Ident, Vec<TypeConstraint>>,
    pub constants: HashMap<Ident, InnerDataType>,
}

#[derive(Clone, Debug, Default)]
pub struct InnerTrait {
    id: TraitId,
    params: Vec<InnerDataType>,
}

impl InnerTrait {
    pub fn new(id: TraitId, params: Vec<InnerDataType>) -> Self {
        Self {
            id,
            params,
        }
    }
}

impl From<TraitId> for InnerTrait {
    fn from(id: TraitId) -> Self {
        Self {
            id,
            params: Vec::new(),
        }
    }
}

pub struct Impl {
    params: Vec<GenericParam>,
    tr: (TraitId, Vec<InnerDataType>),
    target: InnerDataType,
    associated_types: HashMap<Ident, InnerDataType>,
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
        let primitives = CorePrimitives {
            number: self.data_ctx.insert(DataType::Primitive),
            boolean: self.data_ctx.insert(DataType::Primitive),
            string: self.data_ctx.insert(DataType::Primitive),
        };

        let neg = self.trait_ctx.insert_trait(Trait {
            params: Vec::new(),
            constraints: Vec::new(),
            associated_types: std::iter::once((Ident::new("Out".to_string()), Vec::new())).collect(),
            constants: HashMap::default(),
        });

        let add = self.trait_ctx.insert_trait(Trait {
            params: vec![GenericParam::unconstrained(0)],
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
            target: primitives.number.into(),
            associated_types: std::iter::once((Ident::new("Out".to_string()), primitives.number.into())).collect(),
        });

        // Num + Num
        self.trait_ctx.insert_impl(Impl {
            params: Vec::new(),
            tr: (add, vec![primitives.number.into()]),
            target: primitives.number.into(),
            associated_types: std::iter::once((Ident::new("Out".to_string()), primitives.number.into())).collect(),
        });

        (self, Core {
            primitives,
            traits: CoreTraits {
                neg,
                add,
            },
        })
    }
}

pub struct CorePrimitives {
    pub number: DataId,
    pub boolean: DataId,
    pub string: DataId,
}

pub struct CoreTraits {
    pub neg: TraitId,
    pub add: TraitId,
}

pub struct Core {
    pub primitives: CorePrimitives,
    pub traits: CoreTraits,
}

// Inference

#[derive(PartialEq, Debug)]
pub enum InferError {
    CannotInfer,
    Mismatch(TypeId, TypeId),
    Recursive(TypeId),
}

impl Into<Error> for InferError {
    fn into(self) -> Error {
        // TODO
        match self {
            InferError::CannotInfer => Error::cannot_infer_type(panic!("Cannot infer type!")),
            InferError::Mismatch(_, _) => Error::type_mismatch(
                panic!("Type mismatch!"),
                panic!("Type mismatch!"),
            ),
            InferError::Recursive(_) => panic!("Recursive type!"),
        }
    }
}

pub type TypeId = usize;

#[derive(Clone, Debug)]
pub enum TypeInfo {
    Unknown,
    Ref(TypeId),

    Named(DataId, Vec<TypeId>),

    List(TypeId),
    Tuple(Vec<TypeId>),
    Func(TypeId, TypeId),

    Associated(InnerTrait, TypeId, Ident), // e.g: (Add<Int>, Int, "Output") = Int
}

impl From<DataId> for TypeInfo {
    fn from(id: DataId) -> Self {
        TypeInfo::Named(id, Vec::new())
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
    }

    fn link(&mut self, a: TypeId, b: TypeId) {
        *self.types
            .get_mut(&a)
            .unwrap() = TypeInfo::Ref(b);
    }

    fn unify_inner(&mut self, iter: usize, a: TypeId, b: TypeId) -> Result<(), Error> {
        const MAX_UNIFICATION_DEPTH: usize = 1024;
        if iter > MAX_UNIFICATION_DEPTH {
            panic!("Maximum unification depth reached (this error should not occur without extremely large types)");
        }

        use TypeInfo::*;
        match (self.get(a), self.get(b)) {
            (Ref(a), _) => self.unify_inner(iter + 1, a, b),
            (_, Ref(_)) => self.unify_inner(iter + 1, b, a),
            (Unknown, _) => {
                self.link(a, b);
                Ok(())
            },
            (_, Unknown) => self.unify_inner(iter + 1, b, a), // TODO: does ordering matter?
            (Named(a, a_params), Named(b, b_params)) if a == b && a_params.len() == b_params.len() => a_params
                .into_iter()
                .zip(b_params.into_iter())
                .try_fold((), |_, (a, b)| self.unify_inner(iter + 1, a, b)),
            (List(a), List(b)) => self.unify_inner(iter + 1, a, b),
            (Tuple(a), Tuple(b)) if a.len() == b.len() => a
                .into_iter()
                .zip(b.into_iter())
                .try_fold((), |_, (a, b)| self.unify_inner(iter + 1, a, b)),
            (Func(ai, ao), Func(bi, bo)) => self.unify_inner(iter + 1, ai, bi)
                .and_then(|_| self.unify_inner(iter + 1, ao, bo)),
            (x, y) => {
                println!("mismatch: {:?} [{:?}], {:?} [{:?}]", x, self.region(a), y, self.region(b));
                Err(InferError::Mismatch(a.clone(), b.clone()).into())
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

    pub fn insert_ty(&mut self, ty: &SrcNode<Type>) -> TypeId {
        let info = match &**ty {
            Type::Named(data, args) => TypeInfo::Named(
                *data,
                args.iter().map(|arg| self.insert_ty(arg)).collect(),
            ),
            Type::List(item) => TypeInfo::List(self.insert_ty(item)),
            Type::Tuple(items) => TypeInfo::Tuple(
                items.iter().map(|item| self.insert_ty(item)).collect(),
            ),
            Type::Func(i, o) => TypeInfo::Func(
                self.insert_ty(i),
                self.insert_ty(o),
            ),
        };

        self.insert(info, ty.region())
    }

    fn reconstruct_inner(&self, iter: usize, id: TypeId) -> Result<SrcNode<Type>, Error> {
        const MAX_RECONSTRUCTION_DEPTH: usize = 1024;
        if iter > MAX_RECONSTRUCTION_DEPTH {
            return Err(InferError::Recursive(id).into());
        }

        let ty = match self.get(id) {
            TypeInfo::Unknown => {
                println!("cannot infer: [{:?}]", self.region(id));
                return Err(InferError::CannotInfer.into());
            },
            TypeInfo::Ref(id) => self.reconstruct_inner(iter + 1, id)?.into_inner(),
            TypeInfo::Named(id, params) => Type::Named(id, params.into_iter().map(|a| self.reconstruct_inner(iter + 1, a)).collect::<Result<_, _>>()?),
            TypeInfo::List(a) => Type::List(self.reconstruct_inner(iter + 1, a)?),
            TypeInfo::Tuple(a) => Type::Tuple(a.into_iter().map(|a| self.reconstruct_inner(iter + 1, a)).collect::<Result<_, _>>()?),
            TypeInfo::Func(a, b) => Type::Func(self.reconstruct_inner(iter + 1, a)?, self.reconstruct_inner(iter + 1, b)?),
            TypeInfo::Associated(_, _, _) => todo!("Query trait engine to determine type of associated type"),
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
                Box::new(Type::Named(number, Vec::new())),
                Box::new(Type::Named(boolean, Vec::new())),
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
            Type::List(Box::new(Type::Named(number, Vec::new()))),
        );
    }
}
