use std::collections::HashMap;
use internment::LocalIntern;

type Ident = LocalIntern<String>;

#[derive(Clone, Debug, PartialEq)]
pub enum Atom {
    Num,
    Bool,
}

#[derive(PartialEq, Debug)]
pub enum Type {
    Named(DataId, Vec<Self>), // parameters
    List(Box<Self>),
    Tuple(Vec<Self>),
    Func(Box<Self>, Box<Self>),
}

// Generics

type GenericId = usize;

// Data types

type DataId = usize;

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

type TraitId = usize;

pub enum TypeConstraint {
    MustImpl(TraitId, Vec<InnerDataType>),
}

pub struct Trait {
    pub params: Vec<GenericParam>,
    pub constraints: Vec<TypeConstraint>,
    pub associated_types: HashMap<Ident, Vec<TypeConstraint>>,
    pub constants: HashMap<Ident, InnerDataType>,
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
        };

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
                add,
            },
        })
    }
}

pub struct CorePrimitives {
    number: DataId,
    boolean: DataId,
}

pub struct CoreTraits {
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
}

pub type TypeId = usize;

#[derive(Clone)]
pub enum TypeInfo {
    Unknown,

    Named(DataId, Vec<TypeId>),

    List(TypeId),
    Tuple(Vec<TypeId>),
    Func(TypeId, TypeId),

    AssociatedType(TraitId, TypeId, Ident), // e.g: (Add<Int>, Int, "Output") = Int
}

impl From<DataId> for TypeInfo {
    fn from(id: DataId) -> Self {
        TypeInfo::Named(id, Vec::new())
    }
}

#[derive(Default)]
pub struct InferCtx {
    id_counter: TypeId,
    refs: HashMap<TypeId, TypeId>,
    types: HashMap<TypeId, TypeInfo>,
}

impl InferCtx {
    fn new_id(&mut self) -> TypeId {
        self.id_counter += 1;
        self.id_counter
    }

    pub fn get(&self, id: TypeId) -> TypeInfo {
        let indirect_id = self.refs
            .get(&id)
            .expect("No such TypeID registered with inference context");
        self.types
            .get(indirect_id)
            .unwrap()
            .clone()
    }

    fn link(&mut self, a: TypeId, b: TypeId) {
        self.refs.insert(a, b);
    }

    pub fn unify(&mut self, a: TypeId, b: TypeId) -> Result<(), InferError> {
        use TypeInfo::*;

        match (self.get(a), self.get(b)) {
            (Unknown, _) => {
                self.link(a, b);
                Ok(())
            },
            (_, Unknown) => self.unify(b, a), // TODO: does ordering matter?
            (Named(a, a_params), Named(b, b_params)) if a == b && a_params.len() == b_params.len() => a_params
                .into_iter()
                .zip(b_params.into_iter())
                .try_fold((), |_, (a, b)| self.unify(a, b)),
            (List(a), List(b)) => self.unify(a, b),
            (Tuple(a), Tuple(b)) if a.len() == b.len() => a
                .into_iter()
                .zip(b.into_iter())
                .try_fold((), |_, (a, b)| self.unify(a, b)),
            (Func(ai, ao), Func(bi, bo)) => self.unify(ai, bi).and_then(|_| self.unify(ao, bo)),
            (_, _) => Err(InferError::Mismatch(a.clone(), b.clone())),
        }
    }

    pub fn insert(&mut self, ty: impl Into<TypeInfo>) -> TypeId {
        let id = self.new_id();
        self.refs.insert(id, id);
        self.types.insert(id, ty.into());
        id
    }

    pub fn reconstruct(&self, id: TypeId) -> Result<Type, InferError> {
        match self.get(id) {
            TypeInfo::Unknown => Err(InferError::CannotInfer),
            TypeInfo::Named(id, params) => Ok(Type::Named(id, params.into_iter().map(|a| self.reconstruct(a)).collect::<Result<_, _>>()?)),
            TypeInfo::List(a) => Ok(Type::List(Box::new(self.reconstruct(a)?))),
            TypeInfo::Tuple(a) => Ok(Type::Tuple(a.into_iter().map(|a| self.reconstruct(a)).collect::<Result<_, _>>()?)),
            TypeInfo::Func(a, b) => Ok(Type::Func(Box::new(self.reconstruct(a)?), Box::new(self.reconstruct(b)?))),
            TypeInfo::AssociatedType(_, _, _) => todo!("Query trait engine to determine type of associated type"),
        }
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

        let a = ctx.insert(number);
        let b = ctx.insert(TypeInfo::Unknown);
        let c = ctx.insert(TypeInfo::Func(a, b));
        let d = ctx.insert(boolean);
        ctx.unify(b, d);

        assert_eq!(
            ctx.reconstruct(c),
            Ok(Type::Func(
                Box::new(Type::Named(number, Vec::new())),
                Box::new(Type::Named(boolean, Vec::new())),
            )),
        );
    }
}
