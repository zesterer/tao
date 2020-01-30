use std::collections::HashMap;
use internment::LocalIntern;
use crate::parse::{UnaryOp, BinaryOp};

type Ident = LocalIntern<String>;

#[derive(Clone, Debug, PartialEq)]
pub enum Atom {
    Num,
    Bool,
}

#[derive(PartialEq, Debug)]
pub enum Type {
    Atom(Atom),
    List(Box<Self>),
    Tuple(Vec<Self>),
    Func(Box<Self>, Box<Self>),
}

// Inference

#[derive(PartialEq, Debug)]
pub enum InferError {
    CannotInfer,
    Mismatch(TypeId, TypeId),
}

type TypeId = usize;
type TraitId = usize;

#[derive(Clone)]
pub enum TypeInfo {
    Unknown,

    Atom(Atom),

    List(TypeId),
    Tuple(Vec<TypeId>),
    Func(TypeId, TypeId),

    AssociatedType(TraitId, TypeId, Ident), // e.g: (Add<Int>, Int, "Output") = Int
}

impl From<Atom> for TypeInfo {
    fn from(atom: Atom) -> Self {
        TypeInfo::Atom(atom)
    }
}

#[derive(Default)]
pub struct InferCtx {
    refs: HashMap<TypeId, TypeId>,
    types: HashMap<TypeId, TypeInfo>,
    id_counter: TypeId,
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
        self.refs.insert(b, a);
    }

    pub fn unify(&mut self, a: TypeId, b: TypeId) -> Result<(), InferError> {
        use TypeInfo::*;

        match (self.get(a), self.get(b)) {
            (Unknown, _) => {
                self.link(a, b);
                Ok(())
            },
            (_, Unknown) => self.unify(b, a), // TODO: does ordering matter?
            (Atom(a), Atom(b)) if a == b => Ok(()),
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
            TypeInfo::Atom(atom) => Ok(Type::Atom(atom)),
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
        let a = ctx.insert(Atom::Num);
        let b = ctx.insert(Atom::Bool);
        let c = ctx.insert(TypeInfo::Func(a, b));

        assert_eq!(
            ctx.reconstruct(c),
            Ok(Type::Func(
                Box::new(Type::Atom(Atom::Num)),
                Box::new(Type::Atom(Atom::Bool)),
            )),
        );
    }
}
