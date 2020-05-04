use std::collections::HashMap;
use internment::LocalIntern;
use crate::{
    ast,
    ty::{Type, Primitive},
    error::Error,
    node2::RawTypeNode,
    hir2::{self, Value, Path},
};

type Ident = LocalIntern<String>;

pub type Unary = ast::UnaryOp;
pub type Binary = ast::BinaryOp;

// Raw types have had their name erased (since type checking and inference has already occurred)
#[derive(Clone, Hash, PartialEq, Eq)]
pub enum RawType {
    Primitive(Primitive),
    List(Box<Self>),
    Product(Vec<Self>),
    Sum(Vec<Self>),
    Func(Box<Self>, Box<Self>),
}

type DefId = LocalIntern<(Ident, Vec<RawType>)>;

pub enum Expr {
    Value(Value),
    // Get the value of the given global
    GetGlobal(DefId),
    // Get the value of the given local
    GetLocal(Ident),
    // Perform a built-in unary operation
    Unary(Unary, RawTypeNode<Expr>),
    // Perform a built-in binary operation
    Binary(Binary, RawTypeNode<Expr>, RawTypeNode<Expr>),
    // Call the given expression with the given arguments
    Call(RawTypeNode<Expr>, Vec<RawTypeNode<Expr>>),
    // Create a function with the given parameters and body
    MakeFunc(Vec<RawTypeNode<Expr>>, RawTypeNode<Expr>),
}

#[derive(Default)]
pub struct Program {
    globals: HashMap<DefId, RawTypeNode<Expr>>,
}

impl Program {
    pub fn from_hir(prog: &hir2::Program, entry: Ident) -> Result<(Self, DefId), Error> {
        let mut this = Self::default();

        let entry = this.instantiate_def(prog, entry, Vec::new());

        Ok((this, entry))
    }

    fn instantiate_def(&mut self, prog: &hir2::Program, name: Ident, params: Vec<RawType>) -> DefId {
        let def_id = LocalIntern::new((name, params.clone()));

        if !self.globals.contains_key(&def_id) {
            let def = prog.root.def(name).expect("Expected def to exist");

            let generics = def.generics
                .iter()
                .zip(params.iter())
                .map(|(name, param)| (**name, param))
                .collect::<HashMap<_, _>>();

            let body = self.instantiate_expr(&def.body, &mut |gen| generics.get(&gen).cloned().cloned().unwrap());
            self.globals.insert(def_id, body);
        }

        def_id
    }

    fn instantiate_expr(&mut self, hir_expr: &hir2::TypeExpr, get_generic: &mut impl FnMut(Ident) -> RawType) -> RawTypeNode<Expr> {
        let expr = match &**hir_expr {
            hir2::Expr::Value(val) => Expr::Value(val.clone()),
            _ => todo!(),
        };

        let ty = self.instantiate_type(hir_expr.ty(), get_generic);

        RawTypeNode::new(expr, ty)
    }

    fn instantiate_type(&mut self, ty: &Type, get_generic: &mut impl FnMut(Ident) -> RawType) -> RawType {
        match ty {
            Type::Primitive(prim) => RawType::Primitive(prim.clone()),
            Type::GenParam(ident) => get_generic(*ident),
            _ => todo!(),
        }
    }
}
