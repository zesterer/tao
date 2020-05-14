use std::collections::HashMap;
use internment::LocalIntern;
use crate::{
    ast,
    ty::{Type, Primitive},
    error::Error,
    node::RawTypeNode,
    hir::{self, Value},
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

impl RawType {
    // Return a list of discriminants representing one of the possible pattern inhabitants of this
    // type. A value of `None` implies that this type's most outer layer has in infinite number of
    // possible inhabitants (i.e: `Num`) and so linear matching checks must occur
    // fn inhabitants_discriminants(&self) -> Option<Vec<Discriminant>> {
    //     match self {
    //         RawType::Primitive(prim) => match prim {
    //             Primitive::Boolean => Some(vec![Discriminant::Boolean(false), Discriminant::Boolean(true)]),
    //             Primitive::Number => None,
    //             Primitive::String => None,
    //         },
    //         RawType::List(_) => None,
    //         RawType::Product(_) => None,
    //         RawType::Sum(items) => Some((0..items.len()).map(Discriminant::Variant).collect()),
    //         RawType::Func(_, _) => None,
    //     }
    // }
}

impl hir::TypeBinding {
    // Return a discriminant corresponding with the `RawType::inhabitants_discriminants`
    // discriminant for this pattern. A value of `None` implies that the pattern must be checked
    // linearly because it may not represent a mutually-exclusive choice.
    // fn inhabitant_discriminant(&self) -> Option<Discriminant> {
    //     match &*self.pat {
    //         hir::Pat::Wildcard => None,
    //         hir::Pat::Value(val) => match val {
    //             Value::Boolean(x) => Some(Discriminant::Boolean(*x)),
    //             _ => None,
    //         },
    //         hir::Pat::List(_) => None,
    //         hir::Pat::ListFront(_, _) => None,
    //         hir::Pat::Tuple(_) => None,
    //     }
    // }

    fn make_matcher(&self) -> Matcher {
        match &*self.pat {
            hir::Pat::Wildcard => Matcher::Wildcard,
            hir::Pat::Value(val) => Matcher::Exactly(val.clone()),
            hir::Pat::Tuple(items) => Matcher::Tuple(items
                .iter()
                .map(|item| item.make_matcher())
                .collect()),
            _ => unreachable!(),
        }
    }

    fn make_extractor(&self) -> Extractor {
        match &*self.pat {
            hir::Pat::Wildcard | hir::Pat::Value(_) =>
                Extractor::Just(self.binding.as_ref().map(|ident| **ident)),
            hir::Pat::Tuple(items) => Extractor::Tuple(
                self.binding.as_ref().map(|ident| **ident),
                items.iter().map(|item| item.make_extractor()).collect(),
            ),
            _ => todo!(),
        }
    }
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
    // Construct a tuple using the last N values on the stack
    MakeTuple(Vec<RawTypeNode<Expr>>),
    // Call the given expression with the given arguments
    Call(RawTypeNode<Expr>, Vec<RawTypeNode<Expr>>),
    // Create a function with the given parameter extractor and body
    MakeFunc(HashMap<Ident, Extractor>, RawTypeNode<Expr>),
    MatchValue(RawTypeNode<Expr>, Vec<(Matcher, Extractor, RawTypeNode<Expr>)>),
    // Match on a pattern of complexity 1 (i.e: a single layer of destructuring)
    //Match(RawTypeNode<Expr>, Vec<(Validator, HashMap<Ident, Extractor>, RawTypeNode<Expr>)>),
    // DestructureSum {
    //     pred: RawTypeNode<Expr>,
    //     variants: HashMap<usize, RawTypeNode<Expr>>,
    //     other: RawTypeNode<Expr>,
    // },
    //DestructureList(RawTypeNode<Expr>, Vec<(ListMatcher, Extractor, RawTypeNode<Expr>)>),
}

pub enum Matcher {
    Wildcard,
    Exactly(Value),
    Tuple(Vec<Matcher>),
}

// enum ListMatcher {
//     Wildcard,
//     Len(usize),
//     LenOrMore(usize),
// }

// Describes an operation used to validate a match arm
// enum Validator {
//     Primitive(Option<Value>),
//     Tuple(Vec<Option<Value>>),
//     List(Vec<Option<Value>>), // A list with N matching elements
//     ListFront(Vec<Option<Value>>), // A list with N or more matching elements
//     // TODO: validators for sum types
// }

// Describes the extraction of pattern bindings from a basic pattern
// For example: ((x, y), _, z)
pub enum Extractor {
    Just(Option<Ident>),
    Tuple(Option<Ident>, Vec<Extractor>),
    // TODO: Lists
}

impl Extractor {
    pub fn extracts_anything(&self) -> bool {
        match self {
            Extractor::Just(x) => x.is_some(),
            Extractor::Tuple(x, items) => x.is_some() || items.iter().any(|item| item.extracts_anything()),
        }
    }
}

impl Extractor {
    fn bindings(&self, bindings: &mut Vec<Ident>) {
        let (this, children) = match self {
            Extractor::Just(x) => (x, None),
            Extractor::Tuple(x, xs) => (x, Some(xs)),
        };
        if let Some(this) = this {
            bindings.push(*this);
        }
        for child in children.map(|xs| xs.iter()).into_iter().flatten() {
            child.bindings(bindings);
        }
    }

    pub fn get_bindings(&self) -> Vec<Ident> {
        let mut bindings = Vec::new();
        self.bindings(&mut bindings);
        bindings
    }
}

pub struct Program {
    pub entry: DefId,
    pub globals: HashMap<DefId, RawTypeNode<Expr>>,
}

impl Program {
    pub fn from_hir(prog: &hir::Program, entry: Ident) -> Result<Self, Error> {
        let mut this = Self {
            entry: LocalIntern::new((entry, Vec::new())),
            globals: HashMap::default(),
        };

        let entry = this.instantiate_def(prog, entry, Vec::new());

        Ok(this)
    }

    fn instantiate_def(&mut self, prog: &hir::Program, name: Ident, params: Vec<RawType>) -> DefId {
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

    fn instantiate_expr(&mut self, hir_expr: &hir::TypeExpr, get_generic: &mut impl FnMut(Ident) -> RawType) -> RawTypeNode<Expr> {
        let expr = match &**hir_expr {
            hir::Expr::Value(val) => Expr::Value(val.clone()),
            hir::Expr::Local(local) => Expr::GetLocal(*local),
            hir::Expr::Unary(op, a) => Expr::Unary(**op, self.instantiate_expr(a, get_generic)),
            hir::Expr::Binary(op, a, b) => Expr::Binary(**op, self.instantiate_expr(a, get_generic), self.instantiate_expr(b, get_generic)),
            hir::Expr::Match(pred, arms) => {
                let pred = self.instantiate_expr(pred, get_generic);
                self.instantiate_match(pred, &arms, get_generic)
            },
            hir::Expr::Tuple(items) => Expr::MakeTuple(items
                .iter()
                .map(|item| self.instantiate_expr(item, get_generic))
                .collect()),
            _ => todo!(),
        };

        let ty = self.instantiate_type(hir_expr.ty(), get_generic);

        RawTypeNode::new(expr, ty)
    }

    fn instantiate_match(
        &mut self,
        pred: RawTypeNode<Expr>,
        arms: &[(hir::TypeBinding, hir::TypeExpr)],
        get_generic: &mut impl FnMut(Ident) -> RawType,
    ) -> Expr {
        match pred.ty() {
            RawType::Primitive(_) | RawType::Product(_) => {
                let arms = arms
                    .iter()
                    .map(|(binding, body)| (
                        binding.make_matcher(),
                        binding.make_extractor(),
                        self.instantiate_expr(body, get_generic),
                    ))
                    .collect();

                Expr::MatchValue(pred, arms)
            },
            _ => todo!(),
        }
    }

    fn instantiate_type(&mut self, ty: &Type, get_generic: &mut impl FnMut(Ident) -> RawType) -> RawType {
        match ty {
            Type::Primitive(prim) => RawType::Primitive(prim.clone()),
            Type::GenParam(ident) => get_generic(*ident),
            Type::Tuple(items) => RawType::Product(items
                .iter()
                .map(|item| self.instantiate_type(item, get_generic))
                .collect()),
            Type::List(item) => RawType::List(Box::new(self.instantiate_type(item, get_generic))),
            _ => todo!(),
        }
    }
}
