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
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum RawType {
    Primitive(Primitive),
    List(Box<Self>),
    Product(Vec<Self>),
    Sum(Vec<Self>),
    Func(Box<Self>, Box<Self>),
}

impl RawType {
    pub fn mangle(&self) -> String {
        match self {
            RawType::Primitive(prim) => format!("{}", prim),
            RawType::List(item) => format!("[{}]", item.mangle()),
            RawType::Product(items) => format!("({})", items
                .iter()
                .map(|item| item.mangle())
                .collect::<Vec<_>>()
                .join("*"),
            ),
            RawType::Sum(items) => format!("({})", items
                .iter()
                .map(|item| item.mangle())
                .collect::<Vec<_>>()
                .join("|"),
            ),
            RawType::Func(i, o) => format!("{}->{}", i.mangle(), o.mangle()),
        }
    }

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

pub type DefId = LocalIntern<(Ident, Vec<RawType>)>;

#[derive(Debug)]
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
    // Construct a list using the last N values on the stack
    MakeList(Vec<RawTypeNode<Expr>>),
    // Apply a value to a function
    Apply(RawTypeNode<Expr>, RawTypeNode<Expr>),
    // Create a function with the given parameter extractor and body
    MakeFunc(Extractor, RawTypeNode<Expr>),
    // Make a flat value against a series of arms
    MatchValue(RawTypeNode<Expr>, Vec<(Matcher, Extractor, RawTypeNode<Expr>)>),
}

#[derive(Debug)]
pub enum Matcher {
    Wildcard,
    Exactly(Value),
    Tuple(Vec<Matcher>),
}

impl Matcher {
    pub fn is_refutable(&self) -> bool {
        match self {
            Matcher::Wildcard => false,
            Matcher::Exactly(_) => true,
            Matcher::Tuple(items) => items.iter().any(|item| item.is_refutable()),
        }
    }
}

// Describes the extraction of pattern bindings from a basic pattern
// For example: ((x, y), _, z)
#[derive(Debug)]
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
    pub globals: HashMap<DefId, Option<RawTypeNode<Expr>>>,
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

    pub fn global(&self, id: DefId) -> &RawTypeNode<Expr> {
        self.globals[&id].as_ref().unwrap()
    }

    pub fn globals(&self) -> impl Iterator<Item=(DefId, &RawTypeNode<Expr>)> {
        self.globals.iter().map(|(id, g)| (*id, g.as_ref().unwrap()))
    }

    fn instantiate_def(&mut self, prog: &hir::Program, name: Ident, params: Vec<RawType>) -> DefId {
        let def_id = LocalIntern::new((name, params.clone()));

        if !self.globals.contains_key(&def_id) {
            self.globals.insert(def_id, None); // Insert phoney to keep recursive functions happy

            let def = prog.root.def(name).expect("Expected def to exist");

            let generics = def.generics
                .iter()
                .zip(params.iter())
                .map(|(name, param)| (**name, param))
                .collect::<HashMap<_, _>>();

            let body = self.instantiate_expr(prog, &def.body, &mut |gen| generics.get(&gen).cloned().cloned().unwrap());
            self.globals.insert(def_id, Some(body));
        }

        def_id
    }

    fn instantiate_expr(&mut self, prog: &hir::Program, hir_expr: &hir::TypeExpr, get_generic: &mut impl FnMut(Ident) -> RawType) -> RawTypeNode<Expr> {
        let expr = match &**hir_expr {
            hir::Expr::Value(val) => Expr::Value(val.clone()),
            hir::Expr::Local(local) => Expr::GetLocal(*local),
            hir::Expr::Global(global, generics) => {
                let generics = generics.iter().map(|(_, ty)| self.instantiate_type(ty, get_generic)).collect::<Vec<_>>();
                let def = self.instantiate_def(prog, *global, generics);
                Expr::GetGlobal(def)
            },
            hir::Expr::Unary(op, a) => Expr::Unary(**op, self.instantiate_expr(prog, a, get_generic)),
            hir::Expr::Binary(op, a, b) => Expr::Binary(**op, self.instantiate_expr(prog, a, get_generic), self.instantiate_expr(prog, b, get_generic)),
            hir::Expr::Match(pred, arms) => {
                let pred = self.instantiate_expr(prog, pred, get_generic);
                self.instantiate_match(prog, pred, &arms, get_generic)
            },
            hir::Expr::Tuple(items) => Expr::MakeTuple(items
                .iter()
                .map(|item| self.instantiate_expr(prog, item, get_generic))
                .collect()),
            hir::Expr::List(items) => Expr::MakeList(items
                .iter()
                .map(|item| self.instantiate_expr(prog, item, get_generic))
                .collect()),
            hir::Expr::Func(binding, body) => Expr::MakeFunc(binding.make_extractor(), self.instantiate_expr(prog, body, get_generic)),
            hir::Expr::Apply(f, arg) => Expr::Apply(
                self.instantiate_expr(prog, f, get_generic),
                self.instantiate_expr(prog, arg, get_generic),
            ),
            expr => todo!("{:?}", expr),
        };

        let ty = self.instantiate_type(hir_expr.ty(), get_generic);

        RawTypeNode::new(expr, ty)
    }

    fn instantiate_match(
        &mut self,
        prog: &hir::Program,
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
                        self.instantiate_expr(prog, body, get_generic),
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
            Type::Func(i, o) => RawType::Func(
                Box::new(self.instantiate_type(i, get_generic)),
                Box::new(self.instantiate_type(o, get_generic)),
            ),
            ty => todo!("{:?}", ty),
        }
    }
}
