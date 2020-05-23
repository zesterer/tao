use std::collections::HashMap;
use internment::LocalIntern;
use crate::{
    ast::{self, Literal},
    ty::{Type, Primitive},
    error::Error,
    node::RawTypeNode,
    hir::self,
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
                .join(", "),
            ),
            RawType::Sum(items) => format!("({})", items
                .iter()
                .map(|item| item.mangle())
                .collect::<Vec<_>>()
                .join(" | "),
            ),
            RawType::Func(i, o) => format!("{} -> {}", i.mangle(), o.mangle()),
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
            hir::Pat::Literal(litr) => Matcher::Exactly(litr.clone()),
            hir::Pat::Tuple(items) => Matcher::Product(items
                .iter()
                .map(|item| item.make_matcher())
                .collect()),
            hir::Pat::Record(fields) => Matcher::Product(fields
                .iter()
                .map(|(_, field)| field.make_matcher())
                .collect()),
            hir::Pat::List(items) => Matcher::List(items
                .iter()
                .map(|item| item.make_matcher())
                .collect()),
            hir::Pat::ListFront(items, _) => Matcher::ListFront(items
                .iter()
                .map(|item| item.make_matcher())
                .collect()),
        }
    }

    fn make_extractor(&self) -> Extractor {
        match &*self.pat {
            hir::Pat::Wildcard | hir::Pat::Literal(_) =>
                Extractor::Just(self.binding.as_ref().map(|ident| **ident)),
            hir::Pat::Tuple(items) => Extractor::Product(
                self.binding.as_ref().map(|ident| **ident),
                items.iter().map(|item| item.make_extractor()).collect(),
            ),
            hir::Pat::Record(fields) => Extractor::Product(
                self.binding.as_ref().map(|ident| **ident),
                fields.iter().map(|(_, field)| field.make_extractor()).collect(),
            ),
            hir::Pat::List(items) => Extractor::List(
                self.binding.as_ref().map(|ident| **ident),
                items.iter().map(|item| item.make_extractor()).collect(),
            ),
            hir::Pat::ListFront(items, tail) => Extractor::ListFront(
                self.binding.as_ref().map(|ident| **ident),
                items.iter().map(|item| item.make_extractor()).collect(),
                tail.as_ref().map(|ident| **ident),
            ),
        }
    }
}

pub type DefId = LocalIntern<(Ident, Vec<RawType>)>;

#[derive(Debug)]
pub enum Expr {
    Literal(Literal),
    // Get the value of the given global
    GetGlobal(DefId),
    // Get the value of the given local
    GetLocal(Ident),
    // Perform a built-in unary operation
    Unary(Unary, RawTypeNode<Self>),
    // Perform a built-in binary operation
    Binary(Binary, RawTypeNode<Self>, RawTypeNode<Self>),
    // Construct a tuple with the given values
    MakeTuple(Vec<RawTypeNode<Self>>),
    // Construct a list with the given values
    MakeList(Vec<RawTypeNode<Self>>),
    // Apply a value to a function
    Apply(RawTypeNode<Self>, RawTypeNode<Self>),
    // Access the field of a Tuple
    Access(RawTypeNode<Self>, usize),
    // Update the field of a Tuple
    Update(RawTypeNode<Self>, usize, Ident, RawTypeNode<Self>),
    // Create a function with the given parameter extractor and body
    MakeFunc(Extractor, Vec<Ident>, RawTypeNode<Self>),
    // Make a flat value against a series of arms
    MatchValue(RawTypeNode<Self>, Vec<(Matcher, Extractor, RawTypeNode<Self>)>),
}

impl hir::TypeExpr {
    fn get_env_inner(&self, scope: &mut Vec<Ident>, env: &mut Vec<Ident>) {
        match &**self {
            hir::Expr::Literal(_) => {},
            hir::Expr::Global(_, _) => {},
            hir::Expr::Local(ident) => {
                if scope.iter().find(|name| *name == ident).is_none() {
                    env.push(*ident);
                }
            },
            hir::Expr::Unary(_, a) => a.get_env_inner(scope, env),
            hir::Expr::Binary(_, a, b) => {
                a.get_env_inner(scope, env);
                b.get_env_inner(scope, env);
            },
            hir::Expr::Tuple(items) => for item in items.iter() {
                item.get_env_inner(scope, env);
            },
            hir::Expr::Record(fields) => for (_, field) in fields.iter() {
                field.get_env_inner(scope, env);
            },
            hir::Expr::List(items) => for item in items.iter() {
                item.get_env_inner(scope, env);
            },
            hir::Expr::Apply(f, arg) => {
                f.get_env_inner(scope, env);
                arg.get_env_inner(scope, env);
            },
            hir::Expr::Access(record, _) => record.get_env_inner(scope, env),
            hir::Expr::Update(record, field, value) => {
                scope.push(**field);
                record.get_env_inner(scope, env);
                value.get_env_inner(scope, env);
                scope.pop();
            },
            hir::Expr::Func(binding, body) => {
                let body_env = body.get_env();
                let bindings = binding.binding_idents();
                for ident in body_env {
                    if scope.iter().find(|name| **name == ident).is_none()
                        && !bindings.contains_key(&ident)
                    {
                        env.push(ident);
                    }
                }
            },
            hir::Expr::Match(pred, arms) => {
                pred.get_env_inner(scope, env);
                for (binding, body) in arms.iter() {
                    let mut bindings = binding
                        .binding_idents()
                        .keys()
                        .copied()
                        .collect();
                    let scope_len = scope.len();
                    scope.append(&mut bindings);
                    body.get_env_inner(scope, env);
                    scope.truncate(scope_len);
                }
            },
        }
    }

    fn get_env(&self) -> Vec<Ident> {
        let mut scope = Vec::new();
        let mut env = Vec::new();
        self.get_env_inner(&mut scope, &mut env);
        env
    }
}

#[derive(Debug)]
pub enum Matcher {
    Wildcard,
    Exactly(Literal),
    Product(Vec<Matcher>),
    List(Vec<Matcher>),
    ListFront(Vec<Matcher>),
}

impl Matcher {
    pub fn is_refutable(&self) -> bool {
        match self {
            Matcher::Wildcard => false,
            Matcher::Exactly(_) => true,
            Matcher::Product(items) => items.iter().any(|item| item.is_refutable()),
            Matcher::List(_) => true,
            Matcher::ListFront(items) => items.len() != 0 // List matches everything
        }
    }
}

// Describes the extraction of pattern bindings from a basic pattern
// For example: ((x, y), _, z)
#[derive(Debug)]
pub enum Extractor {
    Just(Option<Ident>),
    Product(Option<Ident>, Vec<Extractor>),
    List(Option<Ident>, Vec<Extractor>),
    ListFront(Option<Ident>, Vec<Extractor>, Option<Ident>),
}

impl Extractor {
    pub fn extracts_anything(&self) -> bool {
        match self {
            Extractor::Just(x) => x.is_some(),
            Extractor::Product(x, items) => x.is_some() || items.iter().any(|item| item.extracts_anything()),
            Extractor::List(x, items) => x.is_some() || items.iter().any(|item| item.extracts_anything()),
            Extractor::ListFront(x, items, tail) => x.is_some() || tail.is_some() || items.iter().any(|item| item.extracts_anything()),
        }
    }

    fn bindings(&self, bindings: &mut Vec<Ident>) {
        let (this, children, tail) = match self {
            Extractor::Just(x) => (x, None, None),
            Extractor::Product(x, xs) => (x, Some(xs), None),
            Extractor::List(x, xs) => (x, Some(xs), None),
            Extractor::ListFront(x, xs, tail) => (x, Some(xs), *tail),
        };
        if let Some(this) = this {
            bindings.push(*this);
        }
        if let Some(tail) = tail {
            bindings.push(tail);
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

        let entry = this.instantiate_def(prog, entry, Vec::new())
            .ok_or_else(|| Error::custom(format!("Cannot find entry point '{}'", *entry)))?;

        Ok(this)
    }

    pub fn globals(&self) -> impl Iterator<Item=(DefId, &RawTypeNode<Expr>)> {
        self.globals.iter().map(|(id, g)| (*id, g.as_ref().unwrap()))
    }

    fn instantiate_def(&mut self, prog: &hir::Program, name: Ident, params: Vec<RawType>) -> Option<DefId> {
        let def_id = LocalIntern::new((name, params.clone()));

        if !self.globals.contains_key(&def_id) {
            self.globals.insert(def_id, None); // Insert phoney to keep recursive functions happy

            let def = prog.root.def(name)?;

            let generics = def.generics
                .iter()
                .zip(params.iter())
                .map(|(name, param)| (**name, param))
                .collect::<HashMap<_, _>>();

            let body = self.instantiate_expr(prog, &def.body, &mut |gen| generics.get(&gen).cloned().cloned().unwrap());
            self.globals.insert(def_id, Some(body));
        }

        Some(def_id)
    }

    fn instantiate_expr(&mut self, prog: &hir::Program, hir_expr: &hir::TypeExpr, get_generic: &mut impl FnMut(Ident) -> RawType) -> RawTypeNode<Expr> {
        let expr = match &**hir_expr {
            hir::Expr::Literal(litr) => Expr::Literal(litr.clone()),
            hir::Expr::Local(local) => Expr::GetLocal(*local),
            hir::Expr::Global(global, generics) => {
                let generics = generics.iter().map(|(_, (_, ty))| self.instantiate_type(ty, get_generic)).collect::<Vec<_>>();
                let def = self.instantiate_def(prog, *global, generics).unwrap();
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
            hir::Expr::Record(fields) => Expr::MakeTuple(fields
                .iter()
                .map(|(_, field)| self.instantiate_expr(prog, field, get_generic))
                .collect()),
            hir::Expr::List(items) => Expr::MakeList(items
                .iter()
                .map(|item| self.instantiate_expr(prog, item, get_generic))
                .collect()),
            hir::Expr::Func(binding, body) => {
                let extractor = binding.make_extractor();
                let e_bindings = extractor.get_bindings();
                let env = body.get_env().into_iter().filter(|ident| !e_bindings.contains(ident)).collect();
                Expr::MakeFunc(extractor, env, self.instantiate_expr(prog, body, get_generic))
            },
            hir::Expr::Apply(f, arg) => Expr::Apply(
                self.instantiate_expr(prog, f, get_generic),
                self.instantiate_expr(prog, arg, get_generic),
            ),
            hir::Expr::Access(record, field) => match &**record.ty() {
                Type::Record(fields) => {
                    let field_idx = fields
                        .iter()
                        .enumerate().find(|(_, (name, _))| name == field)
                        .unwrap().0;
                    Expr::Access(self.instantiate_expr(prog, record, get_generic), field_idx)
                },
                _ => unreachable!(),
            },
            hir::Expr::Update(record, field, value) => match &**record.ty() {
                Type::Record(fields) => {
                    let field_idx = fields
                        .iter()
                        .enumerate().find(|(_, (name, _))| name == field)
                        .unwrap().0;
                    Expr::Update(
                        self.instantiate_expr(prog, record, get_generic),
                        field_idx,
                        **field,
                        self.instantiate_expr(prog, value, get_generic),
                    )
                },
                _ => unreachable!(),
            },
            expr => todo!("{:?}", expr),
        };

        let ty = self.instantiate_type(hir_expr.ty(), get_generic);

        RawTypeNode::new(expr, (hir_expr.span(), ty))
    }

    fn instantiate_match(
        &mut self,
        prog: &hir::Program,
        pred: RawTypeNode<Expr>,
        arms: &[(hir::TypeBinding, hir::TypeExpr)],
        get_generic: &mut impl FnMut(Ident) -> RawType,
    ) -> Expr {
        let arms = arms
            .iter()
            .map(|(binding, body)| (
                binding.make_matcher(),
                binding.make_extractor(),
                self.instantiate_expr(prog, body, get_generic),
            ))
            .collect();

        Expr::MatchValue(pred, arms)
    }

    fn instantiate_type(&mut self, ty: &Type, get_generic: &mut impl FnMut(Ident) -> RawType) -> RawType {
        match ty {
            Type::Primitive(prim) => RawType::Primitive(prim.clone()),
            Type::GenParam(ident) => get_generic(*ident),
            Type::Tuple(items) => RawType::Product(items
                .iter()
                .map(|item| self.instantiate_type(item, get_generic))
                .collect()),
            Type::Record(fields) => RawType::Product(fields
                .iter()
                .map(|(_, field)| self.instantiate_type(field, get_generic))
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
