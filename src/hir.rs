use std::{
    fmt,
    rc::Rc,
    cell::RefCell,
    collections::HashMap,
};
use internment::LocalIntern;
use crate::{
    node::Node,
    parse::{Literal, UnaryOp, BinaryOp, Expr, Decl, Module},
    error::Error,
};

#[derive(Clone)]
pub enum TypeInfo {
    Unknown,
    Ref(Rc<RefCell<Node<TypeInfo>>>),
    Number,
    Boolean,
    String,
    List(Node<TypeInfo>),
    Tuple(Vec<Node<TypeInfo>>),
    Func(Node<TypeInfo>, Node<TypeInfo>),
}

impl From<LocalIntern<String>> for TypeInfo {
    fn from(ident: LocalIntern<String>) -> Self {
        match ident.as_str() {
            "Num" => TypeInfo::Number,
            "Bool" => TypeInfo::Boolean,
            "Str" => TypeInfo::String,
            _ => todo!("Implement non-primitive types"),
        }
    }
}

impl TypeInfo {
    pub fn make_ref(self: &mut Node<Self>) {
        match self.inner() {
            TypeInfo::Ref(_) => {},
            _ => {
                let this = Node::new(TypeInfo::Ref(Rc::new(RefCell::new(self.clone()))), self.region(), ());
                *self = this;
            },
        }
    }

    pub fn reference(self: &mut Node<Self>) -> Node<Self> {
        self.make_ref();
        self.clone()
    }

    pub fn compatible_with(&self, other: &Self) -> bool {
        match (self, other) {
            // Unknowns
            (TypeInfo::Unknown, _) => true,
            (_, TypeInfo::Unknown) => true,

            // Referenced types
            (TypeInfo::Ref(a), TypeInfo::Ref(b)) => if Rc::ptr_eq(a, b) {
                true
            } else {
                a
                    .try_borrow()
                    .map(|a| b
                        .try_borrow()
                        .map(|b| a.compatible_with(&b))
                        .unwrap_or(false))
                    .unwrap_or(false)
            },
            (TypeInfo::Ref(a), b) => a
                .try_borrow()
                .map(|a| a.compatible_with(b))
                .unwrap_or(false),
            (a, TypeInfo::Ref(b)) => b
                .try_borrow()
                .map(|b| a.compatible_with(&b))
                .unwrap_or(false),

            // Atoms
            (TypeInfo::Number, TypeInfo::Number) => true,
            (TypeInfo::Boolean, TypeInfo::Boolean) => true,
            (TypeInfo::String, TypeInfo::String) => true,

            // Complex types
            (TypeInfo::List(a), TypeInfo::List(b)) => a.compatible_with(b),
            (TypeInfo::Tuple(items_a), TypeInfo::Tuple(items_b)) if items_a.len() == items_b.len() => {
                items_a
                    .iter()
                    .zip(items_b.iter())
                    .all(|(a, b)| a.compatible_with(b))
            },
            (TypeInfo::Func(a_in, a_out), TypeInfo::Func(b_in, b_out)) =>
                a_in.compatible_with(b_in) && a_out.compatible_with(b_out),

            _ => false,
        }
    }

    pub fn unify_with(self: &mut Node<Self>, other: &mut Node<Self>) -> Result<(), Error> {
        match (self.inner_mut(), other.inner_mut()) {
            // Unknowns
            (TypeInfo::Unknown, _) => {
                let region = self.region.earliest(other.region);
                other.make_ref();
                other.region = region;
                *self = other.clone();
                Ok(())
            },
            (_, TypeInfo::Unknown) => {
                let region = self.region.earliest(other.region);
                self.make_ref();
                self.make_ref();
                *other = self.clone();
                Ok(())
            },

            // Referenced types
            (TypeInfo::Ref(a), _) => {
                let a = a.try_borrow_mut();
                if a.is_err() {
                    drop(a);
                    Ok(())
                    //Err(Error::recursive_type(self.clone()))
                } else {
                    a.unwrap().unify_with(other)
                }
            },
            (_, TypeInfo::Ref(b)) => {
                let b = b.try_borrow_mut();
                if b.is_err() {
                    drop(b);
                    Ok(())
                    //Err(Error::recursive_type(other.clone()))
                } else {
                    self.unify_with(&mut b.unwrap())
                }
            },

            // Atoms
            (TypeInfo::Number, TypeInfo::Number) => Ok(()),
            (TypeInfo::Boolean, TypeInfo::Boolean) => Ok(()),
            (TypeInfo::String, TypeInfo::String) => Ok(()),

            // Complex types
            (TypeInfo::List(a), TypeInfo::List(b)) => a.unify_with(b),
            (TypeInfo::Tuple(items_a), TypeInfo::Tuple(items_b)) if items_a.len() == items_b.len() => {
                items_a
                    .iter_mut()
                    .zip(items_b.iter_mut())
                    .try_for_each(|(a, b)| a.unify_with(b))
            },
            (TypeInfo::Func(a_in, a_out), TypeInfo::Func(b_in, b_out)) => {
                a_in.unify_with(b_in)?;
                a_out.unify_with(b_out)?;
                Ok(())
            },

            _ => if self.region.later_than(other.region) {
                Err(Error::type_mismatch(other.clone(), self.clone()))
            } else {
                Err(Error::type_mismatch(self.clone(), other.clone()))
            },
        }
    }

    pub fn established<'a>(self: &'a Node<Self>) -> Result<(), Node<Self>> {
        match self.inner() {
            TypeInfo::Unknown => Err(self.clone()),
            TypeInfo::Ref(ty) => ty
                .try_borrow_mut()
                .map(|ty| ty.established())
                .unwrap_or(Ok(())),
            TypeInfo::List(a) => a.established(),
            TypeInfo::Tuple(items) => items.iter().try_for_each(|i| i.established()),
            TypeInfo::Func(i, o) => {
                i.established()?;
                o.established()?;
                Ok(())
            },
            _ => Ok(()),
        }
    }

    pub fn is_complex(&self) -> bool {
        match self {
            TypeInfo::List(_) => true,
            TypeInfo::Func(_, _) => true,
            _ => false,
        }
    }
}

impl Default for TypeInfo {
    fn default() -> Self {
        TypeInfo::Unknown
    }
}

impl<'a> From<&'a Literal> for TypeInfo {
    fn from(lit: &'a Literal) -> TypeInfo {
        match lit {
            Literal::Number(_) => TypeInfo::Number,
            Literal::String(_) => TypeInfo::String,
            Literal::Boolean(_) => TypeInfo::Boolean,
            _ => todo!("Unimplemented literal type"),
        }
    }
}

impl fmt::Debug for TypeInfo {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl fmt::Display for TypeInfo {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeInfo::Unknown => write!(f, "?"),
            TypeInfo::Ref(a) => match a.try_borrow_mut() {
                Ok(a) => write!(f, "{}", a.inner()),
                Err(_) => write!(f, "<recursive>"),
            },
            TypeInfo::Number => write!(f, "Num"),
            TypeInfo::Boolean => write!(f, "Bool"),
            TypeInfo::String => write!(f, "Str"),
            TypeInfo::List(a) => write!(f, "List {}", a.inner()),
            TypeInfo::Tuple(items) => write!(f, "({})", items.iter().map(|i| format!("{}", i.inner())).collect::<Vec<_>>().join(", ")),
            TypeInfo::Func(i, o) => write!(f, "({} -> {})", i.inner(), o.inner()),
        }
    }
}

pub fn unary_op_resolve(op: &Node<UnaryOp>, a: &mut Node<TypeInfo>, o: &mut Node<TypeInfo>) -> Result<(), Error> {
    let possibles = [
        (UnaryOp::Neg, TypeInfo::Number, TypeInfo::Number),
        (UnaryOp::Not, TypeInfo::Boolean, TypeInfo::Boolean),
        (UnaryOp::Head, TypeInfo::String, TypeInfo::String),
        (UnaryOp::Tail, TypeInfo::String, TypeInfo::String),
        {
            let mut item = Node::new(TypeInfo::Unknown, o.region, ());
            item.make_ref();
            let mut list = Node::new(TypeInfo::List(item.clone()), o.region, ());
            list.make_ref();

            (UnaryOp::Head, list.inner().clone(), item.inner().clone())
        },
        {
            let mut list = Node::new(TypeInfo::List(Node::new(TypeInfo::Unknown, o.region, ())), o.region, ());
            list.make_ref();

            (UnaryOp::Tail, list.inner().clone(), list.inner().clone())
        },
    ];

    let mut matches = Vec::new();
    for (i, (pop, pa, po)) in possibles.iter().enumerate() {
        if op == pop && a.compatible_with(pa) && o.compatible_with(po) {
            matches.push((pa, po));
        }
    }

    match matches.len() {
        0 => Err(Error::invalid_unary_op(op.clone(), a.clone())),
        1 => {
            let m = matches[0];
            a.unify_with(&mut Node::new(m.0.clone(), a.region, ()))?;
            o.unify_with(&mut Node::new(m.1.clone(), o.region, ()))?;
            Ok(())
        },
        _ => Ok(()),
    }
}

pub fn binary_op_resolve(
    op: &Node<BinaryOp>,
    a: &mut Node<TypeInfo>,
    b: &mut Node<TypeInfo>,
    o: &mut Node<TypeInfo>,
) -> Result<(), Error> {
    let possibles = [
        (BinaryOp::Add, TypeInfo::Number, TypeInfo::Number, TypeInfo::Number),
        (BinaryOp::Sub, TypeInfo::Number, TypeInfo::Number, TypeInfo::Number),
        (BinaryOp::Mul, TypeInfo::Number, TypeInfo::Number, TypeInfo::Number),
        (BinaryOp::Div, TypeInfo::Number, TypeInfo::Number, TypeInfo::Number),
        (BinaryOp::Rem, TypeInfo::Number, TypeInfo::Number, TypeInfo::Number),

        (BinaryOp::Less, TypeInfo::Number, TypeInfo::Number, TypeInfo::Boolean),
        (BinaryOp::More, TypeInfo::Number, TypeInfo::Number, TypeInfo::Boolean),
        (BinaryOp::LessEq, TypeInfo::Number, TypeInfo::Number, TypeInfo::Boolean),
        (BinaryOp::MoreEq, TypeInfo::Number, TypeInfo::Number, TypeInfo::Boolean),

        (BinaryOp::Eq, TypeInfo::Number, TypeInfo::Number, TypeInfo::Boolean),
        (BinaryOp::Eq, TypeInfo::String, TypeInfo::String, TypeInfo::Boolean),
        (BinaryOp::Eq, TypeInfo::Boolean, TypeInfo::Boolean, TypeInfo::Boolean),
        {
            let mut list = Node::new(TypeInfo::List(Node::new(TypeInfo::Unknown, o.region, ())), o.region, ());
            list.make_ref();

            (BinaryOp::Join, list.inner().clone(), list.inner().clone(), list.inner().clone())
        },
        {
            let mut list = Node::new(TypeInfo::List(Node::new(TypeInfo::Unknown, a.region, ())), a.region, ());
            list.make_ref();

            (BinaryOp::Eq, list.inner().clone(), list.inner().clone(), TypeInfo::Boolean)
        },
    ];

    let mut matches = Vec::new();
    for (i, (pop, pa, pb, po)) in possibles.iter().enumerate() {
        if op == pop && a.compatible_with(pa) && b.compatible_with(pb) && o.compatible_with(po) {
            matches.push((pa, pb, po));
        }
    }

    match matches.len() {
        0 => Err(Error::invalid_binary_op(op.clone(), a.clone(), b.clone())), // TODO: Add complex type checks here
        1 => {
            let m = matches[0];
            a.unify_with(&mut Node::new(m.0.clone(), a.region, ()))?;
            b.unify_with(&mut Node::new(m.1.clone(), b.region, ()))?;
            o.unify_with(&mut Node::new(m.2.clone(), o.region, ()))?;
            Ok(())
        },
        _ => Ok(()),
    }
}

pub enum Scope<'a> {
    Global(HashMap<LocalIntern<String>, RefCell<Node<TypeInfo>>>),
    Local(LocalIntern<String>, RefCell<Node<TypeInfo>>, &'a Self),
}

impl<'a> Default for Scope<'a> {
    fn default() -> Self {
        Scope::Global(HashMap::default())
    }
}

impl<'a> Scope<'a> {
    pub fn push<'b>(&'b self, name: LocalIntern<String>, ty: &mut Node<TypeInfo>) -> Scope<'b>
        where 'a: 'b
    {
        Scope::Local(name, RefCell::new(ty.reference()), self)
    }

    pub fn get_mut(&self, name: LocalIntern<String>) -> Option<std::cell::RefMut<Node<TypeInfo>>> {
        match self {
            Scope::Local(local_name, ty, parent) => if *local_name == name {
                Some(ty.borrow_mut())
            } else {
                parent.get_mut(name)
            },
            Scope::Global(globals) => globals.get(&name).map(|g| g.borrow_mut()),
        }
    }
}

impl Expr {
    pub fn infer_types<'a>(
        self: &mut Node<Self, Node<TypeInfo>>,
        scope: &Scope<'a>,
    ) -> Result<(), Error> {
        match &mut *self.inner {
            Expr::Literal(lit) => **self.meta_mut() = TypeInfo::from(&*lit),
            Expr::Unary(op, a) => {
                a.infer_types(scope)?;
                unary_op_resolve(op, a.meta_mut(), &mut self.meta)?;
            },
            Expr::Binary(op, a, b) => {
                a.infer_types(scope)?;
                b.infer_types(scope)?;
                binary_op_resolve(op, a.meta_mut(), b.meta_mut(), &mut self.meta)?;
            },
            Expr::Branch(p, t, f) => {
                p.meta.unify_with(&mut Node::new(TypeInfo::Boolean, p.region, ()))?;
                p.infer_types(scope)?;

                t.infer_types(scope)?;
                f.infer_types(scope)?;
                self.meta.unify_with(t.meta_mut())?;
                self.meta.unify_with(f.meta_mut())?;
            },
            Expr::Func(arg, body) => {
                let scope = scope.push(*arg.inner, &mut arg.meta);
                body.infer_types(&scope)?;
                self.meta.unify_with(&mut Node::new(TypeInfo::Func(arg.meta.reference(), body.meta.reference()), self.region, ()))?;
            },
            Expr::Apply(f, arg) => {
                f.infer_types(scope)?;
                arg.infer_types(scope)?;
                f.meta.unify_with(&mut Node::new(TypeInfo::Func(arg.meta.reference(), self.meta.reference()), f.region, ()))?;
            },
            Expr::Ident(ident) => {
                let mut binding = scope
                    .get_mut(**ident)
                    .ok_or(Error::no_such_binding(ident.to_string(), ident.region()))?;
                self.meta.unify_with(&mut binding)?;
            },
            Expr::List(items) => {
                for item in items.iter_mut() {
                    item.infer_types(scope)?;
                }
                for i in 0..items.len().saturating_sub(1) {
                    let (first, last) = items[i..].split_first_mut().unwrap();
                    first.meta.unify_with(&mut last[0].meta)?;
                }
                let item_ty = if let Some(first) = items.first_mut() {
                    first.meta.reference()
                } else {
                    Node::new(TypeInfo::Unknown, self.region, ())
                };
                self.meta.unify_with(&mut Node::new(TypeInfo::List(item_ty), self.region, ()))?;
            },
            Expr::Tuple(items) => {
                for item in items.iter_mut() {
                    item.infer_types(scope)?;
                }
                let item_tys = items.iter_mut().map(|i| i.meta.reference()).collect();
                self.meta.unify_with(&mut Node::new(TypeInfo::Tuple(item_tys), self.region, ()))?;
            },
        }

        Ok(())
    }

    pub fn check_types(self: &Node<Self, Node<TypeInfo>>) -> Result<(), Error> {
        let check_type = |ty: &Node<TypeInfo>| ty
            .established()
            .map_err(|_| Error::cannot_infer_type(ty.clone()));

        match self.inner() {
            Expr::Literal(_) => {},
            Expr::Ident(ident) => {},
            Expr::Unary(_, a) => a.check_types()?,
            Expr::Binary(_, a, b) => {
                a.check_types()?;
                b.check_types()?;
            },
            Expr::Branch(p, t, f) => {
                p.check_types()?;
                t.check_types()?;
                f.check_types()?;
            },
            Expr::Func(arg, body) => {
                check_type(arg.meta())?;
                body.check_types()?;
            },
            Expr::Apply(f, arg) => {
                f.check_types()?;
                arg.check_types()?;
            },
            Expr::List(items) => for item in items.iter() {
                item.check_types()?;
            },
            Expr::Tuple(items) => for item in items.iter() {
                item.check_types()?;
            },
        }

        check_type(self.meta())
    }

    pub fn ascribe_types(self: &mut Node<Self, Node<TypeInfo>>) -> Result<(), Error> {
        self.infer_types(&mut Scope::default())?;
        self.check_types()?;
        Ok(())
    }
}

impl Module {
    pub fn infer_types<'a>(&mut self) -> Result<(), Error> {
        let scope = Scope::Global(self.decls
            .iter()
            .filter_map(|decl| match decl.inner() {
                Decl::Value(name, body) => Some((*name.inner(), RefCell::new(body.meta().clone())))
            })
            .collect());

        for decl in &mut self.decls {
            match decl.inner_mut() {
                Decl::Value(name, body) => body.infer_types(&scope)?,
            }
        }

        if let Scope::Global(vals) = scope {
            for (name, ty) in vals {
                for decl in &mut self.decls {
                    match decl.inner_mut() {
                        Decl::Value(val_name, body) if *val_name.inner() == name => {
                            body.meta.unify_with(&mut ty.into_inner())?;
                            break;
                        },
                        _ => {},
                    }
                }
            }
        } else {
            unreachable!()
        }

        Ok(())
    }

    pub fn check_types(&self) -> Result<(), Error> {
        for decl in &self.decls {
            match decl.inner() {
                Decl::Value(_, body) => body.check_types()?,
            }
        }
        Ok(())
    }

    pub fn ascribe_types(&mut self) -> Result<(), Error> {
        self.infer_types()?;
        self.check_types()?;
        Ok(())
    }
}
