use std::{
    fmt,
    ops::DerefMut,
    rc::Rc,
    cell::RefCell,
    collections::HashMap,
};
use internment::LocalIntern;
use crate::{
    node::Node,
    parse::{Literal, UnaryOp, BinaryOp, Expr},
    error::Error,
};

#[derive(Clone, Debug)]
pub enum TypeInfo {
    Unknown,
    Ref(Rc<RefCell<Node<TypeInfo>>>),
    Number,
    Boolean,
    String,
    List(Node<TypeInfo>),
    Func(Node<TypeInfo>, Node<TypeInfo>),
}

impl TypeInfo {
    pub fn make_ref(self: &mut Node<Self>) {
        match self.inner() {
            TypeInfo::Ref(_) => {},
            _ => *self = Node::new(TypeInfo::Ref(Rc::new(RefCell::new(std::mem::take(self)))), self.region(), ()),
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
                a.borrow().compatible_with(&b.borrow())
            },
            (TypeInfo::Ref(a), b) => a.borrow().compatible_with(b),
            (a, TypeInfo::Ref(b)) => a.compatible_with(&b.borrow()),

            // Atoms
            (TypeInfo::Number, TypeInfo::Number) => true,
            (TypeInfo::Boolean, TypeInfo::Boolean) => true,
            (TypeInfo::String, TypeInfo::String) => true,

            // Complex types
            (TypeInfo::List(a), TypeInfo::List(b)) => a.compatible_with(b),
            (TypeInfo::Func(a_in, a_out), TypeInfo::Func(b_in, b_out)) =>
                a_in.compatible_with(b_in) && a_out.compatible_with(b_out),

            _ => false,
        }
    }

    pub fn unify_with(self: &mut Node<Self>, other: &mut Node<Self>) -> Result<(), Error> {
        match (self.inner_mut(), other.inner_mut()) {
            // Unknowns
            (TypeInfo::Unknown, TypeInfo::Unknown) => Ok(()),
            (TypeInfo::Unknown, _) => {
                other.make_ref();
                *self = other.clone();
                Ok(())
            },
            (_, TypeInfo::Unknown) => {
                self.make_ref();
                *other = self.clone();
                Ok(())
            },

            // Referenced types
            (TypeInfo::Ref(a), TypeInfo::Ref(b)) => if Rc::ptr_eq(a, b) {
                Ok(())
            } else {
                a.borrow_mut().unify_with(&mut b.borrow_mut())
            },
            (TypeInfo::Ref(a), _) => a.borrow_mut().unify_with(other),
            (_, TypeInfo::Ref(b)) => self.unify_with(&mut b.borrow_mut()),

            // Atoms
            (TypeInfo::Number, TypeInfo::Number) => Ok(()),
            (TypeInfo::Boolean, TypeInfo::Boolean) => Ok(()),
            (TypeInfo::String, TypeInfo::String) => Ok(()),

            // Complex types
            (TypeInfo::List(a), TypeInfo::List(b)) => a.unify_with(b),
            (TypeInfo::Func(a_in, a_out), TypeInfo::Func(b_in, b_out)) => {
                a_in.unify_with(b_in)?;
                a_out.unify_with(b_out)?;
                Ok(())
            },

            _ => Err(Error::type_mismatch(self.clone(), other.clone())),
        }
    }

    pub fn established<'a>(self: &'a Node<Self>) -> Result<(), &'a Node<Self>> {
        match self.inner() {
            TypeInfo::Unknown => Err(self),
            TypeInfo::List(a) => a.established(),
            TypeInfo::Func(i, o) => {
                i.established()?;
                o.established()?;
                Ok(())
            },
            _ => Ok(()),
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

impl fmt::Display for TypeInfo {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeInfo::Unknown => write!(f, "?"),
            TypeInfo::Ref(a) => write!(f, "{}", a.borrow().inner()),
            TypeInfo::Number => write!(f, "Num"),
            TypeInfo::Boolean => write!(f, "Bool"),
            TypeInfo::String => write!(f, "String"),
            TypeInfo::List(a) => write!(f, "List {}", a.inner()),
            TypeInfo::Func(i, o) => write!(f, "{} -> {}", i.inner(), o.inner()),
        }
    }
}

pub fn unary_op_resolve(op: &Node<UnaryOp>, a: &mut Node<TypeInfo>, o: &mut Node<TypeInfo>) -> Result<(), Error> {
    let possibles = [
        (UnaryOp::Neg, TypeInfo::Number, TypeInfo::Number),
        (UnaryOp::Not, TypeInfo::Boolean, TypeInfo::Boolean),
    ];

    let mut matches = Vec::new();
    for (i, (pop, pa, po)) in possibles.iter().enumerate() {
        if op == pop && a.compatible_with(pa) && o.compatible_with(po) {
            matches.push(i);
        }
    }

    match matches.len() {
        0 => Err(Error::invalid_unary_op(**op, op.region())),
        1 => {
            let m = possibles[matches[0]].clone();
            a.unify_with(&mut Node::new(m.1, a.region, ()));
            o.unify_with(&mut Node::new(m.2, o.region, ()));
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
    ];

    let mut matches = Vec::new();
    for (i, (pop, pa, pb, po)) in possibles.iter().enumerate() {
        if op == pop && a.compatible_with(pa) && b.compatible_with(pb) && o.compatible_with(po) {
            matches.push(i);
        }
    }

    match matches.len() {
        0 => Err(Error::invalid_binary_op(**op, op.region())), // TODO: Add complex type checks here
        1 => {
            let m = possibles[matches[0]].clone();
            a.unify_with(&mut Node::new(m.1, a.region, ()));
            b.unify_with(&mut Node::new(m.2, b.region, ()));
            o.unify_with(&mut Node::new(m.3, o.region, ()));
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
        scope: &mut Scope<'a>,
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
                p.infer_types(scope)?;
                p.meta.unify_with(&mut Node::new(TypeInfo::Boolean, p.region, ()))?;

                t.infer_types(scope)?;
                f.infer_types(scope)?;
                t.meta_mut().unify_with(f.meta_mut())?;
                t.meta_mut().unify_with(&mut self.meta)?;
                f.meta_mut().unify_with(&mut self.meta)?;
            },
            Expr::Func(arg, body) => {
                let mut scope = scope.push(*arg.inner, &mut arg.meta);
                body.infer_types(&mut scope)?;
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
        }

        Ok(())
    }

    pub fn check_types(self: &Node<Self, Node<TypeInfo>>) -> Result<(), Error> {
        self
            .meta()
            .established()
            .map_err(|ty| Error::cannot_infer_type(ty.clone()))?;

        match self.inner() {
            Expr::Unary(_, a) => a.check_types(),
            Expr::Binary(_, a, b) => {
                a.check_types()?;
                b.check_types()?;
                Ok(())
            },
            _ => Ok(()),
        }
    }

    pub fn ascribe_types(self: &mut Node<Self, Node<TypeInfo>>) -> Result<(), Error> {
        self.infer_types(&mut Scope::default())?;
        self.check_types()?;
        Ok(())
    }
}
