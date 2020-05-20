use std::fmt;
use internment::LocalIntern;
use crate::node::SrcNode;

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
    List(SrcNode<Self>),
    Tuple(Vec<SrcNode<Self>>),
    Record(Vec<(SrcNode<Ident>, SrcNode<Self>)>),
    Func(SrcNode<Self>, SrcNode<Self>),
    GenParam(Ident),
}

impl Type {
    pub fn visit(self: &SrcNode<Self>, f: &mut impl FnMut(&SrcNode<Self>)) {
        let iter = match &**self {
            Type::Primitive(_) => {},
            Type::List(item) => item.visit(f),
            Type::Tuple(items) => items
                .iter()
                .for_each(|item| item.visit(f)),
            Type::Record(fields) => fields
                .iter()
                .for_each(|(_, ty)| ty.visit(f)),
            Type::Func(i, o) => {
                i.visit(f);
                o.visit(f);
            },
            Type::GenParam(_) => {},
        };

        f(self);
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Primitive(prim) => write!(f, "{}", prim),
            Type::List(item) => write!(f, "[{}]", **item),
            Type::Tuple(items) => {
                write!(f, "(")?;
                write!(f, "{}", items
                    .iter()
                    .map(|item| format!("{}", **item))
                    .collect::<Vec<_>>()
                    .join(", "))?;
                if items.len() == 1 {
                    write!(f, ",")?;
                }
                write!(f, ")")?;
                Ok(())
            },
            Type::Record(fields) => {
                write!(f, "(")?;
                write!(f, "{}", fields
                    .iter()
                    .map(|(name, field)| format!(".{}: {}", name.as_str(), **field))
                    .collect::<Vec<_>>()
                    .join(", "))?;
                write!(f, ")")?;
                Ok(())
            },
            Type::Func(i, o) => write!(f, "({} -> {})", **i, **o),
            Type::GenParam(ident) => write!(f, "{}", ident),
        }
    }
}
