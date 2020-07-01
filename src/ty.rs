use std::{fmt, convert::TryFrom};
use internment::LocalIntern;
use crate::{
    node::SrcNode,
    hir::data::DataId,
};

type Ident = LocalIntern<String>;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Primitive {
    Boolean,
    Char,
    Number,
    Universe,
}

impl fmt::Display for Primitive {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Primitive::Boolean => write!(f, "Bool"),
            Primitive::Char => write!(f, "Char"),
            Primitive::Number => write!(f, "Num"),
            Primitive::Universe => write!(f, "Universe"),
        }
    }
}

impl<'a> TryFrom<&'a str> for Primitive {
    type Error = ();

    fn try_from(s: &'a str) -> Result<Self, Self::Error> {
        match s {
            "Num" => Ok(Primitive::Number),
            "Bool" => Ok(Primitive::Boolean),
            "Char" => Ok(Primitive::Char),
            "Universe" => Ok(Primitive::Universe),
            _ => Err(()),
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
    Data(SrcNode<DataId>, Vec<SrcNode<Self>>),
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
            Type::Data(data, params) => params
                .iter()
                .for_each(|param| param.visit(f)),
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
                write!(f, "{{{}", if fields.len() > 0 { " " } else { "" })?;
                write!(f, "{}", fields
                    .iter()
                    .map(|(name, field)| format!("{}: {}", name.as_str(), **field))
                    .collect::<Vec<_>>()
                    .join(", "))?;
                write!(f, "{}}}", if fields.len() > 0 { " " } else { "" })?;
                Ok(())
            },
            Type::Func(i, o) => write!(f, "({} -> {})", **i, **o),
            Type::GenParam(ident) => write!(f, "{}", ident),
            Type::Data(data, params) => {
                write!(f, "Data={}", **data)?;
                params
                    .iter()
                    .try_for_each(|param| write!(f, " {}", **param))?;
                Ok(())
            },
        }
    }
}
