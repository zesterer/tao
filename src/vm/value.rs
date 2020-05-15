use std::{
    rc::Rc,
    fmt,
};
#[cfg(not(debug_assertions))]
use std::hint::unreachable_unchecked;
use im_rc::Vector;
use super::CodeAddr;

#[derive(Clone, Debug)]
#[repr(u8)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    String(Rc<String>),
    List(Rc<Vector<Value>>),
    Func(CodeAddr),
}

impl Value {
    pub fn make_list<I: Iterator<Item=Value>>(iter: I) -> Self {
        Value::List(Rc::new(iter.collect()))
    }

    pub fn into_number_unchecked(self) -> f64 {
        match self {
            Value::Number(x) => x,
            #[cfg(debug_assertions)]
            _ => unreachable!(),
            #[cfg(not(debug_assertions))]
            _ => unsafe { unreachable_unchecked() },
        }
    }

    pub fn into_boolean_unchecked(self) -> bool {
        match self {
            Value::Boolean(x) => x,
            #[cfg(debug_assertions)]
            _ => unreachable!(),
            #[cfg(not(debug_assertions))]
            _ => unsafe { unreachable_unchecked() },
        }
    }

    pub fn into_func_unchecked(self) -> CodeAddr {
        match self {
            Value::Func(addr) => addr,
            #[cfg(debug_assertions)]
            _ => unreachable!(),
            #[cfg(not(debug_assertions))]
            _ => unsafe { unreachable_unchecked() },
        }
    }

    pub fn into_list_unchecked(self) -> Rc<Vector<Value>> {
        match self {
            Value::List(list) => list,
            #[cfg(debug_assertions)]
            _ => unreachable!(),
            #[cfg(not(debug_assertions))]
            _ => unsafe { unreachable_unchecked() },
        }
    }

    pub fn index(self, x: usize) -> Self {
        match self {
            Value::List(list) => list[x].clone(),
            #[cfg(debug_assertions)]
            _ => unreachable!(),
            #[cfg(not(debug_assertions))]
            _ => unsafe { unreachable_unchecked() },
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Number(x) => write!(f, "{}", x),
            Value::Boolean(x) => write!(f, "{}", x),
            Value::String(x) => write!(f, "\"{}\"", x),
            Value::List(xs) => write!(f, "[{}]", xs
                .iter()
                .map(|x| format!("{}", x))
                .collect::<Vec<_>>()
                .join(", ")),
            Value::Func(addr) => write!(f, "<func {:#X}>", addr),
        }
    }
}

#[test]
fn size() {
    assert!(std::mem::size_of::<Value>() <= 16);
}
