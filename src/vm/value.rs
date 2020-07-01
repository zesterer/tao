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
    Char(char),
    List(Rc<Vector<Value>>),
    Func(Rc<(CodeAddr, Vec<Value>)>),
    Universe(u64),
}

impl Value {
    pub fn make_list<I: Iterator<Item=Value>>(iter: I) -> Self {
        Value::List(Rc::new(iter.collect()))
    }

    pub fn into_number_unchecked(self) -> f64 {
        match self {
            Value::Number(x) => x,
            #[cfg(debug_assertions)]
            this => unreachable!("Expected number, found {:?}", this),
            #[cfg(not(debug_assertions))]
            _ => unsafe { unreachable_unchecked() },
        }
    }

    pub fn into_boolean_unchecked(self) -> bool {
        match self {
            Value::Boolean(x) => x,
            #[cfg(debug_assertions)]
            this => unreachable!("Expected bool, found {:?}", this),
            #[cfg(not(debug_assertions))]
            _ => unsafe { unreachable_unchecked() },
        }
    }

    pub fn into_char_unchecked(self) -> char {
        match self {
            Value::Char(x) => x,
            #[cfg(debug_assertions)]
            this => unreachable!("Expected char, found {:?}", this),
            #[cfg(not(debug_assertions))]
            _ => unsafe { unreachable_unchecked() },
        }
    }

    pub fn into_func_unchecked(self) -> Rc<(CodeAddr, Vec<Value>)> {
        match self {
            Value::Func(addr) => addr,
            #[cfg(debug_assertions)]
            this => unreachable!("Expected func, found {:?}", this),
            #[cfg(not(debug_assertions))]
            _ => unsafe { unreachable_unchecked() },
        }
    }

    pub fn into_universe_unchecked(self) -> u64 {
        match self {
            Value::Universe(val) => val,
            #[cfg(debug_assertions)]
            this => unreachable!("Expected universe, found {:?}", this),
            #[cfg(not(debug_assertions))]
            _ => unsafe { unreachable_unchecked() },
        }
    }

    pub fn into_list_unchecked(self) -> Rc<Vector<Value>> {
        match self {
            Value::List(list) => list,
            #[cfg(debug_assertions)]
            this => unreachable!("Expected list, found {:?}", this),
            #[cfg(not(debug_assertions))]
            _ => unsafe { unreachable_unchecked() },
        }
    }

    pub fn index(self, x: usize) -> Self {
        match self {
            Value::List(list) => list[x].clone(),
            #[cfg(debug_assertions)]
            this => unreachable!("Expected list, found {:?}", this),
            #[cfg(not(debug_assertions))]
            _ => unsafe { unreachable_unchecked() },
        }
    }

    pub fn as_list_unchecked_mut(&mut self) -> &mut Vector<Value> {
        match self {
            Value::List(list) => Rc::make_mut(list),
            #[cfg(debug_assertions)]
            this => unreachable!("Expected list, found {:?}", this),
            #[cfg(not(debug_assertions))]
            _ => unsafe { unreachable_unchecked() },
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Number(x) => write!(f, "{}", x),
            Value::Char(x) => write!(f, "'{}'", x),
            Value::Boolean(x) => write!(f, "{}", x),
            Value::List(xs) => match xs.get(0) {
                Some(Value::Char(_)) => write!(f, "\"{}\"", xs
                    .iter()
                    .cloned()
                    .map(|v| v.into_char_unchecked())
                    .collect::<String>()),
                _ => write!(f, "[{}]", xs
                    .iter()
                    .map(|x| format!("{}", x))
                    .collect::<Vec<_>>()
                    .join(", ")),
            },
            Value::Func(addr) => write!(f, "<func {:#X}>", addr.0),
            Value::Universe(_) => write!(f, "<universe>"),
        }
    }
}

#[test]
fn size() {
    assert!(std::mem::size_of::<Value>() <= 16);
}
