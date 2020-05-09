use std::rc::Rc;
#[cfg(not(debug_assertions))]
use std::hint::unreachable_unchecked;
use im_rc::Vector;

#[derive(Clone, Debug)]
#[repr(u8)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    String(Rc<String>),
    List(Rc<Vector<Value>>),
}

impl Value {
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
}

#[test]
fn size() {
    assert!(std::mem::size_of::<Value>() <= 16);
}
