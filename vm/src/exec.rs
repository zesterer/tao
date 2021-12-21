use super::*;
use std::fmt;

#[derive(Clone, Debug)]
pub enum Value {
    Int(i64),
    Num(f64),
    Char(char),
    Bool(bool),
    List(Vec<Self>),
    Func(Addr, Vec<Self>),
    Sum(usize, Box<Self>),
}

impl Value {
    pub fn int(self) -> i64 { if let Value::Int(x) = self { x } else { panic!("{}", self) } }
    pub fn num(self) -> f64 { if let Value::Num(x) = self { x } else { panic!("{}", self) } }
    pub fn char(self) -> char { if let Value::Char(c) = self { c } else { panic!("{}", self) } }
    pub fn bool(self) -> bool { if let Value::Bool(x) = self { x } else { panic!("{}", self) } }
    pub fn list(self) -> Vec<Self> { if let Value::List(xs) = self { xs } else { panic!("{}", self) } }
    pub fn func(self) -> (Addr, Vec<Self>) { if let Value::Func(f_addr, captures) = self { (f_addr, captures) } else { panic!("{}", self) } }
    pub fn sum(self) -> (usize, Box<Self>) { if let Value::Sum(variant, inner) = self { (variant, inner) } else { panic!("{}", self) } }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Int(x) => write!(f, "{}", x),
            Value::Num(x) => write!(f, "{}", x),
            Value::Char(c) => write!(f, "{}", c),
            Value::Bool(x) => write!(f, "{}", x),
            Value::List(items) => match items.iter().next() {
                Some(Value::Char(_)) => items
                    .iter()
                    .try_for_each(|c| write!(f, "{}", c)),
                _ => write!(f, "[{}]", items
                    .iter()
                    .map(|x| format!("{}", x))
                    .collect::<Vec<_>>()
                    .join(", ")),
            },
            Value::Func(f_addr, captures) => write!(
                f,
                "Function(addr = 0x{:03X}, captures = {})",
                f_addr.0,
                captures.len(),
            ),
            Value::Sum(variant, inner) => write!(f, "#{} {}", variant, inner),
        }
    }
}

pub fn exec(prog: &Program) -> Option<Value> {
    let mut addr = prog.entry;

    let mut funcs = Vec::new();
    let mut stack = Vec::new();
    let mut locals = Vec::new();

    loop {
        let mut next_addr = addr.incr();

        match prog.instr(addr) {
            Instr::Error(err) => panic!("Error: {}", err),
            Instr::Nop => {},
            Instr::Break => {
                println!("Breakpoint at 0x{:03X?}", addr.0);
                for (i, x) in stack.iter().rev().enumerate() {
                    println!("{:02} | {:?}", i, x);
                }
            },
            Instr::Imm(x) => stack.push(x.clone()),
            Instr::Pop(n) => {
                assert!(n > 0, "Popped zero items, this is probably a bug");
                stack.truncate(stack.len().saturating_sub(n));
            },
            Instr::Replace => {
                let x = stack.pop().unwrap();
                stack.pop();
                stack.push(x);
            },
            Instr::Call(n) => {
                funcs.push(next_addr);
                next_addr = addr.jump(n);
            },
            Instr::Ret => if let Some(addr) = funcs.pop() {
                next_addr = addr;
            } else {
                assert_eq!(locals.len(), 0, "Local stack still has values, this is probably a bug");
                assert_eq!(stack.len(), 1, "Stack size must be 0 on program exit");
                break stack.pop();
            },
            Instr::MakeFunc(i, n) => {
                let f_addr = addr.jump(i);
                let func = Value::Func(f_addr, stack.split_off(stack.len().saturating_sub(n)));
                stack.push(func);
            },
            Instr::ApplyFunc => {
                let (f_addr, mut captures) = stack.pop().unwrap().func();

                funcs.push(next_addr);
                next_addr = f_addr;

                locals.append(&mut captures);
            },
            Instr::MakeList(n) => {
                let val = Value::List(stack.split_off(stack.len().saturating_sub(n)));
                stack.push(val);
            },
            Instr::IndexList(i) => {
                let mut x = stack.pop().unwrap().list();
                if x.len() < i + 1 {
                    panic!("Removing item {} from list {:?} at 0x{:X}", i, x, addr.0);
                }
                stack.push(x.remove(i));
            },
            Instr::SkipList(i) => {
                let mut x = stack.pop().unwrap().list();
                stack.push(Value::List(x.split_off(i)));
            },
            Instr::LenList => {
                let len = stack.pop().unwrap().list().len();
                stack.push(Value::Int(len as i64));
            },
            Instr::JoinList => {
                let mut y = stack.pop().unwrap().list();
                let mut x = stack.pop().unwrap().list();
                x.append(&mut y);
                stack.push(Value::List(x));
            },
            Instr::MakeSum(variant) => {
                let x = stack.pop().unwrap();
                stack.push(Value::Sum(variant, Box::new(x)));
            },
            Instr::IndexSum(variant) => {
                let (v, inner) = stack.pop().unwrap().sum();
                debug_assert_eq!(variant, v);
                stack.push(*inner);
            },
            Instr::VariantSum => {
                let (variant, _) = stack.pop().unwrap().sum();
                stack.push(Value::Int(variant as i64));
            },
            Instr::Dup => stack.push(stack.last().unwrap().clone()),
            Instr::Jump(n) => {
                next_addr = addr.jump(n);
                // println!("Jump from 0x{:03X} to 0x{:03X}", addr.0, next_addr.0);
            },
            Instr::IfNot => {
                if stack.pop().unwrap().bool() {
                    next_addr = next_addr.jump(1);
                }
            },
            Instr::PushLocal => locals.push(stack.pop().unwrap()),
            Instr::PopLocal(n) => locals.truncate(locals.len() - n),
            Instr::GetLocal(x) => stack.push(locals[locals.len() - 1 - x].clone()),
            Instr::NotBool => {
                let x = stack.pop().unwrap().bool();
                stack.push(Value::Bool(!x))
            },
            Instr::NegInt => {
                let x = stack.pop().unwrap().int();
                stack.push(Value::Int(-x))
            },
            Instr::AddInt => {
                let y = stack.pop().unwrap().int();
                let x = stack.pop().unwrap().int();
                stack.push(Value::Int(x + y))
            },
            Instr::SubInt => {
                let y = stack.pop().unwrap().int();
                let x = stack.pop().unwrap().int();
                stack.push(Value::Int(x - y))
            },
            Instr::MulInt => {
                let y = stack.pop().unwrap().int();
                let x = stack.pop().unwrap().int();
                stack.push(Value::Int(x * y))
            },
            Instr::EqInt => {
                let y = stack.pop().unwrap().int();
                let x = stack.pop().unwrap().int();
                stack.push(Value::Bool(x == y))
            },
            Instr::EqBool => {
                let y = stack.pop().unwrap().bool();
                let x = stack.pop().unwrap().bool();
                stack.push(Value::Bool(x == y))
            },
            Instr::EqChar => {
                let y = stack.pop().unwrap().char();
                let x = stack.pop().unwrap().char();
                stack.push(Value::Bool(x == y))
            },
            Instr::LessInt => {
                let y = stack.pop().unwrap().int();
                let x = stack.pop().unwrap().int();
                stack.push(Value::Bool(x < y))
            },
            Instr::MoreInt => {
                let y = stack.pop().unwrap().int();
                let x = stack.pop().unwrap().int();
                stack.push(Value::Bool(x > y))
            },
            Instr::LessEqInt => {
                let y = stack.pop().unwrap().int();
                let x = stack.pop().unwrap().int();
                stack.push(Value::Bool(x <= y))
            },
            Instr::MoreEqInt => {
                let y = stack.pop().unwrap().int();
                let x = stack.pop().unwrap().int();
                stack.push(Value::Bool(x >= y))
            },
            Instr::AndBool => {
                let y = stack.pop().unwrap().bool();
                let x = stack.pop().unwrap().bool();
                stack.push(Value::Bool(x && y))
            },
        }

        // println!("Executing 0x{:03X}... Stack: {}", addr.0, stack.iter().rev().map(|x| format!("{}", x)).collect::<Vec<_>>().join(", "));

        addr = next_addr;
    }
}
