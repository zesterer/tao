use super::*;
use std::fmt;

#[derive(Clone, Debug)]
pub enum Value {
    Int(i64),
    Char(char),
    Bool(bool),
    List(Vec<Self>),
}

impl Value {
    pub fn int(self) -> i64 { if let Value::Int(x) = self { x } else { panic!() } }
    pub fn char(self) -> char { if let Value::Char(c) = self { c } else { panic!() } }
    pub fn bool(self) -> bool { if let Value::Bool(x) = self { x } else { panic!() } }
    pub fn list(self) -> Vec<Self> { if let Value::List(xs) = self { xs } else { panic!() } }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Int(x) => write!(f, "{}", x),
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
        }
    }
}

pub fn exec(prog: &Program) -> Option<Value> {
    let mut addr = prog.entry;

    let mut funcs = Vec::new();
    let mut stack = Vec::new();

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
            Instr::Ret => if let Some(addr) = funcs.pop() {
                next_addr = addr;
            } else {
                assert_eq!(stack.len(), 1, "Stack size must be 0 on program exit");
                break stack.pop();
            },
            Instr::MakeList(n) => {
                let val = Value::List(stack.split_off(stack.len().saturating_sub(n)));
                stack.push(val);
            },
            Instr::Dup(x) => stack.push(stack[stack.len() - 1 - x].clone()),
            Instr::Jump(n) => {
                next_addr = addr.jump(n);
                // println!("Jump from 0x{:03X} to 0x{:03X}", addr.0, next_addr.0);
            },
            Instr::IfNot => {
                if stack.pop().unwrap().bool() {
                    next_addr = next_addr.jump(1);
                }
            },
            Instr::Field(i) => {
                let mut x = stack.pop().unwrap().list();
                stack.push(x.remove(i));
            },
            Instr::NotBool => {
                let x = stack.pop().unwrap().bool();
                stack.push(Value::Bool(!x))
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
