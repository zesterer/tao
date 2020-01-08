use std::fmt;
use im_rc::Vector;
use crate::{
    parse::{Expr, Literal, UnaryOp, BinaryOp},
    node::Node,
    error::Error,
    src::SrcRegion,
    compile::{Addr, Instr, Program},
};

#[derive(Default)]
pub struct Vm {
    expr_stack: Vec<Value>,
    call_stack: Vec<Addr>,
    locals: Vec<Value>,
}

impl Vm {
    fn push(&mut self, val: Value) {
        self.expr_stack.push(val);
    }

    fn pop(&mut self) -> Value {
        self.expr_stack
            .pop()
            .expect("Tried to pop value from empty stack")
    }

    fn local(&self, offset: usize) -> &Value {
        &self.locals[self.locals.len() - 1 - offset]
    }

    pub fn execute(&mut self, prog: &Program) -> Result<Value, Error> {
        let mut ip = prog.entry();
        loop {
            let mut incr_ip = true;
            let instr = prog
                .instr(ip)
                .expect("Attempted to execute instruction outside program bounds");

            //println!("IP = {}, instr = {:?}", ip, instr);
            //println!("expr_stack = {:?}", self.expr_stack);
            //println!("locals = {:?}", self.locals);
            //println!("call_stack = {:?}", self.call_stack);
            //println!("");
            //std::thread::sleep(std::time::Duration::from_millis(100));

            match instr {
                Instr::Value(val) => self.expr_stack.push(val),
                Instr::UnaryOp(op) => {
                    let a = self.pop();
                    self.push(match op {
                        UnaryOp::Neg => a.neg(),
                        UnaryOp::Not => a.not(),
                        UnaryOp::Head => a.head(),
                        UnaryOp::Tail => a.tail(),
                    }.ok_or_else(|| Error::invalid_unary_op(op, prog.region(ip).unwrap()))?);
                },
                Instr::BinaryOp(op) => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(match op {
                        BinaryOp::Add => a.add(b),
                        BinaryOp::Sub => a.sub(b),
                        BinaryOp::Mul => a.mul(b),
                        BinaryOp::Div => a.div(b),
                        BinaryOp::Rem => a.rem(b),
                        BinaryOp::Eq => a.eq(&b).map(Value::Boolean),
                        BinaryOp::Less => a.less(b),
                        BinaryOp::More => a.more(b),
                        BinaryOp::LessEq => a.less_eq(b),
                        BinaryOp::MoreEq => a.more_eq(b),
                        BinaryOp::Join => a.join(b),
                    }.ok_or_else(|| Error::invalid_binary_op(op, prog.region(ip).unwrap()))?);
                },
                Instr::Branch(a, b) => {
                    let new_ip = if self
                        .pop()
                        .truth()
                        .ok_or_else(|| Error::not_truthy(prog.region(ip).unwrap()))?
                    {
                        a
                    } else {
                        b
                    };
                    self.call_stack.push(ip + 1);
                    ip = new_ip;
                    incr_ip = false;
                },
                Instr::Return(pop_n) => {
                    for _ in 0..pop_n {
                        self.locals.pop();
                    }
                    ip = if let Some(addr) = self.call_stack.pop() {
                        addr
                    } else {
                        return Ok(self.pop());
                    };
                    incr_ip = false;
                },
                Instr::Func(addr, env) => {
                    let env = env
                        .iter()
                        .map(|offset| self.local(*offset).clone())
                        .collect::<Vec<_>>();
                    self.push(Value::Func(addr, env));
                },
                Instr::Apply => match self.pop() {
                    Value::Func(addr, locals) => {
                        let arg = self.pop();
                        self.locals.push(arg);
                        self.locals.extend(locals.iter().cloned());
                        self.call_stack.push(ip + 1);
                        ip = addr;
                        incr_ip = false;
                    },
                    _ => return Err(Error::cannot_call(prog.region(ip).unwrap())),
                },
                Instr::MakeList(n) => {
                    let items = (0..n)
                        .map(|_| self.pop())
                        .collect();
                    self.push(Value::List(items));
                },
                Instr::Local(offset) => self.push(self.local(offset).clone()),
                _ => todo!(),
            }

            if incr_ip {
                ip += 1;
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum Value {
    Null,
    Number(f64),
    String(String),
    Boolean(bool),
    List(Vector<Self>),
    Func(Addr, Vec<Value>),
}

impl Value {
    pub fn number(self) -> Option<f64> {
        if let Value::Number(x) = self { Some(x) } else { None }
    }

    pub fn string(self) -> Option<String> {
        if let Value::String(x) = self { Some(x) } else { None }
    }

    pub fn boolean(self) -> Option<bool> {
        if let Value::Boolean(x) = self { Some(x) } else { None }
    }

    pub fn list(self) -> Option<Vector<Self>> {
        if let Value::List(x) = self { Some(x) } else { None }
    }

    pub fn add(self, other: Self) -> Option<Self> {
        self.number().and_then(|a| other.number().map(|b| Value::Number(a + b)))
    }

    pub fn sub(self, other: Self) -> Option<Self> {
        self.number().and_then(|a| other.number().map(|b| Value::Number(a - b)))
    }

    pub fn mul(self, other: Self) -> Option<Self> {
        self.number().and_then(|a| other.number().map(|b| Value::Number(a * b)))
    }

    pub fn div(self, other: Self) -> Option<Self> {
        self.number().and_then(|a| other.number().map(|b| Value::Number(a / b)))
    }

    pub fn rem(self, other: Self) -> Option<Self> {
        self.number().and_then(|a| other.number().map(|b| Value::Number(a % b)))
    }

    pub fn eq(&self, other: &Self) -> Option<bool> {
        match (self, other) {
            (Value::Null, Value::Null) => Some(true),
            (Value::Number(a), Value::Number(b)) => Some(a == b),
            (Value::String(a), Value::String(b)) => Some(a == b),
            (Value::Boolean(a), Value::Boolean(b)) => Some(a == b),
            (Value::List(a), Value::List(b)) => Some(if a.len() == b.len() {
                a.iter().zip(b.iter()).try_fold(true, |acc, (a, b)| Ok(acc || a.eq(b)?))?
            } else {
                false
            }),
            _ => Some(false),
        }
    }

    pub fn less(self, other: Self) -> Option<Self> {
        self.number().and_then(|a| other.number().map(|b| Value::Boolean(a < b)))
    }

    pub fn more(self, other: Self) -> Option<Self> {
        self.number().and_then(|a| other.number().map(|b| Value::Boolean(a > b)))
    }

    pub fn less_eq(self, other: Self) -> Option<Self> {
        self.number().and_then(|a| other.number().map(|b| Value::Boolean(a <= b)))
    }

    pub fn more_eq(self, other: Self) -> Option<Self> {
        self.number().and_then(|a| other.number().map(|b| Value::Boolean(a >= b)))
    }

    pub fn join(self, other: Self) -> Option<Self> {
        match (self, other) {
            (Value::List(mut a), Value::List(b)) => {
                a.append(b);
                Some(Value::List(a))
            },
            (Value::String(mut a), Value::String(b)) => {
                a += &b;
                Some(Value::String(a))
            },
            _ => None,
        }
    }

    pub fn neg(self) -> Option<Self> {
        self.number().map(|x| Value::Number(-x))
    }

    pub fn not(self) -> Option<Self> {
        self.boolean().map(|x| Value::Boolean(!x))
    }

    pub fn head(self) -> Option<Self> {
        self.list().map(|x| x.front().cloned().unwrap_or(Value::Null))
    }

    pub fn tail(self) -> Option<Self> {
        self.list().map(|mut x| Value::List(if x.len() == 0 {
            Vector::new()
        } else {
            x.split_off(1)
        }))
    }

    pub fn truth(self) -> Option<bool> {
        self.boolean()
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Null => write!(f, "null"),
            Value::Number(x) => write!(f, "{}", x),
            Value::String(x) => write!(f, "\"{}\"", x),
            Value::Boolean(x) => write!(f, "{}", x),
            Value::List(x) => {
                write!(f, "[")?;
                x.iter().map(|x| write!(f, "{}, ", x)).collect::<Result<_, _>>()?;
                write!(f, "]")
            },
            Value::Func(_, _) => write!(f, "<func>"),
        }
    }
}
