use std::fmt;
use internment::LocalIntern;
use im_rc::Vector;
use crate::{
    parse::{Expr, Literal, UnaryOp, BinaryOp},
    node::Node,
    error::Error,
    src::SrcRegion,
};

type Addr = usize;

#[derive(Clone, Debug)]
pub enum Instr {
    Value(Value<'static>), // Push a value onto the stack
    UnaryOp(UnaryOp), // Apply a unary operation to the latest stack item
    BinaryOp(BinaryOp), // Apply a binary operation to the 2 latest stack items
    Branch(Addr, Addr), // Call one of two procedures based on the truth of the last stack item
    Return(usize), // Return from the current procedure, popping n locals
    Func(Addr, Vec<usize>), // Create a function from the given address, list of stack value offsets, and parameter
    Apply, // Apply the last stack item to the function before it on the stack
    MakeList(usize), // Make a list of the last n stack items (in reverse order)
    Local(usize), // Place the local with the given offset onto the value stack
}

#[derive(Default, Debug)]
pub struct Program {
    instrs: Vec<Instr>,
    entry: Addr,
}

impl Program {
    pub fn instr(&self, addr: Addr) -> Option<Instr> {
        self.instrs.get(addr).cloned()
    }

    fn insert_procedure(&mut self, mut instrs: Vec<Instr>) -> Addr {
        let addr = self.instrs.len();
        self.instrs.append(&mut instrs);
        addr
    }

    fn compile_procedure(
        &mut self,
        expr: &Node<Expr>,
        locals: &Vec<LocalIntern<String>>,
        pop_n: usize,
    ) -> Addr {
        let mut instrs = Vec::new();
        self.compile_expr(expr, &mut instrs, locals);
        instrs.push(Instr::Return(pop_n));
        self.insert_procedure(instrs)
    }

    fn compile_expr(
        &mut self,
        expr: &Node<Expr>,
        instrs: &mut Vec<Instr>,
        locals: &Vec<LocalIntern<String>>,
    ) {
        let offset_of = |ident| locals.len().saturating_sub(1) - locals
            .iter()
            .enumerate()
            .find(|(_, local)| **local == ident)
            .expect("Could not find local")
            .0;

        match &**expr {
            Expr::Literal(x) => instrs.push(Instr::Value(match x {
                Literal::Null => Value::Null,
                Literal::Number(x) => Value::Number(*x),
                Literal::String(x) => Value::String(x.clone()),
                Literal::Boolean(x) => Value::Boolean(*x),
            })),
            Expr::Unary(op, a) => {
                self.compile_expr(a, instrs, locals);
                instrs.push(Instr::UnaryOp(**op));
            },
            Expr::Binary(op, a, b) => {
                self.compile_expr(a, instrs, locals);
                self.compile_expr(b, instrs, locals);
                instrs.push(Instr::BinaryOp(**op));
            },
            Expr::Branch(p, t, f) => {
                let t = self.compile_procedure(t, locals, 0);
                let f = self.compile_procedure(f, locals, 0);
                self.compile_expr(p, instrs, locals);
                instrs.push(Instr::Branch(t, f));
            },
            Expr::Func(name, body) => {
                let mut env_locals = expr.env_locals();

                // Find environment from current locals
                let mut env_offsets = Vec::new();
                for env_local in &env_locals {
                    env_offsets.push(offset_of(*env_local));
                }

                env_locals.insert(0, **name);

                let body = self.compile_procedure(body, &env_locals, env_locals.len());
                instrs.push(Instr::Func(body, env_offsets));
            },
            Expr::Apply(f, arg) => {
                self.compile_expr(arg, instrs, locals);
                self.compile_expr(f, instrs, locals);
                instrs.push(Instr::Apply);
            },
            Expr::List(items) => {
                for item in items.iter().rev() {
                    self.compile_expr(item, instrs, locals);
                }
                instrs.push(Instr::MakeList(items.len()));
            },
            Expr::Ident(ident) => instrs.push(Instr::Local(offset_of(**ident))),
        }
    }

    pub fn compile(expr: &Node<Expr>) -> Self {
        let mut this = Self::default();
        let entry = this.compile_procedure(expr, &mut Vec::new(), 0);
        this.entry = entry;
        this
    }
}

impl Expr {
    fn build_env_locals(
        &self,
        shadowed: &mut Vec<LocalIntern<String>>,
        env: &mut Vec<LocalIntern<String>>,
    ) {
        match self {
            Expr::Unary(_, a) => a.build_env_locals(shadowed, env),
            Expr::Binary(_, a, b) => {
                a.build_env_locals(shadowed, env);
                b.build_env_locals(shadowed, env);
            },
            Expr::Branch(p, t, f) => {
                p.build_env_locals(shadowed, env);
                t.build_env_locals(shadowed, env);
                f.build_env_locals(shadowed, env);
            },
            Expr::Func(name, body) => {
                shadowed.push(**name);
                body.build_env_locals(shadowed, env);
            },
            Expr::Apply(f, arg) => {
                f.build_env_locals(shadowed, env);
                arg.build_env_locals(shadowed, env);
            },
            Expr::List(items) => for item in items {
                item.build_env_locals(shadowed, env);
            },
            Expr::Ident(ident) => {
                if !shadowed.contains(&*ident) {
                    env.push(**ident);
                }
            },
            _ => {},
        }
    }

    fn env_locals(&self) -> Vec<LocalIntern<String>> {
        let mut env = Vec::new();
        self.build_env_locals(&mut Vec::new(), &mut env);
        env
    }
}

#[derive(Default)]
pub struct Vm {
    expr_stack: Vec<Value<'static>>,
    call_stack: Vec<Addr>,
    locals: Vec<Value<'static>>,
}

impl Vm {
    fn push(&mut self, val: Value<'static>) {
        self.expr_stack.push(val);
    }

    fn pop(&mut self) -> Value<'static> {
        self.expr_stack
            .pop()
            .expect("Tried to pop value from empty stack")
    }

    fn local(&self, offset: usize) -> &Value<'static> {
        &self.locals[self.locals.len() - 1 - offset]
    }

    pub fn execute(&mut self, prog: &Program) -> Result<Value<'static>, Error> {
        let mut ip = prog.entry;
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
                    }.ok_or_else(|| Error::invalid_unary_op(op, SrcRegion::none()))?);
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
                    }.ok_or_else(|| Error::invalid_binary_op(op, SrcRegion::none()))?);
                },
                Instr::Branch(a, b) => {
                    let new_ip = if self
                        .pop()
                        .truth()
                        .ok_or_else(|| Error::not_truthy(SrcRegion::none()))?
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
                    self.push(Value::NewFunc(addr, env));
                },
                Instr::Apply => match self.pop() {
                    Value::NewFunc(addr, locals) => {
                        let arg = self.pop();
                        self.locals.push(arg);
                        self.locals.extend(locals.iter().cloned());
                        self.call_stack.push(ip + 1);
                        ip = addr;
                        incr_ip = false;
                    },
                    _ => return Err(Error::cannot_call(SrcRegion::none())),
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

pub fn eval<'a>(expr: &'a Node<Expr>, scope: &Vector<(LocalIntern<String>, Value<'a>)>) -> Result<Value<'a>, Error> {
    match &**expr {
        Expr::Literal(x) => match x {
            Literal::Null => Ok(Value::Null),
            Literal::Number(x) => Ok(Value::Number(*x)),
            Literal::String(x) => Ok(Value::String(x.clone())),
            Literal::Boolean(x) => Ok(Value::Boolean(*x)),
        },
        Expr::Unary(op, a) => match &**op {
            UnaryOp::Neg => eval(a, scope)?.neg(),
            UnaryOp::Not => eval(a, scope)?.not(),
            UnaryOp::Head => eval(a, scope)?.head(),
            UnaryOp::Tail => eval(a, scope)?.tail(),
        }.ok_or_else(|| Error::invalid_unary_op(op.inner().clone(), op.region())),
        Expr::Binary(op, a, b) => match &**op {
            BinaryOp::Add => eval(a, scope)?.add(eval(b, scope)?),
            BinaryOp::Sub => eval(a, scope)?.sub(eval(b, scope)?),
            BinaryOp::Mul => eval(a, scope)?.mul(eval(b, scope)?),
            BinaryOp::Div => eval(a, scope)?.div(eval(b, scope)?),
            BinaryOp::Rem => eval(a, scope)?.rem(eval(b, scope)?),
            BinaryOp::Eq => eval(a, scope)?.eq(&eval(b, scope)?).map(Value::Boolean),
            BinaryOp::Less => eval(a, scope)?.less(eval(b, scope)?),
            BinaryOp::More => eval(a, scope)?.more(eval(b, scope)?),
            BinaryOp::LessEq => eval(a, scope)?.less_eq(eval(b, scope)?),
            BinaryOp::MoreEq => eval(a, scope)?.more_eq(eval(b, scope)?),
            BinaryOp::Join => eval(a, scope)?.join(eval(b, scope)?),
        }.ok_or_else(|| Error::invalid_binary_op(op.inner().clone(), op.region())),
        Expr::Branch(p, t, f) => eval(if eval(p, scope)?
            .truth()
            .ok_or_else(|| Error::not_truthy(p.region()))? { t } else { f }, scope),
        Expr::Func(name, body) => Ok(Value::Func(name, scope.clone(), body)),
        Expr::Apply(f, arg) => {
            let arg = eval(arg, scope)?;
            match eval(f, scope)? {
                Value::Func(name, mut scope, body) => {
                    scope.push_back((**name, arg));
                    eval(&body, &scope)
                },
                _ => Err(Error::cannot_call(f.region())),
            }
        },
        Expr::List(items) => Ok(Value::List(items.iter().map(|item| eval(item, scope)).collect::<Result<_, _>>()?)),
        Expr::Ident(ident) => Ok(scope
            .iter()
            .rev()
            .find(|(name, _)| *name == **ident)
            .ok_or_else(|| Error::no_such_binding(ident.to_string(), ident.region()))?
            .1
            .clone()),
    }
}

#[derive(Clone, Debug)]
pub enum Value<'a> {
    Null,
    Number(f64),
    String(String),
    Boolean(bool),
    List(Vector<Self>),
    Func(&'a Node<LocalIntern<String>>, Vector<(LocalIntern<String>, Self)>, &'a Node<Expr>),

    NewFunc(Addr, Vec<Value<'static>>),
}

impl<'a> Value<'a> {
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

impl<'a> fmt::Display for Value<'a> {
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
            Value::Func(_, _, _) => write!(f, "<func>"),
            Value::NewFunc(_, _) => write!(f, "<func>"),
        }
    }
}
