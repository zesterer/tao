use super::*;
use im::{vector, Vector};
use std::{fmt, rc::Rc};

#[derive(Clone, Debug)]
pub struct Effect {
    addr: Addr,
    captures: Vector<Value>,
}

#[derive(Clone, Debug)]
pub enum Value {
    Int(i64),
    Real(f64),
    Char(char),
    List(Vector<Self>),
    Func(Addr, Vector<Self>),
    Sum(usize, Rc<Self>),
    Universe(u64),
    Effect(Rc<Effect>),
}

impl Value {
    pub fn new_bool(x: bool) -> Self {
        Value::Sum(x as usize, Rc::new(Value::List(Vector::new())))
    }

    pub fn int(self) -> i64 {
        if let Value::Int(x) = self {
            x
        } else {
            panic!("{}", self)
        }
    }
    pub fn real(self) -> f64 {
        if let Value::Real(x) = self {
            x
        } else {
            panic!("{}", self)
        }
    }
    pub fn char(self) -> char {
        if let Value::Char(c) = self {
            c
        } else {
            panic!("{}", self)
        }
    }
    pub fn bool(self) -> bool {
        if let Value::Sum(x, _) = self {
            x > 0
        } else {
            panic!("{}", self)
        }
    }
    pub fn list(self) -> Vector<Self> {
        if let Value::List(xs) = self {
            xs
        } else {
            panic!("{}", self)
        }
    }
    pub fn func(self) -> (Addr, Vector<Self>) {
        if let Value::Func(f_addr, captures) = self {
            (f_addr, captures)
        } else {
            panic!("{}", self)
        }
    }
    pub fn sum(self) -> (usize, Rc<Self>) {
        if let Value::Sum(variant, inner) = self {
            (variant, inner)
        } else {
            panic!("{}", self)
        }
    }
    pub fn universe(self) -> u64 {
        if let Value::Universe(x) = self {
            x
        } else {
            panic!("{}", self)
        }
    }
    pub fn eff(self) -> Rc<Effect> {
        if let Value::Effect(eff) = self {
            eff
        } else {
            panic!("{}", self)
        }
    }

    pub fn display(self) -> String {
        match self {
            Value::Int(x) => format!("{}", x),
            Value::Real(x) => format!("{}", x),
            Value::Char(c) => format!("{}", c),
            _ => todo!(),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Int(x) => write!(f, "{}i", x),
            Value::Real(x) => write!(f, "{}f", x),
            Value::Char(c) => write!(f, "{}", c),
            Value::List(items) => match items.iter().next() {
                Some(Value::Char(_)) => items.iter().try_for_each(|c| write!(f, "{}", c)),
                _ => write!(
                    f,
                    "[{}]",
                    items
                        .iter()
                        .map(|x| format!("{}", x))
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
            },
            Value::Func(addr, captures) => write!(
                f,
                "Function(addr = 0x{:03X}, captures = {})",
                addr.0,
                captures.len(),
            ),
            Value::Sum(variant, inner) => write!(f, "#{} {}", variant, inner),
            Value::Universe(x) => write!(f, "Universe({})", x),
            Value::Effect(eff) => write!(
                f,
                "Effect(addr = 0x{:03X}, captures = {})",
                eff.addr.0,
                eff.captures.len(),
            ),
        }
    }
}

pub fn exec<E: Env>(prog: &Program, env: &mut E) -> Option<Value> {
    let mut addr = prog.entry;
    let mut universe_counter = 0;

    let mut funcs = Vec::new();
    let mut stack = Vec::new();
    let mut locals = if prog.does_io {
        vec![Value::Universe(universe_counter)]
    } else {
        Vec::new()
    };
    let mut handlers: Vector<(_, Value, usize)> = Vector::new();

    let mut tick = 0u64;
    loop {
        let mut next_addr = addr.incr();

        // println!("Executing 0x{:03X}... Stack: {}", addr.0, stack.iter().rev().map(|x| format!("{}", x)).collect::<Vec<_>>().join(", "));

        match prog.instr(addr) {
            Instr::Error(err) => panic!("Error: {}", err),
            Instr::Nop => {}
            Instr::Break => {
                println!("Breakpoint at 0x{:03X?}", addr.0);
                for (i, x) in stack.iter().rev().enumerate() {
                    println!("{:02} | {:?}", i, x);
                }
            }
            Instr::Imm(x) => stack.push(x.clone()),
            Instr::Pop(n) => {
                assert!(n > 0, "Popped zero items, this is probably a bug");
                stack.truncate(stack.len().saturating_sub(n));
            }
            Instr::Replace => {
                let x = stack.pop().unwrap();
                stack.pop();
                stack.push(x);
            }
            Instr::Swap => {
                let x = stack.pop().unwrap();
                let y = stack.pop().unwrap();
                stack.push(x);
                stack.push(y);
            }
            Instr::Call(n) => {
                funcs.push(next_addr);
                next_addr = addr.jump(n);
            }
            Instr::Ret => {
                if let Some(addr) = funcs.pop() {
                    next_addr = addr;
                } else {
                    assert_eq!(
                        locals.len(),
                        0,
                        "Local stack still has values, this is probably a bug"
                    );
                    assert_eq!(stack.len(), 1, "Stack size must be 1 on program exit");
                    // println!("Executed {} instructions.", tick);
                    break if prog.does_io {
                        let mut r = stack.pop().unwrap().list();
                        assert_eq!(r.remove(0).universe(), universe_counter);
                        None
                    } else {
                        stack.pop()
                    };
                }
            }
            Instr::MakeFunc(i, n) => {
                let f_addr = addr.jump(i);
                let func = Value::Func(
                    f_addr,
                    stack.split_off(stack.len().saturating_sub(n)).into(),
                );
                stack.push(func);
            }
            Instr::ApplyFunc => {
                let (f_addr, captures) = stack.pop().unwrap().func();

                funcs.push(next_addr);
                next_addr = f_addr;

                locals.extend(captures.into_iter());
            }
            Instr::MakeList(n) => {
                let val = Value::List(stack.split_off(stack.len().saturating_sub(n)).into());
                stack.push(val);
            }
            Instr::IndexList(i) => {
                let mut x = stack.pop().unwrap().list();
                if x.len() < i + 1 {
                    panic!("Removing item {} from list {:?} at 0x{:X}", i, x, addr.0);
                }
                stack.push(x.remove(i));
            }
            Instr::SkipListImm(i) => {
                let x = stack.pop().unwrap().list();
                stack.push(Value::List(x.skip(i)));
            }
            Instr::SetList(idx) => {
                let item = stack.pop().unwrap();
                let mut xs = stack.pop().unwrap().list();
                xs[idx] = item;
                stack.push(Value::List(xs));
            }
            Instr::LenList => {
                let len = stack.pop().unwrap().list().len();
                stack.push(Value::Int(len as i64));
            }
            Instr::JoinList => {
                let y = stack.pop().unwrap().list();
                let mut x = stack.pop().unwrap().list();
                x.append(y);
                stack.push(Value::List(x));
            }
            Instr::SkipList => {
                let i = stack.pop().unwrap().int();
                let xs = stack.pop().unwrap().list();
                stack.push(Value::List(xs.skip((i as usize).min(xs.len()))));
            }
            Instr::TrimList => {
                let i = stack.pop().unwrap().int();
                let mut xs = stack.pop().unwrap().list();
                xs.truncate((i as usize).min(xs.len()));
                stack.push(Value::List(xs));
            }
            Instr::MakeSum(variant) => {
                let x = stack.pop().unwrap();
                stack.push(Value::Sum(variant, Rc::new(x)));
            }
            Instr::IndexSum(variant) => {
                let (v, inner) = stack.pop().unwrap().sum();
                debug_assert_eq!(variant, v);
                stack.push((*inner).clone());
            }
            Instr::VariantSum => {
                let (variant, _) = stack.pop().unwrap().sum();
                stack.push(Value::Int(variant as i64));
            }
            Instr::Dup => stack.push(stack.last().unwrap().clone()),
            Instr::Jump(n) => {
                next_addr = addr.jump(n);
            }
            Instr::IfNot => {
                if stack.pop().unwrap().bool() {
                    next_addr = next_addr.jump(1);
                }
            }
            Instr::PushLocal => locals.push(stack.pop().unwrap()),
            Instr::PopLocal(n) => locals.truncate(locals.len() - n),
            Instr::GetLocal(x) => stack.push(locals[locals.len() - 1 - x].clone()),
            Instr::NotBool => {
                let x = stack.pop().unwrap().bool();
                stack.push(Value::new_bool(!x))
            }
            Instr::NegInt => {
                let x = stack.pop().unwrap().int();
                stack.push(Value::Int(-x))
            }
            Instr::NegReal => {
                let x = stack.pop().unwrap().real();
                stack.push(Value::Real(-x))
            }
            Instr::Display => {
                let s = stack.pop().unwrap().display();
                stack.push(Value::List(s.chars().map(Value::Char).collect()))
            }
            Instr::Codepoint => {
                let c = stack.pop().unwrap().char();
                stack.push(Value::Int(c as u64 as i64))
            }
            Instr::AddInt => {
                let y = stack.pop().unwrap().int();
                let x = stack.pop().unwrap().int();
                stack.push(Value::Int(x + y))
            }
            Instr::SubInt => {
                let y = stack.pop().unwrap().int();
                let x = stack.pop().unwrap().int();
                stack.push(Value::Int(x - y))
            }
            Instr::MulInt => {
                let y = stack.pop().unwrap().int();
                let x = stack.pop().unwrap().int();
                stack.push(Value::Int(x * y))
            }
            Instr::EqInt => {
                let y = stack.pop().unwrap().int();
                let x = stack.pop().unwrap().int();
                stack.push(Value::new_bool(x == y))
            }
            Instr::EqBool => {
                let y = stack.pop().unwrap().bool();
                let x = stack.pop().unwrap().bool();
                stack.push(Value::new_bool(x == y))
            }
            Instr::EqChar => {
                let y = stack.pop().unwrap().char();
                let x = stack.pop().unwrap().char();
                stack.push(Value::new_bool(x == y))
            }
            Instr::LessInt => {
                let y = stack.pop().unwrap().int();
                let x = stack.pop().unwrap().int();
                stack.push(Value::new_bool(x < y))
            }
            Instr::MoreInt => {
                let y = stack.pop().unwrap().int();
                let x = stack.pop().unwrap().int();
                stack.push(Value::new_bool(x > y))
            }
            Instr::LessEqInt => {
                let y = stack.pop().unwrap().int();
                let x = stack.pop().unwrap().int();
                stack.push(Value::new_bool(x <= y))
            }
            Instr::MoreEqInt => {
                let y = stack.pop().unwrap().int();
                let x = stack.pop().unwrap().int();
                stack.push(Value::new_bool(x >= y))
            }
            Instr::AndBool => {
                let y = stack.pop().unwrap().bool();
                let x = stack.pop().unwrap().bool();
                stack.push(Value::new_bool(x && y))
            }
            Instr::Print => {
                let s = stack.pop().unwrap().list();
                let universe = stack.pop().unwrap().universe();
                assert!(
                    universe == universe_counter,
                    "Universe forked, the thread of prophecy has been broken"
                );
                universe_counter += 1;
                env.print(s.into_iter().map(|c| c.char()).collect::<String>());
                stack.push(Value::Universe(universe_counter))
            }
            Instr::Input => {
                let universe = stack.pop().unwrap().universe();
                assert!(
                    universe == universe_counter,
                    "Universe forked, the thread of prophecy has been broken"
                );
                universe_counter += 1;

                let s = env.input();

                stack.push(Value::List(vector![
                    Value::Universe(universe_counter),
                    Value::List(s.trim_end().chars().map(Value::Char).collect()),
                ]));
            }
            Instr::Rand => {
                let max = stack.pop().unwrap().int();
                let universe = stack.pop().unwrap().universe();
                assert!(
                    universe == universe_counter,
                    "Universe forked, the thread of prophecy has been broken"
                );
                universe_counter += 1;

                let n = env.rand(max);

                stack.push(Value::List(vector![
                    Value::Universe(universe_counter),
                    Value::Int(n),
                ]));
            }
            Instr::MakeEffect(i, n) => {
                let addr = addr.jump(i);
                let func = Value::Effect(Rc::new(Effect {
                    addr,
                    captures: stack.split_off(stack.len().saturating_sub(n)).into(),
                }));
                stack.push(func);
            }
            Instr::Propagate => {
                let eff = stack.pop().unwrap().eff();

                funcs.push(next_addr);
                next_addr = eff.addr;

                locals.extend(eff.captures.iter().cloned());
            }
            Instr::Suspend(eff_id) => {
                let handler = handlers
                    .iter()
                    .rev()
                    .find(|(e, _, _)| *e == eff_id)
                    .unwrap_or_else(|| {
                        panic!(
                            "No such effect handler for {:?} on effect stack. Effect stack:\n{:#?}",
                            eff_id, handlers,
                        )
                    });
                let (f_addr, captures) = handler.1.clone().func();

                funcs.push(next_addr);
                next_addr = f_addr;

                locals.push(stack[handler.2].clone()); // Push effect
                locals.extend(captures.into_iter());
            }
            Instr::Register(eff_id) => {
                let handler = stack.pop().unwrap();
                // let state = stack.pop().unwrap();
                handlers.push_back((eff_id, handler, stack.len() - 1));
            }
            Instr::EndHandlers(n) => {
                let out = stack.pop().unwrap();
                let state = stack.pop().unwrap();
                handlers.truncate(handlers.len() - n);
                stack.push(Value::List([out, state].into_iter().collect()));
            }
            Instr::Resume(eff_id) => {
                let out_and_state = stack.pop().unwrap().list();
                let out = out_and_state[0].clone();
                let state = out_and_state[1].clone();

                // Replace old state
                let stack_idx = handlers
                    .iter_mut()
                    .rev()
                    .find(|(e, _, _)| *e == eff_id)
                    .unwrap()
                    .2;
                stack[stack_idx] = state;

                stack.push(out);
            }
        }

        addr = next_addr;
    }
}

pub trait Env {
    fn input(&mut self) -> String;
    fn print(&mut self, s: String);
    fn rand(&mut self, max: i64) -> i64;
}
