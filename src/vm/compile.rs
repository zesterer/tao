use std::{
    rc::Rc,
    collections::HashMap,
};
use internment::LocalIntern;
use crate::{
    error::Error,
    ast::{UnaryOp, BinaryOp},
    node::RawTypeNode,
    ty::Primitive,
    mir, hir,
};
use super::{
    CodeAddr, Instr, Program, Value,
    builder::ProcBuilder,
};

type Ident = LocalIntern<String>;

fn push_constant_num(x: f64) -> Instr {
    if x.fract() == 0.0 && x > i32::MIN as f64 && x < i32::MAX as f64 {
        Instr::Integer(x as i32)
    } else if ((x as f32 as f64) - x).abs() < f64::EPSILON {
        Instr::Float(x as f32)
    } else {
        todo!()
    }
}

const DEBUG: bool = true;

impl RawTypeNode<mir::Expr> {
    pub fn compile(&self, program: &mut Program, scope: &mut (&mir::Program, &mut impl FnMut(CodeAddr, mir::DefId), Vec<Ident>), builder: &mut ProcBuilder) {
        if DEBUG {
            builder.emit_debug(format!("{:?}", self.span()));
        }

        match &**self {
            mir::Expr::Value(val) => match val {
                hir::Value::Number(x) => {
                    builder.emit_instr(push_constant_num(*x));
                },
                hir::Value::Boolean(x) => {
                    builder.emit_instr(if *x { Instr::True } else { Instr::False });
                },
                hir::Value::String(x) => {
                    let s = builder.emit_const(Value::String(Rc::new(x.to_string())));
                    builder.emit_instr(Instr::LoadConst(s));
                },
            },
            mir::Expr::GetLocal(local) => {
                let offset = scope.2
                    .iter()
                    .rev()
                    .enumerate()
                    .find(|(_, ident)| *ident == local)
                    .unwrap().0 as u32;

                builder.emit_instr(Instr::LoadLocal(offset));
            },
            mir::Expr::GetGlobal(global) => builder.emit_global_call(*global),
            mir::Expr::Unary(op, a) => {
                a.compile(program, scope, builder);
                match (op, a.ty()) {
                    (UnaryOp::Neg, mir::RawType::Primitive(Primitive::Number)) => builder.emit_instr(Instr::NegNum),
                    (UnaryOp::Not, mir::RawType::Primitive(Primitive::Boolean)) => builder.emit_instr(Instr::NotBool),
                    _ => todo!(),
                };
            },
            mir::Expr::Binary(op, a, b) => {
                b.compile(program, scope, builder);
                a.compile(program, scope, builder);
                match (op, a.ty(), b.ty()) {
                    (BinaryOp::Add, mir::RawType::Primitive(Primitive::Number), mir::RawType::Primitive(Primitive::Number)) => builder.emit_instr(Instr::AddNum),
                    (BinaryOp::Sub, mir::RawType::Primitive(Primitive::Number), mir::RawType::Primitive(Primitive::Number)) => builder.emit_instr(Instr::SubNum),
                    (BinaryOp::Mul, mir::RawType::Primitive(Primitive::Number), mir::RawType::Primitive(Primitive::Number)) => builder.emit_instr(Instr::MulNum),
                    (BinaryOp::Div, mir::RawType::Primitive(Primitive::Number), mir::RawType::Primitive(Primitive::Number)) => builder.emit_instr(Instr::DivNum),
                    (BinaryOp::Rem, mir::RawType::Primitive(Primitive::Number), mir::RawType::Primitive(Primitive::Number)) => builder.emit_instr(Instr::RemNum),
                    (BinaryOp::Eq, mir::RawType::Primitive(Primitive::Number), mir::RawType::Primitive(Primitive::Number)) => builder.emit_instr(Instr::EqNum),
                    (BinaryOp::Eq, mir::RawType::Primitive(Primitive::Boolean), mir::RawType::Primitive(Primitive::Boolean)) => builder.emit_instr(Instr::EqBool),
                    (BinaryOp::And, mir::RawType::Primitive(Primitive::Boolean), mir::RawType::Primitive(Primitive::Boolean)) => builder.emit_instr(Instr::AndBool),
                    (BinaryOp::Or, mir::RawType::Primitive(Primitive::Boolean), mir::RawType::Primitive(Primitive::Boolean)) => builder.emit_instr(Instr::OrBool),
                    (BinaryOp::Join, mir::RawType::List(_), mir::RawType::List(_)) => builder.emit_instr(Instr::JoinList),
                    _ => todo!(),
                };
            },
            mir::Expr::MakeTuple(items) => {
                for item in items.iter().rev() {
                    item.compile(program, scope, builder);
                }
                builder.emit_instr(Instr::MakeList(items.len() as u32));
            },
            mir::Expr::MakeList(items) => {
                for item in items.iter().rev() {
                    item.compile(program, scope, builder);
                }
                builder.emit_instr(Instr::MakeList(items.len() as u32));
            },
            mir::Expr::MatchValue(pred, arms) => {
                pred.compile(program, scope, builder);
                let mut exit_jumps = Vec::new();
                for (i, (matcher, extractor, body)) in arms.iter().enumerate() {
                    if !matcher.is_refutable() || arms.len() == 1 {
                        extractor.compile(builder);
                        let bindings = extractor.get_bindings();
                        bindings.iter().for_each(|b| scope.2.push(*b)); // Push locals
                        body.compile(program, scope, builder);
                        bindings.iter().for_each(|_| { // Pop locals
                            scope.2.pop();
                            builder.emit_instr(Instr::PopLocal);
                        });
                        break; // This arm matches everything, so no need to continue
                    } else {
                        let fail_jump_addr = if i < arms.len() - 1 { // Don't check for last arm
                            builder.emit_instr(Instr::Dup); // Duplicate predicate if it's needed later during extraction
                            matcher.compile(builder);
                            Some(builder.emit_instr(Instr::Nop)) // To be patched later
                        } else {
                            None
                        };

                        extractor.compile(builder);
                        let bindings = extractor.get_bindings();
                        bindings.iter().for_each(|b| scope.2.push(*b)); // Push locals
                        body.compile(program, scope, builder);
                        bindings.iter().for_each(|_| { // Pop locals
                            scope.2.pop();
                            builder.emit_instr(Instr::PopLocal);
                        });
                        // Don't jump for the last arm, since we'd be jumping to the next instr anyway
                        if i < arms.len() - 1 {
                            exit_jumps.push(builder.emit_instr(Instr::Nop)); // To be patched later
                        }

                        if let Some(fail_jump_addr) = fail_jump_addr {
                            builder.patch_instr(fail_jump_addr, Instr::JumpIfNot(builder.next_addr()));
                        }
                    }
                }
                for exit_jump in exit_jumps {
                    builder.patch_instr(exit_jump, Instr::Jump(builder.next_addr()));
                }
            },
            mir::Expr::MakeFunc(extractor, env, body) => {
                // Create body
                let func_addr = {
                    let mut builder = ProcBuilder::default();

                    extractor.compile(&mut builder);
                    let bindings = extractor.get_bindings();
                    let mut func_scope = env.clone();
                    bindings.iter().for_each(|b| func_scope.push(*b)); // Push locals
                    body.compile(program, &mut (scope.0, scope.1, func_scope), &mut builder);
                    bindings.iter().for_each(|_| { // Pop pattern locals
                        builder.emit_instr(Instr::PopLocal);
                    });
                    env.iter().for_each(|_| { // Pop environment
                        builder.emit_instr(Instr::PopLocal);
                    });
                    builder.emit_instr(Instr::Return(0));

                    let (global_addr, global_calls) = builder.link(program);

                    for (addr, id) in global_calls {
                        (scope.1)(addr, id);
                    }

                    global_addr
                };

                for local in env.iter().rev() {
                    let offset = scope.2
                        .iter()
                        .rev()
                        .enumerate()
                        .find(|(_, ident)| *ident == local)
                        .unwrap().0 as u32;

                    builder.emit_instr(Instr::LoadLocal(offset));
                }

                builder.emit_instr(Instr::MakeFunc(env.len() as u16, func_addr));
            },
            mir::Expr::Apply(f, arg) => {
                arg.compile(program, scope, builder);
                f.compile(program, scope, builder);
                builder.emit_instr(Instr::ApplyFunc);
            },
            expr => todo!("{:?}", expr),
        }
    }
}

impl mir::Matcher {
    pub fn compile(&self, builder: &mut ProcBuilder) {
        match self {
            // Should be handled in `Expr::DestructureValue` compilation
            mir::Matcher::Wildcard => unreachable!(),
            mir::Matcher::Exactly(val) => match val {
                // If the value is true, and we're looking for true... then we already have the answer
                // No need to do `if x = true` when you can just do `if x`.
                hir::Value::Boolean(true) => {},
                // If the value is true and we're looking for false... just negate the result. Easy.
                hir::Value::Boolean(false) => { builder.emit_instr(Instr::NotBool); },
                hir::Value::Number(x) => {
                    builder.emit_instr(push_constant_num(*x));
                    builder.emit_instr(Instr::EqNum);
                },
                hir::Value::String(x) => todo!(),
            },
            mir::Matcher::Tuple(items) => {
                let mut fail_jumps = Vec::new();
                let refutable_items = items
                    .iter()
                    .enumerate()
                    .filter(|(_, item)| item.is_refutable())
                    .collect::<Vec<_>>();
                for (i, item) in refutable_items.iter() {
                    builder.emit_instr(Instr::Dup);
                    builder.emit_instr(Instr::IndexList(*i as u32));
                    item.compile(builder);
                    fail_jumps.push(builder.emit_instr(Instr::Nop));
                }
                builder.emit_instr(Instr::Pop); // Pop matched value
                builder.emit_instr(Instr::True);

                // If any of the tuples failed to match
                if fail_jumps.len() > 0 {
                    let true_jump = builder.emit_instr(Instr::Nop);

                    for addr in fail_jumps {
                        builder.patch_instr(addr, Instr::JumpIfNot(builder.next_addr()));
                    }
                    builder.emit_instr(Instr::Pop); // Pop matched value
                    builder.emit_instr(Instr::False);

                    builder.patch_instr(true_jump, Instr::Jump(builder.next_addr()));
                }
            },
            mir::Matcher::List(items) => {
                let mut fail_jumps = Vec::new();

                builder.emit_instr(Instr::Dup);
                builder.emit_instr(Instr::LenEqList(items.len() as u32));
                fail_jumps.push(builder.emit_instr(Instr::Nop));

                let refutable_items = items
                    .iter()
                    .enumerate()
                    .filter(|(_, item)| item.is_refutable())
                    .collect::<Vec<_>>();
                for (i, item) in refutable_items.iter() {
                    builder.emit_instr(Instr::Dup);
                    builder.emit_instr(Instr::IndexList(*i as u32));
                    item.compile(builder);
                    fail_jumps.push(builder.emit_instr(Instr::Nop));
                }
                builder.emit_instr(Instr::Pop); // Pop matched value
                builder.emit_instr(Instr::True);

                // If any of the tuples failed to match
                if fail_jumps.len() > 0 {
                    let true_jump = builder.emit_instr(Instr::Nop);

                    for addr in fail_jumps {
                        builder.patch_instr(addr, Instr::JumpIfNot(builder.next_addr()));
                    }
                    builder.emit_instr(Instr::Pop); // Pop matched value
                    builder.emit_instr(Instr::False);

                    builder.patch_instr(true_jump, Instr::Jump(builder.next_addr()));
                }
            },
            mir::Matcher::ListFront(items) => {
                let mut fail_jumps = Vec::new();

                builder.emit_instr(Instr::Dup);
                builder.emit_instr(Instr::LenMoreEqList(items.len() as u32));
                fail_jumps.push(builder.emit_instr(Instr::Nop));

                let refutable_items = items
                    .iter()
                    .enumerate()
                    .filter(|(_, item)| item.is_refutable())
                    .collect::<Vec<_>>();
                for (i, item) in refutable_items.iter() {
                    builder.emit_instr(Instr::Dup);
                    builder.emit_instr(Instr::IndexList(*i as u32));
                    item.compile(builder);
                    fail_jumps.push(builder.emit_instr(Instr::Nop));
                }
                builder.emit_instr(Instr::Pop); // Pop matched value
                builder.emit_instr(Instr::True);

                // If any of the tuples failed to match
                if fail_jumps.len() > 0 {
                    let true_jump = builder.emit_instr(Instr::Nop);

                    for addr in fail_jumps {
                        builder.patch_instr(addr, Instr::JumpIfNot(builder.next_addr()));
                    }
                    builder.emit_instr(Instr::Pop); // Pop matched value
                    builder.emit_instr(Instr::False);

                    builder.patch_instr(true_jump, Instr::Jump(builder.next_addr()));
                }
            },
        }
    }
}

impl mir::Extractor {
    pub fn compile(&self, builder: &mut ProcBuilder) {
        match self {
            mir::Extractor::Just(this) => if this.is_some() {
                builder.emit_instr(Instr::PushLocal);
            } else {
                builder.emit_instr(Instr::Pop);
            },
            mir::Extractor::Tuple(this, items) | mir::Extractor::List(this, items) => {
                if this.is_some() {
                    builder.emit_instr(Instr::Dup);
                    builder.emit_instr(Instr::PushLocal);
                }

                for (i, item) in items.iter().enumerate() {
                    if item.extracts_anything() {
                        builder.emit_instr(Instr::Dup);
                        builder.emit_instr(Instr::IndexList(i as u32));
                        item.compile(builder);
                    }
                }

                builder.emit_instr(Instr::Pop);
            },
            mir::Extractor::ListFront(this, items, tail) => {
                if this.is_some() {
                    builder.emit_instr(Instr::Dup);
                    builder.emit_instr(Instr::PushLocal);
                }
                if tail.is_some() {
                    builder.emit_instr(Instr::Dup);
                    builder.emit_instr(Instr::TailList(items.len() as u32));
                    builder.emit_instr(Instr::PushLocal);
                }

                for (i, item) in items.iter().enumerate() {
                    if item.extracts_anything() {
                        builder.emit_instr(Instr::Dup);
                        builder.emit_instr(Instr::IndexList(i as u32));
                        item.compile(builder);
                    }
                }

                builder.emit_instr(Instr::Pop);
            },
        }
    }
}

impl mir::Program {
    pub fn compile(&self) -> Result<Program, Error> {
        let mut program = Program::default();

        let mut global_refs = Vec::new();

        let globals = self
            .globals()
            .map(|(id, global)| {
                let mut builder = ProcBuilder::default();

                if DEBUG {
                    builder.emit_debug(format!(":: {} {} of {}", id.0, id.1.iter().map(|ty| ty.mangle()).collect::<Vec<_>>().join(", "), global.ty().mangle()));
                }

                let mut scope = (
                    self,
                    &mut |global_addr, id| global_refs.push((global_addr, id)),
                    Vec::new(),
                );

                global.compile(&mut program, &mut scope, &mut builder);
                builder.emit_instr(Instr::Return(0));
                let (global_addr, global_calls) = builder.link(&mut program);

                for (addr, id) in global_calls {
                    //println!("global_addr: {:#X}", global_addr);
                    //println!("local_addr: {:#X}", local_addr);
                    global_refs.push((addr, id));
                }

                (id, global_addr)
            })
            .collect::<HashMap<_, _>>();

        for (addr, id) in global_refs {
            //println!("ADDR: {:#X}", addr);
            //println!("{:?}", program);
            program.patch_instr(addr, Instr::Call(globals[&id]));
        }

        program.set_entry(globals[&self.entry]);

        Ok(program)
    }
}
