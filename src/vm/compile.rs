use std::rc::Rc;
use internment::LocalIntern;
use crate::{
    error::Error,
    ast::{UnaryOp, BinaryOp},
    ty::Primitive,
    mir, hir,
};
use super::{
    Instr, Program, Value,
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

impl mir::Expr {
    pub fn compile(&self, scope: &mut Vec<Ident>, builder: &mut ProcBuilder) {
        match self {
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
                let offset = scope
                    .iter()
                    .rev()
                    .enumerate()
                    .find(|(_, ident)| *ident == local)
                    .unwrap().0 as u32;
                builder.emit_instr(Instr::LoadLocal(offset));
            },
            mir::Expr::Unary(op, a) => {
                a.compile(scope, builder);
                match (op, a.ty()) {
                    (UnaryOp::Neg, mir::RawType::Primitive(Primitive::Number)) => builder.emit_instr(Instr::NegNum),
                    (UnaryOp::Not, mir::RawType::Primitive(Primitive::Boolean)) => builder.emit_instr(Instr::NotBool),
                    _ => todo!(),
                };
            },
            mir::Expr::Binary(op, a, b) => {
                b.compile(scope, builder);
                a.compile(scope, builder);
                match (op, a.ty(), b.ty()) {
                    (BinaryOp::Add, mir::RawType::Primitive(Primitive::Number), mir::RawType::Primitive(Primitive::Number)) => builder.emit_instr(Instr::AddNum),
                    (BinaryOp::Sub, mir::RawType::Primitive(Primitive::Number), mir::RawType::Primitive(Primitive::Number)) => builder.emit_instr(Instr::SubNum),
                    (BinaryOp::Mul, mir::RawType::Primitive(Primitive::Number), mir::RawType::Primitive(Primitive::Number)) => builder.emit_instr(Instr::MulNum),
                    (BinaryOp::Div, mir::RawType::Primitive(Primitive::Number), mir::RawType::Primitive(Primitive::Number)) => builder.emit_instr(Instr::DivNum),
                    (BinaryOp::Rem, mir::RawType::Primitive(Primitive::Number), mir::RawType::Primitive(Primitive::Number)) => builder.emit_instr(Instr::RemNum),
                    (BinaryOp::Eq, mir::RawType::Primitive(Primitive::Number), mir::RawType::Primitive(Primitive::Number)) => builder.emit_instr(Instr::EqNum),
                    _ => todo!(),
                };
            },
            mir::Expr::MakeTuple(items) => {
                for item in items.iter().rev() {
                    item.compile(scope, builder);
                }
                builder.emit_instr(Instr::MakeList(items.len() as u32));
            },
            mir::Expr::MatchValue(pred, arms) => {
                pred.compile(scope, builder);
                let mut exit_jumps = Vec::new();
                for (matcher, extractor, body) in arms.iter() {
                    if matches!(matcher, mir::Matcher::Wildcard) || arms.len() == 1 {
                        extractor.compile(builder);
                        let bindings = extractor.get_bindings();
                        bindings.iter().for_each(|b| scope.push(*b)); // Push locals
                        body.compile(scope, builder);
                        bindings.iter().for_each(|_| { // Pop locals
                            scope.pop();
                            builder.emit_instr(Instr::PopLocal);
                        });
                        break; // This arm matches everything, so no need to continue
                    } else {
                        builder.emit_instr(Instr::Dup); // Duplicate predicate
                        matcher.compile(builder);
                        let fail_jump_addr = builder.emit_instr(Instr::Nop); // To be patched later

                        extractor.compile(builder);
                        let bindings = extractor.get_bindings();
                        bindings.iter().for_each(|b| scope.push(*b)); // Push locals
                        body.compile(scope, builder);
                        bindings.iter().for_each(|_| { // Pop locals
                            scope.pop();
                            builder.emit_instr(Instr::PopLocal);
                        });
                        let exit_jump_addr = builder.emit_instr(Instr::Nop); // To be patched later
                        exit_jumps.push(exit_jump_addr);
                        builder.patch_instr(fail_jump_addr, Instr::JumpIfNot(builder.next_addr()));
                    }
                }
                for exit_jump in exit_jumps {
                    builder.patch_instr(exit_jump, Instr::Jump(builder.next_addr()));
                }
            },
            _ => todo!(),
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
            mir::Matcher::Tuple(_) => todo!(),
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
            mir::Extractor::Tuple(this, items) => {
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
        }
    }
}

impl mir::Program {
    pub fn compile(&self) -> Result<Program, Error> {
        let mut program = Program::default();

        //let mut global_refs = HashMap::default();

        self.globals
            .iter()
            .for_each(|(name, expr)| {
                let mut builder = ProcBuilder::default();
                let mut scope = Vec::new();
                expr.compile(&mut scope, &mut builder);
                builder.link(&mut program);
            });

        Ok(program)
    }
}
