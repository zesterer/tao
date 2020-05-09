use std::{
    collections::HashMap,
    rc::Rc,
};
use crate::{
    error::Error,
    ast::{UnaryOp, BinaryOp},
    ty::Primitive,
    mir, hir2,
};
use super::{
    Instr, Program, Value,
    builder::ProcBuilder,
};

impl mir::Expr {
    pub fn compile(&self, builder: &mut ProcBuilder) {
        match self {
            mir::Expr::Value(val) => match val {
                hir2::Value::Number(x) => if x.fract() == 0.0 && *x > i32::MIN as f64 && *x < i32::MAX as f64 {
                    builder.emit_instr(Instr::Integer(*x as i32));
                } else {
                    builder.emit_instr(Instr::Float(*x as f32));
                },
                hir2::Value::Boolean(x) => {
                    builder.emit_instr(if *x { Instr::True } else { Instr::False });
                },
                hir2::Value::String(x) => {
                    let s = builder.emit_const(Value::String(Rc::new(x.to_string())));
                    builder.emit_instr(Instr::LoadConst(s));
                },
            },
            mir::Expr::Unary(op, a) => {
                a.compile(builder);
                match (op, a.ty()) {
                    (UnaryOp::Neg, mir::RawType::Primitive(Primitive::Number)) => builder.emit_instr(Instr::NegNum),
                    (UnaryOp::Not, mir::RawType::Primitive(Primitive::Boolean)) => builder.emit_instr(Instr::NotBool),
                    _ => todo!(),
                };
            },
            mir::Expr::Binary(op, a, b) => {
                b.compile(builder);
                a.compile(builder);
                match (op, a.ty(), b.ty()) {
                    (BinaryOp::Add, mir::RawType::Primitive(Primitive::Number), mir::RawType::Primitive(Primitive::Number)) => builder.emit_instr(Instr::AddNum),
                    (BinaryOp::Sub, mir::RawType::Primitive(Primitive::Number), mir::RawType::Primitive(Primitive::Number)) => builder.emit_instr(Instr::SubNum),
                    (BinaryOp::Mul, mir::RawType::Primitive(Primitive::Number), mir::RawType::Primitive(Primitive::Number)) => builder.emit_instr(Instr::MulNum),
                    (BinaryOp::Div, mir::RawType::Primitive(Primitive::Number), mir::RawType::Primitive(Primitive::Number)) => builder.emit_instr(Instr::DivNum),
                    (BinaryOp::Rem, mir::RawType::Primitive(Primitive::Number), mir::RawType::Primitive(Primitive::Number)) => builder.emit_instr(Instr::RemNum),
                    _ => todo!(),
                };
            },
            _ => todo!(),
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
                expr.compile(&mut builder);
                builder.link(&mut program);
            });

        Ok(program)
    }
}
