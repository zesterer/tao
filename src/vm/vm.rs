use super::{Instr, Program, Value};

#[derive(Default)]
pub struct Vm;

impl Vm {
    pub fn execute(mut self, prog: &Program) -> Value {
        let mut expr_stack = Vec::new();
        let mut call_stack = Vec::new();
        let mut local_stack = Vec::new();

        let mut ip = prog.entry();

        loop {
            let instr = unsafe { prog.fetch_instr_unchecked(ip) };
            ip += 1;

            match instr {
                Instr::Integer(x) => expr_stack.push(Value::Number(x as f64)),
                Instr::Float(x) => expr_stack.push(Value::Number(x as f64)),
                Instr::True => expr_stack.push(Value::Boolean(true)),
                Instr::False => expr_stack.push(Value::Boolean(false)),

                Instr::LoadConst(addr) => expr_stack.push(prog.fetch_const(addr)),

                Instr::PushLocal => local_stack.push(expr_stack.pop().unwrap()),

                Instr::Jump(addr) => ip = addr,
                Instr::JumpIf(addr) => {
                    if matches!(expr_stack.pop().unwrap(), Value::Boolean(true)) {
                        ip = addr;
                    }
                },
                Instr::Return(n) => {
                    let ret_val = expr_stack.pop().unwrap();
                    expr_stack.truncate(expr_stack.len() - n as usize);
                    if let Some(ret_addr) = call_stack.pop() {
                        ip = ret_addr;
                        expr_stack.push(ret_val);
                    } else {
                        return ret_val;
                    }
                },
            }
        }
    }
}
