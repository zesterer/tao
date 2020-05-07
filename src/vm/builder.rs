use super::{Instr, CodeAddr, ConstAddr, Program, Value};

#[derive(Default)]
pub struct ProcBuilder {
    code: Vec<Instr>,
    consts: Vec<Value>,
}

impl ProcBuilder {
    pub fn emit_instr(&mut self, instr: Instr) -> CodeAddr {
        self.code.push(instr);
        (self.code.len() - 1) as CodeAddr
    }

    pub fn emit_const(&mut self, c: Value) -> ConstAddr {
        self.consts.push(c);
        (self.consts.len() - 1) as ConstAddr
    }

    pub fn patch_instr(&mut self, addr: CodeAddr, instr: Instr) {
        self.code[addr as usize] = instr;
    }

    pub fn link(mut self, program: &mut Program) {
        // Emit constants
        let const_offset = program.next_const_addr();
        for c in self.consts.into_iter() {
            program.emit_const(c);
        }


        // Emit instructions
        let code_offset = program.next_instr_addr();
        for instr in self.code.into_iter() {
            // Patch jumps to account for procedure offset
            let instr = match instr {
                Instr::LoadConst(addr) => Instr::LoadConst(const_offset + addr),
                Instr::Jump(addr) => Instr::Jump(code_offset + addr),
                Instr::JumpIf(addr) => Instr::JumpIf(code_offset + addr),
                instr => instr,
            };
            program.emit_instr(instr);
        }

        program.emit_instr(Instr::Return(0));
    }
}
