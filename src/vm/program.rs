use super::Value;

pub type CodeAddr = u32;
pub type ConstAddr = u32;

#[derive(Copy, Clone, Debug)]
#[repr(u32)]
pub enum Instr {
    /// Push the given integer, converted to a `Value::Number`
    Integer(i32),
    /// Push the given float, converted to a `Value::Number`
    // TODO: Not precise enough
    Float(f32),
    /// Push `true`
    True,
    /// Push `false`
    False,

    NegNum,
    NotBool,
    AddNum,
    SubNum,
    MulNum,
    DivNum,
    RemNum,

    /// Load a constant from the program constants
    LoadConst(ConstAddr),
    /// Push the top value in the stack as a local
    PushLocal,

    /// Jump to the address
    Jump(u32),
    /// Jump to the address if the last value in the stack is `true`
    JumpIf(u32),
    /// Pop the top value in the stack, consider this a return value
    /// Then, pop N additional items and return to the last pushed
    /// address, and push the return value
    Return(u32),
}

#[test]
fn size() {
    assert!(std::mem::size_of::<Instr>() <= 8);
}

#[derive(Default)]
pub struct Program {
    code: Vec<Instr>,
    consts: Vec<Value>,
    entry: CodeAddr,
}

impl Program {
    pub fn entry(&self) -> CodeAddr {
        self.entry
    }

    pub unsafe fn fetch_instr_unchecked(&self, addr: CodeAddr) -> Instr {
        debug_assert!((addr as usize) < self.code.len(), "addr = {}, len = {}", addr, self.code.len());
        *self.code.get_unchecked(addr as usize)
    }

    pub fn fetch_const(&self, addr: ConstAddr) -> Value {
        self.consts[addr as usize].clone()
    }

    pub fn emit_const(&mut self, c: Value) -> ConstAddr {
        self.consts.push(c);
        (self.consts.len() - 1) as ConstAddr
    }

    pub fn emit_instr(&mut self, instr: Instr) -> CodeAddr {
        self.code.push(instr);
        (self.code.len() - 1) as CodeAddr
    }

    pub fn set_entry(&mut self, addr: CodeAddr) {
        self.entry = addr;
    }

    pub fn next_instr_addr(&self) -> CodeAddr {
        self.code.len() as CodeAddr
    }

    pub fn next_const_addr(&self) -> ConstAddr {
        self.consts.len() as ConstAddr
    }
}
