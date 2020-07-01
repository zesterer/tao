use std::fmt;
use crate::mir;
use super::Value;

pub type CodeAddr = u32;
pub type ConstAddr = u32;

#[derive(Copy, Clone)]
#[repr(u8)]
pub enum Instr {
    Nop = 0,

    // Duplicate the last item on the stack
    Dup,
    // Pop the last item on the stack
    Pop,

    /// Push the given integer, converted to a `Value::Number`
    Integer(i32),
    /// Push the given float, converted to a `Value::Number`
    // TODO: Not precise enough
    Float(f32),
    /// Push `true`
    True,
    /// Push `false`
    False,
    /// Push the given character
    Char(char),

    // Push a function that points to the given address with the environment of the last N stack items
    MakeFunc(u16, u32),
    // Apply the argument one below the top of the stack to the function at the top of the stack
    ApplyFunc,
    /// Push a list made from the last N items on the stack (reversed)
    MakeList(u32),
    // Index the list at the top of the stack
    IndexList(u32),
    // Set the index of the list one below the top of the stack fo the value at the top
    SetList(u32),
    // Find the tail of the list at the top of the stack, with the first N items removed
    TailList(u32),
    // If the list has the given length, push true to the stack
    LenEqList(u32),
    // If the list has the given length or more, push true to the stack
    LenMoreEqList(u32),

    NegNum,
    AddNum,
    SubNum,
    MulNum,
    DivNum,
    RemNum,
    EqNum,
    MoreNum,
    LessNum,
    MoreEqNum,
    LessEqNum,

    NotBool,
    EqBool,
    AndBool,
    OrBool,

    EqChar,

    JoinList,

    /// Load a constant from the program constants
    LoadConst(ConstAddr),
    /// Push a copy of the local with the given offset on to the stack
    LoadLocal(u32),
    /// Push the top value in the stack on to the local stack
    PushLocal,
    /// Pop the last local from the local stack
    PopLocal,

    /// Jump to the address
    Jump(u32),
    /// Jump to the address if the last value in the stack is `false`
    JumpIfNot(u32),
    // Call the given address
    Call(CodeAddr),
    /// Pop the top value in the stack, consider this a return value
    /// Then, pop N additional items and return to the last pushed
    /// address, and push the return value
    Return(u32),

    /// Intrinsics
    Intrinsic(mir::Intrinsic),
}

impl fmt::Debug for Instr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Instr::Nop => write!(f, "nop"),
            Instr::Dup => write!(f, "dup"),
            Instr::Pop => write!(f, "pop"),
            Instr::Integer(x) => write!(f, "int {}", x),
            Instr::Float(x) => write!(f, "float {}", x),
            Instr::True => write!(f, "true"),
            Instr::False => write!(f, "false"),
            Instr::Char(c) => write!(f, "char '{}'", c),
            Instr::MakeFunc(n, addr) => write!(f, "func.make {} {:#X}", n, addr),
            Instr::ApplyFunc => write!(f, "func.apply"),
            Instr::MakeList(n) => write!(f, "list.make {}", n),
            Instr::IndexList(x) => write!(f, "list.index {}", x),
            Instr::SetList(x) => write!(f, "list.set {}", x),
            Instr::TailList(x) => write!(f, "list.tail {}", x),
            Instr::LenEqList(n) => write!(f, "list.len_eq {}", n),
            Instr::LenMoreEqList(n) => write!(f, "list.len_more_eq {}", n),
            Instr::NegNum => write!(f, "num.neg"),
            Instr::AddNum => write!(f, "num.add"),
            Instr::SubNum => write!(f, "num.sub"),
            Instr::MulNum => write!(f, "num.mul"),
            Instr::DivNum => write!(f, "num.div"),
            Instr::RemNum => write!(f, "num.rem"),
            Instr::EqNum => write!(f, "num.eq"),
            Instr::MoreNum => write!(f, "num.more"),
            Instr::LessNum => write!(f, "num.less"),
            Instr::MoreEqNum => write!(f, "num.more_eq"),
            Instr::LessEqNum => write!(f, "num.less_eq"),
            Instr::NotBool => write!(f, "bool.not"),
            Instr::EqBool => write!(f, "bool.eq"),
            Instr::AndBool => write!(f, "bool.and"),
            Instr::OrBool => write!(f, "bool.or"),
            Instr::EqChar => write!(f, "char.eq"),
            Instr::JoinList => write!(f, "list.join"),
            Instr::LoadConst(addr) => write!(f, "const {:#X}", addr),
            Instr::LoadLocal(offset) => write!(f, "load_local {}", offset),
            Instr::PushLocal => write!(f, "push_local"),
            Instr::PopLocal => write!(f, "pop_local"),
            Instr::Jump(addr) => write!(f, "jump {:#X}", addr),
            Instr::JumpIfNot(addr) => write!(f, "jump_if_not {:#X}", addr),
            Instr::Call(addr) => write!(f, "call {:#X}", addr),
            Instr::Return(n) => write!(f, "return {}", n),
            Instr::Intrinsic(i) => write!(f, "intrinsic {:?}", i),
        }
    }
}

#[test]
fn size() {
    assert!(std::mem::size_of::<Instr>() <= 8);
}

#[derive(Default)]
pub struct Program {
    code: Vec<Instr>,
    consts: Vec<Value>,
    debug: Vec<(CodeAddr, String)>,
    entry: CodeAddr,
    is_pure: bool,
}

impl Program {
    pub fn entry(&self) -> CodeAddr {
        self.entry
    }

    pub fn is_pure(&self) -> bool {
        self.is_pure
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

    pub fn patch_instr(&mut self, addr: CodeAddr, instr: Instr) {
        debug_assert!(matches!(self.code[addr as usize], Instr::Nop));
        self.code[addr as usize] = instr;
    }

    pub fn set_entry(&mut self, addr: CodeAddr) {
        self.entry = addr;
    }

    pub fn set_pure(&mut self, is_pure: bool) {
        self.is_pure = is_pure;
    }

    pub fn next_instr_addr(&self) -> CodeAddr {
        self.code.len() as CodeAddr
    }

    pub fn next_const_addr(&self) -> ConstAddr {
        self.consts.len() as ConstAddr
    }

    pub fn emit_debug(&mut self, addr: CodeAddr, s: String) {
        self.debug.push((addr, s));
    }
}

impl fmt::Debug for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "-- Code --")?;
        for (addr, instr) in self.code.iter().enumerate() {
            let debug = if let Some((_, s)) = self.debug.iter().find(|(a, _)| *a == addr as u32) {
                &*s
            } else {
                ""
            };
            writeln!(f, "{:>#5X} | {:?} {}", addr, instr, debug)?;
        }
        writeln!(f, "-- Data --")?;
        for (addr, val) in self.consts.iter().enumerate() {
            writeln!(f, "{:>#5X} | {}", addr, val)?;
        }
        // writeln!(f, "-- Debug --")?;
        // for (addr, s) in self.debug.iter() {
        //     writeln!(f, "{:>#5X} | {}", addr, s)?;
        // }
        writeln!(f, "Entry: {:#X}", self.entry())?;
        Ok(())
    }
}
