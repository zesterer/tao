use super::*;

#[derive(Clone, Debug)]
pub enum Instr {
    Error(&'static str),
    Nop,
    Break,
    Imm(Value),
    Pop(usize),
    Replace,
    Ret,
    MakeList(usize), // T * N => [T]
    Dup(usize), // Duplicate value in position (len - 1 - N) in the scope
    Jump(isize),
    IfNot,
    Field(usize), // Nth field of list/tuple
    NotBool, // Bool -> Bool
    AddInt, // Int -> Int -> Int
    SubInt, // Int -> Int -> Int
    EqInt, // Int -> Int -> Bool
    EqBool, // Bool -> Bool -> Bool
    AndBool, // Bool -> Bool -> Bool
}

impl Instr {
    pub fn bool(x: bool) -> Self {
        Self::Imm(Value::Bool(x))
    }
}

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
pub struct Addr(pub usize);

impl Addr {
    pub fn incr(self) -> Self { Self(self.0 + 1) }
    pub fn jump(self, rel: isize) -> Self { Self((self.0 as isize + rel) as usize) }
    pub fn jump_to(self, other: Self) -> isize { other.0 as isize - self.0 as isize }
}

#[derive(Default, Debug)]
pub struct Program {
    instrs: Vec<Instr>,
    pub entry: Addr,
    debug: Vec<(Addr, String)>,
}

impl Program {
    pub fn debug(&mut self, msg: impl ToString) {
        self.debug.push((self.next_addr(), msg.to_string()));
    }

    pub fn next_addr(&self) -> Addr { Addr(self.instrs.len()) }

    pub fn instr(&self, ip: Addr) -> Instr {
        self.instrs
            .get(ip.0)
            .cloned()
            .unwrap_or(Instr::Error("out of bounds instruction"))
    }

    pub fn push(&mut self, instr: Instr) -> Addr {
        let addr = self.next_addr();
        self.instrs.push(instr);
        addr
    }

    pub fn fixup(&mut self, addr: Addr, tgt: Addr, make_instr: impl FnOnce(isize) -> Instr) {
        self.instrs[addr.0] = make_instr(addr.jump_to(tgt));
    }

    pub fn display(&self) {
        let mut debug = self.debug.iter().peekable();
        for addr in (0..self.instrs.len()).map(Addr) {
            while debug.peek().map_or(false, |(a, _)| *a == addr) {
                println!(" ...  | <--------- {}", debug.next().unwrap().1);
            }

            let instr = self.instr(addr);

            let stack_diff = match instr {
                Instr::Error(_) | Instr::Nop | Instr::Break => 0,
                Instr::Imm(_) => 1,
                Instr::Pop(n) => -(n as isize),
                Instr::Replace => -1,
                Instr::Ret => 0,
                Instr::MakeList(n) => -(n as isize) + 1,
                Instr::Dup(x) => 1,
                Instr::Jump(x) => 0,
                Instr::IfNot => -1,
                Instr::Field(i) => 0,
                Instr::NotBool => 0,
                Instr::AddInt
                | Instr::SubInt
                | Instr::EqInt
                | Instr::EqBool
                | Instr::AndBool => -1,
            };

            let instr_display = match instr {
                Instr::Error(msg) => format!("error \"{}\"", msg),
                Instr::Nop => format!("nop"),
                Instr::Break => format!("break"),
                Instr::Imm(x) => format!("imm `{}`", x),
                Instr::Pop(n) => format!("pop {}", n),
                Instr::Replace => format!("replace"),
                Instr::Ret => format!("ret"),
                Instr::MakeList(n) => format!("list.make {}", n),
                Instr::Dup(x) => format!("dup +{}", x),
                Instr::Jump(x) => format!("jump {:+} (0x{:03X})", x, addr.jump(x).0),
                Instr::IfNot => format!("if_not"),
                Instr::Field(i) => format!("field #{}", i),
                Instr::NotBool => format!("bool.not"),
                Instr::AddInt => format!("int.add"),
                Instr::SubInt => format!("int.sub"),
                Instr::EqInt => format!("int.eq"),
                Instr::EqBool => format!("bool.eq"),
                Instr::AndBool => format!("bool.and"),
            };

            println!("0x{:03X} | {:+1} | {}", addr.0, stack_diff, instr_display);
        }
    }
}
