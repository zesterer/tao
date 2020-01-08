use internment::LocalIntern;
use crate::{
    parse::{Expr, Literal, UnaryOp, BinaryOp},
    node::Node,
    error::Error,
    src::SrcRegion,
    eval::Value,
};

pub type Addr = usize;

#[derive(Clone, Debug)]
pub enum Instr {
    Value(Value), // Push a value onto the stack
    UnaryOp(UnaryOp), // Apply a unary operation to the latest stack item
    BinaryOp(BinaryOp), // Apply a binary operation to the 2 latest stack items
    Branch(Addr, Addr), // Call one of two procedures based on the truth of the last stack item
    Return(usize), // Return from the current procedure, popping n locals
    Func(Addr, Vec<usize>), // Create a function from the given address, list of stack value offsets, and parameter
    Apply, // Apply the last stack item to the function before it on the stack
    MakeList(usize), // Make a list of the last n stack items (in reverse order)
    Local(usize), // Place the local with the given offset onto the value stack
}

#[derive(Default)]
struct Procedure {
    instrs: Vec<Instr>,
    regions: Vec<SrcRegion>,
}

impl Procedure {
    pub fn push(&mut self, instr: Instr, region: SrcRegion) {
        self.instrs.push(instr);
        self.regions.push(region);
    }
}

#[derive(Default, Debug)]
pub struct Program {
    instrs: Vec<Instr>,
    regions: Vec<SrcRegion>,
    entry: Addr,
}

impl Program {
    pub fn instr(&self, addr: Addr) -> Option<Instr> {
        self.instrs.get(addr).cloned()
    }

    pub fn region(&self, addr: Addr) -> Option<SrcRegion> {
        self.regions.get(addr).copied()
    }

    pub fn entry(&self) -> Addr {
        self.entry
    }

    fn insert_procedure(&mut self, mut proc: Procedure) -> Addr {
        let addr = self.instrs.len();
        self.instrs.append(&mut proc.instrs);
        self.regions.append(&mut proc.regions);
        addr
    }

    fn compile_procedure(
        &mut self,
        expr: &Node<Expr>,
        locals: &Vec<Node<LocalIntern<String>>>,
        pop_n: usize,
    ) -> Result<Addr, Error> {
        let mut proc = Procedure::default();
        self.compile_expr(expr, &mut proc, locals)?;
        proc.push(Instr::Return(pop_n), SrcRegion::none());
        Ok(self.insert_procedure(proc))
    }

    fn compile_expr(
        &mut self,
        expr: &Node<Expr>,
        proc: &mut Procedure,
        locals: &Vec<Node<LocalIntern<String>>>,
    ) -> Result<(), Error> {
        let offset_of = |ident, region| Ok(locals.len().saturating_sub(1) - locals
            .iter()
            .enumerate()
            .find(|(_, local)| **local == ident)
            .ok_or(Error::no_such_binding(ident.to_string(), region))?
            .0);

        match &**expr {
            Expr::Literal(x) => proc.push(Instr::Value(match x {
                Literal::Null => Value::Null,
                Literal::Number(x) => Value::Number(*x),
                Literal::String(x) => Value::String(x.clone()),
                Literal::Boolean(x) => Value::Boolean(*x),
            }), expr.region()),
            Expr::Unary(op, a) => {
                self.compile_expr(a, proc, locals)?;
                proc.push(Instr::UnaryOp(**op), expr.region());
            },
            Expr::Binary(op, a, b) => {
                self.compile_expr(a, proc, locals)?;
                self.compile_expr(b, proc, locals)?;
                proc.push(Instr::BinaryOp(**op), expr.region());
            },
            Expr::Branch(p, t, f) => {
                let t = self.compile_procedure(t, locals, 0)?;
                let f = self.compile_procedure(f, locals, 0)?;
                self.compile_expr(p, proc, locals)?;
                proc.push(Instr::Branch(t, f), expr.region());
            },
            Expr::Func(name, body) => {
                let mut env_locals = expr.env_locals();

                // Find environment from current locals
                let mut env_offsets = Vec::new();
                for env_local in &env_locals {
                    env_offsets.push(offset_of(**env_local, env_local.region())?);
                }

                env_locals.insert(0, (*name).clone());

                let body = self.compile_procedure(body, &env_locals, env_locals.len())?;
                proc.push(Instr::Func(body, env_offsets), expr.region());
            },
            Expr::Apply(f, arg) => {
                self.compile_expr(arg, proc, locals)?;
                self.compile_expr(f, proc, locals)?;
                proc.push(Instr::Apply, expr.region());
            },
            Expr::List(items) => {
                for item in items.iter().rev() {
                    self.compile_expr(item, proc, locals)?;
                }
                proc.push(Instr::MakeList(items.len()), expr.region());
            },
            Expr::Ident(ident) => proc.push(Instr::Local(offset_of(**ident, ident.region())?), expr.region()),
        }

        Ok(())
    }

    pub fn compile(expr: &Node<Expr>) -> Result<Self, Error> {
        let mut this = Self::default();
        let entry = this.compile_procedure(expr, &mut Vec::new(), 0)?;
        this.entry = entry;
        Ok(this)
    }
}

impl Expr {
    fn build_env_locals(
        &self,
        shadowed: &mut Vec<LocalIntern<String>>,
        env: &mut Vec<Node<LocalIntern<String>>>,
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
                    env.push((*ident).clone());
                }
            },
            _ => {},
        }
    }

    fn env_locals(&self) -> Vec<Node<LocalIntern<String>>> {
        let mut env = Vec::new();
        self.build_env_locals(&mut Vec::new(), &mut env);
        env
    }
}
