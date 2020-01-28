use std::{
    fmt,
    collections::HashMap,
};
use internment::LocalIntern;
use crate::{
    parse::{Literal, UnaryOp, BinaryOp, Expr, Decl, Module},
    node::Node,
    error::Error,
    src::SrcRegion,
    eval::Value,
    hir::TypeInfo,
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
    Call(Addr), // Call a procedure (these addresses will be incorrect until linking has occured)
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
    entries: Vec<Addr>,
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
        expr: &Node<Expr, Node<TypeInfo>>,
        globals: &[LocalIntern<String>],
        locals: &Vec<Node<LocalIntern<String>>>,
        pop_n: usize,
    ) -> Result<Addr, Error> {
        let mut proc = Procedure::default();
        self.compile_expr(expr, &mut proc, globals, locals)?;
        proc.push(Instr::Return(pop_n), SrcRegion::none());
        let addr = self.insert_procedure(proc);
        self.entries.push(addr);
        Ok(addr)
    }

    fn compile_expr(
        &mut self,
        expr: &Node<Expr, Node<TypeInfo>>,
        proc: &mut Procedure,
        globals: &[LocalIntern<String>],
        locals: &Vec<Node<LocalIntern<String>>>,
    ) -> Result<(), Error> {
        let offset_of = |ident, region| Ok(locals.len().saturating_sub(1) - locals
            .iter()
            .enumerate()
            .find(|(_, local)| **local == ident)
            .ok_or(Error::no_such_binding(ident.to_string(), region))?
            .0);

        let globals_index = |ident| globals
            .iter()
            .enumerate()
            .find(|(_, global)| **global == ident)
            .map(|(idx, _)| idx);

        match &**expr {
            Expr::Literal(x) => proc.push(Instr::Value(match x {
                Literal::Null => Value::Null,
                Literal::Number(x) => Value::Number(*x),
                Literal::String(x) => Value::String(x.clone()),
                Literal::Boolean(x) => Value::Boolean(*x),
            }), expr.region()),
            Expr::Unary(op, a) => {
                self.compile_expr(a, proc, globals, locals)?;
                proc.push(Instr::UnaryOp(**op), expr.region());
            },
            Expr::Binary(op, a, b) => {
                self.compile_expr(a, proc, globals, locals)?;
                self.compile_expr(b, proc, globals, locals)?;
                proc.push(Instr::BinaryOp(**op), expr.region());
            },
            Expr::Branch(p, t, f) => {
                let t = self.compile_procedure(t, globals, locals, 0)?;
                let f = self.compile_procedure(f, globals, locals, 0)?;
                self.compile_expr(p, proc, globals, locals)?;
                proc.push(Instr::Branch(t, f), expr.region());
            },
            Expr::Func(name, body) => {
                let mut env_locals = expr.env_locals(globals);

                // Find environment from current locals
                let mut env_offsets = Vec::new();
                for env_local in &env_locals {
                    match offset_of(**env_local, env_local.region()) {
                        Ok(offset) => env_offsets.push(offset),
                        Err(err) => {
                            globals_index(**env_local)
                                .ok_or(err)?;
                        },
                    }
                }

                env_locals.insert(0, Node::new(**name, name.region(), ()));

                let body = self.compile_procedure(body, globals, &env_locals, env_locals.len())?;
                proc.push(Instr::Func(body, env_offsets), expr.region());
            },
            Expr::Apply(f, arg) => {
                self.compile_expr(arg, proc, globals, locals)?;
                self.compile_expr(f, proc, globals, locals)?;
                proc.push(Instr::Apply, expr.region());
            },
            Expr::List(items) => {
                for item in items.iter().rev() {
                    self.compile_expr(item, proc, globals, locals)?;
                }
                proc.push(Instr::MakeList(items.len()), expr.region());
            },
            Expr::Tuple(items) => { // Tuples are compiled to lists
                for item in items.iter().rev() {
                    self.compile_expr(item, proc, globals, locals)?;
                }
                proc.push(Instr::MakeList(items.len()), expr.region());
            },
            Expr::Ident(ident) => {
                let addr = match offset_of(**ident, ident.region()) {
                    Ok(addr) => proc.push(Instr::Local(addr), expr.region()),
                    Err(err) => {
                        let idx = globals_index(*ident.inner()).ok_or(err)?;
                        proc.push(Instr::Call(idx), expr.region()); // This will get changed during linking
                    },
                };
            },
        }

        Ok(())
    }

    pub fn from_expr(expr: &Node<Expr, Node<TypeInfo>>) -> Result<Self, Error> {
        let mut this = Self::default();
        let entry = this.compile_procedure(expr, &[], &mut Vec::new(), 0)?;
        this.entry = entry;
        Ok(this)
    }

    pub fn from_module(module: &Module) -> Result<Self, Error> {
        let mut this = Self::default();

        // Gather a list of globals
        let globals = module.decls
            .iter()
            .filter_map(|decl| match decl.inner() {
                Decl::Value(name, _, _) => Some(*name.inner())
            })
            .collect::<Vec<_>>();

        // Compile globals
        let mut decl_values = HashMap::new();
        for decl in &module.decls {
            match decl.inner() {
                Decl::Value(name, _, body) => {
                    decl_values.insert(
                        *name.inner(),
                        this.compile_procedure(body, &globals, &mut Vec::new(), 0)?,
                    );
                },
            }
        }

        // Link globals
        for instr in &mut this.instrs {
            match instr {
                Instr::Call(idx) => *idx = *decl_values.get(&globals[*idx]).unwrap(),
                _ => {},
            }
        }

        // Set entry point to main
        this.entry = *decl_values
            .get(&LocalIntern::new("main".to_string()))
            .ok_or_else(|| Error::no_main())?;

        Ok(this)
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "-- Program --")?;
        for (i, instr) in self.instrs.iter().enumerate() {
            if self.entries.contains(&i) {
                writeln!(f, "-- Entry --")?;
            }
            writeln!(f, "    {}: {:?}", i, instr)?;
        }
        Ok(())
    }
}

impl Expr {
    fn build_env_locals(
        &self,
        globals: &[LocalIntern<String>],
        shadowed: &Vec<LocalIntern<String>>,
        env: &mut Vec<Node<LocalIntern<String>>>,
    ) {
        match self {
            Expr::Unary(_, a) => a.build_env_locals(globals, shadowed, env),
            Expr::Binary(_, a, b) => {
                a.build_env_locals(globals, shadowed, env);
                b.build_env_locals(globals, shadowed, env);
            },
            Expr::Branch(p, t, f) => {
                p.build_env_locals(globals, shadowed, env);
                t.build_env_locals(globals, shadowed, env);
                f.build_env_locals(globals, shadowed, env);
            },
            Expr::Func(name, body) => {
                let mut shadowed = shadowed.clone();
                shadowed.push(**name);
                body.build_env_locals(globals, &shadowed, env);
            },
            Expr::Apply(f, arg) => {
                f.build_env_locals(globals, shadowed, env);
                arg.build_env_locals(globals, shadowed, env);
            },
            Expr::List(items) => for item in items {
                item.build_env_locals(globals, shadowed, env);
            },
            Expr::Tuple(items) => for item in items {
                item.build_env_locals(globals, shadowed, env);
            },
            Expr::Ident(ident) => {
                if !shadowed.contains(&*ident) && !globals.contains(&*ident) {
                    env.push((*ident).clone());
                }
            },
            _ => {},
        }
    }

    fn env_locals(&self, globals: &[LocalIntern<String>]) -> Vec<Node<LocalIntern<String>>> {
        let mut env = Vec::new();
        self.build_env_locals(globals, &Vec::new(), &mut env);
        env
    }
}
