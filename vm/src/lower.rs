use super::*;

fn litr_to_value(litr: &mir::Literal) -> Value {
    match litr {
        mir::Literal::Nat(x) => Value::Int(*x as i64),
        mir::Literal::Char(c) => Value::Char(*c),
        mir::Literal::Bool(x) => Value::Bool(*x),
    }
}

impl Program {
    // [.., T] -> [.., Bool]
    pub fn compile_matcher(&mut self, mir: &MirContext, binding: &MirNode<mir::Binding>, stack: &[()]) {
        match &binding.pat {
            mir::Pat::Wildcard => {
                self.push(Instr::Pop(1));
                self.push(Instr::bool(true));
            },
            mir::Pat::Literal(expr) => {
                self.compile_expr(mir, expr, stack);
                self.push(match mir.reprs.get(*binding.meta()) {
                    repr::Repr::Prim(repr::Prim::Bool) => Instr::EqBool,
                    repr::Repr::Prim(repr::Prim::Nat) => Instr::EqInt,
                    repr::Repr::Prim(repr::Prim::Int) => Instr::EqInt,
                    r => todo!("{:?}", r),
                });
            },
            mir::Pat::Tuple(fields) => {
                self.debug("Tuple!");
                self.push(Instr::bool(true));
                for (i, field) in fields.iter().enumerate() {
                    self.debug("Field!");

                    self.push(Instr::Dup(1));
                    self.push(Instr::Field(i));
                    self.compile_matcher(mir, field, stack);

                    self.push(Instr::AndBool);
                }
                self.debug("End tuple!");
                self.push(Instr::Replace);
            },
        }
    }

    // [..] -> [.., T]
    pub fn compile_expr(&mut self, mir: &MirContext, expr: &mir::Expr, stack: &[()]) {
        match &*expr {
            mir::Expr::Literal(litr) => { self.push(Instr::Imm(litr_to_value(litr))); },
            mir::Expr::Intrinsic(intrinsic, args) => {
                for arg in args {
                    self.compile_expr(mir, arg, stack);
                }
                self.push(match intrinsic {
                    mir::Intrinsic::MakeList(_, n) => Instr::MakeList(*n),
                    mir::Intrinsic::NotBool => Instr::NotBool,
                    mir::Intrinsic::AddNat => Instr::AddInt,
                    mir::Intrinsic::AddInt => Instr::AddInt,
                    mir::Intrinsic::SubNat => Instr::SubInt,
                    mir::Intrinsic::SubInt => Instr::SubInt,
                });
            },
            mir::Expr::MakeTuple(fields) => {
                for field in fields {
                    self.compile_expr(mir, field, stack);
                }
                self.push(Instr::MakeList(fields.len()));
            },
            mir::Expr::Match(pred, arms) => {
                self.debug("Match!");
                self.compile_expr(mir, pred, stack);

                let mut fixups = Vec::new();

                for (binding, body) in arms {
                    self.debug("Arm!");
                    self.push(Instr::Dup(0));
                    self.compile_matcher(mir, binding, stack);

                    self.push(Instr::IfNot);
                    let matcher_failed = self.push(Instr::Jump(0)); // Fixed by #0

                    self.push(Instr::Pop(1)); // Pop predicate because pattern match was successful
                    self.compile_expr(mir, body, stack);
                    fixups.push(self.push(Instr::Jump(0))); // Fixed by #1

                    self.fixup(matcher_failed, self.next_addr(), Instr::Jump); // Fixes #0
                }

                let end_match = self.next_addr();

                for end_arm in fixups {
                    self.fixup(end_arm, end_match, Instr::Jump); // Fixes #1
                }

                self.debug("End match!");
            },
            x => todo!("{:?}", x),
        }
    }

    pub fn compile_proc(&mut self, mir: &MirContext, proc: ProcId) -> Addr {
        self.debug(format!("Proc {:?}", proc));
        let addr = self.next_addr();
        self.compile_expr(mir, &mir.procs.get(proc).body, &mut Vec::new());
        self.push(Instr::Ret);
        addr
    }

    pub fn from_mir(mir: &MirContext) -> Self {
        let mut this = Self::default();

        this.entry = this.compile_proc(mir, mir.entry.expect("No entry point"));

        this
    }
}
