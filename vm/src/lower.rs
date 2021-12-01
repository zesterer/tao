use super::*;

fn const_to_value(constant: &mir::Const) -> Value {
    match constant {
        mir::Const::Nat(x) => Value::Int(*x as i64),
        mir::Const::Int(x) => Value::Int(*x),
        mir::Const::Char(c) => Value::Char(*c),
        mir::Const::Bool(x) => Value::Bool(*x),
        mir::Const::Str(s) => Value::List(s
            .chars()
            .map(Value::Char)
            .collect()),
        mir::Const::Tuple(fields) => Value::List(fields
            .iter()
            .map(const_to_value)
            .collect()),
        mir::Const::List(items) => Value::List(items
            .iter()
            .map(const_to_value)
            .collect()),
        mir::Const::Sum(variant, inner) => Value::Sum(*variant, Box::new(const_to_value(inner))),
    }
}

impl Program {
    // [.., T] => [..]
    pub fn compile_extractor(&mut self, mir: &MirContext, binding: &MirNode<mir::Binding>) {
        if let Some(_) = binding.name {
            self.push(Instr::Dup);
            self.push(Instr::PushLocal);
        }

        match &binding.pat {
            mir::Pat::Wildcard => {},
            mir::Pat::Const(_) => {},
            mir::Pat::Single(inner) => {
                self.push(Instr::Dup);
                self.compile_extractor(mir, inner);
            },
            mir::Pat::Add(lhs, rhs) => {
                self.push(Instr::Dup);
                self.push(Instr::Imm(Value::Int(*rhs as i64)));
                self.push(Instr::SubInt);
                self.compile_extractor(mir, lhs);
            },
            mir::Pat::Tuple(items) | mir::Pat::ListExact(items) => {
                for (i, item) in items.iter().enumerate() {
                    if item.binds() {
                        self.push(Instr::Dup);
                        self.push(Instr::IndexList(i));

                        self.compile_extractor(mir, item);
                    }
                }
            },
            mir::Pat::ListFront(items, tail) => {
                if let Some(tail) = tail.as_ref() {
                    self.push(Instr::Dup); // Used for tail later
                }

                for (i, item) in items.iter().enumerate() {
                    if item.binds() {
                        self.push(Instr::Dup);
                        self.push(Instr::IndexList(i));

                        self.compile_extractor(mir, item);
                    }
                }

                if let Some(tail) = tail.as_ref() {
                    self.push(Instr::SkipList(items.len()));
                    self.compile_extractor(mir, tail);
                }
            },
            mir::Pat::Variant(variant, inner) => {
                self.push(Instr::Dup);
                self.push(Instr::IndexSum(*variant));
                self.compile_extractor(mir, inner);
            },
        }

        self.push(Instr::Pop(1));
    }

    // [.., [T]] -> [.., Bool]
    pub fn compile_item_matcher<'a>(&mut self, items: impl IntoIterator<Item = &'a MirNode<mir::Binding>>, is_list: bool, fail_fixup: impl IntoIterator<Item = Addr>) {
        let mut fixups = fail_fixup.into_iter().collect::<Vec<_>>();

        for (i, item) in items.into_iter().enumerate() {
            self.push(Instr::Dup);
            if is_list {
                self.push(Instr::IndexList(i));
            }
            self.compile_matcher(item);

            self.push(Instr::IfNot);
            fixups.push(self.push(Instr::Jump(0))); // Fixed by #2
        }

        self.push(Instr::Pop(1));
        self.push(Instr::bool(true));
        let success = self.push(Instr::Jump(0)); // Fixed by #3

        for fixup in fixups {
            self.fixup(fixup, self.next_addr(), Instr::Jump); // Fixes #2
        }

        self.push(Instr::Pop(1));
        self.push(Instr::bool(false));

        self.fixup(success, self.next_addr(), Instr::Jump); // Fixes #3
    }

    // [.., T] -> [.., Bool]
    pub fn compile_matcher(&mut self, binding: &MirNode<mir::Binding>) {
        match &binding.pat {
            mir::Pat::Wildcard => {
                self.push(Instr::Pop(1));
                self.push(Instr::bool(true));
            },
            mir::Pat::Const(constant) => match constant {
                mir::Const::Bool(true) => {},
                mir::Const::Bool(false) => {
                    self.push(Instr::NotBool);
                },
                constant => {
                    self.push(Instr::Imm(const_to_value(constant)));
                    self.push(match &binding.meta().1 {
                        repr::Repr::Prim(repr::Prim::Bool) => Instr::EqBool,
                        repr::Repr::Prim(repr::Prim::Nat) => Instr::EqInt,
                        repr::Repr::Prim(repr::Prim::Int) => Instr::EqInt,
                        r => todo!("{:?}", r),
                    });
                },
            },
            mir::Pat::Single(inner) => self.compile_matcher(inner),
            mir::Pat::Add(lhs, rhs) => {
                self.push(Instr::Dup);
                self.push(Instr::Imm(Value::Int(*rhs as i64)));
                self.push(Instr::MoreEqInt);
                self.push(Instr::IfNot);
                let fail_fixup = self.push(Instr::Jump(0)); // Fixed by #2
                self.compile_item_matcher(Some(lhs), false, Some(fail_fixup));
            },
            mir::Pat::Tuple(items) => {
                self.compile_item_matcher(items, true, None);
            },
            mir::Pat::ListExact(items) => if items.len() == 0 {
                self.push(Instr::LenList);
                self.push(Instr::Imm(Value::Int(items.len() as i64)));
                self.push(Instr::EqInt);
            } else {
                self.push(Instr::Dup);
                self.push(Instr::LenList);
                self.push(Instr::Imm(Value::Int(items.len() as i64)));
                self.push(Instr::EqInt);
                self.push(Instr::IfNot);
                let fail_fixup = Some(self.push(Instr::Jump(0))); // Fixed by #2

                self.compile_item_matcher(items, true, fail_fixup);
            },
            mir::Pat::ListFront(items, tail) => {
                self.push(Instr::Dup);
                self.push(Instr::LenList);
                self.push(Instr::Imm(Value::Int(items.len() as i64)));
                self.push(Instr::MoreEqInt);
                self.push(Instr::IfNot);
                let mut fail_fixup = vec![self.push(Instr::Jump(0))]; // Fixed by #2

                if let Some(tail) = tail.as_ref() {
                    self.push(Instr::Dup);
                    self.push(Instr::SkipList(items.len()));
                    self.compile_matcher(tail);
                    self.push(Instr::IfNot);
                    fail_fixup.push(self.push(Instr::Jump(0))); // Fixed by #2
                }

                self.compile_item_matcher(items, true, fail_fixup);
            },
            mir::Pat::Variant(variant, inner) => {
                self.push(Instr::Dup);
                self.push(Instr::VariantSum);
                self.push(Instr::Imm(Value::Int(*variant as i64)));
                self.push(Instr::EqInt);
                self.push(Instr::IfNot);
                let fail_fixup = self.push(Instr::Jump(0)); // Fixed by #2
                self.push(Instr::IndexSum(*variant));
                self.compile_item_matcher(Some(inner), false, Some(fail_fixup));
            },
        }
    }

    // [..] -> [.., T]
    pub fn compile_expr(
        &mut self,
        mir: &MirContext,
        expr: &mir::Expr,
        stack: &mut Vec<Ident>,
        proc_fixups: &mut Vec<(ProcId, Addr)>,
    ) {
        match &*expr {
            mir::Expr::Const(constant) => { self.push(Instr::Imm(const_to_value(constant))); },
            mir::Expr::Local(local) => {
                let idx = stack
                    .iter()
                    .rev()
                    .enumerate()
                    .find(|(_, name)| *name == local)
                    .unwrap()
                    .0;
                self.push(Instr::GetLocal(idx));
            },
            mir::Expr::Global(global, _) => { proc_fixups.push((*global, self.push(Instr::Call(0)))); }, // Fixed by #4
            mir::Expr::Intrinsic(intrinsic, args) => {
                for arg in args {
                    self.compile_expr(mir, arg, stack, proc_fixups);
                }
                use mir::Intrinsic::*;
                match intrinsic {
                    MakeList(_) => { self.push(Instr::MakeList(args.len())); },
                    NotBool => { self.push(Instr::NotBool); },
                    AddNat | AddInt => { self.push(Instr::AddInt); },
                    SubNat | SubInt => { self.push(Instr::SubInt); },
                    MulNat | MulInt => { self.push(Instr::MulInt); },
                    EqNat | EqInt => { self.push(Instr::EqInt); },
                    EqChar => { self.push(Instr::EqChar); },
                    NotEqNat | NotEqInt => {
                        self.push(Instr::EqInt);
                        self.push(Instr::NotBool);
                    },
                    NotEqChar => {
                        self.push(Instr::EqChar);
                        self.push(Instr::NotBool);
                    },
                    LessNat | LessInt => { self.push(Instr::LessInt); },
                    MoreNat | MoreInt => { self.push(Instr::MoreInt); },
                    LessEqNat | LessEqInt => { self.push(Instr::LessEqInt); },
                    MoreEqNat | MoreEqInt => { self.push(Instr::MoreEqInt); },
                    Join(_) => { self.push(Instr::JoinList); },
                };
            },
            mir::Expr::Tuple(fields) => {
                for field in fields {
                    self.compile_expr(mir, field, stack, proc_fixups);
                }
                self.push(Instr::MakeList(fields.len()));
            },
            mir::Expr::List(items) => {
                for item in items {
                    self.compile_expr(mir, item, stack, proc_fixups);
                }
                self.push(Instr::MakeList(items.len()));
            },
            mir::Expr::Match(pred, arms) => {
                self.compile_expr(mir, pred, stack, proc_fixups);

                let mut end_matches = Vec::new();

                for (i, (binding, body)) in arms.iter().enumerate() {
                    let is_last = i + 1 == arms.len();

                    let mut fail_jumps = Vec::new();

                    // Skip pattern match if pattern is irrefutable or it's the last pattern
                    if binding.is_refutable() && !is_last {
                        self.push(Instr::Dup);
                        self.compile_matcher(binding);

                        self.push(Instr::IfNot);
                        fail_jumps.push(self.push(Instr::Jump(0)));
                    }

                    self.compile_extractor(mir, binding);

                    let old_stack = stack.len();
                    let names = binding.binding_names();
                    stack.extend(names.iter().copied()); // Start scope

                    self.compile_expr(mir, body, stack, proc_fixups);

                    if names.len() > 0 {
                        self.push(Instr::PopLocal(names.len()));
                    }
                    stack.truncate(old_stack); // End scope

                    if !is_last {
                        end_matches.push(self.push(Instr::Jump(0))); // Fixed by #1
                    }

                    for fail_jump in fail_jumps {
                        self.fixup(fail_jump, self.next_addr(), Instr::Jump); // Fixes #0
                    }
                }

                let end_match = self.next_addr();

                for end_arm in end_matches {
                    self.fixup(end_arm, end_match, Instr::Jump); // Fixes #1
                }
            },
            mir::Expr::Func(captures, arg, body) => {
                let old_stack = stack.len();

                let jump_over = self.push(Instr::Jump(0)); // Fixed by #5

                let f_addr = self.next_addr();

                let mut f_stack = Vec::new();
                f_stack.push(*arg); // Will be pushed to locals stack on application
                f_stack.append(&mut captures.clone());

                self.compile_expr(mir, body, &mut f_stack, proc_fixups);
                self.push(Instr::PopLocal(1 + captures.len())); // +1 is for the argument
                self.push(Instr::Ret);

                self.fixup(jump_over, self.next_addr(), Instr::Jump); // Fixes #5

                for &capture in captures.iter() {
                    let idx = stack
                        .iter()
                        .rev()
                        .enumerate()
                        .find(|(_, name)| **name == capture)
                        .unwrap_or_else(|| unreachable!("{}", capture))
                        .0;
                    self.push(Instr::GetLocal(idx));
                }

                self.push(Instr::MakeFunc(self.next_addr().jump_to(f_addr), captures.len()));

                stack.truncate(old_stack);
            },
            mir::Expr::Apply(f, arg) => {
                self.compile_expr(mir, f, stack, proc_fixups);
                self.compile_expr(mir, arg, stack, proc_fixups);
                self.push(Instr::PushLocal);
                self.push(Instr::ApplyFunc);
            },
            mir::Expr::Variant(variant, inner) => {
                self.compile_expr(mir, inner, stack, proc_fixups);
                self.push(Instr::MakeSum(*variant));
            },
            mir::Expr::Access(record, field) => {
                self.compile_expr(mir, record, stack, proc_fixups);
                self.push(Instr::IndexList(*field));
            },
            mir::Expr::AccessVariant(inner, variant) => {
                self.compile_expr(mir, inner, stack, proc_fixups);
                self.push(Instr::IndexSum(*variant));
            },
            mir::Expr::Debug(inner) => {
                self.compile_expr(mir, inner, stack, proc_fixups);
                self.push(Instr::Break);
            },
        }
    }

    pub fn compile_proc(&mut self, mir: &MirContext, proc: ProcId, proc_fixups: &mut Vec<(ProcId, Addr)>) -> Addr {
        self.debug(format!("Proc {:?}", proc));
        let addr = self.next_addr();
        self.compile_expr(mir, &mir.procs.get(proc).unwrap().body, &mut Vec::new(), proc_fixups);
        self.push(Instr::Ret);
        addr
    }

    pub fn from_mir(mir: &MirContext) -> Self {
        let mut this = Self::default();

        let mut procs = HashMap::new();
        let mut proc_fixups = Vec::new();

        for (proc_id, _) in mir.procs.iter() {
            procs.insert(proc_id, this.compile_proc(mir, proc_id, &mut proc_fixups));
        }

        for (proc_id, addr) in proc_fixups {
            this.fixup(addr, procs[&proc_id], Instr::Call); // Fixes #4
        }

        this.entry = procs[&mir.entry.expect("No entry point")];

        this
    }
}
