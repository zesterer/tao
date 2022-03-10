use super::*;
use std::collections::BTreeMap;

fn litr_to_value(literal: &mir::Literal) -> Option<Value> {
    Some(match literal {
        mir::Literal::Never => return None, // Evaluating a `Never` is UB anyway, so who cares what it generates?
        mir::Literal::Unknown(x) => *x,
        mir::Literal::Nat(x) => Value::Int(*x as i64),
        mir::Literal::Int(x) => Value::Int(*x),
        mir::Literal::Real(x) => Value::Real(*x),
        mir::Literal::Char(c) => Value::Char(*c),
        mir::Literal::Bool(x) => Value::Bool(*x),
        mir::Literal::Tuple(fields) => Value::List(fields
            .iter()
            .map(litr_to_value)
            .collect::<Option<_>>()?),
        mir::Literal::List(items) => Value::List(items
            .iter()
            .map(litr_to_value)
            .collect::<Option<_>>()?),
        mir::Literal::Sum(variant, inner) => Value::Sum(*variant, Box::new(litr_to_value(inner)?)),
        mir::Literal::Union(id, inner) => {
            assert_eq!(*id as usize as u64, *id, "usize too small for this union variant");
            Value::Sum(*id as usize, Box::new(litr_to_value(inner)?))
        },
        mir::Literal::Data(_, inner) => litr_to_value(inner)?,
    })
}

impl Program {
    // [.., T] => [..]
    pub fn compile_extractor(&mut self, mir: &MirContext, binding: &MirNode<mir::Binding>) {
        // The total number of locals this pattern produces
        let mut binds = binding.bindings().len();

        if let Some(_) = binding.name {
            if binds == 1 {
                self.push(Instr::PushLocal);
                return;
            } else {
                self.push(Instr::Dup);
                self.push(Instr::PushLocal);
            }

            binds -= 1; // Account for the local we just generate
        }

        if binds == 0 {
            self.push(Instr::Pop(1));
            return;
        }

        match &binding.pat {
            mir::Pat::Wildcard => {},
            mir::Pat::Literal(_) => {},
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
            mir::Pat::UnionVariant(id, inner) => {
                self.push(Instr::Dup);
                assert_eq!(*id as usize as u64, *id, "usize too small for this union variant");
                self.push(Instr::IndexSum(*id as usize));
                self.compile_extractor(mir, inner);
            },
            mir::Pat::Data(_, inner) => {
                self.push(Instr::Dup);
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
        if !binding.is_refutable() {
            self.push(Instr::Pop(1));
            self.push(Instr::bool(true));
        } else {
            match &binding.pat {
                mir::Pat::Wildcard => unreachable!(), // Caught by refutability check above
                mir::Pat::Literal(litr) => match litr {
                    mir::Literal::Bool(true) => {},
                    mir::Literal::Bool(false) => {
                        self.push(Instr::NotBool);
                    },
                    litr => {
                        if let Some(val) = litr_to_value(litr) {
                            self.push(Instr::Imm(val));
                        }
                        match binding.meta() {
                            repr::Repr::Prim(repr::Prim::Bool) => self.push(Instr::EqBool),
                            repr::Repr::Prim(repr::Prim::Nat) => self.push(Instr::EqInt),
                            repr::Repr::Prim(repr::Prim::Int) => self.push(Instr::EqInt),
                            repr::Repr::Prim(repr::Prim::Char) => self.push(Instr::EqChar),
                            r => todo!("repr = {:?}, litr = {:?}", r, litr),
                        };
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
                mir::Pat::UnionVariant(id, inner) => {
                    self.push(Instr::Dup);
                    self.push(Instr::VariantSum);
                    self.push(Instr::Imm(Value::Int(*id as i64)));
                    self.push(Instr::EqInt);
                    self.push(Instr::IfNot);
                    let fail_fixup = self.push(Instr::Jump(0)); // Fixed by #2
                    assert_eq!(*id as usize as u64, *id, "usize too small for this union variant");
                    self.push(Instr::IndexSum(*id as usize));
                    self.compile_item_matcher(Some(inner), false, Some(fail_fixup));
                },
                mir::Pat::Data(_, inner) => self.compile_matcher(inner),
            }
        }
    }

    // [..] -> [.., T]
    pub fn compile_expr(
        &mut self,
        mir: &MirContext,
        expr: &mir::Expr,
        stack: &mut Vec<mir::Local>,
        proc_fixups: &mut Vec<(ProcId, Addr)>,
    ) {
        match &*expr {
            mir::Expr::Undefined => {}, // Do the minimum possible work, execution is undefined anyway
            mir::Expr::Literal(literal) => {
                if let Some(val) = litr_to_value(literal) {
                    self.push(Instr::Imm(val));
                }
            },
            mir::Expr::Local(local) => {
                let idx = stack
                    .iter()
                    .rev()
                    .enumerate()
                    .find(|(_, name)| *name == local)
                    .unwrap_or_else(|| panic!("Tried to find local ${}, but it was not found. Stack: {:?}", local.0, stack))
                    .0;
                self.push(Instr::GetLocal(idx));
            },
            mir::Expr::Global(global, _) => { proc_fixups.push((*global, self.push(Instr::Call(0)))); }, // Fixed by #4
            mir::Expr::Intrinsic(intrinsic, args) => {
                for arg in args {
                    self.compile_expr(mir, arg, stack, proc_fixups);
                }
                use mir::Intrinsic;
                match intrinsic {
                    Intrinsic::Debug => { self.push(Instr::Break); },
                    Intrinsic::MakeList(_) => { self.push(Instr::MakeList(args.len())); },
                    Intrinsic::NotBool => { self.push(Instr::NotBool); },
                    Intrinsic::NegNat | Intrinsic::NegInt => { self.push(Instr::NegInt); },
                    Intrinsic::NegReal => { self.push(Instr::NegReal); },
                    Intrinsic::AddNat | Intrinsic::AddInt => { self.push(Instr::AddInt); },
                    Intrinsic::SubNat | Intrinsic::SubInt => { self.push(Instr::SubInt); },
                    Intrinsic::MulNat | Intrinsic::MulInt => { self.push(Instr::MulInt); },
                    Intrinsic::EqNat | Intrinsic::EqInt => { self.push(Instr::EqInt); },
                    Intrinsic::EqChar => { self.push(Instr::EqChar); },
                    Intrinsic::NotEqNat | Intrinsic::NotEqInt => {
                        self.push(Instr::EqInt);
                        self.push(Instr::NotBool);
                    },
                    Intrinsic::NotEqChar => {
                        self.push(Instr::EqChar);
                        self.push(Instr::NotBool);
                    },
                    Intrinsic::LessNat | Intrinsic::LessInt => { self.push(Instr::LessInt); },
                    Intrinsic::MoreNat | Intrinsic::MoreInt => { self.push(Instr::MoreInt); },
                    Intrinsic::LessEqNat | Intrinsic::LessEqInt => { self.push(Instr::LessEqInt); },
                    Intrinsic::MoreEqNat | Intrinsic::MoreEqInt => { self.push(Instr::MoreEqInt); },
                    Intrinsic::Join(_) => { self.push(Instr::JoinList); },
                    Intrinsic::AndBool => { self.push(Instr::AndBool); },
                    Intrinsic::Union(ty) => {
                        assert_eq!(*ty as usize as u64, *ty, "usize too small for this union variant");
                        self.push(Instr::MakeSum(*ty as usize));
                    },
                    Intrinsic::Print => { self.push(Instr::Print); },
                    Intrinsic::Input => { self.push(Instr::Input); },
                    Intrinsic::UpdateField(idx) => { self.push(Instr::SetList(*idx)); },
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
            mir::Expr::Func(arg, body) => {
                let captures = body.required_locals(Some(**arg));

                let old_stack = stack.len();

                let jump_over = self.push(Instr::Jump(0)); // Fixed by #5

                let f_addr = self.next_addr();

                let mut f_stack = Vec::new();
                f_stack.push(**arg); // Will be pushed to locals stack on application
                f_stack.append(&mut captures.clone());

                // A function with an undefined body doesn't need to be compiled!
                if !matches!(&**body, mir::Expr::Undefined) {
                    self.compile_expr(mir, body, &mut f_stack, proc_fixups);
                    self.push(Instr::PopLocal(1 + captures.len())); // +1 is for the argument
                    self.push(Instr::Ret);
                }

                self.fixup(jump_over, self.next_addr(), Instr::Jump); // Fixes #5

                for &capture in captures.iter() {
                    let idx = stack
                        .iter()
                        .rev()
                        .enumerate()
                        .find(|(_, name)| **name == capture)
                        .unwrap_or_else(|| unreachable!("${}", capture.0))
                        .0;
                    self.push(Instr::GetLocal(idx));
                }

                self.push(Instr::MakeFunc(self.next_addr().jump_to(f_addr), captures.len()));

                stack.truncate(old_stack);
            },
            mir::Expr::Go(arg, body, init) => {
                self.compile_expr(mir, init, stack, proc_fixups);

                let luup = self.next_addr();
                // Execute body
                self.push(Instr::PushLocal);
                stack.push(**arg);
                self.compile_expr(mir, body, stack, proc_fixups);
                stack.pop();
                self.push(Instr::PopLocal(1));

                const NEXT_VARIANT: usize = 0;
                const DONE_VARIANT: usize = 1;

                // Try unwrapping result
                self.push(Instr::Dup);
                self.push(Instr::VariantSum);
                self.push(Instr::Imm(Value::Int(NEXT_VARIANT as i64)));
                self.push(Instr::EqInt);
                self.push(Instr::IfNot);
                let done = self.push(Instr::Jump(0)); // Fixed by #6
                self.push(Instr::IndexSum(NEXT_VARIANT));
                let again = self.push(Instr::Jump(0));
                self.fixup(again, luup, Instr::Jump);

                self.fixup(done, self.next_addr(), Instr::Jump); // Fixes #6
                self.push(Instr::IndexSum(DONE_VARIANT));
                self.push(Instr::Replace);
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
            mir::Expr::Data(_, inner) => {
                self.compile_expr(mir, inner, stack, proc_fixups);
            },
            mir::Expr::AccessData(inner, _) => {
                self.compile_expr(mir, inner, stack, proc_fixups);
            },
        }
    }

    pub fn compile_proc(&mut self, mir: &MirContext, proc: ProcId, entry_io: bool, proc_fixups: &mut Vec<(ProcId, Addr)>) -> Addr {
        self.debug(format!("Proc {:?}", proc));
        let addr = self.next_addr();
        self.compile_expr(mir, &mir.procs.get(proc).unwrap().body, &mut Vec::new(), proc_fixups);
        if entry_io {
            self.push(Instr::ApplyFunc);
        }
        self.push(Instr::Ret);
        addr
    }

    pub fn from_mir(mir: &MirContext) -> Self {
        let mut this = Self::default();

        let entry = mir.entry.expect("No entry point");
        this.does_io = if let repr::Repr::Func(i, o) = mir.procs.get(entry).unwrap().body.meta() {
            if let (repr::Repr::Prim(repr::Prim::Universe), repr::Repr::Tuple(xs)) = (&**i, &**o) {
                if let [repr::Repr::Prim(repr::Prim::Universe), repr::Repr::Tuple(xs)] = &xs[..] {
                    xs.len() == 0
                } else {
                    false
                }
            } else {
                false
            }
        } else {
            false
        };

        let mut procs = BTreeMap::new();
        let mut proc_fixups = Vec::new();

        for proc_id in mir.reachable_procs() {
            procs.insert(proc_id, this.compile_proc(mir, proc_id, this.does_io && proc_id == entry, &mut proc_fixups));
        }

        for (proc_id, addr) in proc_fixups {
            this.fixup(addr, procs[&proc_id], Instr::Call); // Fixes #4
        }

        this.entry = procs[&entry];

        this
    }
}
