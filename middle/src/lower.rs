use super::*;

// Type instantiations for generic types
pub struct TyInsts<'a> {
    pub self_ty: Option<Repr>,
    pub gen: &'a [Repr],
}

pub struct Lowerer<'a> {
    pub ctx: &'a mut Context,
    pub hir: &'a HirContext,
    pub con: &'a ConContext,
    pub lower_stack: Vec<(ConProcId, bool)>,
}

impl<'a> Lowerer<'a> {
    pub fn lower_proc(&mut self, proc: ConProcId) -> ProcId {
        let id = Procs::id_of_con(proc.clone());

        // Instantiate proc if not already in progress
        if let Some((_, is_recursive)) = self.lower_stack.iter_mut().find(|(stack_id, _)| *stack_id == id) {
            *is_recursive = true;
        } else if self.ctx.procs.get(id).is_none() {
            self.lower_stack.push((id, false));
            let body = self.lower_expr(self.con.get_proc(proc), &mut Vec::new());
            let (_, is_recursive) = self.lower_stack.pop().unwrap();
            self.ctx.procs.insert(id, Proc { body, is_recursive });
        }

        id
    }

    pub fn lower_litr(&mut self, litr: &hir::Literal) -> mir::Literal {
        match litr {
            hir::Literal::Nat(x) => mir::Literal::Int(*x as i64),
            hir::Literal::Int(x) => mir::Literal::Int(*x),
            hir::Literal::Str(s) => mir::Literal::List(s.chars().map(mir::Literal::Char).collect()),
            hir::Literal::Real(x) => mir::Literal::Real(*x),
            hir::Literal::Char(c) => mir::Literal::Char(*c),
        }
    }

    pub fn lower_data(&mut self, data: ConDataId) -> ConDataId {
        if self.ctx.reprs.declare(data) {
            let con_data = self.con.get_data(data);
            let variants = con_data
                .cons
                .iter()
                .map(|(_, ty)| self.lower_ty(*ty))
                .collect();
            self.ctx.reprs.define(data, Data {
                is_recursive: con_data.is_recursive,
                repr: Repr::Sum(variants),
            });
        }
        data
    }

    pub fn lower_ty(&mut self, ty: ConTyId) -> Repr {
        fn prim_to_mir(prim: ty::Prim) -> repr::Prim {
            match prim {
                ty::Prim::Nat | ty::Prim::Int => repr::Prim::Int,
                ty::Prim::Real => repr::Prim::Real,
                ty::Prim::Char => repr::Prim::Char,
                ty::Prim::Universe => repr::Prim::Universe,
            }
        }

        match self.con.get_ty(ty) {
            ConTy::Prim(prim) => Repr::Prim(prim_to_mir(*prim)),
            ConTy::List(item) => Repr::List(Box::new(self.lower_ty(*item))),
            ConTy::Func(i, o) => Repr::Func(
                Box::new(self.lower_ty(*i)),
                Box::new(self.lower_ty(*o)),
            ),
            ConTy::Data(data_id) => Repr::Data(self.lower_data(*data_id)),
            ConTy::Record(fields) => Repr::Tuple(fields.iter().map(|(_, field)| self.lower_ty(*field)).collect()),
            ConTy::Effect(effs, out) => Repr::Effect(effs.clone(), Box::new(self.lower_ty(*out))),
        }
    }

    pub fn lower_binding(&mut self, con_binding: &ConBinding, bindings: &mut Vec<(Ident, Local)>) -> mir::MirNode<mir::Binding> {
        let pat = match &*con_binding.pat {
            hir::Pat::Error => unreachable!(),
            hir::Pat::Wildcard => mir::Pat::Wildcard,
            hir::Pat::Literal(litr) => mir::Pat::Literal(self.lower_litr(litr)),
            hir::Pat::Single(inner) => mir::Pat::Single(self.lower_binding(inner, bindings)),
            hir::Pat::Add(lhs, rhs) => mir::Pat::Add(self.lower_binding(lhs, bindings), **rhs),
            hir::Pat::ListExact(items) => mir::Pat::ListExact(items
                .iter()
                .map(|item| self.lower_binding(item, bindings))
                .collect()),
            hir::Pat::ListFront(items, tail) => mir::Pat::ListFront(
                items
                    .iter()
                    .map(|item| self.lower_binding(item, bindings))
                    .collect(),
                tail.as_ref().map(|tail| self.lower_binding(tail, bindings)),
            ),
            hir::Pat::Decons(data, variant, inner) => {
                let variant = self.hir.datas
                    .get_data(data.data_id())
                    .cons
                    .iter()
                    .enumerate()
                    .find(|(_, (name, _))| **name == *variant)
                    .unwrap()
                    .0;
                self.lower_data(*data);
                let pat = mir::Pat::Variant(variant, self.lower_binding(inner, bindings));
                mir::Pat::Data(*data, MirNode::new(mir::Binding { pat, name: None }, self.ctx.reprs.get(*data).repr.clone()))
            },
            hir::Pat::Record(fields, _) => {
                let mut fields = fields
                    .iter()
                    .map(|(name, field)| (*name, self.lower_binding(field, bindings)))
                    .collect::<Vec<_>>();
                fields.sort_by_key(|(name, _)| name.as_ref());
                mir::Pat::Tuple(fields.into_iter().map(|(_, field)| field).collect())
            },
            pat => todo!("{:?}", pat),
        };

        let binding = mir::Binding {
            pat,
            name: if let Some(name) = &con_binding.name {
                let local = Local::new();
                bindings.push((**name, local));
                Some(local)
            } else {
                None
            },
        };

        MirNode::new(binding, self.lower_ty(*con_binding.meta()))
    }

    pub fn lower_expr(&mut self, con_expr: &ConExpr, stack: &mut Vec<(Ident, Local)>) -> mir::MirNode<mir::Expr> {
        let expr = match &**con_expr {
            hir::Expr::Error => unreachable!(),
            hir::Expr::Literal(litr) => mir::Expr::Literal(self.lower_litr(litr)),
            hir::Expr::Local(local) => mir::Expr::Local(stack
                .iter()
                .rev()
                .find(|(name, _)| name == local)
                .expect("No such local")
                .1),
            hir::Expr::Global(proc) => mir::Expr::Global(self.lower_proc(*proc)),
            hir::Expr::Match(_, pred, arms) => {
                let arms = arms
                    .iter()
                    .map(|(binding, arm)| {
                        let old_stack = stack.len();
                        let binding = self.lower_binding(binding, stack);
                        let arm = self.lower_expr(arm, stack);
                        stack.truncate(old_stack);
                        (binding, arm)
                    })
                    .collect();
                mir::Expr::Match(self.lower_expr(pred, stack), arms)
            },
            hir::Expr::List(items, tails) => {
                let mut list = mir::Expr::List(items
                    .iter()
                    .map(|item| self.lower_expr(item, stack))
                    .collect());

                for tail in tails {
                    let tail = self.lower_expr(tail, stack);
                    list = mir::Expr::Intrinsic(
                        mir::Intrinsic::Join(match tail.meta() {
                            Repr::List(item) => (**item).clone(),
                            _ => unreachable!(),
                        }),
                        vec![
                            MirNode::new(list, tail.meta().clone()),
                            tail,
                        ],
                    );
                }

                list
            },
            hir::Expr::Func(arg, body) => {
                let arg_local = Local::new();
                stack.push((**arg, arg_local));
                let body = self.lower_expr(body, stack);
                stack.pop();

                mir::Expr::Func(MirNode::new(arg_local, self.lower_ty(*arg.meta())), body)
            },
            hir::Expr::Apply(f, arg) => mir::Expr::Apply(self.lower_expr(f, stack), self.lower_expr(arg, stack)),
            hir::Expr::Cons(data, variant, inner) => {
                let variant = self.hir.datas
                    .get_data(data.data_id())
                    .cons
                    .iter()
                    .enumerate()
                    .find(|(_, (name, _))| **name == *variant)
                    .unwrap()
                    .0;
                self.lower_data(*data);
                let expr = mir::Expr::Variant(variant, self.lower_expr(inner, stack));
                mir::Expr::Data(*data, MirNode::new(expr, self.ctx.reprs.get(*data).repr.clone()))
            },
            hir::Expr::Access(record, field) => {
                let (record_ty, _, indirections) = self.con.follow_field_access(self.hir, *record.meta(), **field).unwrap();
                let field_idx = if let ConTy::Record(fields) = self.con.get_ty(record_ty) {
                    let mut fields = fields.iter().map(|(name, _)| *name).collect::<Vec<_>>();
                    fields.sort_by_key(|name| name.as_ref());
                    fields.iter().enumerate().find(|(_, name)| **name == **field).unwrap().0
                } else {
                    unreachable!();
                };
                let mut record = self.lower_expr(record, stack);
                // Perform indirections for field accesses
                for _ in 0..indirections {
                    let (data, variant_repr) = if let Repr::Data(data) = record.meta() {
                        if let Repr::Sum(variants) = &self.ctx.reprs.get(*data).repr {
                            (*data, variants[0].clone())
                        } else {
                            unreachable!()
                        }
                    } else {
                        unreachable!()
                    };
                    record = MirNode::new(mir::Expr::AccessData(record, data), Repr::Data(data));
                    record = MirNode::new(mir::Expr::AccessVariant(record, 0), variant_repr);
                }

                mir::Expr::Access(record, field_idx)
            },
            hir::Expr::Record(fields, _) => {
                let mut fields = fields
                    .iter()
                    .map(|(name, field)| (**name, self.lower_expr(field, stack)))
                    .collect::<Vec<_>>();
                fields.sort_by_key(|(name, _)| name.as_ref());
                mir::Expr::Tuple(fields.into_iter().map(|(_, field)| field).collect())
            },
            hir::Expr::ClassAccess(ty, class, field) => panic!("Class access should not still exist during MIR lowering"),
            hir::Expr::Intrinsic(name, args) => {
                match name.inner() {
                    hir::Intrinsic::TypeName => {
                        let name = match self.con.get_ty(*args.first().expect("type_name intrinsic must have an argument").meta()) {
                            ConTy::List(inner) => self.con.display(self.hir, *inner).to_string(),
                            _ => panic!("type_name argument must be list of type"),
                        };
                        mir::Expr::Literal(mir::Literal::List(name.chars().map(mir::Literal::Char).collect()))
                    },
                    hir::Intrinsic::NegNat | hir::Intrinsic::NegInt => mir::Expr::Intrinsic(mir::Intrinsic::NegInt, vec![self.lower_expr(&args[0], stack)]),
                    hir::Intrinsic::NegReal => mir::Expr::Intrinsic(mir::Intrinsic::NegReal, vec![self.lower_expr(&args[0], stack)]),
                    hir::Intrinsic::DisplayInt => mir::Expr::Intrinsic(mir::Intrinsic::DisplayInt, vec![self.lower_expr(&args[0], stack)]),
                    hir::Intrinsic::CodepointChar => mir::Expr::Intrinsic(mir::Intrinsic::CodepointChar, vec![self.lower_expr(&args[0], stack)]),
                    hir::Intrinsic::EqChar => mir::Expr::Intrinsic(mir::Intrinsic::EqChar, vec![
                        self.lower_expr(&args[0], stack),
                        self.lower_expr(&args[1], stack),
                    ]),
                    hir::Intrinsic::EqNat => mir::Expr::Intrinsic(mir::Intrinsic::EqInt, vec![
                        self.lower_expr(&args[0], stack),
                        self.lower_expr(&args[1], stack),
                    ]),
                    hir::Intrinsic::LessNat => mir::Expr::Intrinsic(mir::Intrinsic::LessInt, vec![
                        self.lower_expr(&args[0], stack),
                        self.lower_expr(&args[1], stack),
                    ]),
                    hir::Intrinsic::AddNat | hir::Intrinsic::AddInt => mir::Expr::Intrinsic(mir::Intrinsic::AddInt, vec![
                        self.lower_expr(&args[0], stack),
                        self.lower_expr(&args[1], stack),
                    ]),
                    hir::Intrinsic::MulNat | hir::Intrinsic::MulInt => mir::Expr::Intrinsic(mir::Intrinsic::MulInt, vec![
                        self.lower_expr(&args[0], stack),
                        self.lower_expr(&args[1], stack),
                    ]),
                    hir::Intrinsic::Go => {
                        let next_local = Local::new();
                        let func = self.lower_expr(&args[0], stack);
                        let next = self.lower_expr(&args[1], stack);
                        let output_repr = if let Repr::Func(_, o) = func.meta() { (**o).clone() } else { unreachable!() };
                        mir::Expr::Go(
                            MirNode::new(next_local, next.meta().clone()),
                            MirNode::new(mir::Expr::Apply(func, MirNode::new(mir::Expr::Local(next_local), next.meta().clone())), output_repr),
                            next,
                        )
                    },
                    hir::Intrinsic::Print => mir::Expr::Intrinsic(mir::Intrinsic::Print, vec![
                        self.lower_expr(&args[0], stack),
                        self.lower_expr(&args[1], stack),
                    ]),
                    hir::Intrinsic::Input => mir::Expr::Intrinsic(mir::Intrinsic::Input, vec![
                        self.lower_expr(&args[0], stack),
                    ]),
                    hir::Intrinsic::Rand => mir::Expr::Intrinsic(mir::Intrinsic::Rand, vec![
                        self.lower_expr(&args[0], stack),
                        self.lower_expr(&args[1], stack),
                    ]),
                    hir::Intrinsic::LenList => mir::Expr::Intrinsic(mir::Intrinsic::LenList, vec![
                        self.lower_expr(&args[0], stack),
                    ]),
                    hir::Intrinsic::SkipList => mir::Expr::Intrinsic(mir::Intrinsic::SkipList, vec![
                        self.lower_expr(&args[0], stack),
                        self.lower_expr(&args[1], stack),
                    ]),
                    hir::Intrinsic::TrimList => mir::Expr::Intrinsic(mir::Intrinsic::TrimList, vec![
                        self.lower_expr(&args[0], stack),
                        self.lower_expr(&args[1], stack),
                    ]),
                    hir::Intrinsic::JoinList => {
                        let Repr::List(item_repr) = self.lower_ty(*args[0].meta())
                            else { panic!("Joining non-lists!") };
                        mir::Expr::Intrinsic(mir::Intrinsic::Join(*item_repr), vec![
                            self.lower_expr(&args[0], stack),
                            self.lower_expr(&args[1], stack),
                        ])
                    },
                    hir::Intrinsic::Propagate => match self.con.get_ty(*args[0].meta()) {
                        ConTy::Effect(effs, _) => mir::Expr::Intrinsic(mir::Intrinsic::Propagate(effs.clone()), vec![
                            self.lower_expr(&args[0], stack),
                        ]),
                        _ => unreachable!(),
                        // _ => self.lower_expr(&args[0], stack).into_inner(),
                    },
                    hir::Intrinsic::Dispatch => panic!("Type dispatching should have occurred during concretisation!"),
                }
            },
            hir::Expr::Update(record, fields) => {
                let mut mir_record = self.lower_expr(record, stack);

                for (field_name, field) in fields {
                    let (record_ty, _, indirections) = self.con.follow_field_access(self.hir, *record.meta(), **field_name).unwrap();
                    let field_idx = if let ConTy::Record(fields) = self.con.get_ty(record_ty) {
                        let mut fields = fields.iter().map(|(name, _)| *name).collect::<Vec<_>>();
                        fields.sort_by_key(|name| name.as_ref());
                        fields.iter().enumerate().find(|(_, name)| **name == **field_name).unwrap().0
                    } else {
                        unreachable!();
                    };
                    // Perform indirections for field accesses, unwrapping until we reach the record
                    let mut datas = Vec::new();
                    for _ in 0..indirections {
                        let (data, variant_repr) = if let Repr::Data(data) = mir_record.meta() {
                            let repr = &self.ctx.reprs.get(*data).repr;
                            if let Repr::Sum(variants) = repr {
                                datas.push((*data, repr.clone()));
                                (*data, variants[0].clone())
                            } else {
                                unreachable!()
                            }
                        } else {
                            unreachable!()
                        };
                        mir_record = MirNode::new(mir::Expr::AccessData(mir_record, data), Repr::Data(data));
                        mir_record = MirNode::new(mir::Expr::AccessVariant(mir_record, 0), variant_repr);
                    }

                    // Update field
                    let record_repr = mir_record.meta().clone();
                    let field = self.lower_expr(field, stack);
                    mir_record = MirNode::new(mir::Expr::Intrinsic(Intrinsic::UpdateField(field_idx), vec![mir_record, field]), record_repr);

                    // Re-wrap the record
                    for (data, sum_repr) in datas.into_iter().rev() {
                        let sum = MirNode::new(mir::Expr::Variant(0, mir_record), sum_repr);
                        mir_record = MirNode::new(mir::Expr::Data(data, sum), Repr::Data(data));
                    }
                }

                mir_record.into_inner()
            },
            hir::Expr::Basin(effs, inner) => mir::Expr::Basin(effs.clone(), self.lower_expr(inner, stack)),
            hir::Expr::Handle { expr, handlers } => {
                let expr = self.lower_expr(expr, stack);
                let expr_repr = expr.meta().clone();

                let default_state_repr = Repr::Tuple(Vec::new());
                let mut is_state = false;

                let handlers = handlers
                    .iter()
                    .map(|hir::Handler { eff, send, state, recv }| {
                        let send_local = Local::new();
                        let state_local = Local::new();
                        is_state = state.is_some();
                        mir::Handler {
                            eff: *eff,
                            send: MirNode::new(send_local, self.lower_ty(*send.meta())),
                            state: state.as_ref()
                                .map(|state| MirNode::new(state_local, self.lower_ty(*state.meta())))
                                .unwrap_or(MirNode::new(state_local, default_state_repr.clone())),
                            recv: {
                                let old_len = stack.len();
                                stack.push((**send, send_local));
                                if let Some(state) = state {
                                    stack.push((**state, state_local));
                                }
                                let recv = if state.is_none() {
                                    let out = self.lower_expr(recv, stack);
                                    let out_repr = out.meta().clone();
                                    MirNode::new(mir::Expr::Tuple(vec![
                                        out,
                                        MirNode::new(mir::Expr::Tuple(Vec::new()), default_state_repr.clone())
                                    ]), Repr::Tuple(vec![out_repr, default_state_repr.clone()]))
                                } else {
                                    self.lower_expr(recv, stack)
                                };
                                stack.truncate(old_len);
                                recv
                            },
                        }
                    })
                    .collect();

                let handler = mir::Expr::Handle {
                    expr: if !is_state {
                        MirNode::new(mir::Expr::Tuple(vec![
                            expr,
                            MirNode::new(mir::Expr::Tuple(Vec::new()), default_state_repr.clone())
                        ]), Repr::Tuple(vec![expr_repr.clone(), default_state_repr.clone()]))
                    } else {
                        expr
                    },
                    handlers,
                };

                if is_state {
                    handler
                } else {
                    mir::Expr::Access(MirNode::new(handler, Repr::Tuple(vec![
                        if let Repr::Effect(_, out) = expr_repr {
                            (*out).clone()
                        } else {
                            unreachable!()
                        },
                        default_state_repr.clone(),
                    ])), 1)
                }
            },
            hir::Expr::Suspend(eff, inner) => mir::Expr::Intrinsic(mir::Intrinsic::Suspend(*eff), vec![
                self.lower_expr(inner, stack),
            ]),
        };

        MirNode::new(expr, self.lower_ty(*con_expr.meta()))
    }
}
