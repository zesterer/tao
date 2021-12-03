use super::*;

// TODO: use `ToHir`?
fn litr_ty_info(litr: &ast::Literal, infer: &mut Infer, span: Span) -> TyInfo {
    match litr {
        ast::Literal::Nat(_) => TyInfo::Prim(ty::Prim::Nat), // TODO: Numeric subtyping
        ast::Literal::Num(_) => TyInfo::Prim(ty::Prim::Num),
        ast::Literal::Bool(_) => TyInfo::Prim(ty::Prim::Bool),
        ast::Literal::Char(_) => TyInfo::Prim(ty::Prim::Char),
        ast::Literal::Str(_) => TyInfo::List(infer.insert(span, TyInfo::Prim(ty::Prim::Char))),
    }
}

pub enum Scope<'a> {
    Empty,
    Recursive(SrcNode<Ident>, TyVar, DefId, Vec<(Span, TyVar)>),
    Binding(&'a Scope<'a>, SrcNode<Ident>, TyVar),
    Many(&'a Scope<'a>, Vec<(SrcNode<Ident>, TyVar)>),
}

impl<'a> Scope<'a> {
    pub fn empty() -> Self { Self::Empty }

    fn with(&self, name: SrcNode<Ident>, ty: TyVar) -> Scope<'_> {
        Scope::Binding(self, name, ty)
    }

    fn with_many(&self, many: Vec<(SrcNode<Ident>, TyVar)>) -> Scope<'_> {
        Scope::Many(self, many)
    }

    // bool = is_local
    fn find(&self, infer: &mut Infer, span: Span, name: &Ident) -> Option<(TyVar, Option<(DefId, Vec<(Span, TyVar)>)>)> {
        match self {
            Self::Empty => None,
            Self::Recursive(def, ty, def_id, tys) => if &**def == name {
                Some((infer.try_reinstantiate(span, *ty), Some((*def_id, tys.clone()))))
            } else {
                None
            },
            Self::Binding(_, local, ty) if &**local == name => Some((*ty, None)),
            Self::Binding(parent, _, _) => parent.find(infer, span, name),
            Self::Many(parent, locals) => if let Some((_, ty)) = locals.iter().find(|(local, _)| &**local == name) {
                Some((*ty, None))
            } else {
                parent.find(infer, span, name)
            },
        }
    }
}

pub trait ToHir: Sized {
    type Output;

    fn to_hir(self: &SrcNode<Self>, infer: &mut Infer, scope: &Scope) -> InferNode<Self::Output>;
}

impl ToHir for ast::Type {
    type Output = ();

    fn to_hir(self: &SrcNode<Self>, infer: &mut Infer, scope: &Scope) -> InferNode<()> {
        let info = match &**self {
            ast::Type::Error => TyInfo::Error,
            ast::Type::Unknown => TyInfo::Unknown(None),
            ast::Type::List(item) => TyInfo::List(item.to_hir(infer, scope).meta().1),
            ast::Type::Tuple(items) => TyInfo::Tuple(items
                .iter()
                .map(|item| item.to_hir(infer, scope).meta().1)
                .collect()),
            ast::Type::Record(fields) => TyInfo::Record(fields
                .iter()
                .map(|(name, field)| (**name, field.to_hir(infer, scope).meta().1))
                .collect()),
            ast::Type::Func(i, o) => TyInfo::Func(i.to_hir(infer, scope).meta().1, o.to_hir(infer, scope).meta().1),
            ast::Type::Data(name, params) => match (name.as_str(), params.len()) {
                ("Nat", 0) => TyInfo::Prim(Prim::Nat),
                ("Int", 0) => TyInfo::Prim(Prim::Int),
                ("Num", 0) => TyInfo::Prim(Prim::Num),
                ("Bool", 0) => TyInfo::Prim(Prim::Bool),
                ("Char", 0) => TyInfo::Prim(Prim::Char),
                _ => {
                    let params = params
                        .iter()
                        .map(|param| param.to_hir(infer, scope).meta().1)
                        .collect::<Vec<_>>();

                    if let Some((scope, (gen_idx, gen))) = infer
                        .gen_scope()
                        .and_then(|scope| Some((scope, infer.ctx().tys.get_gen_scope(scope).find(**name)?)))
                    {
                        TyInfo::Gen(gen_idx, scope, gen.span())
                    } else if let Some(alias_id) = infer.ctx().datas.lookup_alias(**name) {
                        if let Some(alias) = infer.ctx().datas.get_alias(alias_id) {
                            let alias_gen_scope = infer.ctx().tys.get_gen_scope(alias.gen_scope);
                            if alias_gen_scope.len() != params.len() {
                                let err = Error::WrongNumberOfGenerics(
                                    self.span(),
                                    params.len(),
                                    alias_gen_scope.span,
                                    alias_gen_scope.len(),
                                );
                                infer.ctx_mut().emit(err);
                                TyInfo::Error
                            } else {
                                let (alias_ty, alias_gen_scope) = (alias.ty, alias.gen_scope);
                                let get_gen = |index, scope, ctx: &Context| {
                                    assert!(index < params.len(), "{:?}, {}, {:?}, {:?}, {:?}", name, ctx.datas.get_alias(alias_id).unwrap().name, ctx.tys.get_gen_scope(scope).get(index), alias_gen_scope, scope);
                                    params[index]
                                };
                                TyInfo::Ref(infer.instantiate(alias_ty, name.span(), &get_gen))
                            }
                        } else {
                            let err_ty = infer.insert(self.span(), TyInfo::Error);
                            infer.emit(InferError::RecursiveAlias(alias_id, err_ty, name.span()));
                            TyInfo::Ref(err_ty)
                        }
                    } else if let Some(data) = infer.ctx().datas.lookup_data(**name) {
                        let data_gen_scope = infer.ctx().tys.get_gen_scope(infer.ctx().datas.name_gen_scope(**name));
                        if data_gen_scope.len() != params.len() {
                            let err = Error::WrongNumberOfGenerics(
                                self.span(),
                                params.len(),
                                data_gen_scope.span,
                                data_gen_scope.len(),
                            );
                            infer.ctx_mut().emit(err);
                            TyInfo::Error
                        } else {
                            TyInfo::Data(data, params)
                        }
                    } else {
                        infer.ctx_mut().emit(Error::NoSuchData(name.clone()));
                        TyInfo::Error
                    }
                },
            },
        };

        InferNode::new((), (self.span(), infer.insert(self.span(), info)))
    }
}

impl ToHir for ast::Binding {
    type Output = hir::Binding<InferMeta>;

    fn to_hir(self: &SrcNode<Self>, infer: &mut Infer, scope: &Scope) -> InferNode<Self::Output> {
        let (info, pat) = match &*self.pat {
            ast::Pat::Error => (TyInfo::Error, hir::Pat::Error),
            ast::Pat::Wildcard => (TyInfo::Unknown(None), hir::Pat::Wildcard),
            ast::Pat::Literal(litr) => (litr_ty_info(litr, infer, self.pat.span()), hir::Pat::Literal(*litr)),
            ast::Pat::Single(inner) => {
                let binding = inner.to_hir(infer, scope);
                // TODO: don't use `Ref` to link types
                (TyInfo::Ref(binding.meta().1), hir::Pat::Single(binding))
            },
            ast::Pat::Binary(op, lhs, rhs) => {
                let lhs = lhs.to_hir(infer, scope);
                match (&**rhs, &**op) {
                    (ast::Literal::Nat(rhs_nat), ast::BinaryOp::Add) => {
                        let nat = infer.insert(rhs.span(), TyInfo::Prim(Prim::Nat));
                        infer.make_eq(lhs.meta().1, nat, self.span());
                        (TyInfo::Ref(nat), hir::Pat::Add(lhs, SrcNode::new(*rhs_nat, rhs.span())))
                    },
                    (_, _) => {
                        let info = litr_ty_info(rhs, infer, self.pat.span());
                        let rhs_ty = infer.insert(rhs.span(), info);
                        infer.emit(InferError::PatternNotSupported(lhs.meta().1, op.clone(), rhs_ty, self.span()));
                        (TyInfo::Error, hir::Pat::Error)
                    },
                }
            },
            ast::Pat::Tuple(items) => {
                let items = items
                    .iter()
                    .map(|item| item.to_hir(infer, scope))
                    .collect::<Vec<_>>();
                (TyInfo::Tuple(items
                    .iter()
                    .map(|item| item.meta().1)
                    .collect()), hir::Pat::Tuple(items))
            },
            ast::Pat::Record(fields) => {
                let fields = fields
                    .iter()
                    .map(|(name, field)| (**name, field.to_hir(infer, scope)))
                    .collect::<BTreeMap<_, _>>();
                (TyInfo::Record(fields
                    .iter()
                    .map(|(name, field)| (*name, field.meta().1))
                    .collect()), hir::Pat::Record(fields))
            },
            ast::Pat::ListExact(items) => {
                let item_ty = infer.unknown(self.pat.span());
                let items = items
                    .iter()
                    .map(|item| {
                        let item = item.to_hir(infer, scope);
                        infer.make_eq(item.meta().1, item_ty, item.meta().0);
                        item
                    })
                    .collect::<Vec<_>>();
                (TyInfo::List(item_ty), hir::Pat::ListExact(items))
            },
            ast::Pat::ListFront(items, tail) => {
                let item_ty = infer.unknown(self.pat.span());
                let items = items
                    .iter()
                    .map(|item| {
                        let item = item.to_hir(infer, scope);
                        infer.make_eq(item.meta().1, item_ty, item.meta().0);
                        item
                    })
                    .collect::<Vec<_>>();
                let tail = tail.as_ref().map(|tail| {
                    let tail = tail.to_hir(infer, scope);
                    let ty = infer.insert(tail.meta().0, TyInfo::List(item_ty));
                    infer.make_eq(tail.meta().1, ty, tail.meta().0);
                    tail
                });
                (TyInfo::List(item_ty), hir::Pat::ListFront(items, tail))
            },
            ast::Pat::Deconstruct(name, inner) => if let Some(data) = infer.ctx().datas.lookup_cons(**name) {
                let generics_count = infer.ctx().tys.get_gen_scope(infer.ctx().datas.get_data(data).gen_scope).len();
                let generic_tys = (0..generics_count)
                    .map(|_| infer.unknown(name.span()))
                    .collect::<Vec<_>>();

                let inner_ty = infer
                    .ctx()
                    .datas
                    .get_data(data)
                    .cons
                    .iter()
                    .find(|(cons, _)| **cons == **name)
                    .unwrap()
                    .1;

                // Recreate type in context
                let inner_ty = {
                    let data = infer.ctx().datas.get_data(data);
                    let data_gen_scope = data.gen_scope;
                    let get_gen = |index, _, ctx: &Context| generic_tys[index];
                    infer.instantiate(inner_ty, name.span(), &get_gen)
                };

                let inner = inner.to_hir(infer, scope);
                infer.make_eq(inner.meta().1, inner_ty, self.span());

                (TyInfo::Data(data, generic_tys), hir::Pat::Decons(SrcNode::new(data, name.span()), **name, inner))
            } else {
                infer.ctx_mut().emit(Error::NoSuchCons(name.clone()));
                // TODO: Don't use a hard, preserve inner expression
                (TyInfo::Error, hir::Pat::Error)
            },
        };

        let ty = infer.insert(self.span(), info);

        if let Some(ty_hint) = &self.ty {
            let hint = ty_hint.to_hir(infer, scope);
            infer.make_eq(ty, hint.meta().1, self.span());
        }

        InferNode::new(hir::Binding {
            pat: SrcNode::new(pat, self.pat.span()),
            name: self.name.clone(),
        }, (self.span(), ty))
    }
}

impl ToHir for ast::Expr {
    type Output = hir::Expr<InferMeta>;

    fn to_hir(self: &SrcNode<Self>, infer: &mut Infer, scope: &Scope) -> InferNode<Self::Output> {
        let (info, expr) = match &**self {
            ast::Expr::Error => (TyInfo::Error, hir::Expr::Error),
            ast::Expr::Literal(litr) => (litr_ty_info(litr, infer, self.span()), hir::Expr::Literal(*litr)),
            ast::Expr::Local(local) => if let Some((ty, rec)) = scope.find(infer, self.span(), &local) {
                if let Some((def_id, gens)) = rec {
                    (TyInfo::Ref(ty), hir::Expr::Global(def_id, gens))
                } else {
                    (TyInfo::Ref(ty), hir::Expr::Local(*local))
                }
            } else if let Some(def_id) = infer.ctx().defs.lookup(*local) {
                let scope = infer.ctx().tys.get_gen_scope(infer.ctx().defs.get(def_id).gen_scope);
                let generics_count = scope.len();
                let generic_tys = (0..generics_count)
                    .map(|i| TyInfo::Unknown(Some(scope.get(i).span())))
                    .collect::<Vec<_>>()
                    .into_iter()
                    .map(|info| (self.span(), infer.insert(self.span(), info)))
                    .collect::<Vec<_>>();

                // Recreate type in context
                let def = infer.ctx().defs.get(def_id);
                let def_gen_scope = def.gen_scope;
                let def_name = def.name.clone();
                let get_gen = |index: usize, _, ctx: &Context| generic_tys[index].1;
                let ty = if let Some(body) = def.body.as_ref() {
                    let body_ty = body.meta().1;
                    Some(infer.instantiate(body_ty, self.span(), &get_gen))
                } else {
                    let ty_hint = def.ty_hint.clone();
                    if ty_hint.is_fully_specified() {
                        let ty_hint_ty = ty_hint.to_hir(infer, &Scope::Empty).meta().1;
                        Some(infer.try_reinstantiate(self.span(), ty_hint_ty))
                    } else {
                        None
                    }
                };

                if let Some(ty) = ty {
                    (TyInfo::Ref(ty), hir::Expr::Global(def_id, generic_tys))
                } else {
                    infer.ctx_mut().emit(Error::DefTypeNotSpecified(def_name.span(), self.span(), *def_name));
                    (TyInfo::Error, hir::Expr::Error)
                }
            } else {
                infer.ctx_mut().emit(Error::NoSuchLocal(SrcNode::new(*local, self.span())));
                (TyInfo::Error, hir::Expr::Error)
            },
            ast::Expr::Tuple(items) => {
                let items = items
                    .iter()
                    .map(|item| item.to_hir(infer, scope))
                    .collect::<Vec<_>>();
                let tys = items
                    .iter()
                    .map(|item| item.meta().1)
                    .collect();
                (TyInfo::Tuple(tys), hir::Expr::Tuple(items))
            },
            ast::Expr::List(items) => {
                let item_ty = infer.unknown(self.span());
                let items = items
                    .iter()
                    .map(|item| {
                        let item = item.to_hir(infer, scope);
                        infer.make_eq(item.meta().1, item_ty, item.meta().0);
                        item
                    })
                    .collect::<Vec<_>>();
                (TyInfo::List(item_ty), hir::Expr::List(items))
            },
            ast::Expr::ListFront(items, tail) => {
                let item_ty = infer.unknown(self.span());
                let list_ty = infer.insert(self.span(), TyInfo::List(item_ty));

                let tail = tail.to_hir(infer, scope);
                infer.make_eq(tail.meta().1, list_ty, tail.meta().0);

                let items = items
                    .iter()
                    .map(|item| {
                        let item = item.to_hir(infer, scope);
                        infer.make_eq(item.meta().1, item_ty, item.meta().0);
                        item
                    })
                    .collect::<Vec<_>>();
                (TyInfo::Ref(list_ty), hir::Expr::ListFront(items, tail))
            },
            ast::Expr::Record(fields) => {
                let fields = fields
                    .iter()
                    .map(|(name, field)| (name.clone(), field.to_hir(infer, scope)))
                    .collect::<Vec<_>>();
                let tys = fields
                    .iter()
                    .map(|(name, field)| (**name, field.meta().1))
                    .collect();
                (TyInfo::Record(tys), hir::Expr::Record(fields))
            },
            ast::Expr::Access(record, field) => {
                let record = record.to_hir(infer, scope);
                let field_ty = infer.unknown(self.span());
                infer.make_access(record.meta().1, field.clone(), field_ty);
                // TODO: don't use `Ref` to link types
                (TyInfo::Ref(field_ty), hir::Expr::Access(record, field.clone()))
            },
            ast::Expr::Unary(op, a) => {
                let a = a.to_hir(infer, scope);
                let output_ty = infer.unknown(self.span());
                infer.make_unary(op.clone(), a.meta().1, output_ty);
                (TyInfo::Ref(output_ty), hir::Expr::Unary(op.clone(), a))
            },
            ast::Expr::Binary(op, a, b) => {
                let a = a.to_hir(infer, scope);
                let b = b.to_hir(infer, scope);
                let output_ty = infer.unknown(self.span());
                infer.make_binary(op.clone(), a.meta().1, b.meta().1, output_ty);
                (TyInfo::Ref(output_ty), hir::Expr::Binary(op.clone(), a, b))
            },
            ast::Expr::Let(bindings, then) => {
                fn fold<'a>(then: &SrcNode<ast::Expr>, bindings: &'a mut impl Iterator<Item = &'a (SrcNode<ast::Binding>, SrcNode<ast::Expr>)>, infer: &mut Infer, scope: &Scope) -> InferExpr {
                    match bindings.next() {
                        Some((binding, val)) => {
                            let binding = binding.to_hir(infer, scope);
                            let val = val.to_hir(infer, scope);
                            infer.make_eq(binding.meta().1, val.meta().1, binding.meta().0);
                            let then = fold(then, bindings, infer, &scope.with_many(binding.get_bindings()));
                            let meta = *then.meta(); // TODO: Make a TyInfo::Ref?
                            InferNode::new(hir::Expr::Match(
                                val,
                                vec![(binding, then)],
                            ), meta)
                        },
                        None => then.to_hir(infer, scope),
                    }
                }

                let expr = fold(then, &mut bindings.iter(), infer, scope);
                (TyInfo::Ref(expr.meta().1), expr.into_inner())
            },
            ast::Expr::Match(preds, arms) => {
                let mut is_err = false;
                for arm in arms {
                    if arm.0.len() != preds.len() {
                        infer.ctx_mut().emit(Error::WrongNumberOfParams(arm.0.span(), arm.0.len(), preds.span(), preds.len()));
                        is_err = true;
                    }
                }

                if is_err {
                    (TyInfo::Error, hir::Expr::Error)
                } else {
                    let pred = tupleify_expr(preds, infer, scope);

                    let output_ty = infer.unknown(self.span());

                    let arms = arms
                        .iter()
                        .map(|(bindings, body)| {
                            let binding = tupleify_binding(bindings, infer, scope);
                            infer.make_eq(pred.meta().1, binding.meta().1, binding.meta().0);
                            let body = body.to_hir(infer, &scope.with_many(binding.get_bindings()));
                            infer.make_eq(body.meta().1, output_ty, body.meta().0);
                            (binding, body)
                        })
                        .collect();

                    (TyInfo::Ref(output_ty), hir::Expr::Match(pred, arms))
                }
            },
            ast::Expr::If(pred, a, b) => {
                let pred_ty = infer.insert(pred.span(), TyInfo::Prim(Prim::Bool));
                let output_ty = infer.unknown(self.span());
                let pred = pred.to_hir(infer, scope);
                infer.make_eq(pred.meta().1, pred_ty, pred.meta().0);
                let a = a.to_hir(infer, scope);
                let b = b.to_hir(infer, scope);
                infer.make_eq(a.meta().1, output_ty, self.span());
                infer.make_eq(b.meta().1, output_ty, self.span());
                let arms = vec![
                    (InferNode::new(hir::Binding::from_pat(SrcNode::new(hir::Pat::Literal(ast::Literal::Bool(true)), pred.meta().0)), *pred.meta()), a),
                    (InferNode::new(hir::Binding::from_pat(SrcNode::new(hir::Pat::Literal(ast::Literal::Bool(false)), pred.meta().0)), *pred.meta()), b),
                ];
                (TyInfo::Ref(output_ty), hir::Expr::Match(pred, arms))
            },
            ast::Expr::Func(arms) => {
                // TODO: Don't always refuse 0-branch functions? Can they be useful with never types?
                if let Some(first_arm) = arms.first() {
                    let mut is_err = false;
                    for arm in arms {
                        if arm.0.len() != first_arm.0.len() {
                            infer.ctx_mut().emit(Error::WrongNumberOfParams(arm.0.span(), arm.0.len(), first_arm.0.span(), first_arm.0.len()));
                            is_err = true;
                        }
                    }

                    if is_err {
                        (TyInfo::Error, hir::Expr::Error)
                    } else {
                        let output_ty = infer.unknown(self.span());

                        let pseudos = (0..first_arm.0.len())
                            .map(|i| {
                                let name = SrcNode::new(Ident::new(i), self.span());
                                let ty = infer.unknown(first_arm.0[i].span());
                                (name, ty)
                            })
                            .collect::<Vec<_>>();

                        let pred = tupleify_expr(&SrcNode::new(pseudos
                            .iter()
                            .map(|(pseudo, _)| SrcNode::new(ast::Expr::Local(**pseudo), pseudo.span()))
                            .collect(), self.span()), infer, &scope.with_many(pseudos.clone()));

                        let arms = arms
                            .iter()
                            .map(|(bindings, body)| {
                                let binding = tupleify_binding(bindings, infer, scope);
                                infer.make_eq(pred.meta().1, binding.meta().1, binding.meta().0);
                                let body = body.to_hir(infer, &scope.with_many(binding.get_bindings()));
                                infer.make_eq(body.meta().1, output_ty, body.meta().0);
                                (binding, body)
                            })
                            .collect();

                        let pred_meta = *pred.meta();
                        let f = pseudos
                            .into_iter()
                            .rev()
                            .fold(
                                InferNode::new(hir::Expr::Match(pred, arms), (self.span(), output_ty)),
                                |body, (pseudo, pseudo_ty)| {
                                    let f = infer.insert(self.span(), TyInfo::Func(pseudo_ty, body.meta().1));
                                    InferNode::new(hir::Expr::Func(InferNode::new(*pseudo, pred_meta), body), (self.span(), f))
                                },
                            );

                        (TyInfo::Ref(f.meta().1), f.into_inner())
                    }
                } else {
                    infer.ctx_mut().emit(Error::NoBranches(self.span()));
                    (TyInfo::Error, hir::Expr::Error)
                }
            },
            ast::Expr::Apply(f, param) => {
                let f = f.to_hir(infer, scope);
                let param = param.to_hir(infer, scope);
                let output_ty = infer.unknown(self.span());
                let func = infer.insert(f.meta().0, TyInfo::Func(param.meta().1, output_ty));
                infer.make_eq(f.meta().1, func, self.span());

                (TyInfo::Ref(output_ty), hir::Expr::Apply(f, param))
            },
            ast::Expr::Cons(name, inner) => if let Some(data) = infer.ctx().datas.lookup_cons(**name) {
                let generics_count = infer.ctx().tys.get_gen_scope(infer.ctx().datas.get_data(data).gen_scope).len();
                let generic_tys = (0..generics_count)
                    .map(|_| infer.unknown(name.span()))
                    .collect::<Vec<_>>();

                let inner_ty = infer
                    .ctx()
                    .datas
                    .get_data(data)
                    .cons
                    .iter()
                    .find(|(cons, _)| **cons == **name)
                    .unwrap()
                    .1;

                // Recreate type in context
                let inner_ty = {
                    let data = infer.ctx().datas.get_data(data);
                    let data_gen_scope = data.gen_scope;
                    let get_gen = |index, _, ctx: &Context| generic_tys[index];
                    infer.instantiate(inner_ty, name.span(), &get_gen)
                };

                let inner = inner.to_hir(infer, scope);
                infer.make_eq(inner.meta().1, inner_ty, self.span());

                (TyInfo::Data(data, generic_tys), hir::Expr::Cons(SrcNode::new(data, name.span()), **name, inner))
            } else {
                infer.ctx_mut().emit(Error::NoSuchCons(name.clone()));
                // TODO: Don't use a hard, preserve inner expression
                (TyInfo::Error, hir::Expr::Error)
            },
            ast::Expr::Debug(inner) => {
                let inner = inner.to_hir(infer, scope);
                (TyInfo::Ref(inner.meta().1), hir::Expr::Debug(inner))
            },
        };

        InferNode::new(expr, (self.span(), infer.insert(self.span(), info)))
    }
}

// Desugar a list of expressions into a tuple
fn tupleify_expr(items: &SrcNode<Vec<SrcNode<ast::Expr>>>, infer: &mut Infer, scope: &Scope) -> InferExpr {
    let hir_items = items
        .iter()
        .map(|item| item.to_hir(infer, scope))
        .collect::<Vec<_>>();
    let tuple_ty = infer.insert(items.span(), TyInfo::Tuple(hir_items
        .iter()
        .map(|item| item.meta().1)
        .collect()));
    InferNode::new(hir::Expr::Tuple(hir_items), (items.span(), tuple_ty))
}

// Desugar a list of bindings into a tuple
fn tupleify_binding(items: &SrcNode<Vec<SrcNode<ast::Binding>>>, infer: &mut Infer, scope: &Scope) -> InferBinding {
    let hir_items = items
        .iter()
        .map(|item| item.to_hir(infer, scope))
        .collect::<Vec<_>>();
    let tuple_ty = infer.insert(items.span(), TyInfo::Tuple(hir_items
        .iter()
        .map(|item| item.meta().1)
        .collect()));
    let binding = hir::Binding {
        pat: SrcNode::new(hir::Pat::Tuple(hir_items), items.span()),
        name: None,
    };
    InferNode::new(binding, (items.span(), tuple_ty))
}
