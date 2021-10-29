use super::*;

// TODO: use `ToHir`?
fn litr_ty_info(litr: &ast::Literal, infer: &mut Infer, span: Span) -> TyInfo {
    match litr {
        ast::Literal::Nat(_) => TyInfo::Prim(ty::Prim::Nat),
        ast::Literal::Num(_) => TyInfo::Prim(ty::Prim::Num),
        ast::Literal::Bool(_) => TyInfo::Prim(ty::Prim::Bool),
        ast::Literal::Char(_) => TyInfo::Prim(ty::Prim::Char),
        ast::Literal::Str(_) => TyInfo::List(infer.insert(span, TyInfo::Prim(ty::Prim::Char))),
    }
}

pub enum Scope<'a> {
    Empty,
    Binding(&'a Scope<'a>, SrcNode<Ident>, TyVar),
    Many(&'a Scope<'a>, Vec<(SrcNode<Ident>, TyVar)>),
}

impl<'a> Scope<'a> {
    pub fn empty() -> Self { Self::Empty }

    fn with<'b>(&'a self, name: SrcNode<Ident>, ty: TyVar) -> Scope<'b> where 'a: 'b {
        Scope::Binding(self, name, ty)
    }

    fn with_many<'b>(&'a self, many: Vec<(SrcNode<Ident>, TyVar)>) -> Scope<'b> where 'a: 'b {
        Scope::Many(self, many)
    }

    fn find(&self, name: &Ident) -> Option<TyVar> {
        match self {
            Self::Empty => None,
            Self::Binding(_, local, ty) if &**local == name => Some(*ty),
            Self::Binding(parent, _, _) => parent.find(name),
            Self::Many(parent, locals) => if let Some((_, ty)) = locals.iter().find(|(local, _)| &**local == name) {
                Some(*ty)
            } else {
                parent.find(name)
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
            ast::Type::Unknown => TyInfo::Unknown,
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

                    if let Some(scope) = infer
                        .gen_scope()
                        .filter(|scope| infer.ctx().tys.get_gen_scope(*scope).index(**name).is_some())
                    {
                        TyInfo::Gen(**name, scope)
                    } else if let Some(alias) = infer.ctx().datas.lookup_alias(**name) {
                        if let Some(alias) = infer.ctx().datas.get_alias(alias) {
                            let (alias_ty, alias_gen_scope) = (alias.ty, alias.gen_scope);
                            let get_gen = |gen, ctx: &Context| {
                                let generics = ctx.tys.get_gen_scope(alias_gen_scope);
                                params[generics.index(gen).unwrap()]
                            };
                            TyInfo::Ref(infer.instantiate(alias_ty, &get_gen))
                        } else {
                            infer.ctx_mut().emit(Error::RecursiveAlias(alias, name.span()));
                            TyInfo::Error
                        }
                    } else if let Some(data) = infer.ctx().datas.lookup_data(**name) {
                        TyInfo::Data(data, params)
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
            ast::Pat::Wildcard => (TyInfo::Unknown, hir::Pat::Wildcard),
            ast::Pat::Literal(litr) => (litr_ty_info(litr, infer, self.pat.span()), hir::Pat::Literal(*litr)),
            ast::Pat::Single(inner) => {
                let binding = inner.to_hir(infer, scope);
                // TODO: don't use `Ref` to link types
                (TyInfo::Ref(binding.meta().1), hir::Pat::Single(binding))
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
                    .collect::<HashMap<_, _>>();
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
                        infer.make_eq(item.meta().1, item_ty);
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
                        infer.make_eq(item.meta().1, item_ty);
                        item
                    })
                    .collect::<Vec<_>>();
                let tail = tail.as_ref().map(|tail| {
                    let tail = tail.to_hir(infer, scope);
                    let ty = infer.insert(tail.meta().0, TyInfo::List(item_ty));
                    infer.make_eq(tail.meta().1, ty);
                    tail
                });
                (TyInfo::List(item_ty), hir::Pat::ListFront(items, tail))
            },
            ast::Pat::Deconstruct(name, inner) => if let Some(data) = infer.ctx().datas.lookup_cons(**name) {
                let generics_count = infer.ctx().tys.get_gen_scope(infer.ctx().datas.get_data(data).gen_scope).len();
                let generic_tys = (0..generics_count)
                    .map(|_| infer.unknown(name.span()))
                    .collect::<Vec<_>>();

                let inner_ty = infer.ctx().datas.get_data(data).cons[&name];

                // Recreate type in context
                let inner_ty = {
                    let data = infer.ctx().datas.get_data(data);
                    let data_gen_scope = data.gen_scope;
                    let get_gen = |gen, ctx: &Context| {
                        let generics = ctx.tys.get_gen_scope(data_gen_scope);
                        generic_tys[generics.index(gen).unwrap()]
                    };
                    infer.instantiate(inner_ty, &get_gen)
                };

                let inner = inner.to_hir(infer, scope);
                infer.make_eq(inner.meta().1, inner_ty);

                (TyInfo::Data(data, generic_tys), hir::Pat::Decons(SrcNode::new(data, name.span()), inner))
            } else {
                infer.ctx_mut().emit(Error::NoSuchCons(name.clone()));
                // TODO: Don't use a hard, preserve inner expression
                (TyInfo::Error, hir::Pat::Error)
            },
        };

        let ty = infer.insert(self.span(), info);

        if let Some(ty_hint) = &self.ty {
            let hint = ty_hint.to_hir(infer, scope);
            infer.make_eq(ty, hint.meta().1);
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
            ast::Expr::Local(local) => if let Some(ty) = scope.find(&local) {
                (TyInfo::Ref(ty), hir::Expr::Local(*local))
            } else if let Some(def) = infer.ctx().defs.lookup(*local) {
                let generics_count = infer.ctx().tys.get_gen_scope(infer.ctx().defs.get(def).gen_scope).len();
                let generic_tys = (0..generics_count)
                    .map(|_| infer.unknown(self.span()))
                    .collect::<Vec<_>>();

                // Recreate type in context
                let def = infer.ctx().defs.get(def);
                let ty = if let Some(body) = def.body.as_ref() {
                    let def_gen_scope = def.gen_scope;
                    let def_ty = body.meta().1;
                    let get_gen = |gen, ctx: &Context| {
                        let generics = ctx.tys.get_gen_scope(def_gen_scope);
                        generic_tys[generics.index(gen).unwrap()]
                    };
                    infer.instantiate(def_ty, &get_gen)
                } else {
                    infer.unknown(self.span())
                };

                (TyInfo::Ref(ty), hir::Expr::Global(*local))
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
                        infer.make_eq(item.meta().1, item_ty);
                        item
                    })
                    .collect::<Vec<_>>();
                (TyInfo::List(item_ty), hir::Expr::List(items))
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
                            infer.make_eq(binding.meta().1, val.meta().1);
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
                            infer.make_eq(pred.meta().1, binding.meta().1);
                            let body = body.to_hir(infer, &scope.with_many(binding.get_bindings()));
                            infer.make_eq(body.meta().1, output_ty);
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
                infer.make_eq(pred.meta().1, pred_ty);
                let a = a.to_hir(infer, scope);
                let b = b.to_hir(infer, scope);
                infer.make_eq(a.meta().1, output_ty);
                infer.make_eq(b.meta().1, output_ty);
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
                                infer.make_eq(pred.meta().1, binding.meta().1);
                                let body = body.to_hir(infer, &scope.with_many(binding.get_bindings()));
                                infer.make_eq(body.meta().1, output_ty);
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
                                    let binding = hir::Binding::wildcard(pseudo);
                                    let f = infer.insert(self.span(), TyInfo::Func(pseudo_ty, body.meta().1));
                                    InferNode::new(hir::Expr::Func(InferNode::new(binding, pred_meta), body), (self.span(), f))
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
                infer.make_eq(f.meta().1, func);

                (TyInfo::Ref(output_ty), hir::Expr::Apply(f, param))
            },
            ast::Expr::Cons(name, inner) => if let Some(data) = infer.ctx().datas.lookup_cons(**name) {
                let generics_count = infer.ctx().tys.get_gen_scope(infer.ctx().datas.get_data(data).gen_scope).len();
                let generic_tys = (0..generics_count)
                    .map(|_| infer.unknown(name.span()))
                    .collect::<Vec<_>>();

                let inner_ty = infer.ctx().datas.get_data(data).cons[&name];

                // Recreate type in context
                let inner_ty = {
                    let data = infer.ctx().datas.get_data(data);
                    let data_gen_scope = data.gen_scope;
                    let get_gen = |gen, ctx: &Context| {
                        let generics = ctx.tys.get_gen_scope(data_gen_scope);
                        generic_tys[generics.index(gen).unwrap()]
                    };
                    infer.instantiate(inner_ty, &get_gen)
                };

                let inner = inner.to_hir(infer, scope);
                infer.make_eq(inner.meta().1, inner_ty);

                (TyInfo::Data(data, generic_tys), hir::Expr::Cons(SrcNode::new(data, name.span()), inner))
            } else {
                infer.ctx_mut().emit(Error::NoSuchCons(name.clone()));
                // TODO: Don't use a hard, preserve inner expression
                (TyInfo::Error, hir::Expr::Error)
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
