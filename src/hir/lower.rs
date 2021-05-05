use crate::{
    ast,
    Error,
    ErrorCode,
    util::{SrcNode, InferNode, Span},
};
use super::*;

pub enum Scope<'a> {
    Root,
    Local(Ident, TyVar, &'a Self),
    Locals(HashMap<Ident, TyVar>, &'a Self),
}

impl<'a> Scope<'a> {
    fn with<'b>(&'b self, ident: Ident, var: TyVar) -> Scope<'b> where 'a: 'b {
        Scope::Local(ident, var, self)
    }

    fn with_many<'b>(&'b self, locals: HashMap<Ident, TyVar>) -> Scope<'b> where 'a: 'b {
        Scope::Locals(locals, self)
    }

    fn get_local(&self, local: Ident) -> Option<TyVar> {
        match self {
            Scope::Root => None,
            Scope::Local(ident, var, _) if ident == &local => Some(*var),
            Scope::Local(_, _, parent) => parent.get_local(local),
            Scope::Locals(locals, parent) => locals
                .get(&local)
                .copied()
                .or_else(|| parent.get_local(local)),
        }
    }
}

impl ast::Literal {
    fn ty_var(&self, ctx: &mut InferCtx, span: Span) -> TyVar {
        let ty = match self {
            ast::Literal::Nat(_) => TyInfo::Prim(Primitive::Nat),
            ast::Literal::Int(_) => TyInfo::Prim(Primitive::Int),
            ast::Literal::Num(_) => TyInfo::Prim(Primitive::Num),
            ast::Literal::Char(_) => TyInfo::Prim(Primitive::Char),
            ast::Literal::Bool(_) => TyInfo::Prim(Primitive::Bool),
            ast::Literal::Str(_) => TyInfo::List(ctx.info_var(TyInfo::Prim(Primitive::Char), span)),
        };
        ctx.info_var(ty, span)
    }
}

impl ast::Type {
    fn ty_var(self: &SrcNode<Self>, ctx: &mut InferCtx) -> TyVar {
        match self.inner() {
            ast::Type::Unknown => ctx.free_var(self.span()),
            ast::Type::List(inner) => {
                let inner = inner.ty_var(ctx);
                ctx.info_var(TyInfo::List(inner), self.span())
            },
            ast::Type::Tuple(fields) => {
                let fields = fields
                    .iter()
                    .map(|field| field.ty_var(ctx))
                    .collect();
                ctx.info_var(TyInfo::Tuple(fields), self.span())
            },
            ast::Type::Record(fields) => {
                let fields = fields
                    .iter()
                    .map(|(name, field)| (**name, field.ty_var(ctx)))
                    .collect();
                ctx.info_var(TyInfo::Record(fields), self.span())
            },
            ast::Type::Func(i, o) => {
                let i = i.ty_var(ctx);
                let o = o.ty_var(ctx);
                ctx.info_var(TyInfo::Func(i, o), self.span())
            },
            ast::Type::Data(item, params) => if item.base.as_ref().map_or(false, |b| b.inner() == &ast::PathBase::This) && item.path.is_empty() && params.is_empty() {
                ctx.info_var(match item.name.inner().as_str() {
                    "Nat" => TyInfo::Prim(Primitive::Nat),
                    "Int" => TyInfo::Prim(Primitive::Int),
                    "Num" => TyInfo::Prim(Primitive::Num),
                    "Bool" => TyInfo::Prim(Primitive::Bool),
                    "Char" => TyInfo::Prim(Primitive::Char),
                    _ => todo!(),
                }, self.span())
            } else {
                todo!()
            },
        }
    }
}

impl ast::Binding {
    fn lower(self: &SrcNode<Self>, ctx: &mut InferCtx) -> InferBinding {
        let (pat, var) = match self.pat.inner() {
            ast::Pat::Wildcard => (Pat::Wildcard, ctx.free_var(self.pat.span())),
            ast::Pat::Literal(litr) => (Pat::Literal(litr.clone()), litr.ty_var(ctx, self.pat.span())),
            ast::Pat::Tuple(fields) => {
                let (tys, fields) = fields
                    .iter()
                    .map(|field| {
                        let field = field.lower(ctx);
                        (field.ty(), field)
                    })
                    .unzip();
                (Pat::Tuple(fields), ctx.info_var(TyInfo::Tuple(tys), self.span()))
            },
            pat => todo!("Implement {:?}", pat),
        };

        let binding = Binding { pat: SrcNode::new(pat, self.pat.span()), binding: self.binding.clone() };
        let ty_hint = self.ty.ty_var(ctx);
        ctx.unify_eq(ty_hint, var, self.span());
        InferNode::new(binding, (self.span(), var))
    }
}

impl InferBinding {
    fn get_bindings_inner(&self, bindings: &mut HashMap<Ident, TyVar>) {
        if let Some(b) = self.binding.as_ref() {
            assert!(bindings
                .insert(*b.inner(), self.ty())
                .is_none(), "Shadowed name in binding");
        }

        match self.pat.inner() {
            Pat::List(items) => items
                .iter()
                .for_each(|item| item.get_bindings_inner(bindings)),
            Pat::ListFront(items, tail) => {
                items
                    .iter()
                    .for_each(|item| item.get_bindings_inner(bindings));
                if let Some(tail) = tail.as_ref() {
                    assert!(bindings
                        .insert(*tail.inner(), self.ty())
                        .is_none(), "Shadowed name in binding");
                }
            },
            Pat::Tuple(fields) => fields
                .iter()
                .for_each(|field| field.get_bindings_inner(bindings)),
            Pat::Record(fields) => todo!(),
            Pat::Deconstruct(_, _) => todo!(),
            _ => {},
        }
    }

    fn get_bindings(&self) -> HashMap<Ident, TyVar> {
        let mut bindings = HashMap::new();
        self.get_bindings_inner(&mut bindings);
        bindings
    }
}

impl ast::Expr {
    fn lower(self: &SrcNode<Self>, ctx: &mut InferCtx, scope: &Scope) -> InferExpr {
        let (this, var) = match self.inner() {
            ast::Expr::Literal(litr) => {
                let litr_ty = litr.ty_var(ctx, self.span());

                // let ty = ctx.free_var(self.span());
                // ctx.unify_flow(litr_ty, ty, self.span());

                (Expr::Literal(litr.clone()), litr_ty)
            },
            ast::Expr::Coerce(expr) => {
                let var = ctx.free_var(self.span());
                let expr = expr.lower(ctx, scope);
                ctx.unify_flow(expr.ty(), var, self.span());
                (Expr::Coerce(expr), var)
            },
            ast::Expr::Item(item) => if item.base.is_none() && item.path.is_empty() {
                match scope.get_local(*item.name.inner()) {
                    Some(var) => (Expr::Local(*item.name.inner()), var),
                    _ => {
                        ctx.emit_error(Error::new(ErrorCode::TypeMismatch, item.name.span(), format!("Cannot find binding `{}` in the current scope", item.name.inner()))
                            .with_primary(item.name.span(), None));
                        (Expr::Error, ctx.error_var(self.span()))
                    },
                }
            } else {
                todo!()
            },
            ast::Expr::Unary(op, a) => {
                let var = ctx.free_var(self.span());
                let a = a.lower(ctx, scope);
                ctx.unary(a.ty(), **op, var, op.span());
                (Expr::Unary(op.clone(), a), var)
            },
            ast::Expr::Binary(op, a, b) => {
                let var = ctx.free_var(self.span());
                let a = a.lower(ctx, scope);
                let b = b.lower(ctx, scope);
                ctx.binary(a.ty(), b.ty(), **op, var, op.span());
                (Expr::Binary(op.clone(), a, b), var)
            },
            ast::Expr::List(items) => {
                let inner_ty = ctx.free_var(self.span());

                let items = items
                    .iter()
                    .map(|item| {
                        let item = item.lower(ctx, scope);
                        ctx.unify_eq_reason(item.ty(), inner_ty, self.span(), EquateReason::List);
                        item
                    })
                    .collect();

                (Expr::List(items), ctx.info_var(TyInfo::List(inner_ty), self.span()))
            },
            ast::Expr::Tuple(fields) => {
                let (tys, fields) = fields
                    .iter()
                    .map(|field| {
                        let field = field.lower(ctx, scope);
                        (field.ty(), field)
                    })
                    .unzip();

                (Expr::Tuple(fields), ctx.info_var(TyInfo::Tuple(tys), self.span()))
            },
            ast::Expr::Let(binding, pred, body) => {
                let pred = pred.lower(ctx, scope);
                let binding = binding.lower(ctx);
                let body = body.lower(ctx, &scope.with_many(binding.get_bindings()));
                ctx.unify_eq(pred.ty(), binding.ty(), body.span());

                let body_ty = body.ty();
                let arm = MatchArm { binding, body };

                (Expr::Match(pred, vec![arm]), body_ty)
            },
            ast::Expr::If(pred, a, b) => {
                let boolean = ctx.info_var(TyInfo::Prim(Primitive::Bool), pred.span());
                let pred = pred.lower(ctx, scope);
                ctx.unify_eq_reason(pred.ty(), boolean, pred.span(), EquateReason::Conditional);

                let expr_ty = ctx.free_var(self.span());
                let a = a.lower(ctx, scope);
                let b = b.lower(ctx, scope);
                ctx.unify_eq(a.ty(), expr_ty, a.span());
                ctx.unify_eq(b.ty(), expr_ty, b.span());

                let arms = vec![
                    MatchArm {
                        binding: InferNode::new(Binding { pat: SrcNode::new(Pat::Literal(Literal::Bool(true)), pred.span()), binding: None }, (pred.span(), boolean)),
                        body: a,
                    },
                    MatchArm {
                        binding: InferNode::new(Binding { pat: SrcNode::new(Pat::Literal(Literal::Bool(false)), pred.span()), binding: None }, (pred.span(), boolean)),
                        body: b,
                    },
                ];

                (Expr::Match(pred, arms), expr_ty)
            },
            ast::Expr::Match(pred, arms) => {
                let pred = pred.lower(ctx, scope);

                let expr_ty = ctx.free_var(self.span());
                let arms = arms
                    .iter()
                    .map(|arm| {
                        let binding = arm.binding.lower(ctx);
                        let body = arm.body.lower(ctx, &scope.with_many(binding.get_bindings()));
                        ctx.unify_eq(pred.ty(), binding.ty(), body.span());
                        ctx.unify_eq(body.ty(), expr_ty, body.span());

                        MatchArm { binding, body }
                    })
                    .collect();

                (Expr::Match(pred, arms), expr_ty)
            },
            ast::Expr::Func(param, body) => {
                let param = param.lower(ctx);
                let body = body.lower(ctx, &scope.with_many(param.get_bindings()));
                let ty = ctx.info_var(TyInfo::Func(param.ty(), body.ty()), self.span());
                (Expr::Func(param, body), ty)
            },
            ast::Expr::Apply(func, arg) => {
                let func = func.lower(ctx, scope);
                let arg = arg.lower(ctx, scope);

                // let arg_coerce = ctx.free_var(arg.span());
                // ctx.unify_flow(arg.ty(), arg_coerce, self.span());

                let ty = ctx.free_var(self.span());
                // TODO: Should we coerce the arg into an appropriate arg type?
                let f_ty = ctx.info_var(TyInfo::Func(arg.ty(), ty), func.span());
                ctx.unify_eq(func.ty(), f_ty, func.span());
                (Expr::Apply(func, arg), ty)
            },
            ast::Expr::Record(fields) => {
                let (tys, fields) = fields
                    .iter()
                    .map(|(name, field)| {
                        let field = field.lower(ctx, scope);

                        ((**name, field.ty()), (**name, (name.clone(), field)))
                    })
                    .unzip();

                (Expr::Record(fields), ctx.info_var(TyInfo::Record(tys), self.span()))
            },
            ast::Expr::Access(a, field) => {
                let a = a.lower(ctx, scope);
                let var = ctx.free_var(self.span());

                /*
                let record = ctx.info_var(TyInfo::Record(std::iter::once((**field, var)).collect()), self.span());
                ctx.unify_flow(a.ty(), record, field.span());
                */

                ctx.field(a.ty(), **field, var, field.span());

                (Expr::Access(a, field.clone()), var)
            },
            ast::Expr::Construct(item, arg) => {
                todo!()
            },
            expr => todo!("Implement {:?}", expr),
        };

        let ty = ctx.free_var(self.span());
        ctx.unify_flow(var, ty, self.span());

        InferNode::new(this, (self.span(), ty))
    }

    pub fn to_hir(self: &SrcNode<Self>, ctx: &mut Ctx) -> TyExpr {
        let mut infer_ctx = InferCtx::from_ctx(ctx);
        let expr = self.lower(&mut infer_ctx, &Scope::Root);

        // infer_ctx.print_constraints();

        let (solved_tys, errors) = infer_ctx.solve();

        for error in errors {
            ctx.emit_error(error);
        }

        expr.check(&solved_tys)
    }
}

impl ast::Module {
    fn to_hir(self: &SrcNode<Self>, prog: &mut Program, ctx: &mut Ctx, path: Intern<Path>) {
        for def in &self.defs {
            let name = Item(path, *def.name.inner());

            if let Some(old_def) = prog.def_ctx.defs.get(&name) {
                ctx.emit_error(Error::new(
                    ErrorCode::DuplicateDef,
                    def.name.span(),
                    format!("Duplicate definition of `{}`", def.name.inner()),
                )
                    .with_secondary(old_def.name.span(), Some(format!("Previous definition of `{}` is here", def.name.inner()))));
                continue;
            }

            prog.def_ctx.defs.insert(
                name,
                Def {
                    name: def.name.clone(),
                    generics: SrcNode::new(Generics {
                        params: HashMap::default(),
                    }, def.name.span()),
                    body: def.body.to_hir(ctx),
                },
            );
        }

        // TODO: data, submodules, aliases, etc.
    }
}

impl ast::Program {
    pub fn to_hir(&self) -> Result<Program, Vec<Error>> {
        let mut prog = Program::default();
        let mut ctx = Ctx::default();
        self.root.to_hir(&mut prog, &mut ctx, Intern::new(Path::Root));
        let errors = ctx.take_errors();
        if errors.is_empty() {
            Ok(prog)
        } else {
            Err(errors)
        }
    }
}
