use super::*;

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct ItemPath(pub Vec<Ident>);

impl ItemPath {
    pub fn add(&self, part: Ident) -> Self {
        let mut path = self.clone();
        path.0.push(part);
        path
    }
}

#[derive(Debug)]
pub struct Pkg {
    // TODO: Also list pkg dependencies
    pub defs: Store<Def, ItemPath>,
    pub root_module_span: Span,
}

#[derive(Debug)]
pub struct Def {
    decl_span: Span,
    pub body: Option<Expr>,
}

#[derive(Debug)]
pub enum Expr {
    Error,
    Nat(u64),
    Local(Ident),
    Def(Id<Def>),
    Let(Ident, Box<Expr>, Box<Expr>),
}

pub struct PkgCtx<'build> {
    pub build: &'build Build,
    pub pkg: Pkg,
    pub is_err: bool,
}

pub struct Scope<'build, 'ctx> {
    ctx: &'ctx mut PkgCtx<'build>,
    module: ItemPath,
    locals: Vec<Ident>,
}

impl<'build, 'ctx> Scope<'build, 'ctx> {
    fn new(ctx: &'ctx mut PkgCtx<'build>, module: ItemPath) -> Self {
        Self {
            ctx,
            module,
            locals: Vec::new(),
        }
    }

    fn resolve_local(&self, name: &Ident) -> Option<Ident> {
        self.locals
            .iter()
            .rev()
            .find(|local| *local == name)
            .cloned()
    }

    fn resolve_def(&self, name: Ident) -> Option<Id<Def>> {
        if let Some(def_id) = self.ctx.pkg.defs.lookup(&self.module.add(name.clone())) {
            Some(def_id)
        } else {
            None
        }
    }

    fn with_locals<R>(
        &mut self,
        locals: impl IntoIterator<Item = Ident>,
        f: impl FnOnce(&mut Self) -> R,
    ) -> R {
        let old_locals = self.locals.len();
        self.locals.extend(locals);
        let r = f(self);
        self.locals.truncate(old_locals);
        r
    }

    fn lower_expr(&mut self, expr: &SrcNode<syntax::Expr>) -> Expr {
        match &expr.inner {
            syntax::Expr::Nat(x) => Expr::Nat(*x),
            syntax::Expr::Local(local) => {
                if let Some(local) = self.resolve_local(local) {
                    Expr::Local(local)
                } else if let Some(def) = self.resolve_def(local.clone()) {
                    Expr::Def(def)
                } else {
                    self.ctx
                        .emit_error(Error::unresolved_local(local.clone(), expr.span.clone()));
                    Expr::Error
                }
            }
            syntax::Expr::Let(local, rhs, then) => {
                let rhs = self.lower_expr(rhs);
                let then = self.with_locals([local.inner.clone()], |scope| scope.lower_expr(then));
                Expr::Let(local.inner.clone(), Box::new(rhs), Box::new(then))
            }
        }
    }
}

impl<'build> PkgCtx<'build> {
    pub fn new(build: &'build Build, root_module_span: Span) -> Self {
        Self {
            build,
            pkg: Pkg {
                defs: Store::default(),
                root_module_span,
            },
            is_err: false,
        }
    }

    pub fn emit_error(&mut self, err: Error) {
        // Latch the error, preventing compilation
        self.is_err = true;
        err.emit();
    }

    // Recursively declare module elements into the package
    pub fn declare_module(&mut self, path: ItemPath, module: &SrcNode<syntax::Module>) {
        // Declare items
        for item in &module.items {
            match &item.inner {
                syntax::Item::Def(def) => match self.pkg.defs.add(
                    path.add((*def.name).clone()),
                    Def {
                        decl_span: def.name.span.clone(),
                        body: None,
                    },
                ) {
                    Ok(_) => {}
                    Err(old) => {
                        let old_span = old.decl_span.clone();
                        self.emit_error(Error::duplicate_def(
                            def.name.inner.clone(),
                            old_span,
                            def.name.span.clone(),
                        ));
                    }
                },
            }
        }
    }

    pub fn lower_module(&mut self, path: ItemPath, module: &SrcNode<syntax::Module>) {
        // Define items
        for item in &module.items {
            match &item.inner {
                syntax::Item::Def(def) => {
                    let hir_def = Scope::new(self, path.clone()).lower_expr(&def.body);
                    let def_id = self
                        .pkg
                        .defs
                        .lookup(&path.add((*def.name).clone()))
                        .unwrap();
                    self.pkg.defs.get_mut(def_id).body.get_or_insert(hir_def);
                }
            }
        }
    }
}
