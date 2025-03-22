use super::*;

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct ItemPath(Vec<Ident>);

impl ItemPath {
    pub fn add(&self, part: Ident) -> Self {
        let mut path = self.clone();
        path.0.push(part);
        path
    }
}

#[derive(Debug, Default)]
pub struct Pkg {
    // TODO: Also list pkg dependencies
    defs: Store<Def, ItemPath>,
}

#[derive(Debug)]
pub struct Def {
    decl_span: Span,
    body: Option<Expr>,
}

#[derive(Debug)]
pub enum Expr {
    Error,
    Nat(u64),
    Local(()),
    Def(Id<Def>),
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

    fn resolve_local(&self, name: &Ident) -> Option<()> {
        self.locals
            .iter()
            .rev()
            .find(|local| *local == name)
            .map(|_| ())
    }

    fn resolve_def(&self, name: Ident) -> Option<Id<Def>> {
        if let Some(def_id) = self.ctx.pkg.defs.lookup(&self.module.add(name.clone())) {
            Some(def_id)
        } else {
            None
        }
    }

    fn lower_expr(&mut self, expr: &SrcNode<syntax::Expr>) -> Expr {
        match &expr.inner {
            syntax::Expr::Nat(x) => Expr::Nat(*x),
            syntax::Expr::Local(local) => if let Some(local) = self.resolve_local(local) {
                Expr::Local(local)
            } else if let Some(def) = self.resolve_def(local.clone()) {
                Expr::Def(def)
            } else {
                self.ctx.emit_error(Error::unresolved_local(local.clone(), expr.span.clone()));
                Expr::Error
            },
        }
    }
}

impl<'build> PkgCtx<'build> {
    pub fn new(build: &'build Build) -> Self {
        Self {
            build,
            pkg: Pkg::default(),
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
                    Def { decl_span: def.name.span.clone(), body: None },
                ) {
                    Ok(_) => {},
                    Err(old) => {
                        let old_span = old.decl_span.clone();
                        self.emit_error(Error::duplicate_def(
                            def.name.inner.clone(),
                            old_span,
                            def.name.span.clone(),
                        ));
                    },
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
                    let def_id = self.pkg.defs.lookup(&path.add((*def.name).clone())).unwrap();
                    self.pkg.defs.get_mut(def_id).body.get_or_insert(hir_def);
                }
            }
        }
    }
}
