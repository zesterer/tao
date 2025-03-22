use super::*;

#[derive(Debug, Default)]
pub struct Pkg {
    // TODO: Also list pkg dependencies
    defs: HashMap<Ident, Def>,
}

#[derive(Debug)]
pub struct Def {
    decl_span: Span,
    body: Option<Expr>,
}

#[derive(Debug)]
pub enum Expr {
    Nat(u64),
}

pub struct PkgCtx<'build> {
    pub build: &'build Build,
    pub pkg: Pkg,
    pub is_err: bool,
}

impl<'build> PkgCtx<'build> {
    pub fn new(build: &'build Build) -> Self {
        Self {
            build,
            pkg: Pkg::default(),
            is_err: false,
        }
    }

    fn emit_error(&mut self, err: Error) {
        // Latch the error, preventing compilation
        self.is_err = true;
        err.emit();
    }

    fn lower_expr(&mut self, expr: &syntax::Expr) -> Expr {
        match expr {
            syntax::Expr::Nat(x) => Expr::Nat(*x),
        }
    }

    pub fn lower_module(&mut self, module: &SrcNode<syntax::Module>) {
        // Declare items
        for item in &module.items {
            match &item.inner {
                syntax::Item::Def(def) => match self.pkg.defs.entry(def.name.inner.clone()) {
                    Entry::Vacant(e) => {
                        e.insert(Def {
                            decl_span: def.name.span.clone(),
                            body: None,
                        });
                    }
                    Entry::Occupied(e) => {
                        let old_span = e.get().decl_span.clone();
                        self.emit_error(Error::duplicate_def(
                            def.name.inner.clone(),
                            old_span,
                            def.name.span.clone(),
                        ));
                    }
                },
            }
        }

        // Define items
        for item in &module.items {
            match &item.inner {
                syntax::Item::Def(def) => {
                    let hir_def = self.lower_expr(&def.body);
                    self.pkg
                        .defs
                        .get_mut(&def.name)
                        .unwrap()
                        .body
                        .get_or_insert(hir_def);
                }
            }
        }
    }
}
