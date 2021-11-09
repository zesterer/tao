pub use super::*;

#[derive(Default)]
pub struct Context {
    pub datas: Datas,
    pub tys: Types,
    pub defs: Defs,
    pub errors: Vec<Error>,
}

impl Context {
    pub fn from_module(module: &ast::Module) -> (Self, Vec<Error>) {
        let mut this = Self::default();

        let mut errors = Vec::new();

        // Define aliases, data types, and defs before declaration
        for alias in module.aliases() {
            this.datas.declare_alias(*alias.name, alias.name.span());
        }
        for data in module.datas() {
            this.datas.declare_data(*data.name);
        }
        for def in module.defs() {
            let gen_scope = this.tys.insert_gen_scope(GenScope::from_ast(&def.generics));
            this.defs.declare(*def.name, gen_scope, def.ty_hint.clone());
        }

        // IMPORTANT: Aliases must be declared before everything else because their eagerly expand to the type they alias

        // Declare aliases and data types
        for alias in module.aliases() {
            let gen_scope = this.tys.insert_gen_scope(GenScope::from_ast(&alias.generics));

            let mut infer = Infer::new(&mut this, Some(gen_scope));

            let ty = alias.ty.to_hir(&mut infer, &Scope::Empty);

            let (mut checked, mut errs) = infer.into_checked();
            errors.append(&mut errs);

            let ty = checked.reify(ty.meta().1);

            this.datas.define_alias(
                this.datas
                    .lookup_alias(*alias.name)
                    .expect("Alias must be pre-declared before definition"),
                Alias {
                    name: *alias.name,
                    gen_scope,
                    ty,
                },
            );
        }
        for data in module.datas() {
            let gen_scope = this.tys.insert_gen_scope(GenScope::from_ast(&data.generics));

            let mut infer = Infer::new(&mut this, Some(gen_scope));
            let variants = data.variants
                .iter()
                .map(|(name, ty)| {
                    let ty = ty.to_hir(&mut infer, &Scope::Empty);
                    (**name, ty)
                })
                .collect::<Vec<_>>();

            let (mut checked, mut errs) = infer.into_checked();
            errors.append(&mut errs);

            let cons = variants
                .into_iter()
                .map(|(name, ty)| (name, checked.reify(ty.meta().1)))
                .collect();

            this.datas.define_data(
                this.datas
                    .lookup_data(*data.name)
                    .expect("Data must be pre-declared before definition"),
                Data {
                    name: *data.name,
                    gen_scope,
                    cons,
                },
            );
        }
        for def in module.defs() {
            let id = this.defs
                .lookup(*def.name)
                .expect("Def must be pre-declared before definition");
            let gen_scope = this.defs.get(id).gen_scope;

            let mut infer = Infer::new(&mut this, Some(gen_scope));

            let ty_hint = def.ty_hint.to_hir(&mut infer, &Scope::Empty);

            let body = def.body.to_hir(&mut infer, &Scope::Recursive(def.name.clone(), ty_hint.meta().1));
            infer.make_eq(ty_hint.meta().1, body.meta().1);

            let (mut checked, mut errs) = infer.into_checked();
            errors.append(&mut errs);

            let body = body.reify(&mut checked);

            println!("{}: {}", *def.name, this.tys.display(&this.datas, body.meta().1));

            this.defs.define_body(id, body);
        }

        errors.append(&mut this.errors);

        (this, errors)
    }

    pub fn emit(&mut self, error: Error) { self.errors.push(error) }
}
