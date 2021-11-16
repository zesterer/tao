pub use super::*;

pub struct Context {
    pub datas: Datas,
    pub tys: Types,
    pub defs: Defs,
    pub errors: Vec<Error>,
    pub root_span: Span,
}

impl Context {
    pub fn from_module(module: &SrcNode<ast::Module>) -> (Self, Vec<Error>) {
        let mut this = Self {
            datas: Datas::default(),
            tys: Types::default(),
            defs: Defs::default(),
            errors: Vec::default(),
            root_span: module.span(),
        };

        let mut errors = Vec::new();

        let mut aliases = Vec::new();
        let mut datas = Vec::new();
        let mut defs = Vec::new();
        // Define aliases, data types, and defs before declaration
        for (attr, alias) in module.aliases() {
            if let Err(err) = this.datas.declare_alias(*alias.name, alias.name.span()) {
                errors.push(err);
            } else {
                // Only mark for further processing if no errors occurred during declaration
                aliases.push((attr, alias));
            }
        }
        for (attr, data) in module.datas() {
            if let Err(err) = this.datas.declare_data(*data.name, data.name.span()) {
                errors.push(err);
            } else {
                // Only mark for further processing if no errors occurred during declaration
                datas.push((attr, data));
            }
        }
        for (attr, def) in module.defs() {
            let gen_scope = this.tys.insert_gen_scope(GenScope::from_ast(&def.generics));
            if let Err(err) = this.defs.declare(Def {
                name: def.name.clone(),
                attr: attr.clone(),
                gen_scope,
                ty_hint: def.ty_hint.clone(),
                body: None,
            }) {
                errors.push(err);
            } else {
                // Only mark for further processing if no errors occurred during declaration
                defs.push((attr, def));
            }
        }

        // IMPORTANT: Aliases must be declared before everything else because their eagerly expand to the type they alias

        // Declare aliases and data types
        for (attr, alias) in aliases {
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
                    attr: attr.clone(),
                    gen_scope,
                    ty,
                },
            );
        }
        for (attr, data) in datas {
            let gen_scope = this.tys.insert_gen_scope(GenScope::from_ast(&data.generics));

            let mut infer = Infer::new(&mut this, Some(gen_scope));
            let variants = data.variants
                .iter()
                .map(|(name, ty)| {
                    let ty = ty.to_hir(&mut infer, &Scope::Empty);
                    (name.clone(), ty)
                })
                .collect::<Vec<_>>();

            let (mut checked, mut errs) = infer.into_checked();
            errors.append(&mut errs);

            let cons = variants
                .into_iter()
                .map(|(name, ty)| (name, checked.reify(ty.meta().1)))
                .collect();

            if let Err(mut errs) = this.datas.define_data(
                this.datas
                    .lookup_data(*data.name)
                    .expect("Data must be pre-declared before definition"),
                data.name.span(),
                Data {
                    name: *data.name,
                    attr: attr.clone(),
                    gen_scope,
                    cons,
                },
            ) {
                errors.append(&mut errs);
            }
        }
        for (attr, def) in defs {
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

            // println!("{}: {}", *def.name, this.tys.display(&this.datas, body.meta().1));

            this.defs.define_body(id, body);
        }

        errors.append(&mut this.errors);

        (this, errors)
    }

    pub fn emit(&mut self, error: Error) { self.errors.push(error) }
}
