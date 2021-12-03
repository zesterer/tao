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
            let (gen_scope, mut errs) = GenScope::from_ast(&alias.generics);
            errors.append(&mut errs);
            let gen_scope = this.tys.insert_gen_scope(gen_scope);
            if let Err(err) = this.datas.declare_alias(*alias.name, alias.name.span(), gen_scope) {
                errors.push(err);
            } else {
                // Only mark for further processing if no errors occurred during declaration
                aliases.push((attr, alias));
            }
        }
        for (attr, data) in module.datas() {
            let (gen_scope, mut errs) = GenScope::from_ast(&data.generics);
            errors.append(&mut errs);
            let gen_scope = this.tys.insert_gen_scope(gen_scope);
            if let Err(err) = this.datas.declare_data(*data.name, data.name.span(), gen_scope) {
                errors.push(err);
            } else {
                // Only mark for further processing if no errors occurred during declaration
                datas.push((attr, data));
            }
        }
        for (attr, def) in module.defs() {
            let (gen_scope, mut errs) = GenScope::from_ast(&def.generics);
            errors.append(&mut errs);
            let gen_scope = this.tys.insert_gen_scope(gen_scope);
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
            let gen_scope = this.datas.name_gen_scope(*alias.name);

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
            let gen_scope = this.datas.name_gen_scope(*data.name);

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

            let gen_tys = (0..infer.ctx().tys.get_gen_scope(gen_scope).len())
                .map(|i| {
                    let span = infer.ctx().tys.get_gen_scope(gen_scope).get(i).span();
                    (span, infer.insert(span, TyInfo::Gen(i, gen_scope, span)))
                })
                .collect();

            let body = def.body.to_hir(&mut infer, &Scope::Recursive(def.name.clone(), ty_hint.meta().1, id, gen_tys));
            infer.make_eq(ty_hint.meta().1, body.meta().1, EqInfo::default());

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

    // Returns (record_ty, field_ty, number_of_indirections)
    pub fn follow_field_access(&self, mut ty: TyId, field: Ident) -> Option<(TyId, TyId, usize)> {
        let mut already_seen = Vec::new();

        loop {
            match self.tys.get(ty) {
                Ty::Data(data, args) => if already_seen.contains(&data) {
                    // We've already seen this data type, it must be recursive. Give up, it has no fields.
                    break None
                } else {
                    already_seen.push(data);
                    let data = self.datas.get_data(data);
                    if data.cons.len() == 1 {
                        ty = data.cons[0].1;
                    } else {
                        // Sum types have no fields
                        break None;
                    }
                },
                Ty::Record(fields) => if let Some((_, field_ty)) = fields.iter().find(|(name, _)| **name == field) {
                    break Some((ty, *field_ty, already_seen.len()));
                } else {
                    // Record has no such field
                    break None;
                },
                _ => break None, // Only `Data` or `Record` can have fields
            }
        }
    }
}
