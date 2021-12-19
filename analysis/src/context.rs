pub use super::*;

pub struct Context {
    pub classes: Classes,
    pub datas: Datas,
    pub tys: Types,
    pub defs: Defs,
    pub errors: Vec<Error>,
    pub root_span: Span,
}

impl Context {
    pub fn from_module(module: &SrcNode<ast::Module>) -> (Self, Vec<Error>) {
        let mut this = Self {
            classes: Classes::default(),
            datas: Datas::default(),
            tys: Types::default(),
            defs: Defs::default(),
            errors: Vec::default(),
            root_span: module.span(),
        };

        let mut errors = Vec::new();

        let mut classes = Vec::new();
        let mut aliases = Vec::new();
        let mut datas = Vec::new();
        let mut defs = Vec::new();
        // Declare items before declaration
        for (attr, class) in module.classes() {
            let (gen_scope, mut errs) = GenScope::from_ast(&class.generics);
            errors.append(&mut errs);
            let gen_scope = this.tys.insert_gen_scope(gen_scope);
            if let Err(err) = this.classes.declare(class.name.clone(), gen_scope) {
                errors.push(err);
            } else {
                // Only mark for further processing if no errors occurred during declaration
                classes.push((attr, class));
            }
        }
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

        // Now that we have declarations for all classes and data types, we can check generic scope constraints
        let mut gen_scope_errors = this.tys.check_gen_scopes(&this.classes);
        this.errors.append(&mut gen_scope_errors);

        // IMPORTANT: Aliases must be declared before everything else because their eagerly expand to the type they alias

        // Define items
        for (attr, alias) in aliases {
            let gen_scope = this.datas.name_gen_scope(*alias.name);

            let mut infer = Infer::new(&mut this, Some(gen_scope), None);

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

            let mut infer = Infer::new(&mut this, Some(gen_scope), None);
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
        for (attr, class) in classes {
            let gen_scope = this.classes.name_gen_scope(*class.name);

            let mut infer = Infer::new(&mut this, Some(gen_scope), Some(class.name.span()));

            let values = class.items
                .iter()
                .filter_map(|item| match item {
                    ast::ClassItem::Value { name, ty } => Some((
                        name.clone(),
                        ty.to_hir(&mut infer, &Scope::Empty),
                    )),
                })
                .collect::<Vec<_>>();

            let (mut checked, mut errs) = infer.into_checked();
            errors.append(&mut errs);

            let items = values
                .into_iter()
                .map(|(name, ty)| ClassItem::Value {
                    name,
                    ty: SrcNode::new(checked.reify(ty.meta().1), ty.meta().0),
                })
                // TODO:
                //.zip(types.into_iter())
                .collect::<Vec<_>>();

            this.classes.define(
                this.classes
                    .lookup(*class.name)
                    .expect("Class must be pre-declared before definition"),
                Class {
                    name: class.name.clone(),
                    attr: attr.clone(),
                    gen_scope,
                    items,
                },
            );
        }
        for (attr, member) in module.members() {
            let class_id = if let Some(class_id) = this.classes.lookup(*member.class) {
                class_id
            } else {
                errors.push(Error::NoSuchClass(member.class.clone()));
                continue;
            };

            let (gen_scope, mut errs) = GenScope::from_ast(&member.generics);
            errors.append(&mut errs);
            let gen_scope = this.tys.insert_gen_scope(gen_scope);

            let mut infer = Infer::new(&mut this, Some(gen_scope), None);

            let member_ty = member.member.to_hir(&mut infer, &Scope::Empty);

            let (mut checked, mut errs) = infer.into_checked();
            errors.append(&mut errs);

            let member_ty = checked.reify(member_ty.meta().1);

            let member_ = Member {
                gen_scope,
                member: member_ty,
                items: member.items
                    .iter()
                    .map(|item| match item {
                        ast::MemberItem::Value { name, val } => {
                            let mut infer = Infer::new(&mut this, Some(gen_scope), None);

                            let val = val.to_hir(&mut infer, &Scope::Empty);
                            let class = infer.ctx().classes.get(class_id).unwrap();
                            if let Some(field_ty) = class.field(**name).cloned() {
                                let self_ty = member.member.to_hir(&mut infer, &Scope::Empty).meta().1;
                                let val_ty = infer.instantiate(
                                    *field_ty,
                                    field_ty.span(),
                                    &|_, _, _| panic!("Generics not yet supported on classes"),
                                    Some(self_ty),
                                );
                                infer.make_eq(val.meta().1, val_ty, EqInfo::new(name.span(), format!("Type of member item must match class")));
                            }


                            let (mut checked, mut errs) = infer.into_checked();
                            errors.append(&mut errs);

                            let val = val.reify(&mut checked);

                            // TODO: Detect duplicates!
                            (**name, MemberItem::Value { name: name.clone(), val })
                        },
                    })
                    .collect(),
            };

            this.classes.declare_member(class_id, member_);
        }
        for (attr, def) in defs {
            let id = this.defs
                .lookup(*def.name)
                .expect("Def must be pre-declared before definition");
            let gen_scope = this.defs.get(id).gen_scope;

            let mut infer = Infer::new(&mut this, Some(gen_scope), None);

            let ty_hint = def.ty_hint.to_hir(&mut infer, &Scope::Empty);

            let gen_tys = (0..infer.ctx().tys.get_gen_scope(gen_scope).len())
                .map(|i| {
                    let span = infer.ctx().tys.get_gen_scope(gen_scope).get(i).name.span();
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

    pub fn concretize(&self) -> (ConContext, Vec<Error>) {
        ConContext::from_ctx(self)
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

    // TODO: This should probably be implemented on `Infer` so it can work with partially inferred types
    // TODO: Correctly handle class generics
    pub fn lookup_access(&self, ty: TyId, field: SrcNode<Ident>) -> Option<(ClassId, SrcNode<TyId>)> {
        match self.tys.get(ty) {
            Ty::Gen(idx, scope) => {
                self.tys
                    .get_gen_scope(scope)
                    .get(idx)
                    .constraints
                    .as_ref()
                    .expect("Lookup on unchecked gen scope")
                    .iter()
                    .find_map(|c| match c {
                        Constraint::MemberOf(class) => self.classes
                            .get(*class)
                            .unwrap()
                            .items
                            .iter()
                            .find_map(|item| match item {
                                ClassItem::Value { name, ty } if **name == *field => Some((*class, ty.clone())),
                                _ => None,
                            }),
                    })
            },
            _ => None,
        }
    }
}
