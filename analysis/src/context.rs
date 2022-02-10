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
        let mut members_init = Vec::new();
        let mut defs_init = Vec::new();
        // Declare items before declaration
        for (attr, class) in module.classes() {
            let (gen_scope, mut errs) = GenScope::from_ast(&class.generics);
            assert_eq!(gen_scope.len(), 0, "Type parameters on classes not permitted yet");
            errors.append(&mut errs);
            let gen_scope = this.tys.insert_gen_scope(gen_scope);
            match this.classes.declare(class.name.clone(), Class {
                name: class.name.clone(),
                obligations: None,
                attr: attr.to_vec(),
                gen_scope,
                items: None,
            }) {
                Err(err) => errors.push(err),
                // Only mark for further processing if no errors occurred during declaration
                Ok(class_id) => classes.push((attr, class, class_id, gen_scope)),
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
            members_init.push((attr, member, gen_scope));
        }
        for (attr, def) in module.defs() {
            let (gen_scope, mut errs) = GenScope::from_ast(&def.generics);
            errors.append(&mut errs);
            let gen_scope = this.tys.insert_gen_scope(gen_scope);
            defs_init.push((attr, def, gen_scope));
        }

        // Alias definition must go before members and defs because they might have type hints that make use of type
        // aliases
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
                    attr: attr.to_vec(),
                    gen_scope,
                    ty,
                },
            );
        }

        // Now that we have declarations for all classes and data types, we can check generic scope constraints
        let mut gen_scope_errors = this.tys.check_gen_scopes(&this.classes);
        this.errors.append(&mut gen_scope_errors);

        // Derive class obligations
        for (attr, class, class_id, gen_scope) in classes {
            this.classes.define_obligations(
                class_id,
                class
                    .obligation
                    .iter()
                    .filter_map(|obl| match this.classes.lookup(**obl) {
                        Some(class) => Some(SrcNode::new(Obligation::MemberOf(class), obl.span())),
                        None => {
                            errors.push(Error::NoSuchClass(obl.clone()));
                            None
                        },
                    })
                    .collect(),
            );

            let mut infer = Infer::new(&mut this, Some(gen_scope), Some(class.name.span()));

            let values = class.items
                .iter()
                .filter_map(|item| match item {
                    ast::ClassItem::Value { name, ty } => Some((
                        name.clone(),
                        ty.to_hir(&mut infer, &Scope::Empty),
                    )),
                    ast::ClassItem::Type { name, obligations } => {
                        errors.push(Error::Unsupported(name.span(), "associated types"));
                        None
                    },
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

            this.classes.define_items(class_id, items);
        }

        let mut members = Vec::new();
        for (attr, member, gen_scope) in members_init {
            let class_id = if let Some(class_id) = this.classes.lookup(*member.class) {
                class_id
            } else {
                errors.push(Error::NoSuchClass(member.class.clone()));
                continue;
            };

            let mut infer = Infer::new(&mut this, Some(gen_scope), None);

            let member_ty = member.member.to_hir(&mut infer, &Scope::Empty);
            for obl in infer.ctx().classes.get(class_id).obligations.clone().expect("Obligations must be known") {
                match obl.inner() {
                    Obligation::MemberOf(class) => infer.make_impl(member_ty.meta().1, *class, obl.span()),
                }
            }

            let (mut checked, mut errs) = infer.into_checked();
            errors.append(&mut errs);

            let member_ty = checked.reify(member_ty.meta().1);

            let member_id = this.classes.declare_member(class_id, Member {
                gen_scope,
                attr: attr.to_vec(),
                member: member_ty,
                items: None,
            });
            members.push((member, class_id, member_id, gen_scope));
        }
        let mut defs = Vec::new();
        for (attr, def, gen_scope) in defs_init {
            // If the type hint is fully specified, check it
            let ty_hint = if def.ty_hint.is_fully_specified() {
                let mut infer = Infer::new(&mut this, Some(gen_scope), None);
                let ty_hint = def.ty_hint.to_hir(&mut infer, &Scope::Empty);

                let (mut checked, mut errs) = infer.into_checked();
                errors.append(&mut errs);

                Some(checked.reify(ty_hint.meta().1))
            } else {
                None
            };

            if let Err(err) = this.defs.declare(Def {
                name: def.name.clone(),
                attr: attr.to_vec(),
                gen_scope,
                ty_hint,
                body: None,
            }) {
                errors.push(err);
            } else {
                // Only mark for further processing if no errors occurred during declaration
                defs.push((attr, def));
            }
        }

        // Define items
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
                    attr: attr.to_vec(),
                    gen_scope,
                    cons,
                },
            ) {
                errors.append(&mut errs);
            }
        }
        for (member, class_id, member_id, gen_scope) in members {
            let mut infer = Infer::new(&mut this, Some(gen_scope), None);

            let member_ty = member.member.to_hir(&mut infer, &Scope::Empty);
            for obl in infer.ctx().classes.get(class_id).obligations.clone().expect("Obligations must be known") {
                match obl.inner() {
                    Obligation::MemberOf(class) => infer.make_impl(member_ty.meta().1, *class, obl.span()),
                }
            }

            let (mut checked, mut errs) = infer.into_checked();
            errors.append(&mut errs);

            let items = member.items
                .iter()
                .filter_map(|item| {
                    let class = this.classes.get(class_id);
                    match item {
                        ast::MemberItem::Value { name, val } => if class.field(**name).is_none() {
                            errors.push(Error::NoSuchClassItem(name.clone(), class.name.clone()));
                            None
                        } else {
                            // TODO: check that value is a member of the class

                            let mut infer = Infer::new(&mut this, Some(gen_scope), None);

                            let val = val.to_hir(&mut infer, &Scope::Empty);
                            let class = infer.ctx().classes.get(class_id);
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
                            Some((**name, MemberItem::Value { name: name.clone(), val }))
                        },
                        ast::MemberItem::Type { name, ty } => {
                            errors.push(Error::Unsupported(name.span(), "associated types"));
                            None
                        },
                    }
                })
                .collect::<HashMap<_, _>>();

            let class = this.classes.get(class_id);
            for item in class.items.as_ref().expect("Class items must be known here") {
                match item {
                    ClassItem::Value { name, .. } => if !items
                        .iter()
                        .any(|(_, item)| match item {
                            MemberItem::Value { name: member_item_name, .. } => member_item_name == name,
                        })
                    {
                        errors.push(Error::MissingClassItem(member.member.span(), class.name.clone(), name.clone()));
                    },
                }
            }

            this.classes.define_member_items(member_id, class_id, items);
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
}
