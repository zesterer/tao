pub use super::*;

pub struct Context {
    pub classes: Classes,
    pub datas: Datas,
    pub tys: Types,
    pub defs: Defs,
    pub effects: Effects,
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
            effects: Effects::default(),
            errors: Vec::default(),
            root_span: module.span(),
        };

        let mut errors = Vec::new();

        let mut classes = Vec::new();
        let mut aliases = Vec::new();
        let mut datas = Vec::new();
        let mut effects = Vec::new();
        let mut effect_aliases = Vec::new();
        let mut members_init = Vec::new();
        let mut defs_init = Vec::new();
        // Declare items before declaration
        for (attr, class) in module.classes() {
            let (gen_scope, mut errs) = GenScope::from_ast(
                &class.generics,
                class.name.span(),
                // Classes implicitly mention their generic parameters
                |_| true,
                |_| true,
            );
            errors.append(&mut errs);
            let gen_scope = this.tys.insert_gen_scope(gen_scope);
            match this.classes.declare(class.name.clone(), Class {
                name: class.name.clone(),
                attr: attr.to_vec(),
                gen_scope,
                fields: None,
                assoc: Some(class.items
                    .iter()
                    .filter_map(|item| match item {
                        ast::ClassItem::Type { name, obligations } => Some(ClassAssoc { name: name.clone() }),
                        _ => None,
                    })
                    .collect::<Vec<_>>()),
            }) {
                Err(err) => {
                    errors.push(err);
                    continue;
                },
                // Only mark for further processing if no errors occurred during declaration
                Ok(class_id) => classes.push((attr, class, class_id, gen_scope)),
            }
        }
        // Class associated types
        for (attr, class, class_id, gen_scope) in &classes {
            let mut existing_tys = HashMap::new();
            let assoc = class.items
                .iter()
                .filter_map(|item| match item {
                    ast::ClassItem::Type { name, obligations } => {
                        if !obligations.is_empty() {
                            errors.push(Error::Unsupported(obligations.span(), "obligations on associated types"));
                        }

                        if let Some(old) = existing_tys.get(&**name) {
                            errors.push(Error::DuplicateClassItem(**name, *old, name.span()));
                            None
                        } else {
                            existing_tys.insert(**name, name.span());
                            Some(ClassAssoc { name: name.clone() })
                        }
                    },
                    _ => None,
                })
                .collect::<Vec<_>>();
            this.classes.define_assoc(*class_id, assoc);
        }
        for (attr, eff) in module.effects() {
            let (gen_scope, mut errs) = GenScope::from_ast(
                &eff.generics,
                eff.name.span(),
                |_| true,
                |_| true,
            );
            errors.append(&mut errs);
            let gen_scope = this.tys.insert_gen_scope(gen_scope);
            match this.effects.declare(EffectDecl {
                name: eff.name.clone(),
                attr: attr.to_vec(),
                gen_scope,
                send: None,
                recv: None,
            }) {
                Err(err) => {
                    errors.push(err);
                    continue;
                },
                // Only mark for further processing if no errors occurred during declaration
                Ok(eff_id) => effects.push((attr, eff, eff_id, gen_scope)),
            }
        }
        for (attr, alias) in module.effect_aliases() {
            let (gen_scope, mut errs) = GenScope::from_ast(
                &alias.generics,
                alias.name.span(),
                // TODO: Check mentions of generics
                |_| true,
                |_| true,
            );
            errors.append(&mut errs);
            let gen_scope = this.tys.insert_gen_scope(gen_scope);
            match this.effects.declare_alias(EffectAlias {
                name: alias.name.clone(),
                attr: attr.to_vec(),
                gen_scope,
                effects: None,
            }) {
                Err(err) => {
                    errors.push(err);
                    continue;
                },
                // Only mark for further processing if no errors occurred during declaration
                Ok(eff_id) => effect_aliases.push((attr, alias, eff_id, gen_scope)),
            }
        }
        for (attr, alias) in module.aliases() {
            let (gen_scope, mut errs) = GenScope::from_ast(
                &alias.generics,
                alias.name.span(),
                |name| alias.ty.mentions_ty(name),
                |name| alias.ty.mentions_eff(name),
            );
            errors.append(&mut errs);
            let gen_scope = this.tys.insert_gen_scope(gen_scope);
            match this.datas.declare_alias(*alias.name, alias.name.span(), gen_scope) {
                Ok(alias_id) => aliases.push((attr, alias, alias_id)),
                Err(err) => {
                    errors.push(err);
                    continue;
                },
            }
        }
        for (attr, data) in module.datas() {
            let (gen_scope, mut errs) = GenScope::from_ast(
                &data.generics,
                data.name.span(),
                // TODO: Uncomment this when some PhantomData equivalent exists!
                // |name| data.variants.iter().any(|(_, ty)| ty.mentions_ty(name)),
                // |name| data.variants.iter().any(|(_, ty)| ty.mentions_eff(name)),
                |_| true,
                |_| true,
            );
            errors.append(&mut errs);
            let gen_scope = this.tys.insert_gen_scope(gen_scope);
            match this.datas.declare_data(data.name.clone(), gen_scope, &attr) {
                Ok(data_id) => datas.push((attr, data, data_id)),
                Err(err) => {
                    errors.push(err);
                    continue;
                },
            }
        }
        for (attr, member) in module.members() {
            let class_id = if let Some(class_id) = this.classes.lookup(*member.class.name) {
                class_id
            } else {
                errors.push(Error::NoSuchClass(member.class.name.clone()));
                continue;
            };

            let (gen_scope, mut errs) = GenScope::from_ast(
                &member.generics,
                member.member.span(),
                |name| member.member.mentions_ty(name)
                    || member.class.gen_tys.iter().any(|ty| ty.mentions_ty(name))
                    || member.class.gen_effs.iter().any(|ty| ty.mentions_ty(name)),
                |name| member.member.mentions_eff(name)
                    || member.class.gen_tys.iter().any(|ty| ty.mentions_eff(name))
                    || member.class.gen_effs.iter().any(|ty| ty.mentions_eff(name)),
            );
            errors.append(&mut errs);
            let gen_scope = this.tys.insert_gen_scope(gen_scope);
            members_init.push((attr, member, class_id, gen_scope));
        }
        for (attr, def) in module.defs() {
            let (gen_scope, mut errs) = GenScope::from_ast(
                &def.generics,
                def.name.span(),
                |_| true,
                |_| true,
            );
            errors.append(&mut errs);
            let gen_scope = this.tys.insert_gen_scope(gen_scope);
            defs_init.push((attr, def, gen_scope));
        }

        // Now that we have declarations for all classes and data types, we can check generic scope constraints

        for (_, class, class_id, _) in &classes {
            let gen_scope = this.classes.get(*class_id).gen_scope;
            this.reify_gen_scope(
                gen_scope,
                |infer| {
                    let self_ty = infer.set_self_unknown(class.name.span());
                    let gen_tys = (0..infer.ctx().tys.get_gen_scope(gen_scope).len())
                        .map(|idx| {
                            let span = infer.ctx().tys.get_gen_scope(gen_scope).get(idx).name.span();
                            infer.insert(span, TyInfo::Gen(idx, gen_scope, span))
                        })
                        .collect::<Vec<_>>();
                    let gen_effs = (0..infer.ctx().tys.get_gen_scope(gen_scope).len_eff())
                        .map(|idx| {
                            let span = infer.ctx().tys.get_gen_scope(gen_scope).get_eff(idx).name.span();
                            infer.insert_gen_eff(span, idx, gen_scope)
                        })
                        .collect::<Vec<_>>();
                    infer.add_implied_member_single(ImpliedMember {
                        member: SrcNode::new(self_ty, class.name.span()),
                        class: SrcNode::new(*class_id, class.name.span()),
                        gen_tys,
                        gen_effs,
                        items: ImpliedItems::Eq(Vec::new()),
                    });
                },
            );
        }

        for (_, _, effect_id, _) in &effects {
            this.reify_gen_scope(
                this.effects.get_decl(*effect_id).gen_scope,
                |_infer| {},
            );
        }

        for (_, _, alias_id, _) in &effect_aliases {
            this.reify_gen_scope(
                this.effects.get_alias(*alias_id).gen_scope,
                |_infer| {},
            );
        }

        for (_, _, alias_id) in &aliases {
            this.reify_gen_scope(
                this.datas.alias_gen_scope(*alias_id),
                |_infer| {},
            );
        }

        for (_, _, data_id) in &datas {
            this.reify_gen_scope(
                this.datas.data_gen_scope(*data_id),
                |_infer| {},
            );
        }

        for (_, _, _, gen_scope_id) in &members_init {
            this.reify_gen_scope(
                *gen_scope_id,
                |infer| {},
            );
        }

        for (_, def, gen_scope_id) in &defs_init {
            this.reify_gen_scope(
                *gen_scope_id,
                |infer| {},
            );
        }

        // Effect alias definition must go before members and defs because they might have type hints that make use of type
        // aliases
        for (attr, alias, alias_id, gen_scope) in effect_aliases {
            let mut infer = Infer::new(&mut this, Some(gen_scope));
                // TODO: Enforce these?
                //.with_gen_scope_implied();

            let effs = alias.effects
                .iter()
                .filter_map(|(name, gen_tys, gen_effs)| match infer.ctx().effects.lookup(**name) {
                    None => todo!("No such effect!"),
                    Some(Ok(eff)) => Some((
                        SrcNode::new(eff, name.span()),
                        gen_tys
                            .iter()
                            .map(|ty| ty.to_hir(&TypeLowerCfg::other(), &mut infer, &Scope::Empty).meta().1)
                            .collect::<Vec<TyVar>>(),
                        gen_effs
                            .iter()
                            .map(|eff| lower::lower_effect_set(eff, &TypeLowerCfg::other(), &mut infer, &Scope::Empty))
                            .collect::<Vec<EffectVar>>(),
                    )),
                    Some(Err(_)) => todo!("Nested effect aliases"),
                })
                .collect::<Vec<_>>();

            let (mut checked, mut errs) = infer.into_checked();
            errors.append(&mut errs);

            let effs = effs
                .into_iter()
                .map(|(eff, gen_tys, gen_effs)| (
                    eff,
                    gen_tys
                        .into_iter()
                        .map(|ty| checked.reify(ty))
                        .collect(),
                    gen_effs
                        .into_iter()
                        .map(|e| checked.reify_effect(e))
                        .collect(),
                ))
                .collect();

            this.effects.define_alias_effects(alias_id, effs);
        }

        let mut members = Vec::new();
        for (attr, member, class_id, gen_scope) in &members_init {
            let mut infer = Infer::new(&mut this, Some(*gen_scope))
                .with_gen_scope_implied();

            let member_ty = member.member.to_hir(&TypeLowerCfg::member(), &mut infer, &Scope::Empty);

            let mut infer = infer.with_self_var(member_ty.meta().1);

            let gen_tys = member.class.gen_tys
                .iter()
                .map(|ty| ty.to_hir(&TypeLowerCfg::member(), &mut infer, &Scope::Empty))
                .collect::<Vec<_>>();
            let gen_effs = member.class.gen_effs
                .iter()
                .map(|eff| lower::lower_effect_set(eff, &TypeLowerCfg::member(), &mut infer, &Scope::Empty))
                .collect::<Vec<_>>();

            let class_gen_scope = infer.ctx().tys.get_gen_scope(infer.ctx().classes.get(*class_id).gen_scope);
            if class_gen_scope.len() != gen_tys.len() {
                let item_span = class_gen_scope.item_span;
                let class_gen_scope_len = class_gen_scope.len();
                infer.ctx_mut().emit(Error::WrongNumberOfGenerics(
                    member.class.span(),
                    gen_tys.len(),
                    item_span,
                    class_gen_scope_len,
                ));
            } else if class_gen_scope.len_eff() != gen_effs.len() {
                let item_span = class_gen_scope.item_span;
                let class_gen_scope_len_eff = class_gen_scope.len_eff();
                // TODO: Proper error for effects
                infer.ctx_mut().emit(Error::WrongNumberOfGenerics(
                    member.class.span(),
                    gen_effs.len(),
                    item_span,
                    class_gen_scope_len_eff,
                ));
            }

            let (mut checked, mut errs) = infer.into_checked();
            errors.append(&mut errs);

            let member_ty = checked.reify(member_ty.meta().1);
            let gen_tys = gen_tys
                .iter()
                .map(|ty| checked.reify(ty.meta().1))
                .collect::<Vec<_>>();
            let gen_effs = gen_effs
                .iter()
                .map(|eff| checked.reify_effect(*eff))
                .collect::<Vec<_>>();

            let member_id = this.classes.declare_member(*class_id, Member {
                gen_scope: *gen_scope,
                attr: attr.to_vec(),
                member: member_ty,
                class: *class_id,
                gen_tys,
                gen_effs,
                fields: None,
                assoc: None,
            });
            members.push((*member, *class_id, member_id, member_ty, *gen_scope));
        }

        errors.append(&mut this.classes.check_coherence(&this.tys));

        // Alias definition must go before members and defs because they might have type hints that make use of type
        // aliases
        for (attr, alias, _) in aliases {
            let gen_scope = this.datas.name_gen_scope(*alias.name);

            let mut infer = Infer::new(&mut this, Some(gen_scope));
                // TODO: Enforce these?
                //.with_gen_scope_implied();

            let ty = alias.ty.to_hir(&TypeLowerCfg::other(), &mut infer, &Scope::Empty);

            let (mut checked, mut errs) = infer.into_checked();
            errors.append(&mut errs);

            let ty = checked.reify(ty.meta().1);

            this.datas.define_alias(
                this.datas
                    .lookup_alias(*alias.name)
                    .expect("Alias must be pre-declared before definition"),
                Alias {
                    name: alias.name.clone(),
                    attr: attr.to_vec(),
                    gen_scope,
                    ty,
                },
            );
        }

        // Check for lang items
        this.errors.append(&mut this.classes.check_lang_items());
        this.errors.append(&mut this.datas.check_lang_items());

        for (attr, eff, eff_id, gen_scope) in effects {
            let mut infer = Infer::new(&mut this, Some(gen_scope))
                .with_gen_scope_implied();

            let send = eff.send.to_hir(&TypeLowerCfg::other(), &mut infer, &Scope::Empty);
            let recv = eff.recv.to_hir(&TypeLowerCfg::other(), &mut infer, &Scope::Empty);

            let (mut checked, mut errs) = infer.into_checked();
            errors.append(&mut errs);

            let send = checked.reify(send.meta().1);
            let recv = checked.reify(recv.meta().1);

            this.effects.define_send_recv(eff_id, send, recv);
        }

        // Class fields
        for (attr, class, class_id, gen_scope) in &classes {
            let mut existing_fields = HashMap::new();
            let fields = class.items
                .iter()
                .filter_map(|item| match item {
                    ast::ClassItem::Value { name, ty } => {
                        let mut infer = Infer::new(&mut this, Some(*gen_scope));
                        let self_ty = infer.set_self_unknown(class.name.span());
                        let gen_tys = (0..infer.ctx().tys.get_gen_scope(*gen_scope).len())
                            .map(|idx| {
                                let span = infer.ctx().tys.get_gen_scope(*gen_scope).get(idx).name.span();
                                infer.insert(span, TyInfo::Gen(idx, *gen_scope, span))
                            })
                            .collect::<Vec<_>>();
                        let gen_effs = (0..infer.ctx().tys.get_gen_scope(*gen_scope).len_eff())
                            .map(|idx| {
                                let span = infer.ctx().tys.get_gen_scope(*gen_scope).get(idx).name.span();
                                infer.insert_gen_eff(span, idx, *gen_scope)
                            })
                            .collect::<Vec<_>>();
                        infer.add_implied_member(ImpliedMember {
                            member: SrcNode::new(self_ty, class.name.span()),
                            class: SrcNode::new(*class_id, class.name.span()),
                            gen_tys,
                            gen_effs,
                            items: ImpliedItems::Eq(Vec::new()),
                        });
                        let mut infer = infer.with_gen_scope_implied();

                        let ty = ty.to_hir(&TypeLowerCfg::other(), &mut infer, &Scope::Empty);

                        let (mut checked, mut errs) = infer.into_checked();
                        errors.append(&mut errs);
                        checked.reify(ty.meta().1);

                        if let Some(old) = existing_fields.get(&**name) {
                            errors.push(Error::DuplicateClassItem(**name, *old, name.span()));
                            None
                        } else {
                            existing_fields.insert(**name, name.span());
                            Some(ClassField {
                                name: name.clone(),
                                ty: SrcNode::new(checked.reify(ty.meta().1), ty.meta().0),
                            })
                        }
                    },
                    _ => None,
                })
                .collect::<Vec<_>>();
            this.classes.define_fields(*class_id, fields);
        }
        // Member associated types
        for (member, class_id, member_id, member_ty, gen_scope) in &members {
            let assoc = member.items
                .iter()
                .filter_map(|item| {
                    let member_ty = this.classes.get_member(*member_id).member;
                    let mut infer = Infer::new(&mut this, Some(*gen_scope))
                        .with_self_type(member_ty, member.member.span());
                    let gen_tys = member.class.gen_tys
                        .iter()
                        .map(|ty| ty.to_hir(&TypeLowerCfg::other(), &mut infer, &Scope::Empty).meta().1)
                        .collect();
                    let gen_effs = member.class.gen_effs
                        .iter()
                        .map(|eff| lower::lower_effect_set(eff, &TypeLowerCfg::other(), &mut infer, &Scope::Empty))
                        .collect();
                    let assoc_tys = member.items
                        .iter()
                        .filter_map(|item| match item {
                             ast::MemberItem::Type { name, ty } => Some((
                                 name.clone(),
                                  ty.to_hir(&TypeLowerCfg::other(), &mut infer, &Scope::Empty).meta().1,
                              )),
                             _ => None,
                        })
                        .collect();
                    infer.add_implied_member(ImpliedMember {
                        member: SrcNode::new(infer.self_type().unwrap(), member.member.span()),
                        class: SrcNode::new(*class_id, infer.ctx().classes.get(*class_id).name.span()),
                        gen_tys,
                        gen_effs,
                        items: ImpliedItems::Eq(assoc_tys),
                    });
                    let mut infer = infer.with_gen_scope_implied();

                    let class = infer.ctx().classes.get(*class_id);

                    match item {
                        ast::MemberItem::Type { name, ty } => if class.assoc_ty(**name).is_none() {
                            errors.push(Error::NoSuchClassItem(name.clone(), class.name.clone()));
                            None
                        } else {
                            let ty = ty.to_hir(&TypeLowerCfg::other(), &mut infer, &Scope::Empty);

                            let (mut checked, mut errs) = infer.into_checked();
                            errors.append(&mut errs);

                            let ty = checked.reify(ty.meta().1);

                            Some((name.clone(), ty))
                        },
                        _ => None,
                    }
                })
                .collect::<Vec<_>>();
            let mut existing_tys = HashMap::new();
            let assoc = assoc
                .into_iter()
                .filter_map(|(name, item)| {
                    if let Some(old) = existing_tys.get(&*name) {
                        errors.push(Error::DuplicateMemberItem(*name, *old, name.span()));
                        None
                    } else {
                        existing_tys.insert(*name, name.span());
                        Some((*name, item))
                    }
                })
                .collect::<HashMap<_, _>>();

            let class = this.classes.get(*class_id);

            for class_assoc in class.assoc.as_ref().expect("Class associated types must be known here") {
                if !assoc.contains_key(&*class_assoc.name) {
                    errors.push(Error::MissingClassItem(member.member.span(), class.name.clone(), class_assoc.name.clone()));
                }
            }

            this.classes.define_member_assoc(*member_id, *class_id, assoc);
        }

        // Define datas
        for (attr, data, data_id) in datas {
            let gen_scope_id = this.datas.name_gen_scope(*data.name);

            let mut infer = Infer::new(&mut this, Some(gen_scope_id))
                .with_gen_scope_implied();

            // Generate `Self` type
            let gen_scope = infer.ctx().tys.get_gen_scope(gen_scope_id);
            let gen_tys_len = gen_scope.len();
            let gen_effs_len = gen_scope.len_eff();
            let gen_tys = (0..gen_tys_len)
                .map(|idx| {
                    let gen_scope = infer.ctx().tys.get_gen_scope(gen_scope_id);
                    let span = gen_scope.get(idx).name.span();
                    infer.insert(span, TyInfo::Gen(idx, gen_scope_id, span))
                })
                .collect();
            let gen_effs = (0..gen_effs_len)
                .map(|idx| {
                    let gen_scope = infer.ctx().tys.get_gen_scope(gen_scope_id);
                    let span = gen_scope.get_eff(idx).name.span();
                    infer.insert_gen_eff(span, idx, gen_scope_id)
                })
                .collect();
            let self_ty = infer.insert(data.name.span(), TyInfo::Data(data_id, gen_tys, gen_effs));
            let mut infer = infer.with_self_var(self_ty);

            let variants = data.variants
                .iter()
                .map(|(name, ty)| {
                    let ty = ty.to_hir(&TypeLowerCfg::other(), &mut infer, &Scope::Empty);
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
                data_id,
                data.name.span(),
                Data {
                    name: data.name.clone(),
                    attr: attr.to_vec(),
                    gen_scope: gen_scope_id,
                    variance_ty: None,
                    variance_eff: None,
                    cons,
                },
            ) {
                errors.append(&mut errs);
            }
        }

        this.datas.derive_variance(&this.tys);

        // Enforce member obligations
        for (member, class_id, member_id, member_ty, gen_scope) in &members {
            let mut infer = Infer::new(&mut this, Some(*gen_scope))
                .with_gen_scope_implied();

            let member_ty = infer.instantiate_local(*member_ty, member.member.span());

            let member_gen_tys = member.class.gen_tys
                .iter()
                .map(|arg| arg.to_hir(&TypeLowerCfg::other(), &mut infer, &Scope::Empty).meta().1)
                .collect::<Vec<_>>();
            let member_gen_effs = member.class.gen_effs
                .iter()
                .map(|eff| lower::lower_effect_set(eff, &TypeLowerCfg::other(), &mut infer, &Scope::Empty))
                .collect::<Vec<_>>();

            // infer.add_implied_member(ImpliedMember {
            //     member: SrcNode::new(member_ty.meta().1, member_ty.meta().0),
            //     class: SrcNode::new(*class_id, infer.ctx().classes.get(*class_id).name.span()),
            //     items: ImpliedItems::Eq(Vec::new()),
            // });

            // for member in infer
            //     .ctx()
            //     .tys
            //     .get_gen_scope(*gen_scope)
            //     .implied_members
            //     .clone()
            //     .expect("Implied members must be known")
            // {
            //     let implied_member = infer.instantiate_local(*member.member, member.member.span());
            //     infer.make_impl(implied_member, *member.class, member.class.span(), Vec::new(), member.class.span());
            // }

            // Enforce class obligations
            let class_gen_scope = infer.ctx().classes.get(*class_id).gen_scope;
            for member_obl in infer
                .ctx()
                .tys
                .get_gen_scope(class_gen_scope)
                .implied_members
                .clone()
                .expect("Implied members must be known")
            {
                let obl_member_ty = infer.instantiate(
                    *member_obl.member,
                    member_obl.member.span(),
                    &mut |idx, _, _| member_gen_tys.get(idx).copied(),
                    &mut |idx, _| member_gen_effs.get(idx).copied(),
                    Some(member_ty),
                    invariant(),
                );
                let obl_member_gen_tys = member_obl.gen_tys
                    .iter()
                    .map(|ty| infer.instantiate(
                        *ty,
                        member_obl.member.span(),
                        &mut |idx, _, _| member_gen_tys.get(idx).copied(),
                        &mut |idx, _| member_gen_effs.get(idx).copied(),
                        Some(member_ty),
                        invariant(),
                    ))
                    .collect();
                // TODO
                let obl_member_gen_effs = Vec::new()/*member_obl.gen_effs
                    .iter()
                    .map(|ty| infer.instantiate(
                        *ty,
                        member_obl.member.span(),
                        &mut |idx, _, _| member_gen_tys.get(idx).copied(),
                        &mut |idx, _| todo!(),
                        Some(member_ty),
                        invariant(),
                    ))
                    .collect()*/;
                let obl_assoc = match &member_obl.items {
                    ImpliedItems::Real(_) => Vec::new(),
                    ImpliedItems::Eq(assoc) => assoc
                        .iter()
                        .map(|(name, assoc)| (name.clone(), infer.instantiate(
                            *assoc,
                            name.span(),
                            &mut |idx, _, _| member_gen_tys.get(idx).copied(),
                            &mut |idx, _| member_gen_effs.get(idx).copied(),
                            Some(member_ty),
                            invariant(),
                        )))
                        .collect(),
                };
                infer.make_impl(
                    obl_member_ty,
                    (*member_obl.class, obl_member_gen_tys, obl_member_gen_effs),
                    member_obl.class.span(),
                    obl_assoc,
                    member.class.span(),
                );
            }

            let (mut checked, mut errs) = infer.into_checked();
            errors.append(&mut errs);
        }

        let mut defs = Vec::new();
        for (attr, def, gen_scope) in defs_init {
            // If the type hint is fully specified, check it
            let ty_hint = if def.ty_hint.is_fully_specified() {
                let mut infer = Infer::new(&mut this, Some(gen_scope))
                    .with_debug(attr.iter().find(|a| **a.name == "ty_debug").is_some())
                    .with_gen_scope_implied();
                let ty_hint = def.ty_hint.to_hir(&TypeLowerCfg::other(), &mut infer, &Scope::Empty);

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
                continue;
            } else {
                // Only mark for further processing if no errors occurred during declaration
                defs.push((attr, def));
            }
        }

        // Check for lang items
        this.errors.append(&mut this.defs.check_lang_items());

        // Member fields
        for (member, class_id, member_id, member_ty, gen_scope) in &members {
            let fields = member.items
                .iter()
                .filter_map(|item| {
                    let mut infer = Infer::new(&mut this, Some(*gen_scope))
                        .with_self_type(*member_ty, member.member.span())
                        .with_gen_scope_implied();
                    let self_ty = infer.self_type().unwrap();
                    let gen_tys = member.class.gen_tys
                        .iter()
                        .map(|ty| ty.to_hir(&TypeLowerCfg::other(), &mut infer, &Scope::Empty).meta().1)
                        .collect::<Vec<_>>();
                    let gen_effs = member.class.gen_effs
                        .iter()
                        .enumerate()
                        .map(|(i, eff)| lower::lower_effect_set(eff, &TypeLowerCfg::other(), &mut infer, &Scope::Empty))
                        .collect::<Vec<_>>();

                    // TODO: Report error on `gen_tys`/`gen_effs` length mismatch with class?

                    // TODO: This seems to break things, unsure why
                    // infer.add_implied_member(ImpliedMember {
                    //     member: SrcNode::new(infer.self_type().unwrap(), member.member.span()),
                    //     class: SrcNode::new(*class_id, infer.ctx().classes.get(*class_id).name.span()),
                    //     gen_tys: gen_tys.clone(),
                    //     gen_effs: gen_effs.clone(),
                    //     items: ImpliedItems::Real(*member_id),
                    // });

                    let class = infer.ctx().classes.get(*class_id);

                    match item {
                        ast::MemberItem::Value { name, val } => if class.field(**name).is_none() {
                            errors.push(Error::NoSuchClassItem(name.clone(), class.name.clone()));
                            None
                        } else {
                            let val = val.to_hir(&(), &mut infer, &Scope::Empty);
                            let class = infer.ctx().classes.get(*class_id);
                            if let Some(field_ty) = class.field(**name).cloned() {
                                let val_ty = infer.instantiate(
                                    *field_ty,
                                    None,
                                    &mut |idx, _, _| gen_tys.get(idx).copied(),
                                    &mut |idx, _| gen_effs.get(idx).copied(),
                                    Some(self_ty),
                                    contravariant(),
                                );
                                infer.make_flow(val.meta().1, val_ty, EqInfo::new(name.span(), format!("Type of member item must match class")));
                            }

                            let (mut checked, mut errs) = infer.into_checked();
                            errors.append(&mut errs);

                            let val = val.reify(&mut checked);

                            Some((name.clone(), val))
                        },
                        _ => None,
                    }
                })
                .collect::<Vec<_>>();
            let mut existing_fields = HashMap::new();
            let fields = fields
                .into_iter()
                .filter_map(|(name, item)| {
                    if let Some(old) = existing_fields.get(&*name) {
                        errors.push(Error::DuplicateMemberItem(*name, *old, name.span()));
                        None
                    } else {
                        existing_fields.insert(*name, name.span());
                        Some((*name, item))
                    }
                })
                .collect::<HashMap<_, _>>();

            let class = this.classes.get(*class_id);

            for field in class.fields.as_ref().expect("Class fields must be known here") {
                if !fields.contains_key(&*field.name) {
                    errors.push(Error::MissingClassItem(member.member.span(), class.name.clone(), field.name.clone()));
                }
            }

            this.classes.define_member_fields(*member_id, *class_id, fields);
        }

        // Def impls
        for (attr, def) in defs {
            let id = this.defs
                .lookup(*def.name)
                .expect("Def must be pre-declared before definition");
            let gen_scope = this.defs.get(id).gen_scope;

            let mut infer = Infer::new(&mut this, Some(gen_scope))
                .with_debug(attr.iter().find(|a| **a.name == "ty_debug").is_some())
                .with_gen_scope_implied();

            let ty_hint = def.ty_hint.to_hir(&TypeLowerCfg::other(), &mut infer, &Scope::Empty);

            let gen_tys = (0..infer.ctx().tys.get_gen_scope(gen_scope).len())
                .map(|i| {
                    let span = infer.ctx().tys.get_gen_scope(gen_scope).get(i).name.span();
                    (span, infer.insert(span, TyInfo::Gen(i, gen_scope, span)))
                })
                .collect();
            let gen_effs = (0..infer.ctx().tys.get_gen_scope(gen_scope).len_eff())
                .map(|i| {
                    let span = infer.ctx().tys.get_gen_scope(gen_scope).get_eff(i).name.span();
                    infer.insert_gen_eff(span, i, gen_scope)
                })
                .collect();

            let body = def.body.to_hir(&(), &mut infer, &Scope::Recursive(def.name.clone(), ty_hint.meta().1, id, gen_tys, gen_effs));
            infer.make_flow(body.meta().1, ty_hint.meta().1, body.meta().0);

            let (mut checked, mut errs) = infer.into_checked();
            errors.append(&mut errs);


            let body = body.reify(&mut checked);

            // println!("{}: {}", *def.name, this.tys.display(&this.datas, body.meta().1));

            this.defs.define_body(id, body);
        }

        /*
        for data in this.datas.iter_datas() {
            let data = this.datas.get_data(data);
            println!("data {} ={}", *data.name, data.cons
                .iter()
                .map(|(name, ty)| format!("\n    | {} {}", **name, this.tys.display(&this, *ty)))
                .collect::<String>());
        }
        */

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
                Ty::Data(data, _gen_tys, _gen_effs) => if already_seen.contains(&data) {
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
                Ty::Record(fields, _) => if let Some((_, field_ty)) = fields.iter().find(|(name, _)| **name == field) {
                    break Some((ty, *field_ty, already_seen.len()));
                } else {
                    // Record has no such field
                    break None;
                },
                _ => break None, // Only `Data` or `Record` can have fields
            }
        }
    }

    pub fn reify_gen_scope(&mut self, gen_scope_id: GenScopeId, f: impl FnOnce(&mut Infer)) {
        let mut infer = Infer::new(self, Some(gen_scope_id));

        f(&mut infer);

        let gen_scope = infer
            .ctx()
            .tys
            .get_gen_scope(gen_scope_id);
        let ast_implied_members = gen_scope
            .ast_implied_members
            .clone();

        let infer_members = ast_implied_members
            .into_iter()
            .filter_map(|member| {
                let class = if let Some(class_id) = infer.ctx_mut().classes.lookup(*member.class.name) {
                    SrcNode::new(class_id, member.class.span())
                } else {
                    infer.ctx_mut().errors.push(Error::NoSuchClass(member.class.name.clone()));
                    return None;
                };

                let ty = member.member.to_hir(&TypeLowerCfg::other(), &mut infer, &Scope::Empty);
                let gen_tys = member.class.gen_tys
                    .iter()
                    .map(|ty| ty.to_hir(&TypeLowerCfg::other(), &mut infer, &Scope::Empty).meta().1)
                    .collect::<Vec<_>>();
                let gen_effs = member.class.gen_effs
                    .iter()
                    .map(|eff| lower::lower_effect_set(eff, &TypeLowerCfg::other(), &mut infer, &Scope::Empty))
                    .collect::<Vec<_>>();
                let items = member.assoc
                    .iter()
                    .map(|(name, assoc)| (name.clone(), assoc.to_hir(&TypeLowerCfg::other(), &mut infer, &Scope::Empty).meta().1))
                    .collect::<Vec<_>>();

                let gen_scope = infer.ctx().tys.get_gen_scope(infer.ctx().classes.get(*class).gen_scope);
                if gen_scope.len() != gen_tys.len() || gen_scope.len_eff() != gen_effs.len() {
                    // TODO: Proper error for effect length mismatch
                    let item_span = gen_scope.item_span;
                    let gen_scope_len = gen_scope.len();
                    infer.ctx_mut().emit(Error::WrongNumberOfGenerics(
                        member.span(),
                        gen_tys.len(),
                        item_span,
                        gen_scope_len,
                    ));
                }

                infer.add_implied_member_single(ImpliedMember {
                    member: SrcNode::new(ty.meta().1, ty.meta().0),
                    class: class.clone(),
                    gen_tys: gen_tys.clone(),
                    gen_effs: gen_effs.clone(),
                    items: ImpliedItems::Eq(items.clone()),
                });

                Some((ty, class, gen_tys, gen_effs, items, member.span()))
            })
            .collect::<Vec<_>>();

        let (mut checked, mut errs) = infer.into_checked();
        checked.ctx_mut().errors.append(&mut errs);

        let implied_members = infer_members
            .into_iter()
            .map(|(ty, class, gen_tys, gen_effs, items, span)| {
                SrcNode::new(TyImpliedMember {
                    member: SrcNode::new(checked.reify(ty.meta().1), ty.meta().0),
                    gen_tys: gen_tys
                        .iter()
                        .map(|ty| checked.reify(*ty))
                        .collect(),
                    gen_effs: gen_effs
                        .iter()
                        .map(|eff| checked.reify_effect(*eff))
                        .collect(),
                    class,
                    items: ImpliedItems::Eq(items
                        .iter()
                        .map(|(name, assoc)| (name.clone(), checked.reify(*assoc)))
                        .collect()),
                }, span)
            })
            .collect();

        self
            .tys
            .get_gen_scope_mut(gen_scope_id)
            .implied_members = Some(implied_members);
    }
}
