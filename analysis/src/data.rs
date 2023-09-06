use super::*;

pub struct Data {
    pub name: SrcNode<Ident>,
    pub attr: Vec<SrcNode<ast::Attr>>,
    pub gen_scope: GenScopeId,
    pub variance_ty: Option<Vec<Variance>>,
    pub variance_eff: Option<Vec<Variance>>,
    pub cons: Vec<(SrcNode<Ident>, TyId)>,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct DataId(usize, Ident);

impl fmt::Debug for DataId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.1)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct AliasId(usize);

pub struct Alias {
    pub name: SrcNode<Ident>,
    pub attr: Vec<SrcNode<ast::Attr>>,
    pub gen_scope: GenScopeId,
    pub ty: TyId,
}

#[derive(Default)]
pub struct Lang {
    pub go: Option<DataId>,
    pub r#bool: Option<DataId>,
}

#[derive(Default)]
pub struct Datas {
    // TODO: Don't use `Result`
    name_lut: HashMap<Ident, (Span, Result<DataId, AliasId>, GenScopeId)>,
    cons_lut: HashMap<Ident, (Span, DataId)>,
    alias_lut: HashMap<Ident, Alias>,
    datas: Vec<(Span, Option<Data>, GenScopeId, Ident)>,
    aliases: Vec<(Span, Option<Alias>, GenScopeId)>,
    pub lang: Lang,
}

impl Datas {
    pub fn iter_datas(&self) -> impl Iterator<Item = DataId> + '_ {
        (0..self.datas.len()).map(|i| DataId(i, Ident::new(self.datas[i].3)))
    }

    pub fn iter_aliases(&self) -> impl Iterator<Item = AliasId> {
        (0..self.aliases.len()).map(|i| AliasId(i))
    }

    pub fn data_gen_scope(&self, data: DataId) -> GenScopeId {
        self.datas[data.0].2
    }

    pub fn alias_gen_scope(&self, alias: AliasId) -> GenScopeId {
        self.aliases[alias.0].2
    }

    pub fn name_gen_scope(&self, name: Ident) -> GenScopeId {
        self.name_lut[&name].2
    }

    pub fn lookup_data(&self, name: Ident) -> Option<DataId> {
        self.name_lut
            .get(&name)
            .and_then(|data| data.1.as_ref().ok())
            .copied()
    }

    pub fn lookup_alias(&self, name: Ident) -> Option<AliasId> {
        self.name_lut
            .get(&name)
            .and_then(|data| data.1.as_ref().err())
            .copied()
    }

    pub fn lookup_cons(&self, name: Ident) -> Option<DataId> {
        self.cons_lut.get(&name).map(|(_, id)| *id)
    }

    pub fn get_data(&self, data: DataId) -> &Data {
        self.datas[data.0]
            .1
            .as_ref()
            .expect("Declared data accessed before being defined")
    }

    pub fn get_data_span(&self, data: DataId) -> Span {
        self.datas[data.0].0
    }

    pub fn get_alias(&self, alias: AliasId) -> Option<&Alias> {
        self.aliases[alias.0]
            .1
            .as_ref()
    }

    pub fn get_alias_span(&self, alias: AliasId) -> Span {
        self.aliases[alias.0].0
    }

    pub fn declare_data(&mut self, name: SrcNode<Ident>, gen_scope: GenScopeId, attr: &[SrcNode<ast::Attr>]) -> Result<DataId, Error> {
        let id = DataId(self.datas.len(), *name);
        if let Err(old) = self.name_lut.try_insert(*name, (name.span(), Ok(id), gen_scope)) {
            Err(Error::DuplicateTypeName(*name, old.entry.get().0, name.span()))
        } else {
            if let Some(lang) = attr
                .iter()
                .find(|a| &**a.name == "lang")
                .and_then(|a| a.args.as_ref())
            {
                if lang.iter().find(|a| &**a.name == "go").is_some() { self.lang.go = Some(id); }
                if lang.iter().find(|a| &**a.name == "bool").is_some() { self.lang.r#bool = Some(id); }
            }

            self.datas.push((name.span(), None, gen_scope, *name));
            Ok(id)
        }
    }

    pub fn check_lang_items(&self) -> Vec<Error> {
        let mut errors = Vec::new();

        if self.lang.go.is_none() { errors.push(Error::MissingLangItem("go")); }
        if self.lang.r#bool.is_none() { errors.push(Error::MissingLangItem("bool")); }

        errors
    }

    pub fn declare_alias(&mut self, name: Ident, span: Span, gen_scope: GenScopeId) -> Result<AliasId, Error> {
        let id = AliasId(self.aliases.len());
        if let Err(old) = self.name_lut.try_insert(name, (span, Err(id), gen_scope)) {
            Err(Error::DuplicateTypeName(name, old.entry.get().0, span))
        } else {
            self.aliases.push((span, None, gen_scope));
            Ok(id)
        }
    }

    pub fn define_data(&mut self, id: DataId, span: Span, data: Data) -> Result<(), Vec<Error>> {
        let mut errors = Vec::new();
        for (cons, _) in &data.cons {
            if let Err(old) = self.cons_lut.try_insert(**cons, (cons.span(), id)) {
                errors.push(Error::DuplicateConsName(**cons, old.entry.get().0, cons.span()));
            }
        }
        self.datas[id.0].1 = Some(data);
        if errors.len() == 0 {
            Ok(())
        } else {
            Err(errors)
        }
    }

    pub fn define_alias(&mut self, id: AliasId, alias: Alias) {
        self.aliases[id.0].1 = Some(alias);
    }

    // Result<gen_ty, gen_eff>
    pub fn derive_variance_ty(&mut self, tys: &Types, ty: TyId, stack: &mut Vec<DataId>, apply_variance: &mut dyn FnMut(Result<usize, usize>, Variance)) {
        match tys.get(ty) {
            Ty::Error(_) | Ty::Prim(_) | Ty::SelfType => {},
            Ty::Gen(idx, _) => apply_variance(Ok(idx), Variance::Out),
            Ty::List(item) => self.derive_variance_ty(tys, item, stack, apply_variance),
            Ty::Record(fields, _) => fields.values().for_each(|field| self.derive_variance_ty(tys, *field, stack, apply_variance)),
            Ty::Func(i, o) => {
                self.derive_variance_ty(tys, i, stack, &mut |idx, var| apply_variance(idx, var.flip())); // Contravariance
                self.derive_variance_ty(tys, o, stack, apply_variance);
            },
            Ty::Data(data_id, gen_tys, _gen_effs) => {
                self.derive_variance_for(tys, data_id, stack);

                let ty_variances = self.get_data(data_id).variance_ty.clone()
                    // If recursive, no variance applied (TODO: is this sound?)
                    .unwrap_or_else(|| vec![Variance::None; gen_tys.len()]);
                for (i, ty) in gen_tys.into_iter().enumerate() {
                    self.derive_variance_ty(tys, ty, stack, &mut |idx, var| apply_variance(idx, var.project_through(ty_variances[i])));
                }
                // TODO: effects
            },
            // Everything projected through an associated type is invariant
            Ty::Assoc(ty, (_, gen_tys, _gen_effs), _) => {
                // TODO: Does the self type need to be invariant?
                self.derive_variance_ty(tys, ty, stack, &mut |idx, var| apply_variance(idx, var.combine(Variance::InOut)));
                for ty in gen_tys {
                    self.derive_variance_ty(tys, ty, stack, &mut |idx, var| apply_variance(idx, var.combine(Variance::InOut)));
                }
                // TODO: effects
            },
            Ty::Effect(eff, ty) => {
                self.derive_variance_ty(tys, ty, stack, apply_variance);
                match tys.get_effect(eff) {
                    Effect::Error => {},
                    Effect::Known(effs) => effs
                        .iter()
                        .filter_map(|eff| eff.as_ref().ok())
                        .for_each(|eff_inst| match eff_inst {
                            EffectInst::Gen(idx, _) => apply_variance(Err(*idx), Variance::InOut),
                            EffectInst::Concrete(_, gen_tys, _gen_effs) => {
                                for ty in gen_tys {
                                    self.derive_variance_ty(tys, *ty, stack, &mut |idx, var| apply_variance(idx, var.combine(Variance::InOut)));
                                }
                                // TODO: effects
                            },
                        }),
                }
            },
            ty => todo!("{ty:?}"),
        }
    }

    pub fn derive_variance_for(&mut self, tys: &Types, data_id: DataId, stack: &mut Vec<DataId>) {
        let data = self.get_data(data_id);
        if stack.contains(&data_id) {
            // Recursive
        } else if data.variance_ty.is_none() {
            let mut ty_variances = vec![Variance::None; tys.get_gen_scope(data.gen_scope).len()];
            let mut eff_variances = vec![Variance::None; tys.get_gen_scope(data.gen_scope).len_eff()];
            for (_, ty) in data.cons.clone() {
                stack.push(data_id);
                self.derive_variance_ty(tys, ty, stack, &mut |idx, var| match idx {
                    Ok(idx) => ty_variances[idx] = ty_variances[idx].combine(var),
                    Err(idx) => eff_variances[idx] = eff_variances[idx].combine(var),
                });
                stack.pop();
            }
            self.datas[data_id.0].1.as_mut().unwrap().variance_ty = Some(ty_variances);
            self.datas[data_id.0].1.as_mut().unwrap().variance_eff = Some(eff_variances);
        }
    }

    pub fn derive_variance(&mut self, tys: &Types) {
        for data_id in self.iter_datas().collect::<Vec<_>>() {
            self.derive_variance_for(tys, data_id, &mut Vec::new());
        }
    }
}
