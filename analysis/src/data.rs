use super::*;

pub struct Data {
    pub name: SrcNode<Ident>,
    pub attr: Vec<SrcNode<ast::Attr>>,
    pub gen_scope: GenScopeId,
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
    datas: Vec<(Span, Option<Data>, GenScopeId, Ident)>,
    aliases: Vec<(Span, Option<Alias>, GenScopeId)>,
    pub lang: Lang,
}

impl Datas {
    pub fn iter_datas(&self) -> impl Iterator<Item = DataId> + '_ {
        (0..self.datas.len()).map(|i| DataId(i, Ident::new(self.datas[i].3)))
    }

    pub fn iter_aliases(&self) -> impl Iterator<Item = AliasId> {
        (0..self.aliases.len()).map(AliasId)
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
        self.aliases[alias.0].1.as_ref()
    }

    pub fn get_alias_span(&self, alias: AliasId) -> Span {
        self.aliases[alias.0].0
    }

    pub fn declare_data(
        &mut self,
        name: SrcNode<Ident>,
        gen_scope: GenScopeId,
        attr: &[SrcNode<ast::Attr>],
    ) -> Result<DataId, Error> {
        let id = DataId(self.datas.len(), *name);
        if let Err(old) = self
            .name_lut
            .try_insert(*name, (name.span(), Ok(id), gen_scope))
        {
            Err(Error::DuplicateTypeName(
                *name,
                old.entry.get().0,
                name.span(),
            ))
        } else {
            if let Some(lang) = attr
                .iter()
                .find(|a| &**a.name == "lang")
                .and_then(|a| a.args.as_ref())
            {
                if lang.iter().any(|a| &**a.name == "go") {
                    self.lang.go = Some(id);
                }
                if lang.iter().any(|a| &**a.name == "bool") {
                    self.lang.r#bool = Some(id);
                }
            }

            self.datas.push((name.span(), None, gen_scope, *name));
            Ok(id)
        }
    }

    pub fn check_lang_items(&self) -> Vec<Error> {
        let mut errors = Vec::new();

        if self.lang.go.is_none() {
            errors.push(Error::MissingLangItem("go"));
        }
        if self.lang.r#bool.is_none() {
            errors.push(Error::MissingLangItem("bool"));
        }

        errors
    }

    pub fn declare_alias(
        &mut self,
        name: Ident,
        span: Span,
        gen_scope: GenScopeId,
    ) -> Result<AliasId, Error> {
        let id = AliasId(self.aliases.len());
        if let Err(old) = self.name_lut.try_insert(name, (span, Err(id), gen_scope)) {
            Err(Error::DuplicateTypeName(name, old.entry.get().0, span))
        } else {
            self.aliases.push((span, None, gen_scope));
            Ok(id)
        }
    }

    pub fn define_data(&mut self, id: DataId, _span: Span, data: Data) -> Result<(), Vec<Error>> {
        let mut errors = Vec::new();
        for (cons, _) in &data.cons {
            if let Err(old) = self.cons_lut.try_insert(**cons, (cons.span(), id)) {
                errors.push(Error::DuplicateConsName(
                    **cons,
                    old.entry.get().0,
                    cons.span(),
                ));
            }
        }
        self.datas[id.0].1 = Some(data);
        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    pub fn define_alias(&mut self, id: AliasId, alias: Alias) {
        self.aliases[id.0].1 = Some(alias);
    }
}
