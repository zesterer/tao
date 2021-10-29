use super::*;

pub struct Data {
    pub name: Ident,
    pub gen_scope: GenScopeId,
    pub cons: HashMap<Ident, TyId>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct DataId(usize);

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct AliasId(usize);

pub struct Alias {
    pub name: Ident,
    pub gen_scope: GenScopeId,
    pub ty: TyId,
}

#[derive(Default)]
pub struct Datas {
    // TODO: Don't use `Result`
    name_lut: HashMap<Ident, Result<DataId, AliasId>>,
    cons_lut: HashMap<Ident, DataId>,
    alias_lut: HashMap<Ident, Alias>,
    datas: Vec<Option<Data>>,
    aliases: Vec<(Span, Option<Alias>)>,
}

impl Datas {
    pub fn lookup_data(&self, name: Ident) -> Option<DataId> {
        self.name_lut
            .get(&name)
            .and_then(|data| data.as_ref().ok())
            .copied()
    }

    pub fn lookup_alias(&self, name: Ident) -> Option<AliasId> {
        self.name_lut
            .get(&name)
            .and_then(|data| data.as_ref().err())
            .copied()
    }

    pub fn lookup_cons(&self, name: Ident) -> Option<DataId> {
        self.cons_lut.get(&name).copied()
    }

    pub fn get_data(&self, data: DataId) -> &Data {
        self.datas[data.0]
            .as_ref()
            .expect("Declared data accessed before being defined")
    }

    pub fn get_alias(&self, alias: AliasId) -> Option<&Alias> {
        self.aliases[alias.0]
            .1
            .as_ref()
    }

    pub fn get_alias_span(&self, alias: AliasId) -> Span {
        self.aliases[alias.0].0
    }

    pub fn declare_data(&mut self, name: Ident) -> DataId {
        let id = DataId(self.datas.len());
        self.datas.push(None);
        self.name_lut.insert(name, Ok(id));
        id
    }

    pub fn declare_alias(&mut self, name: Ident, span: Span) -> AliasId {
        let id = AliasId(self.aliases.len());
        self.aliases.push((span, None));
        self.name_lut.insert(name, Err(id));
        id
    }

    pub fn define_data(&mut self, id: DataId, data: Data) {
        for cons in data.cons.keys() {
            self.cons_lut.insert(*cons, id);
        }
        self.datas[id.0] = Some(data);
    }

    pub fn define_alias(&mut self, id: AliasId, alias: Alias) {
        self.aliases[id.0].1 = Some(alias);
    }
}
