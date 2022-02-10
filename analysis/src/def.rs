use super::*;

pub struct Def {
    pub name: SrcNode<Ident>,
    pub attr: Vec<SrcNode<ast::Attr>>,
    pub gen_scope: GenScopeId,
    pub ty_hint: Option<TyId>,
    pub body: Option<TyExpr>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct DefId(usize);

#[derive(Default)]
pub struct Defs {
    lut: HashMap<Ident, (Span, DefId)>,
    defs: Vec<Def>,
}

impl Defs {
    pub fn iter(&self) -> impl Iterator<Item = (DefId, &Def)> {
        self.defs
            .iter()
            .enumerate()
            .map(|(i, d)| (DefId(i), d))
    }

    pub fn get(&self, def: DefId) -> &Def {
        &self.defs[def.0]
    }

    pub fn lookup(&self, name: Ident) -> Option<DefId> {
        self.lut.get(&name).map(|(_, id)| *id)
    }

    pub fn declare(&mut self, def: Def) -> Result<DefId, Error> {
        let id = DefId(self.defs.len());
        let name = *def.name;
        let span = def.name.span();
        self.defs.push(def);
        if let Err(old) = self.lut.try_insert(name, (span, id)) {
            Err(Error::DuplicateDefName(name, old.entry.get().0, span))
        } else {
            Ok(id)
        }
    }

    pub fn define_body(&mut self, id: DefId, expr: TyExpr) {
        self.defs[id.0].body = Some(expr);
    }
}
