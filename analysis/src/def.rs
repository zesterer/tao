use super::*;

pub struct Def {
    pub name: SrcNode<Ident>,
    pub attr: Vec<SrcNode<ast::Attr>>,
    pub gen_scope: GenScopeId,
    pub ty_hint: Option<TyId>,
    pub body: Option<TyExpr>,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct DefId(pub usize, Ident);

impl fmt::Debug for DefId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.1)
    }
}

#[derive(Default)]
pub struct Lang {
    // pub io_unit: Option<DefId>,
}

#[derive(Default)]
pub struct Defs {
    lut: HashMap<Ident, (Span, DefId)>,
    defs: Vec<Def>,
    pub lang: Lang,
}

impl Defs {
    pub fn iter(&self) -> impl Iterator<Item = (DefId, &Def)> {
        self.defs
            .iter()
            .enumerate()
            .map(|(i, d)| (DefId(i, *d.name), d))
    }

    pub fn get(&self, def: DefId) -> &Def {
        &self.defs[def.0]
    }

    pub fn lookup(&self, name: Ident) -> Option<DefId> {
        self.lut.get(&name).map(|(_, id)| *id)
    }

    pub fn declare(&mut self, def: Def) -> Result<DefId, Error> {
        let id = DefId(self.defs.len(), *def.name);
        let name = *def.name;
        let span = def.name.span();
        if let Err(old) = self.lut.try_insert(name, (span, id)) {
            Err(Error::DuplicateDefName(name, old.entry.get().0, span))
        } else {
            if let Some(_lang) = def
                .attr
                .iter()
                .find(|a| &**a.name == "lang")
                .and_then(|a| a.args.as_ref())
            {
                // if lang.iter().find(|a| &**a.name == "io_unit").is_some() {
                //     self.lang.io_unit = Some(id);
                // }
            }

            self.defs.push(def);
            Ok(id)
        }
    }

    pub fn check_lang_items(&self) -> Vec<Error> {
        let errors = Vec::new();

        // if self.lang.io_unit.is_none() { errors.push(Error::MissingLangItem("io_unit")); }

        errors
    }

    pub fn define_body(&mut self, id: DefId, expr: TyExpr) {
        self.defs[id.0].body = Some(expr);
    }
}
