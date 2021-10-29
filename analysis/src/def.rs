use super::*;

pub struct Def {
    pub name: Ident,
    pub gen_scope: GenScopeId,
    pub ty_hint: SrcNode<ast::Type>,
    pub body: Option<TyExpr>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct DefId(usize);

#[derive(Default)]
pub struct Defs {
    lut: HashMap<Ident, DefId>,
    defs: Vec<Def>,
}

impl Defs {
    pub fn get(&self, def: DefId) -> &Def {
        &self.defs[def.0]
    }

    pub fn lookup(&self, name: Ident) -> Option<DefId> {
        self.lut.get(&name).copied()
    }

    pub fn declare(&mut self, name: Ident, gen_scope: GenScopeId, ty_hint: SrcNode<ast::Type>) -> DefId {
        let id = DefId(self.defs.len());
        self.defs.push(Def {
            name,
            gen_scope,
            ty_hint,
            body: None,
        });
        self.lut.insert(name, id);
        id
    }

    pub fn define_body(&mut self, id: DefId, expr: TyExpr) {
        self.defs[id.0].body = Some(expr);
    }
}
