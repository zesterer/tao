use super::*;

pub struct EffectDecl {
    pub name: SrcNode<Ident>,
    pub attr: Vec<SrcNode<ast::Attr>>,
    pub gen_scope: GenScopeId,
    pub send: Option<TyId>,
    pub recv: Option<TyId>,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct EffectDeclId(usize, Ident);

impl fmt::Debug for EffectDeclId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.1)
    }
}

pub struct EffectAlias {
    pub name: SrcNode<Ident>,
    pub attr: Vec<SrcNode<ast::Attr>>,
    pub gen_scope: GenScopeId,
    pub effects: Option<Vec<(SrcNode<EffectDeclId>, Vec<TyId>)>>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct EffectAliasId(usize);

#[derive(Default)]
pub struct Lang {
    // pub not: Option<EffectDeclId>,
}

#[derive(Default)]
pub struct Effects {
    lut: HashMap<Ident, (Span, Result<EffectDeclId, EffectAliasId>)>,
    effect_decls: Vec<EffectDecl>,
    effect_aliases: Vec<EffectAlias>,
    pub lang: Lang,
}

impl Effects {
    pub fn get_decl(&self, eff: EffectDeclId) -> &EffectDecl {
        &self.effect_decls[eff.0]
    }

    pub fn get_alias(&self, alias: EffectAliasId) -> &EffectAlias {
        &self.effect_aliases[alias.0]
    }

    pub fn iter(&self) -> impl Iterator<Item = (EffectDeclId, &EffectDecl)> {
        self.effect_decls
            .iter()
            .enumerate()
            .map(|(i, eff)| (EffectDeclId(i, *eff.name), eff))
    }

    // pub fn iter(&self) -> impl Iterator<Item = (EffectDeclId, &EffectDecl)> {
    //     self.effect_names.iter().enumerate().map(|(i, eff)| (EffectDeclId(i), eff))
    // }

    pub fn lookup(&self, name: Ident) -> Option<Result<EffectDeclId, EffectAliasId>> {
        self.lut.get(&name).map(|(_, id)| *id)
    }

    pub fn declare(&mut self, eff: EffectDecl) -> Result<EffectDeclId, Error> {
        let id = EffectDeclId(self.effect_decls.len(), *eff.name);
        let span = eff.name.span();
        if let Err(old) = self.lut.try_insert(*eff.name, (span, Ok(id))) {
            Err(Error::DuplicateEffectDecl(
                *eff.name,
                old.entry.get().0,
                span,
            ))
        } else {
            if let Some(_lang) = eff
                .attr
                .iter()
                .find(|a| &**a.name == "lang")
                .and_then(|a| a.args.as_ref())
            {
                // if lang.iter().find(|a| &**a.name == "not").is_some() {
                //     self.lang.not = Some(id);
                // }
            }

            self.effect_decls.push(eff);
            Ok(id)
        }
    }

    pub fn declare_alias(&mut self, alias: EffectAlias) -> Result<EffectAliasId, Error> {
        let id = EffectAliasId(self.effect_aliases.len());
        let span = alias.name.span();
        if let Err(old) = self.lut.try_insert(*alias.name, (span, Err(id))) {
            Err(Error::DuplicateEffectDecl(
                *alias.name,
                old.entry.get().0,
                span,
            ))
        } else {
            self.effect_aliases.push(alias);
            Ok(id)
        }
    }

    pub fn check_lang_items(&self) -> Vec<Error> {
        let errors = Vec::new();

        // if self.lang.not.is_none() { errors.push(Error::MissingLangItem("not")); }

        errors
    }

    pub fn define_send_recv(&mut self, id: EffectDeclId, send: TyId, recv: TyId) {
        self.effect_decls[id.0].send = Some(send);
        self.effect_decls[id.0].recv = Some(recv);
    }

    pub fn define_alias_effects(
        &mut self,
        id: EffectAliasId,
        effs: Vec<(SrcNode<EffectDeclId>, Vec<TyId>)>,
    ) {
        self.effect_aliases[id.0].effects = Some(effs);
    }
}
