use super::*;

pub struct Effect {
    pub name: SrcNode<Ident>,
    pub attr: Vec<SrcNode<ast::Attr>>,
    pub gen_scope: GenScopeId,
    pub send: Option<TyId>,
    pub recv: Option<TyId>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct EffectId(usize);

#[derive(Default)]
pub struct Lang {
    // pub not: Option<EffectId>,
}

#[derive(Default)]
pub struct Effects {
    lut: HashMap<Ident, (Span, EffectId)>,
    effects: Vec<Effect>,
    pub lang: Lang,
}

impl Effects {
    pub fn get(&self, eff: EffectId) -> &Effect {
        &self.effects[eff.0]
    }

    pub fn iter(&self) -> impl Iterator<Item = (EffectId, &Effect)> {
        self.effects.iter().enumerate().map(|(i, eff)| (EffectId(i), eff))
    }

    pub fn lookup(&self, name: Ident) -> Option<EffectId> {
        self.lut.get(&name).map(|(_, id)| *id)
    }

    pub fn declare(&mut self, eff: Effect) -> Result<EffectId, Error> {
        let id = EffectId(self.effects.len());
        let span = eff.name.span();
        if let Err(old) = self.lut.try_insert(*eff.name, (span, id)) {
            Err(Error::DuplicateEffectName(*eff.name, old.entry.get().0, span))
        } else {
            if let Some(lang) = eff.attr
                .iter()
                .find(|a| &**a.name == "lang")
                .and_then(|a| a.args.as_ref())
            {
                // if lang.iter().find(|a| &**a.name == "not").is_some() {
                //     self.lang.not = Some(id);
                // }
            }

            self.effects.push(eff);
            Ok(id)
        }
    }

    pub fn check_lang_items(&self) -> Vec<Error> {
        let mut errors = Vec::new();

        // if self.lang.not.is_none() { errors.push(Error::MissingLangItem("not")); }

        errors
    }

    pub fn define_send_recv(&mut self, id: EffectId, send: TyId, recv: TyId) {
        self.effects[id.0].send = Some(send);
        self.effects[id.0].recv = Some(recv);
    }
}
