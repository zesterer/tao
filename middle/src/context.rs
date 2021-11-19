use super::*;

pub struct Context {
    pub reprs: Reprs,
    pub procs: Procs,
    pub entry: Option<ProcId>,
}

impl Context {
    pub fn from_hir(hir: &HirContext) -> (Self, Vec<Error>) {
        let mut this = Self {
            reprs: Reprs::default(),
            procs: Procs::default(),
            entry: None,
        };

        let mut errors = Vec::new();

        let mut entries = hir.defs
            .iter()
            .filter_map(|(id, def)| def.attr
                .iter()
                .find(|attr| attr.as_str() == "main")
                .zip(Some((id, def))));

        if let Some((entry_attr, (id, main))) = entries.next() {
            if let Some((_, (_, second))) = entries.next() {
                errors.push(Error::MultipleEntryPoints(main.name.span(), second.name.span()));
            }

            let gen_scope = hir.tys.get_gen_scope(main.gen_scope);
            if gen_scope.len() == 0 {
                let main = lower::lower_def(&mut this, hir, id, Vec::new());
                this.entry = Some(main);
            } else {
                errors.push(Error::GenericEntryPoint(main.name.clone(), gen_scope.span, entry_attr.span()));
            }
        } else {
            errors.push(Error::NoEntryPoint(hir.root_span));
        }

        (this, errors)
    }

    pub fn optimize(&mut self) {
        for _ in 0..10 {
            opt::FlattenSingleField::default().apply(self);
            opt::ConstFold::default().apply(self);
            opt::RemoveUnusedBindings::default().apply(self);
        }
    }
}
