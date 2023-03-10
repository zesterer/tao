use super::*;

pub struct ClassAssoc {
    pub name: SrcNode<Ident>,
}

pub struct ClassField {
    pub name: SrcNode<Ident>,
    pub ty: SrcNode<TyId>,
}

pub struct Class {
    pub name: SrcNode<Ident>,
    pub attr: Vec<SrcNode<ast::Attr>>,
    pub gen_scope: GenScopeId,
    pub assoc: Option<Vec<ClassAssoc>>,
    pub fields: Option<Vec<ClassField>>,
}

impl Class {
    pub fn assoc_ty(&self, assoc: Ident) -> Option<()> {
        self.assoc
            .as_ref()
            .expect("Class associated types must be known here")
            .iter()
            .find_map(|a| if *a.name == assoc { Some(()) } else { None })
    }

    pub fn field(&self, field: Ident) -> Option<&SrcNode<TyId>> {
        self.fields
            .as_ref()
            .expect("Class fields must be known here")
            .iter()
            .find_map(|f| if *f.name == field { Some(&f.ty) } else { None })
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ClassId(usize);

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct MemberId(pub usize);

#[derive(Default)]
pub struct Lang {
    pub not: Option<ClassId>,
    pub neg: Option<ClassId>,
    pub add: Option<ClassId>,
    pub sub: Option<ClassId>,
    pub mul: Option<ClassId>,
    pub div: Option<ClassId>,
    pub eq: Option<ClassId>,
    pub ord_ext: Option<ClassId>,
    pub and: Option<ClassId>,
    pub or: Option<ClassId>,
    pub join: Option<ClassId>,
}

#[derive(Default)]
pub struct Classes {
    lut: HashMap<Ident, (Span, ClassId)>,
    classes: Vec<Class>,
    members: Vec<Member>,
    member_lut: HashMap<ClassId, Vec<MemberId>>,
    pub lang: Lang,
}

impl Classes {
    pub fn get(&self, class: ClassId) -> &Class {
        &self.classes[class.0]
    }

    pub fn iter(&self) -> impl Iterator<Item = (ClassId, &Class)> {
        self.classes
            .iter()
            .enumerate()
            .map(|(i, class)| (ClassId(i), class))
    }

    pub fn iter_members(&self) -> impl Iterator<Item = (MemberId, &Member)> {
        self.members
            .iter()
            .enumerate()
            .map(|(i, member)| (MemberId(i), member))
    }

    pub fn class_gen_scope(&self, class: ClassId) -> GenScopeId {
        self.classes[class.0].gen_scope
    }

    pub fn lookup(&self, name: Ident) -> Option<ClassId> {
        self.lut.get(&name).map(|(_, id)| *id)
    }

    pub fn declare(&mut self, name: SrcNode<Ident>, class: Class) -> Result<ClassId, Error> {
        let id = ClassId(self.classes.len());
        let span = name.span();
        if let Err(old) = self.lut.try_insert(*name, (span, id)) {
            Err(Error::DuplicateClassName(*name, old.entry.get().0, span))
        } else {
            if let Some(lang) = class
                .attr
                .iter()
                .find(|a| &**a.name == "lang")
                .and_then(|a| a.args.as_ref())
            {
                if lang.iter().find(|a| &**a.name == "not").is_some() {
                    self.lang.not = Some(id);
                }
                if lang.iter().find(|a| &**a.name == "neg").is_some() {
                    self.lang.neg = Some(id);
                }
                if lang.iter().find(|a| &**a.name == "add").is_some() {
                    self.lang.add = Some(id);
                }
                if lang.iter().find(|a| &**a.name == "sub").is_some() {
                    self.lang.sub = Some(id);
                }
                if lang.iter().find(|a| &**a.name == "mul").is_some() {
                    self.lang.mul = Some(id);
                }
                if lang.iter().find(|a| &**a.name == "div").is_some() {
                    self.lang.div = Some(id);
                }
                if lang.iter().find(|a| &**a.name == "eq").is_some() {
                    self.lang.eq = Some(id);
                }
                if lang.iter().find(|a| &**a.name == "ord_ext").is_some() {
                    self.lang.ord_ext = Some(id);
                }
                if lang.iter().find(|a| &**a.name == "and_").is_some() {
                    self.lang.and = Some(id);
                }
                if lang.iter().find(|a| &**a.name == "or_").is_some() {
                    self.lang.or = Some(id);
                }
                if lang.iter().find(|a| &**a.name == "join").is_some() {
                    self.lang.join = Some(id);
                }
            }

            self.classes.push(class);
            Ok(id)
        }
    }

    pub fn check_lang_items(&self) -> Vec<Error> {
        let mut errors = Vec::new();

        if self.lang.not.is_none() {
            errors.push(Error::MissingLangItem("not"));
        }
        if self.lang.neg.is_none() {
            errors.push(Error::MissingLangItem("neg"));
        }
        if self.lang.add.is_none() {
            errors.push(Error::MissingLangItem("add"));
        }
        if self.lang.sub.is_none() {
            errors.push(Error::MissingLangItem("sub"));
        }
        if self.lang.mul.is_none() {
            errors.push(Error::MissingLangItem("mul"));
        }
        if self.lang.div.is_none() {
            errors.push(Error::MissingLangItem("div"));
        }
        if self.lang.eq.is_none() {
            errors.push(Error::MissingLangItem("eq"));
        }
        if self.lang.ord_ext.is_none() {
            errors.push(Error::MissingLangItem("ord_ext"));
        }
        if self.lang.and.is_none() {
            errors.push(Error::MissingLangItem("and_"));
        }
        if self.lang.or.is_none() {
            errors.push(Error::MissingLangItem("or_"));
        }
        if self.lang.join.is_none() {
            errors.push(Error::MissingLangItem("join"));
        }

        errors
    }

    pub fn define_assoc(&mut self, id: ClassId, assoc: Vec<ClassAssoc>) {
        self.classes[id.0].assoc = Some(assoc);
    }

    pub fn define_fields(&mut self, id: ClassId, fields: Vec<ClassField>) {
        self.classes[id.0].fields = Some(fields);
    }

    pub fn get_member(&self, id: MemberId) -> &Member {
        &self.members[id.0]
    }

    // TODO: Pre-insert member here so we can do inference inside members themselves
    pub fn declare_member(&mut self, class: ClassId, member: Member) -> MemberId {
        let id = MemberId(self.members.len());
        self.members.push(member);

        self.member_lut.entry(class).or_default().push(id);
        id
    }

    pub fn define_member_assoc(
        &mut self,
        id: MemberId,
        _class: ClassId,
        assoc: HashMap<Ident, TyId>,
    ) {
        self.members[id.0].assoc = Some(assoc);
    }

    pub fn define_member_fields(
        &mut self,
        id: MemberId,
        _class: ClassId,
        fields: HashMap<Ident, TyExpr>,
    ) {
        self.members[id.0].fields = Some(fields);
    }

    pub fn lookup_member(
        &self,
        hir: &Context,
        ctx: &ConContext,
        ty: ConTyId,
        (class, gen_tys, gen_effs): (ClassId, Vec<ConTyId>, Vec<Vec<ConEffectId>>),
    ) -> Option<MemberId> {
        // Returns true if member covers ty
        fn covers(
            hir: &Context,
            ctx: &ConContext,
            member: TyId,
            ty: ConTyId,
            gen_ty_links: &mut HashMap<usize, ConTyId>,
            gen_eff_links: &mut HashMap<usize, ConEffectId>,
        ) -> bool {
            match (hir.tys.get(member), ctx.get_ty(ty)) {
                (Ty::Gen(idx, _), _) => *gen_ty_links.entry(idx).or_insert(ty) == ty,
                (Ty::Prim(a), ConTy::Prim(b)) if a == *b => true,
                (Ty::List(x), ConTy::List(y)) => {
                    covers(hir, ctx, x, *y, gen_ty_links, gen_eff_links)
                }
                // TODO: Care about field names!
                (Ty::Record(xs, _), ConTy::Record(ys)) if xs.len() == ys.len() => xs
                    .into_iter()
                    .zip(ys.iter())
                    .all(|((_, x), (_, y))| covers(hir, ctx, x, *y, gen_ty_links, gen_eff_links)),
                (Ty::Func(x_i, x_o), ConTy::Func(y_i, y_o)) => {
                    covers(hir, ctx, x_i, *y_i, gen_ty_links, gen_eff_links)
                        && covers(hir, ctx, x_o, *y_o, gen_ty_links, gen_eff_links)
                }
                (Ty::Data(x, xs), ConTy::Data(y)) if x == y.0 .0 && xs.len() == y.0 .1.len() => xs
                    .into_iter()
                    .zip(y.0 .1.iter())
                    .all(|(x, y)| covers(hir, ctx, x, *y, gen_ty_links, gen_eff_links)),
                // Flatten empty effects
                (_, ConTy::Effect(eff, ty)) if eff.is_empty() => {
                    covers(hir, ctx, member, *ty, gen_ty_links, gen_eff_links)
                }
                (Ty::Effect(_, _), ConTy::Effect(_, _)) => todo!(),
                (Ty::Error(_), _) => panic!("Error ty during monomorphisation"),
                _ => false,
            }
        }

        fn covers_eff(
            hir: &Context,
            _ctx: &ConContext,
            member: EffectId,
            effs: &[ConEffectId],
            _gen_ty_links: &mut HashMap<usize, ConTyId>,
            _gen_eff_links: &mut HashMap<usize, ConEffectId>,
        ) -> bool {
            match (hir.tys.get_effect(member), effs) {
                (Effect::Known(member_effs), effs) if member_effs.len() <= 1 || effs.len() <= 1  => effs
                    .iter()
                    .all(|eff| member_effs
                        .iter()
                        .any(|member_eff| match (member_eff, eff) {
                            (Ok(EffectInst::Gen(_idx, _)), _eff) => {
                                // println!("Compare {:?} with {:?}", gen_eff_links.get(idx), eff);
                                // gen_eff_links.entry(*idx).or_insert(*eff) == eff
                                // TODO: Is this correct? Can all generics be assumed to match?!
                                true
                            },
                            (x, y) => todo!("You know, you should really impl effect-polymorphic class monomorphisation: {:?} covers {:?}", x, y),
                        })),
                (Effect::Error, _) => panic!("Error eff during monomorphisation"),
                (x, y) => todo!("{:?} covers {:?}", x, y),
            }
        }

        self.member_lut.get(&class).and_then(|xs| {
            let candidates =
                xs.iter()
                    .filter(|m| {
                        let member = self.get_member(**m);
                        let mut gen_ty_links = HashMap::new();
                        let mut gen_eff_links = HashMap::new();
                        covers(
                            hir,
                            ctx,
                            member.member,
                            ty,
                            &mut gen_ty_links,
                            &mut gen_eff_links,
                        ) && member.gen_tys.iter().zip(gen_tys.iter()).all(
                            |(member_gen_ty, gen_ty)| {
                                covers(
                                    hir,
                                    ctx,
                                    *member_gen_ty,
                                    *gen_ty,
                                    &mut gen_ty_links,
                                    &mut gen_eff_links,
                                )
                            },
                        ) && member.gen_effs.iter().zip(gen_effs.iter()).all(
                            |(member_gen_eff, gen_eff)| {
                                covers_eff(
                                    hir,
                                    ctx,
                                    member_gen_eff.expect("Error eff during monomorphisation"),
                                    gen_eff,
                                    &mut gen_ty_links,
                                    &mut gen_eff_links,
                                )
                            },
                        )
                    })
                    .collect::<Vec<_>>();

            assert!(
                candidates.len() <= 1,
                "Multiple member candidates detected during lowering <{:?} as {:?}>.\n\
                    This means that incoherence has occurred!\n\
                    Candidate members:\n{}",
                ctx.get_ty(ty),
                **self.get(class).name,
                candidates
                    .iter()
                    .map(|c| {
                        let c = self.get_member(**c);
                        let gen_scope = hir.tys.get_gen_scope(c.gen_scope);
                        format!(
                            "- {}member {} of {}{} (in {})\n",
                            if gen_scope.is_empty() {
                                String::new()
                            } else {
                                format!(
                                    "for {} ",
                                    (0..gen_scope.len())
                                        .map(|idx| format!("{}", *gen_scope.get(idx).name))
                                        .collect::<Vec<_>>()
                                        .join(", ")
                                )
                            },
                            hir.tys.display(hir, c.member),
                            **self.get(class).name,
                            c.gen_tys
                                .iter()
                                .map(|ty| format!(" {}", hir.tys.display(hir, *ty)))
                                .collect::<String>(),
                            hir.tys.get_span(c.member).src(),
                        )
                    })
                    .collect::<Vec<_>>()
                    .join("")
            );

            candidates.first().copied().copied()
        })
    }

    pub fn members_of(&self, class: ClassId) -> impl Iterator<Item = (MemberId, &Member)> {
        self.member_lut
            .get(&class)
            .map(|m| m.as_slice())
            .unwrap_or(&[])
            .iter()
            .map(|m| (*m, self.get_member(*m)))
    }
}

pub enum MemberItem {
    Value { name: SrcNode<Ident>, val: TyExpr },
    Type { name: SrcNode<Ident>, ty: TyId },
}

pub struct Member {
    pub gen_scope: GenScopeId,
    pub attr: Vec<SrcNode<ast::Attr>>,
    pub member: TyId,
    pub class: ClassId,
    pub gen_tys: Vec<TyId>,
    pub gen_effs: Vec<Option<EffectId>>,
    pub assoc: Option<HashMap<Ident, TyId>>,
    pub fields: Option<HashMap<Ident, TyExpr>>,
}

impl Member {
    pub fn assoc_ty(&self, assoc: Ident) -> Option<TyId> {
        self.assoc
            .as_ref()
            .expect("Member associated types not initialised")
            .get(&assoc)
            .copied()
    }

    pub fn field(&self, field: Ident) -> Option<&TyExpr> {
        self.fields
            .as_ref()
            .expect("Member fields not initialised")
            .get(&field)
    }
}
