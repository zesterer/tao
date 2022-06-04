use super::*;

pub type ConMeta = ConTyId;
pub type ConNode<T> = Node<T, ConMeta>;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ConTy {
    Prim(Prim),
    List(ConTyId),
    Record(BTreeMap<Ident, ConTyId>),
    Func(ConTyId, ConTyId),
    Data(ConDataId),
    Effect(ConEffectId, ConTyId),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ConTyId(usize);

impl ConTyId {
    pub fn id(&self) -> u64 { self.0 as u64 }
}

#[derive(Debug)]
pub struct ConEffect {
    send: ConTyId,
    recv: ConTyId,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum ConProc {
    Def(DefId, Vec<ConTyId>),
    Field(ConTyId, MemberId, Vec<ConTyId>, Ident),
}

impl fmt::Display for ConProc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ConProc::Def(def, params) => write!(f, "{}<{}>",
                def.0,
                params.iter().map(|ty| ty.0.to_string()).collect::<Vec<_>>().join(", "),
            ),
            ConProc::Field(ty, member, args, field) => write!(f, "<{} in {}{}>.{}",
                ty.0,
                member.0,
                args.iter().map(|ty| format!(" {}", ty.0.to_string())).collect::<String>(),
                field,
            ),
        }
    }
}

pub type ConProcId = Intern<ConProc>;

pub type ConDataId = Intern<(DataId, Vec<ConTyId>)>;

pub type ConEffectId = Intern<(EffectDeclId, Vec<ConTyId>)>;

pub struct TyInsts<'a> {
    self_ty: Option<ConTyId>,
    gen: &'a [ConTyId],
}

pub struct ConData {
    pub is_recursive: bool,
    pub cons: Vec<(Ident, ConTyId)>,
}

pub struct ConContext {
    datas: HashMap<ConDataId, Result<ConData, bool>>,
    effects: HashMap<ConEffectId, ConEffect>,
    tys: Vec<ConTy>,
    ty_lookup: HashMap<ConTy, ConTyId>,
    procs: HashMap<ConProcId, Option<ConExpr>>,
    entry: Option<ConProcId>,
}

impl ConContext {
    pub fn from_ctx(hir: &Context) -> (Self, Vec<Error>) {
        let mut this = Self {
            datas: HashMap::default(),
            effects: HashMap::default(),
            tys: Vec::new(),
            ty_lookup: HashMap::default(),
            procs: HashMap::default(),
            entry: None,
        };

        let mut errors = Vec::new();

        let mut entries = hir.defs
            .iter()
            .filter_map(|(id, def)| if def.attr
                .iter()
                .find(|attr| attr.name.as_str() == "entry")
                .is_some()
            {
                Some((id, def))
            } else {
                None
            })
            .collect::<Vec<_>>();
        // If no entry point attribute exists, use 'main'
        if entries.is_empty() {
            entries.extend(hir.defs
                .lookup(Ident::new("main"))
                .map(|id| (id, hir.defs.get(id))));
        }

        let mut entries = entries.into_iter();
        if let Some((id, main)) = entries.next() {
            if let Some((_, second)) = entries.next() {
                errors.push(Error::MultipleEntryPoints(main.name.span(), second.name.span()));
            }

            let gen_scope = hir.tys.get_gen_scope(main.gen_scope);
            if gen_scope.len() == 0 {
                let main_def = Intern::new(ConProc::Def(id, Vec::new()));
                this.lower_proc(hir, main_def);
                this.entry = Some(main_def);
            } else {
                errors.push(Error::GenericEntryPoint(main.name.clone(), gen_scope.get(0).name.span()));
            }
        } else {
            errors.push(Error::NoEntryPoint(hir.root_span));
        }

        (this, errors)
    }

    pub fn entry_proc(&self) -> ConProcId {
        self.entry.clone().unwrap()
    }

    pub fn get_proc(&self, proc: ConProcId) -> &ConExpr {
        // Can't fail
        self.procs[&proc].as_ref().unwrap()
    }

    pub fn get_ty(&self, ty: ConTyId) -> &ConTy {
        &self.tys[ty.0]
    }

    pub fn get_data(&self, data: ConDataId) -> &ConData {
        self.datas[&data].as_ref().expect("Data should be fully defined")
    }

    pub fn get_effect(&self, eff: ConEffectId) -> &ConEffect {
        &self.effects[&eff]
    }

    pub fn insert_ty(&mut self, ty: ConTy) -> ConTyId {
        *self.ty_lookup
            .entry(ty.clone())
            .or_insert_with(|| {
                let id = ConTyId(self.tys.len());
                self.tys.push(ty);
                id
            })
    }

    fn derive_links(&self, hir: &Context, member: TyId, ty: ConTyId, link_gen: &mut impl FnMut(usize, ConTyId)) {
        match (hir.tys.get(member), self.get_ty(ty)) {
            (Ty::Prim(x), ConTy::Prim(y)) => assert_eq!(x, *y),
            (Ty::Gen(gen_idx, _), _) => link_gen(gen_idx, ty),
            (Ty::List(x), ConTy::List(y)) => self.derive_links(hir, x, *y, link_gen),
            (Ty::Record(xs), ConTy::Record(ys)) => xs
                .into_iter()
                .zip(ys.into_iter())
                .for_each(|((_, x), (_, y))| self.derive_links(hir, x, *y, link_gen)),
            (Ty::Func(x_i, x_o), ConTy::Func(y_i, y_o)) => {
                self.derive_links(hir, x_i, *y_i, link_gen);
                self.derive_links(hir, x_o, *y_o, link_gen);
            },
            (Ty::Data(_, xs), ConTy::Data(y)) => xs
                .into_iter()
                .zip(y.1.iter())
                .for_each(|(x, y)| self.derive_links(hir, x, *y, link_gen)),
            (Ty::Effect(x, x_out), ConTy::Effect(y, y_out)) => {
                self.derive_links_effect(hir, x, *y, link_gen);
                self.derive_links(hir, x_out, *y_out, link_gen);
            },
            (x, y) => todo!("{:?}", (x, y)),
        }
    }

    fn derive_links_effect(&self, hir: &Context, member: EffectId, eff: ConEffectId, link_gen: &mut impl FnMut(usize, ConTyId)) {
        // TODO: link gen for effects when polymorphic effects are added
        match (hir.tys.get_effect(member), self.get_effect(eff)) {
            (Effect::Known(_, xs), _) => xs
                .into_iter()
                .zip(eff.1.iter())
                .for_each(|(x, y)| self.derive_links(hir, x, *y, link_gen)),
            (x, y) => todo!("{:?}", (x, y)),
        }
    }

    pub fn lower_data(&mut self, hir: &Context, data: DataId, args: &[ConTyId]) -> ConDataId {
        let id = Intern::new((data, args.to_vec()));
        if let Some(data) = self.datas.get_mut(&id) {
            if let Err(is_recursive) = data {
                // We're already in the process of initialising this data type so it must be recursive
                *is_recursive = true;
            }
        } else {
            self.datas.insert(id, Err(false)); // Prevent overflow with phoney value
            let mut data = ConData {
                is_recursive: false,
                cons: hir.datas
                    .get_data(data)
                    .cons
                    .iter()
                    .map(|(name, ty)| (**name, self.lower_ty(hir, *ty, &TyInsts {
                        self_ty: None,
                        gen: &args,
                    })))
                    .collect(),
            };
            // Mark the data type as recursive if the recursive flag got set during lowering
            if *self.datas.get(&id).unwrap().as_ref().map(|_| ()).unwrap_err() {
                data.is_recursive = true;
            }
            self.datas.insert(id, Ok(data));
        }

        id
    }

    pub fn lower_ty(&mut self, hir: &Context, ty: TyId, ty_insts: &TyInsts) -> ConTyId {
        let cty = match hir.tys.get(ty) {
            Ty::Error(_) => panic!("Concretizable type cannot be an error"),
            Ty::Prim(prim) => ConTy::Prim(prim),
            Ty::List(item) => ConTy::List(self.lower_ty(hir, item, ty_insts)),
            Ty::Record(fields) => ConTy::Record(fields
                .into_iter()
                .map(|(name, field)| (name, self.lower_ty(hir, field, ty_insts)))
                .collect()),
            Ty::Func(i, o) => ConTy::Func(
                self.lower_ty(hir, i, ty_insts),
                self.lower_ty(hir, o, ty_insts),
            ),
            Ty::Data(data, args) => {
                let args = args
                    .into_iter()
                    .map(|arg| self.lower_ty(hir, arg, ty_insts))
                    .collect::<Vec<_>>();
                ConTy::Data(self.lower_data(hir, data, &args))
            },
            Ty::Gen(idx, _) => return ty_insts.gen[idx],
            Ty::SelfType => return ty_insts.self_ty.expect("Self type required during concretization but none was provided"),
            Ty::Assoc(ty, (class_id, args), assoc) => {
                let self_ty = self.lower_ty(hir, ty, ty_insts);
                let args = args
                    .into_iter()
                    .map(|arg| self.lower_ty(hir, arg, ty_insts))
                    .collect::<Vec<_>>();
                let member = hir.classes
                    .lookup_member(hir, self, self_ty, (class_id, args.clone()))
                    .map(|m| hir.classes.get_member(m))
                    .unwrap_or_else(|| panic!(
                        "Could not select member candidate for {} as {}{}",
                        self.display(hir, self_ty),
                        *hir.classes.get(class_id).name,
                        args.iter().map(|arg| format!(" {}", self.display(hir, *arg))).collect::<String>(),
                    ));
                let member_gen_scope = hir.tys.get_gen_scope(member.gen_scope);

                let mut links = HashMap::new();
                self.derive_links(hir, member.member, self_ty, &mut |gen_idx, ty| { links.insert(gen_idx, ty); });
                assert_eq!(
                    args.len(),
                    member.args.len(),
                    "Member and instance args must be the same length in member {} of {}",
                    hir.tys.display(hir, member.member),
                    *hir.classes.get(class_id).name,
                );
                for (member_arg, arg) in member.args.iter().zip(args.iter()) {
                    self.derive_links(hir, *member_arg, *arg, &mut |gen_idx, ty| { links.insert(gen_idx, ty); });
                }
                let gen = (0..member_gen_scope.len())
                    .map(|idx| *links.get(&idx).expect("Generic type not mentioned in member"))
                    .collect::<Vec<_>>();

                let assoc = member
                    .assoc_ty(*assoc)
                    .unwrap();
                return self.lower_ty(hir, assoc, &TyInsts {
                    self_ty: Some(self_ty),
                    gen: &gen,
                });
            },
            Ty::Effect(eff, out) => match hir.tys.get_effect(eff) {
                Effect::Error => panic!("Concretizable effect cannot be an error"),
                Effect::Known(decl, args) => {
                    let args = args
                        .into_iter()
                        .map(|arg| self.lower_ty(hir, arg, ty_insts))
                        .collect::<Vec<_>>();
                    let out = self.lower_ty(hir, out, ty_insts);
                    ConTy::Effect(Intern::new((decl, args.to_vec())), out)
                },
            },
        };

        self.insert_ty(cty)
    }

    // Returns (record_ty, field_ty, number_of_indirections)
    pub fn follow_field_access(&self, hir: &Context, mut ty: ConTyId, field: Ident) -> Option<(ConTyId, ConTyId, usize)> {
        let mut already_seen = Vec::new();

        loop {
            match self.get_ty(ty).clone() {
                ConTy::Data(data_id) => if already_seen.contains(&data_id.0) {
                    // We've already seen this data type, it must be recursive. Give up, it has no fields.
                    break None
                } else {
                    already_seen.push(data_id.0);
                    let data = self.get_data(data_id);
                    if data.cons.len() == 1 {
                        ty = data.cons[0].1;
                    } else {
                        // Sum types have no fields
                        break None;
                    }
                },
                ConTy::Record(fields) => if let Some((_, field_ty)) = fields.iter().find(|(name, _)| **name == field) {
                    break Some((ty, *field_ty, already_seen.len()));
                } else {
                    // Record has no such field
                    break None;
                },
                _ => break None, // Only `Data` or `Record` can have fields
            }
        }
    }

    pub fn lower_proc(&mut self, hir: &Context, proc: ConProcId) {
        if !self.procs.contains_key(&proc) {
            self.procs.insert(proc, None);

            let body = match &*proc {
                ConProc::Def(def, gen) => self.lower_expr(
                    hir,
                    hir.defs
                        .get(*def)
                        .body
                        .as_ref()
                        .unwrap(),
                    &TyInsts { self_ty: None, gen },
                ),
                ConProc::Field(self_ty, member_id, args, field) => {
                    let member = hir.classes.get_member(*member_id);
                    let member_gen_scope = hir.tys.get_gen_scope(member.gen_scope);

                    let mut links = HashMap::new();
                    self.derive_links(hir, member.member, *self_ty, &mut |gen_idx, ty| { links.insert(gen_idx, ty); });
                    assert_eq!(
                        args.len(),
                        member.args.len(),
                        "Member and instance args must be the same length in member {} of {}",
                        hir.tys.display(hir, member.member),
                        *hir.classes.get(hir.classes.get_member(*member_id).class).name,
                    );
                    for (member_arg, arg) in member.args.iter().zip(args.iter()) {
                        self.derive_links(hir, *member_arg, *arg, &mut |gen_idx, ty| { links.insert(gen_idx, ty); });
                    }
                    let gen = (0..member_gen_scope.len())
                        .map(|idx| *links.get(&idx).expect("Generic type not mentioned in member"))
                        .collect::<Vec<_>>();
                    self.lower_expr(
                        hir,
                        member
                            .field(*field)
                            .unwrap(),
                        &TyInsts { self_ty: Some(*self_ty), gen: &gen },
                    )
                },
            };
            self.procs.insert(proc, Some(body));
        }
    }

    pub fn lower_binding(&mut self, hir: &Context, binding: &TyBinding, ty_insts: &TyInsts) -> ConBinding {
        let pat = match &*binding.pat {
            hir::Pat::Error => panic!("Error pattern should not exist during concretization"),
            hir::Pat::Wildcard => hir::Pat::Wildcard,
            hir::Pat::Literal(litr) => hir::Pat::Literal(*litr),
            hir::Pat::Single(inner) => hir::Pat::Single(self.lower_binding(hir, inner, ty_insts)),
            hir::Pat::Add(lhs, rhs) => hir::Pat::Add(self.lower_binding(hir, lhs, ty_insts), rhs.clone()),
            hir::Pat::Record(fields) => hir::Pat::Record(fields
                .iter()
                .map(|(name, field)| (*name, self.lower_binding(hir, field, ty_insts)))
                .collect()),
            hir::Pat::ListExact(items) => hir::Pat::ListExact(items
                .iter()
                .map(|item| self.lower_binding(hir, item, ty_insts))
                .collect()),
            hir::Pat::ListFront(items, tail) => hir::Pat::ListFront(items
                .iter()
                .map(|item| self.lower_binding(hir, item, ty_insts))
                .collect(), tail.as_ref().map(|tail| self.lower_binding(hir, tail, ty_insts))),
            hir::Pat::Decons(data, variant, inner) => {
                let ty = self.lower_ty(hir, binding.meta().1, ty_insts);
                let ConTy::Data(data) = self.get_ty(ty) else { unreachable!() };
                hir::Pat::Decons(*data, *variant, self.lower_binding(hir, inner, ty_insts))
            },
        };

        ConNode::new(
            hir::Binding {
                pat: SrcNode::new(pat, binding.pat.span()),
                name: binding.name.clone(),
            },
            self.lower_ty(hir, binding.meta().1, ty_insts),
        )
    }

    pub fn lower_expr(&mut self, hir: &Context, ty_expr: &TyExpr, ty_insts: &TyInsts) -> ConExpr {
        let expr = match ty_expr.inner() {
            hir::Expr::Error => panic!("Error expression should not exist during concretization"),
            hir::Expr::Literal(litr) => hir::Expr::Literal(*litr),
            hir::Expr::Local(local) => hir::Expr::Local(*local),
            hir::Expr::Global((x, args)) => {
                let args = args
                    .iter()
                    .map(|arg| self.lower_ty(hir, arg.1, ty_insts))
                    .collect::<Vec<_>>();
                let id = Intern::new(ConProc::Def(*x, args.clone()));
                self.lower_proc(hir, id);
                hir::Expr::Global(id)
            },
            hir::Expr::List(items, tails) => hir::Expr::List(
                items
                    .iter()
                    .map(|item| self.lower_expr(hir, item, ty_insts))
                    .collect(),
                tails
                    .iter()
                    .map(|tail| self.lower_expr(hir, tail, ty_insts))
                    .collect(),
            ),
            hir::Expr::Record(fields) => hir::Expr::Record(fields
                .iter()
                .map(|(name, field)| (name.clone(), self.lower_expr(hir, field, ty_insts)))
                .collect()),
            hir::Expr::Access(record, field) => hir::Expr::Access(self.lower_expr(hir, record, ty_insts), field.clone()),
            hir::Expr::Match(hidden_outer, pred, arms) => hir::Expr::Match(
                *hidden_outer,
                self.lower_expr(hir, pred, ty_insts),
                arms
                    .iter()
                    .map(|(binding, arm)| (
                        self.lower_binding(hir, binding, ty_insts),
                        self.lower_expr(hir, arm, ty_insts),
                    ))
                    .collect(),
            ),
            hir::Expr::Func(arg, body) => hir::Expr::Func(
                ConNode::new(**arg, self.lower_ty(hir, arg.meta().1, ty_insts)),
                self.lower_expr(hir, body, ty_insts),
            ),
            hir::Expr::Apply(f, arg) => hir::Expr::Apply(
                self.lower_expr(hir, f, ty_insts),
                self.lower_expr(hir, arg, ty_insts),
            ),
            hir::Expr::Cons(data, variant, inner) => {
                let ty = self.lower_ty(hir, ty_expr.meta().1, ty_insts);
                let ConTy::Data(data) = self.get_ty(ty) else { unreachable!() };
                hir::Expr::Cons(*data, *variant, self.lower_expr(hir, inner, ty_insts))
            },
            hir::Expr::ClassAccess(ty, class, field) => {
                let self_ty = self.lower_ty(hir, ty.1, ty_insts);
                let (class_id, args) = class.as_ref().expect("Uninferred class during concretization");
                let args = args
                    .iter()
                    .map(|arg| self.lower_ty(hir, *arg, ty_insts))
                    .collect::<Vec<_>>();
                let member_id = hir.classes
                    .lookup_member(hir, self, self_ty, (*class_id, args.clone()))
                    .unwrap_or_else(|| panic!(
                        "Could not select member candidate for '{}' as '{}{}'",
                        self.display(hir, self_ty),
                        *hir.classes.get(*class_id).name,
                        args.iter().map(|arg| format!(" {}", self.display(hir, *arg))).collect::<String>(),
                    ));

                let id = Intern::new(ConProc::Field(self_ty, member_id, args, **field));
                self.lower_proc(hir, id);
                hir::Expr::Global(id)
            },
            hir::Expr::Intrinsic(name, args) => hir::Expr::Intrinsic(name.clone(), args
                .into_iter()
                .map(|arg| self.lower_expr(hir, arg, ty_insts))
                .collect()),
            hir::Expr::Update(record, fields) => hir::Expr::Update(self.lower_expr(hir, record, ty_insts), fields
                .iter()
                .map(|(name, field)| (name.clone(), self.lower_expr(hir, field, ty_insts)))
                .collect()),
            hir::Expr::Basin(eff, inner) => hir::Expr::Basin(
                self.lower_effect(hir, *eff, ty_insts),
                self.lower_expr(hir, inner, ty_insts),
            ),
            hir::Expr::Suspend(eff, inner) => hir::Expr::Suspend(
                self.lower_effect(hir, *eff, ty_insts),
                self.lower_expr(hir, inner, ty_insts),
            ),
            hir::Expr::Handle { expr, eff, send, recv } => hir::Expr::Handle {
                expr: self.lower_expr(hir, expr, ty_insts),
                eff: self.lower_effect(hir, *eff, ty_insts),
                send: ConNode::new(**send, self.lower_ty(hir, send.meta().1, ty_insts)),
                recv: self.lower_expr(hir, recv, ty_insts),
            },
        };

        ConNode::new(expr, self.lower_ty(hir, ty_expr.meta().1, ty_insts))
    }

    pub fn lower_effect(&mut self, hir: &Context, eff: EffectId, ty_insts: &TyInsts) -> ConEffectId {
        let (decl, args) = match hir.tys.get_effect(eff) {
            Effect::Error => panic!("Error effect should not exist during concretization"),
            Effect::Known(decl, args) => (decl, args
                .into_iter()
                .map(|arg| self.lower_ty(hir, arg, ty_insts))
                .collect::<Vec<_>>()),
        };
        let id = Intern::new((decl, args.clone()));
        if !self.effects.contains_key(&id) {
            let decl = hir.effects.get_decl(decl);
            let ty_insts = TyInsts { self_ty: None, gen: &args };
            let eff = ConEffect {
                send: self.lower_ty(hir, decl.send.unwrap(), &ty_insts),
                recv: self.lower_ty(hir, decl.recv.unwrap(), &ty_insts),
            };
            self.effects.insert(id, eff);
        }
        id
    }

    pub fn display<'a>(&'a self, hir: &'a Context, ty: ConTyId) -> ConTyDisplay<'a> {
        ConTyDisplay {
            con_ctx: self,
            datas: &hir.datas,
            effects: &hir.effects,
            ty,
            lhs_exposed: false,
        }
    }
}

#[derive(Clone)]
pub struct ConTyDisplay<'a> {
    con_ctx: &'a ConContext,
    datas: &'a Datas,
    effects: &'a Effects,
    ty: ConTyId,
    lhs_exposed: bool,
}

impl<'a> ConTyDisplay<'a> {
    fn with_ty(&self, ty: ConTyId, lhs_exposed: bool) -> Self {
        Self { ty, lhs_exposed, ..self.clone() }
    }
}

impl<'a> fmt::Display for ConTyDisplay<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.con_ctx.get_ty(self.ty).clone() {
            ConTy::Prim(prim) => write!(f, "{}", prim),
            ConTy::List(item) => write!(f, "[{}]", self.with_ty(item, false)),
            ConTy::Record(fields) => write!(f, "{{ {} }}", fields
                .into_iter()
                .map(|(name, field)| format!("{}: {}", name, self.with_ty(field, false)))
                .collect::<Vec<_>>()
                .join(", ")),
            ConTy::Func(i, o) if self.lhs_exposed => write!(f, "({} -> {})", self.with_ty(i, true), self.with_ty(o, self.lhs_exposed)),
            ConTy::Func(i, o) => write!(f, "{} -> {}", self.with_ty(i, true), self.with_ty(o, self.lhs_exposed)),
            ConTy::Data(data_id) if self.lhs_exposed && data_id.1.len() > 0 => write!(f, "({}{})", *self.datas.get_data(data_id.0).name, data_id.1
                .iter()
                .map(|param| format!(" {}", self.with_ty(*param, true)))
                .collect::<String>()),
            ConTy::Data(data_id) => write!(f, "{}{}", *self.datas.get_data(data_id.0).name, data_id.1
                .iter()
                .map(|param| format!(" {}", self.with_ty(*param, true)))
                .collect::<String>()),
            ConTy::Effect(eff_id, out) => write!(f, "{}{} ~ {}", *self.effects.get_decl(eff_id.0).name, eff_id.1
                .iter()
                .map(|param| format!(" {}", self.with_ty(*param, true)))
                .collect::<String>(), self.with_ty(out, true)),
        }
    }
}
