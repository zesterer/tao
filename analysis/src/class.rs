use super::*;

pub enum ClassItem {
    Value {
        name: SrcNode<Ident>,
        ty: SrcNode<TyId>,
    },
}

pub struct Class {
    pub name: SrcNode<Ident>,
    pub obligations: Option<Vec<SrcNode<Obligation>>>,
    pub attr: Vec<SrcNode<ast::Attr>>,
    pub gen_scope: GenScopeId,
    pub items: Option<Vec<ClassItem>>,
}

impl Class {
    pub fn field(&self, field: Ident) -> Option<&SrcNode<TyId>> {
        self.items
            .as_ref()
            .expect("Class fields must be known here")
            .iter()
            .find_map(|item| match item {
                ClassItem::Value { name, ty } if **name == field => Some(ty),
                _ => None,
            })
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct ClassId(usize);

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct MemberId(usize);

#[derive(Default)]
pub struct Classes {
    lut: HashMap<Ident, (Span, ClassId)>,
    classes: Vec<Class>,
    members: Vec<Member>,
    member_lut: HashMap<ClassId, Vec<MemberId>>,
}

impl Classes {
    pub fn get(&self, class: ClassId) -> &Class {
        &self.classes[class.0]
    }

    pub fn iter(&self) -> impl Iterator<Item = (ClassId, &Class)> {
        self.classes.iter().enumerate().map(|(i, class)| (ClassId(i), class))
    }

    pub fn lookup(&self, name: Ident) -> Option<ClassId> {
        self.lut.get(&name).map(|(_, id)| *id)
    }

    pub fn declare(&mut self, name: SrcNode<Ident>, class: Class) -> Result<ClassId, Error> {
        let id = ClassId(self.classes.len());
        let span = name.span();
        self.classes.push(class);
        if let Err(old) = self.lut.try_insert(*name, (span, id)) {
            Err(Error::DuplicateClassName(*name, old.entry.get().0, span))
        } else {
            Ok(id)
        }
    }

    pub fn define_obligations(&mut self, id: ClassId, obligations: Vec<SrcNode<Obligation>>) {
        self.classes[id.0].obligations = Some(obligations);
    }

    pub fn define_items(&mut self, id: ClassId, items: Vec<ClassItem>) {
        self.classes[id.0].items = Some(items);
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

    pub fn define_member_items(&mut self, id: MemberId, class: ClassId, items: HashMap<Ident, MemberItem>) {
        self.members[id.0].items = Some(items);
    }

    pub fn lookup_member(&self, hir: &Context, ctx: &ConContext, ty: ConTyId, class: ClassId) -> Option<&Member> {
        // Returns true if member covers ty
        fn covers(hir: &Context, ctx: &ConContext, member: TyId, ty: ConTyId) -> bool {
            match (hir.tys.get(member), ctx.get_ty(ty)) {
                (Ty::Gen(_, _), _) => true, // Blanket impls match everything
                (Ty::Prim(a), ConTy::Prim(b)) if a == *b => true,
                (Ty::List(x), ConTy::List(y)) => covers(hir, ctx, x, *y),
                (Ty::Tuple(xs), ConTy::Tuple(ys)) if xs.len() == ys.len() => xs
                    .into_iter()
                    .zip(ys.into_iter())
                    .all(|(x, y)| covers(hir, ctx, x, *y)),
                (Ty::Record(xs), ConTy::Record(ys)) if xs.len() == ys.len() => xs
                    .into_iter()
                    .zip(ys.into_iter())
                    .all(|((_, x), (_, y))| covers(hir, ctx, x, *y)),
                (Ty::Func(x_i, x_o), ConTy::Func(y_i, y_o)) => {
                    covers(hir, ctx, x_i, *y_i) && covers(hir, ctx, x_o, *y_o)
                },
                (Ty::Data(x, xs), ConTy::Data(y)) if x == y.0 && xs.len() == y.1.len() => xs
                    .into_iter()
                    .zip(y.1.iter())
                    .all(|(x, y)| covers(hir, ctx, x, *y)),
                _ => false,
            }
        }

        self.member_lut
            .get(&class)
            .and_then(|xs| {
                let candidates = xs
                    .iter()
                    .map(|m| self.get_member(*m))
                    .filter(|member| covers(hir, ctx, member.member, ty))
                    .collect::<Vec<_>>();

                assert!(candidates.len() <= 1, "Multiple member candidates detected during lowering <{:?} as {:?}>, incoherence has occurred", ctx.get_ty(ty), class);

                candidates.first().copied()
            })
    }

    pub fn members_of(&self, class: ClassId) -> impl Iterator<Item = &Member> {
        self.member_lut
            .get(&class)
            .map(|m| m.as_slice())
            .unwrap_or(&[])
            .iter()
            .map(|m| self.get_member(*m))
    }
}

pub enum MemberItem {
    Value {
        name: SrcNode<Ident>,
        val: TyExpr,
    },
}

pub struct Member {
    pub gen_scope: GenScopeId,
    pub attr: Vec<SrcNode<ast::Attr>>,
    pub member: TyId,
    pub items: Option<HashMap<Ident, MemberItem>>,
}

impl Member {
    pub fn field(&self, field: Ident) -> Option<&TyExpr> {
        self.items
            .as_ref()
            .expect("Member items not initialised")
            .get(&field)
            .and_then(|item| match item {
                MemberItem::Value { name, val } if **name == field => Some(val),
                _ => None,
            })
    }
}
