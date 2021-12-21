use super::*;

pub enum ClassItem {
    Value {
        name: SrcNode<Ident>,
        ty: SrcNode<TyId>,
    },
}

pub struct Class {
    pub name: SrcNode<Ident>,
    pub obligations: Vec<SrcNode<Obligation>>,
    pub attr: ast::Attr,
    pub gen_scope: GenScopeId,
    pub items: Vec<ClassItem>,
}

impl Class {
    pub fn field(&self, field: Ident) -> Option<&SrcNode<TyId>> {
        self.items
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
    lut: HashMap<Ident, (Span, ClassId, GenScopeId)>,
    classes: Vec<Option<Class>>,
    members: Vec<Option<Member>>,
    member_lut: HashMap<ClassId, Vec<MemberId>>,
}

impl Classes {
    pub fn name_gen_scope(&self, name: Ident) -> GenScopeId {
        self.lut[&name].2
    }

    pub fn get(&self, class: ClassId) -> Option<&Class> {
        self.classes[class.0].as_ref()
    }

    pub fn iter(&self) -> impl Iterator<Item = (ClassId, &Class)> {
        self.classes.iter().enumerate().map(|(i, class)| (ClassId(i), class.as_ref().unwrap()))
    }

    pub fn lookup(&self, name: Ident) -> Option<ClassId> {
        self.lut.get(&name).map(|(_, id, _)| *id)
    }

    pub fn declare(&mut self, name: SrcNode<Ident>, gen_scope: GenScopeId) -> Result<ClassId, Error> {
        let id = ClassId(self.classes.len());
        let span = name.span();
        self.classes.push(None);
        if let Err(old) = self.lut.try_insert(*name, (span, id, gen_scope)) {
            Err(Error::DuplicateClassName(*name, old.entry.get().0, span))
        } else {
            Ok(id)
        }
    }

    pub fn define(&mut self, id: ClassId, class: Class) {
        self.classes[id.0] = Some(class);
    }

    pub fn get_member(&self, id: MemberId) -> Option<&Member> {
        self.members[id.0].as_ref()
    }

    pub fn declare_member(&mut self, gen_scope: GenScopeId) -> MemberId {
        let id = MemberId(self.members.len());
        self.members.push(None);
        id
    }

    pub fn define_member(&mut self, id: MemberId, class: ClassId, member: Member) {
        self.members[id.0] = Some(member);
        self.member_lut.entry(class).or_default().push(id);
    }

    // TODO: Is this needed?
    pub fn lookup_member_concrete(&self, hir: &Context, ctx: &ConContext, ty: ConTyId, class: ClassId) -> Option<&Member> {
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
                (Ty::Data(x, xs), ConTy::Data(y, ys)) if x == *y && xs.len() == ys.len() => xs
                    .into_iter()
                    .zip(ys.into_iter())
                    .all(|(x, y)| covers(hir, ctx, x, *y)),
                _ => false,
            }
        }

        self.member_lut
            .get(&class)
            .and_then(|xs| {
                let candidates = xs
                    .iter()
                    .map(|m| self.get_member(*m).expect("Member must be defined before lookup"))
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
            .map(|m| self.get_member(*m).expect("Member must be defined before lookup"))
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
    pub member: TyId,
    pub items: HashMap<Ident, MemberItem>,
}

impl Member {
    pub fn field(&self, field: Ident) -> Option<&TyExpr> {
        self.items
            .get(&field)
            .and_then(|item| match item {
                MemberItem::Value { name, val } if **name == field => Some(val),
                _ => None,
            })
    }
}
