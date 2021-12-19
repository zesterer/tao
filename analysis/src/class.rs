use super::*;

pub enum ClassItem {
    Value {
        name: SrcNode<Ident>,
        ty: SrcNode<TyId>,
    },
}

pub struct Class {
    pub name: SrcNode<Ident>,
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

#[derive(Default)]
pub struct Classes {
    lut: HashMap<Ident, (Span, ClassId, GenScopeId)>,
    classes: Vec<Option<Class>>,
    members: HashMap<ClassId, Vec<Member>>,
}

impl Classes {
    pub fn name_gen_scope(&self, name: Ident) -> GenScopeId {
        self.lut[&name].2
    }

    pub fn get(&self, class: ClassId) -> Option<&Class> {
        self.classes[class.0].as_ref()
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

    pub fn declare_member(&mut self, class: ClassId, member: Member) {
        self.members.entry(class).or_default().push(member);
    }

    pub fn lookup_member(&self, ty: TyId, class: ClassId) -> Option<&Member> {
        self.members
            .get(&class)
            .and_then(|xs| {
                assert!(xs.len() <= 1, "Multiple implementors are not yet supported");
                xs.first()
            })
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
