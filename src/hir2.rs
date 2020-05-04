use std::collections::HashMap;
use internment::LocalIntern;
use crate::{
    ast,
    mir,
    error::Error,
    src::SrcRegion,
    ty::{TypeEngine, DataId, TypeId, TypeInfo, Primitive, Type, TraitId, InnerTrait, InferCtx, Core},
    node2::{Node, SrcNode, TypeNode},
};

type Ident = LocalIntern<String>;

pub type InferNode<T> = Node<T, (SrcRegion, TypeId)>;

impl<T> Node<T, (SrcRegion, TypeId)> {
    pub fn region(&self) -> SrcRegion {
        self.attr().0
    }

    pub fn type_id(&self) -> TypeId {
        self.attr().1
    }
}

pub enum Data {}
pub enum Trait {}

#[derive(Clone)]
pub enum Value {
    Number(f64),
    String(LocalIntern<String>),
    Boolean(bool),
}

impl Value {
    pub fn get_type_info(&self, infer: &mut InferCtx) -> TypeInfo {
        match self {
            Value::Boolean(_) => TypeInfo::Primitive(Primitive::Boolean),
            Value::Number(_) => TypeInfo::Primitive(Primitive::Number),
            Value::String(_) => TypeInfo::Primitive(Primitive::String),
        }
    }
}

#[derive(PartialEq, Eq, Hash)]
pub struct Path(Vec<Ident>);

impl Path {
    pub fn intern<'a, T: ToString, I: IntoIterator<Item=T>>(parts: I) -> LocalIntern<Self> {
        LocalIntern::new(Path(parts.into_iter().map(|part| LocalIntern::new(part.to_string())).collect()))
    }

    pub fn from_path(path: &ast::Path) -> LocalIntern<Self> {
        Self::intern(path.parts().iter().map(|part| &**part))
    }
}

pub enum Pat { // TODO: <M>
    Value(Value),
    Ident(Ident),
}

impl InferNode<Pat> {
    fn ident_types(&self) -> HashMap<Ident, TypeId> {
        match &**self {
            Pat::Value(_) => HashMap::default(),
            Pat::Ident(ident) => std::iter::once((*ident, self.type_id())).collect(),
        }
    }
}

pub enum Expr<M> {
    Value(Value),
    Local(Ident),
    Global(Ident),
    Unary(SrcNode<ast::UnaryOp>, Node<Self, M>),
    Binary(SrcNode<ast::BinaryOp>, Node<Self, M>, Node<Self, M>),
    List(Vec<Node<Self, M>>),
    Tuple(Vec<Node<Self, M>>),
    Func(Node<Pat, M>, Node<Self, M>),
    Apply(Node<Self, M>, Node<Self, M>), // TODO: Should application be a binary operator?
    Match(Node<Self, M>, Vec<(Node<Pat, M>, Node<Self, M>)>),
}

type InferExpr = InferNode<Expr<(SrcRegion, TypeId)>>;
type InferPat = InferNode<Pat>;
pub type TypeExpr = TypeNode<Expr<SrcNode<Type>>>;
pub type TypedPat = TypeNode<Pat>;

#[derive(Clone)]
pub enum Scope<'a> {
    //Module(&'a Module),
    Root,
    Def(SrcNode<Ident>, Vec<SrcNode<Ident>>, Option<SrcNode<Type>>, &'a Self),
    Local(Ident, TypeId, &'a Self),
    Many(HashMap<Ident, TypeId>, &'a Self),
}

impl<'a> Scope<'a> {
    fn with_def<'b>(&'b self, ident: SrcNode<Ident>, generics: Vec<SrcNode<Ident>>, ty: Option<SrcNode<Type>>) -> Scope<'b> where 'a: 'b {
        Scope::Def(ident, generics, ty, self)
    }

    fn with<'b>(&'b self, ident: Ident, ty: TypeId) -> Scope<'b> where 'a: 'b {
        Scope::Local(ident, ty, self)
    }

    fn with_many<'b>(&'b self, locals: HashMap<Ident, TypeId>) -> Scope<'b> where 'a: 'b {
        if locals.len() == 1 {
            let local = locals.into_iter().next().unwrap();
            self.with(local.0, local.1)
        } else {
            Scope::Many(locals, self)
        }
    }

    fn get_local(&self, ident: Ident, ctx: &mut InferCtx, region: SrcRegion) -> Option<TypeId> {
        match self {
            Scope::Local(local, ty, parent) => Some(*ty)
                .filter(|_| local == &ident)
                .or_else(|| parent.get_local(ident, ctx, region)),
            Scope::Many(locals, parent) => locals
                .get(&ident)
                .copied()
                .or_else(|| parent.get_local(ident, ctx, region)),
            Scope::Def(def, generics, ty, parent) => if **def == ident {
                if let Some(ty) = ty {
                    Some(ctx.insert_ty(generics, ty))
                } else {
                    // Err(Error::custom(format!("Definition '{}' was found but requires an explicit type to be used recursively", **def))
                    //     .with_region(def.region())
                    //     .with_region(region)
                    //     .with_hint(format!("Consider adding a type annotation to '{}' that fully describes it", **def)))

                    // TODO: Is this correct? Can this lead to incorrect typing?
                    Some(ctx.insert(TypeInfo::Unknown, def.region()))
                }
            } else {
                parent.get_local(ident, ctx, region)
            },
            Scope::Root => None,
            //Scope::Module(module) => module.get_def_type(ident, ctx, region).map(Ok),
        }
    }
}

#[derive(Clone)]
pub struct DataCtx<'a> {
    module: &'a Module,
    core: &'a Core,
    generics: HashMap<Ident, TypeId>,
}

impl<'a> DataCtx<'a> {
    fn get_data_type(&self, name: &SrcNode<Ident>, params: &[TypeId], infer: &mut InferCtx, region: SrcRegion) -> Result<TypeId, Error> {
        if let Some(ty_id) = self
            .generics
            .get(&**name)
        {
            Ok(*ty_id)
        } else if let Some(ty_info) = match name.as_str() {
            "Num" => Some(TypeInfo::Primitive(Primitive::Number)),
            "Bool" => Some(TypeInfo::Primitive(Primitive::Boolean)),
            "Str" => Some(TypeInfo::Primitive(Primitive::String)),
            _ => None,
        } {
            if params.len() == 0 {
                Ok(infer.insert(ty_info, region))
            } else {
                Err(Error::custom(format!("Primitive type '{}' cannot be parameterised", **name))
                    .with_region(name.region())
                    .with_region(infer.region(params[0]))
                    .with_hint(format!("Remove all type parameters from '{}'", **name)))
            }
        } else {
            if let Some(res) = self.module
                .get_data_type(**name)
                .map(|(ty, generics)| if params.len() != generics.len() {
                    Err(Error::custom(format!("Type '{}' expected {} parameters, found {}", **name, generics.len(), params.len()))
                        .with_region(ty.region())
                        .with_region(region))
                } else {
                    Ok(infer.insert_ty_inner(&|ident| {
                        generics
                            .iter()
                            .enumerate()
                            .find(|(_, g)| ***g == ident)
                            .map(|(i, _)| TypeInfo::Ref(params[i]))
                    }, ty))
                })
            {
                res
            } else {
                Err(Error::custom(format!("No such data type '{}'", **name))
                    .with_region(name.region()))
            }
        }
    }

    fn get_def_type(&self, ident: Ident, infer: &mut InferCtx, region: SrcRegion) -> Option<TypeId> {
        self.module.get_def_type(ident, infer, region)
    }
}

struct PhoneyData {
    name: SrcNode<Ident>,
    generics: Vec<SrcNode<Ident>>,
}

fn op_to_unary_trait(core: &Core, op: ast::UnaryOp) -> TraitId {
    match op {
        ast::UnaryOp::Neg => core.traits.neg,
        _ => todo!(),
    }
}

fn op_to_binary_trait(core: &Core, op: ast::BinaryOp) -> TraitId {
    match op {
        ast::BinaryOp::Add => core.traits.add,
        _ => todo!(),
    }
}

pub struct Def {
    pub generics: Vec<SrcNode<Ident>>,
    pub name: SrcNode<Ident>,
    pub body: TypeExpr,
}

pub struct TypeAlias {
    pub generics: Vec<SrcNode<Ident>>,
    pub name: SrcNode<Ident>,
    pub ty: SrcNode<Type>,
}

pub struct Program {
    type_engine: TypeEngine,
    core: Core,
    pub root: Module,
}

impl Program {
    pub fn new() -> Self {
        let (type_engine, core) = TypeEngine::default().with_core();
        Self {
            type_engine,
            core,
            root: Module::default(),
        }
    }

    pub fn new_root(module: &SrcNode<ast::Module>) -> Result<Self, Error> {
        let mut this = Self::new();
        for decl in module.decls.iter() {
            match &**decl {
                ast::Decl::Def(def) => this.insert_def(def)?,
                ast::Decl::TypeAlias(type_alias) => this.insert_type_alias(type_alias)?,
                ast::Decl::Data(_) => todo!("Data types"),
            }
        }
        Ok(this)
    }

    pub fn root(&self) -> &Module {
        &self.root
    }

    fn create_scope(&self) -> Scope {
        Scope::Root
    }


    pub fn insert_def(&mut self, ast_def: &ast::Def) -> Result<(), Error> {
        // Check for double declaration
        if let Some(existing_def) = self.root.defs.get(&**ast_def.name) {
            return Err(Error::custom(format!("Definition with name '{}' already exists", **ast_def.name))
                .with_region(existing_def.name.region())
                .with_region(ast_def.name.region()));
        }

        let mut infer = InferCtx::default();
        let data = DataCtx {
            module: &self.root,
            core: &self.core,
            generics: ast_def
                .generics
                .iter()
                .map(|ident| (**ident, infer.insert(TypeInfo::GenParam(**ident), ident.region())))
                .collect(),
        };
        let scope = self.create_scope();

        // Attempt to fully infer just from the type signature - needed for recursive definitions!
        let scope = scope.with_def(ast_def.name.clone(), ast_def.generics.clone(), {
            let ty_id = ast_def.ty.to_type_id(&data, &mut infer)?;
            infer.reconstruct(ty_id).ok()
        });

        let body = {
            let mut body = ast_def.body.to_hir(&data, &mut infer, &scope)?;
            body.infer(&data, &mut infer, &scope)?;

            // Unify with optional type annotation
            let ty_id = ast_def.ty.to_type_id(&data, &mut infer)?;
            infer.unify(body.type_id(), ty_id)?;

            body.into_checked(&mut infer)?
        };

        let generics = ast_def.generics
            .iter()
            .map(|g| SrcNode::new(**g, g.region()))
            .collect::<Vec<_>>();

        // Ensure that all generic parameters are in use
        for gen in generics.iter() {
            let mut uses_gen = false;
            body.ty().visit(&mut |ty| {
                if **ty == Type::GenParam(**gen) {
                    uses_gen = true;
                }
            });
            if !uses_gen {
                return Err(Error::custom(format!("Type parameter '{}' must be mentioned by type {}", **gen, **body.ty()))
                    .with_region(gen.region())
                    .with_region(body.ty().region())
                    .with_hint(format!("Consider removing '{}' from the list of generics", **gen)));
            }
        }

        self.root.defs.insert(*ast_def.name, Def {
            generics,
            name: ast_def.name.clone(),
            body,
        });
        Ok(())
    }

    pub fn insert_type_alias(&mut self, ast_type_alias: &ast::TypeAlias) -> Result<(), Error> {
        let mut infer = InferCtx::default();
        let data = DataCtx {
            module: &self.root,
            core: &self.core,
            generics: ast_type_alias
                .generics
                .iter()
                .map(|ident| (**ident, infer.insert(TypeInfo::GenParam(**ident), ident.region())))
                .collect(),
        };

        let ty_info = TypeInfo::Ref(ast_type_alias.ty.to_type_id(&data, &mut infer)?);
        let region = ast_type_alias.ty.region();
        let generics = ast_type_alias.generics
            .iter()
            .map(|g| SrcNode::new(**g, g.region()))
            .collect::<Vec<_>>();

        let ty_id = infer.insert(ty_info, region);
        let ty = infer.reconstruct(ty_id)?;

        // Ensure that all generic parameters are in use
        for gen in generics.iter() {
            let mut uses_gen = false;
            ty.visit(&mut |ty| {
                if **ty == Type::GenParam(**gen) {
                    uses_gen = true;
                }
            });
            if !uses_gen {
                return Err(Error::custom(format!("Type parameter '{}' must be mentioned by type {}", **gen, *ty))
                    .with_region(gen.region())
                    .with_region(ty.region())
                    .with_hint(format!("Consider removing '{}' from the list of generics", **gen)));
            }
        }

        self.root.type_aliases.insert(*ast_type_alias.name, TypeAlias {
            generics,
            name: ast_type_alias.name.clone(),
            ty,
        });
        Ok(())
    }
}

#[derive(Default)]
pub struct Module {
    modules: HashMap<Ident, Module>,
    traits: HashMap<Ident, Trait>,
    datatypes: HashMap<Ident, Data>,
    type_aliases: HashMap<Ident, TypeAlias>,
    defs: HashMap<Ident, Def>,
}

impl Module {
    pub fn def(&self, name: Ident) -> Option<&Def> {
        self.defs.get(&name)
    }

    fn get_def_type(&self, ident: Ident, infer: &mut InferCtx, region: SrcRegion) -> Option<TypeId> {
        self.defs
            .get(&ident)
            .map(|def| infer.insert_ty(&def.generics, def.body.ty()))
    }

    fn get_data_type(&self, ident: Ident) -> Option<(&SrcNode<Type>, &[SrcNode<Ident>])> {
        self
            .type_aliases
            .get(&ident)
            .map(|type_alias| (&type_alias.ty, type_alias.generics.as_slice()))
    }
}

// AST to HIR conversions

impl ast::Pat {
    fn to_hir(self: &SrcNode<Self>, ctx: &mut InferCtx) -> Result<InferNode<Pat>, Error> {
        let hir_pat = match &**self {
            ast::Pat::Ident(ident) => Pat::Ident(*ident),
            ast_pat => todo!("HIR parsing of {:?}", ast_pat),
        };

        Ok(InferNode::new(hir_pat, (self.region(), ctx.insert(TypeInfo::Unknown, self.region()))))
    }
}

impl ast::Literal {
    fn to_hir(&self) -> Result<Value, Error> {
        Ok(match self {
            ast::Literal::Number(x) => Value::Number(*x),
            ast::Literal::String(x) => Value::String(*x),
            ast::Literal::Boolean(x) => Value::Boolean(*x),
        })
    }
}

impl ast::Type {
    fn to_type_id(self: &SrcNode<Self>, data: &DataCtx, infer: &mut InferCtx) -> Result<TypeId, Error> {
        let info = match &**self {
            ast::Type::Unknown => TypeInfo::Unknown,
            ast::Type::Data(name, params) => {
                let params = params
                    .iter()
                    .map(|param| param.to_type_id(data, infer))
                    .collect::<Result<Vec<_>, _>>()?;
                TypeInfo::Ref(data.get_data_type(name, &params, infer, self.region())?)
            },
            ast::Type::List(ty) => TypeInfo::List(ty.to_type_id(data, infer)?),
            ast::Type::Tuple(items) => TypeInfo::Tuple(items
                .iter()
                .map(|item| item.to_type_id(data, infer))
                .collect::<Result<_, _>>()?),
            ast::Type::Func(i, o) => TypeInfo::Func(
                i.to_type_id(data, infer)?,
                o.to_type_id(data, infer)?,
            ),
        };

        Ok(infer.insert(info, self.region()))
    }
}

impl ast::Expr {
    fn to_hir(self: &SrcNode<Self>, data: &DataCtx, infer: &mut InferCtx, scope: &Scope) -> Result<InferExpr, Error> {
        let hir_expr = match &**self {
            ast::Expr::Literal(litr) => Expr::Value(litr.to_hir()?),
            ast::Expr::Path(path) => if path.len() == 1 {
                if scope
                    .get_local(path.base(), infer, self.region())
                    .is_some()
                {
                    Expr::Local(path.base())
                } else if data.get_def_type(path.base(), infer, self.region()).is_some() {
                    Expr::Global(path.base())
                } else {
                    return Err(Error::no_such_binding(path.base().to_string(), self.region()));
                }
            } else {
                todo!("Complex paths")
            },

            ast::Expr::Unary(op, a) => Expr::Unary(op.clone(), a.to_hir(data, infer, scope)?),
            ast::Expr::Binary(op, a, b) => Expr::Binary(op.clone(), a.to_hir(data, infer, scope)?, b.to_hir(data, infer, scope)?),
            ast::Expr::List(items) => Expr::List(items.iter().map(|item| item.to_hir(data, infer, scope)).collect::<Result<_, _>>()?),
            ast::Expr::Tuple(items) => Expr::Tuple(items.iter().map(|item| item.to_hir(data, infer, scope)).collect::<Result<_, _>>()?),
            ast::Expr::Func(param, param_ty, body) => {
                let param = param.to_hir(infer)?;

                // Unify param_ty with type hint
                if let Some(param_ty) = param_ty {
                    let param_ty_id = param_ty.to_type_id(data, infer)?;
                    infer.unify(param.type_id(), param_ty_id)?;
                }

                let body_scope = scope.with_many(param.ident_types());

                Expr::Func(param, body.to_hir(data, infer, &body_scope)?)
            },
            ast::Expr::Apply(f, arg) => Expr::Apply(f.to_hir(data, infer, scope)?, arg.to_hir(data, infer, scope)?),
            ast::Expr::Let(pat, pat_ty, val, then) => {
                let pat = pat.to_hir(infer)?;

                // Unify pattern with type hint
                if let Some(pat_ty) = pat_ty {
                    let pat_ty_id = pat_ty.to_type_id(data, infer)?;
                    infer.unify(pat.type_id(), pat_ty_id)?;
                }

                // `let a = b in c` desugars to `b:(a -> c)`
                let then = InferNode::new(Expr::Func(pat, then.to_hir(data, infer, scope)?), (then.region(), infer.insert(TypeInfo::Unknown, then.region())));
                Expr::Apply(then, val.to_hir(data, infer, scope)?)
            },
            ast::Expr::If(pred, a, b) => {
                Expr::Match(pred.to_hir(data, infer, scope)?, vec![
                    (InferNode::new(Pat::Value(Value::Boolean(true)), (a.region(), infer.insert(TypeInfo::Unknown, a.region()))), a.to_hir(data, infer, scope)?),
                    (InferNode::new(Pat::Value(Value::Boolean(false)), (b.region(), infer.insert(TypeInfo::Unknown, b.region()))), b.to_hir(data, infer, scope)?),
                ])
            },
            ast_expr => todo!("HIR parsing of {:?}", ast_expr),
        };

        Ok(InferNode::new(hir_expr, (self.region(), infer.insert(TypeInfo::Unknown, self.region()))))
    }
}

impl Pat {
    fn infer(self: &mut InferNode<Self>, data: &DataCtx, infer: &mut InferCtx) -> Result<(), Error> {
        let type_id = match &mut **self {
            Pat::Value(x) => {
                let type_info = x.get_type_info(infer);
                infer.insert(type_info, self.region())
            },
            Pat::Ident(ident) => infer.insert(TypeInfo::Unknown, self.region()),
        };

        infer.unify(self.type_id(), type_id)?;

        Ok(())
    }

    fn into_checked(self: InferNode<Self>, infer: &InferCtx) -> Result<TypedPat, Error> {
        let meta = infer.reconstruct(self.type_id())?;

        Ok(TypeNode::new(match self.into_inner() {
            Pat::Value(val) => Pat::Value(val),
            Pat::Ident(ident) => Pat::Ident(ident),
        }, meta))
    }
}

impl Expr<(SrcRegion, TypeId)> {
    fn infer(self: &mut InferNode<Self>, data: &DataCtx, infer: &mut InferCtx, scope: &Scope) -> Result<(), Error> {
        let region = self.region();
        let type_id = match &mut **self {
            Expr::Value(x) => {
                let type_info = x.get_type_info(infer);
                infer.insert(type_info, region)
            },
            Expr::Local(ident) => scope
                .get_local(*ident, infer, region)
                .ok_or_else(|| Error::no_such_binding(ident.to_string(), region))?,
            Expr::Global(ident) => data.get_def_type(*ident, infer, region).unwrap(),
            Expr::Unary(op, x) => {
                x.infer(data, infer, scope)?;
                infer.insert(TypeInfo::Associated(
                    op_to_unary_trait(&data.core, **op).into(),
                    x.type_id(),
                    LocalIntern::new("Out".to_string()),
                ), region)
            },
            Expr::Binary(op, x, y) => {
                x.infer(data, infer, scope)?;
                y.infer(data, infer, scope)?;
                infer.insert(TypeInfo::Associated(
                    InnerTrait::new(op_to_binary_trait(&data.core, **op), vec![y.type_id().into()]),
                    x.type_id(),
                    LocalIntern::new("Out".to_string()),
                ), region)
            },
            Expr::List(items) => {
                let item_ty = infer.insert(TypeInfo::Unknown, region);
                for item in items.iter_mut() {
                    item.infer(data, infer, scope)?;
                    infer.unify(item_ty, item.type_id())?;
                }
                infer.insert(TypeInfo::List(item_ty), region)
            },
            Expr::Tuple(items) => {
                let items = items
                    .iter_mut()
                    .map(|item| -> Result<_, Error> {
                        item.infer(data, infer, scope)?;
                        Ok(item.type_id())
                    })
                    .collect::<Result<_, _>>()?;
                infer.insert(TypeInfo::Tuple(items), region)
            },
            Expr::Func(param, body) => {
                param.infer(data, infer)?;
                let body_scope = scope.with_many(param.ident_types());
                body.infer(data, infer, &body_scope)?;
                infer.insert(TypeInfo::Func(param.type_id(), body.type_id()), region)
            },
            Expr::Apply(f, arg) => {
                f.infer(data, infer, scope)?;
                arg.infer(data, infer, scope)?;
                let out_ty = infer.insert(TypeInfo::Unknown, region);
                let f_ty = infer.insert(TypeInfo::Func(
                    arg.type_id(),
                    out_ty,
                ), f.region());
                infer.unify(f.type_id(), f_ty)?;
                out_ty
            },
            Expr::Match(pred, arms) => {
                pred.infer(data, infer, scope)?;
                let out_ty = infer.insert(TypeInfo::Unknown, region);
                for (pat, body) in arms.iter_mut() {
                    pat.infer(data, infer)?;
                    infer.unify(pat.type_id(), pred.type_id())?;
                    let body_scope = scope.with_many(pat.ident_types());
                    body.infer(data, infer, &body_scope)?;
                    infer.unify(body.type_id(), out_ty)?;
                }
                out_ty
            },
            _ => todo!(),
        };

        infer.unify(self.type_id(), type_id)?;

        Ok(())
    }

    fn into_checked(self: InferNode<Self>, infer: &InferCtx) -> Result<TypeExpr, Error> {
        let meta = infer.reconstruct(self.type_id())?;

        Ok(TypeNode::new(match self.into_inner() {
            Expr::Value(val) => Expr::Value(val),
            Expr::Local(ident) => Expr::Local(ident),
            Expr::Global(ident) => Expr::Global(ident),
            Expr::Unary(op, x) => Expr::Unary(
                op,
                x.into_checked(infer)?,
            ),
            Expr::Binary(op, x, y) => Expr::Binary(
                op,
                x.into_checked(infer)?,
                y.into_checked(infer)?,
            ),
            Expr::List(items) => Expr::List(
                items.into_iter().map(|item| item.into_checked(infer)).collect::<Result<_, _>>()?,
            ),
            Expr::Tuple(items) => Expr::Tuple(
                items.into_iter().map(|item| item.into_checked(infer)).collect::<Result<_, _>>()?,
            ),
            Expr::Func(param, body) => Expr::Func(
                param.into_checked(infer)?,
                body.into_checked(infer)?,
            ),
            Expr::Apply(f, arg) => Expr::Apply(
                f.into_checked(infer)?,
                arg.into_checked(infer)?,
            ),
            Expr::Match(pred, arms) => Expr::Match(
                pred.into_checked(infer)?,
                arms
                    .into_iter()
                    .map(|(pat, body)| -> Result<_, Error> {
                        Ok((
                            pat.into_checked(infer)?,
                            body.into_checked(infer)?,
                        ))
                    })
                    .collect::<Result<_, _>>()?,
            ),
            _ => todo!(),
        }, meta))
    }
}

