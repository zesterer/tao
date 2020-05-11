use std::collections::HashMap;
use internment::LocalIntern;
use crate::{
    ast,
    mir,
    error::Error,
    src::SrcRegion,
    ty::{TypeEngine, DataId, TypeId, TypeInfo, Constraint, Primitive, Type, TraitId, InferCtx, Core},
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
    Wildcard,
    Value(Value),
    Ident(Ident),
}

impl InferNode<Pat> {
    fn ident_types(&self) -> HashMap<Ident, TypeId> {
        match &**self {
            Pat::Wildcard | Pat::Value(_) => HashMap::default(),
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
                    Some(ctx.instantiate_ty(generics, ty, region))
                } else {
                    // TODO: Is this correct? Can this lead to incorrect typing?
                    Some(ctx.insert(TypeInfo::Unknown(None), def.region()))
                }
            } else {
                parent.get_local(ident, ctx, region)
            },
            Scope::Root => None,
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
                    Ok(infer.instantiate_ty_inner(&|name| generics
                        .iter()
                        .zip(params.iter())
                        .find(|(gen, _)| ***gen == name)
                        .map(|(_, ty)| *ty), ty, region))
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

            // Unify with optional type annotation
            let ty_id = ast_def.ty.to_type_id(&data, &mut infer)?;

            infer.unify(body.type_id(), ty_id)?;

            infer.solve_all()?;
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

        let region = ast_type_alias.ty.region();
        let generics = ast_type_alias.generics
            .iter()
            .map(|g| SrcNode::new(**g, g.region()))
            .collect::<Vec<_>>();

        let ty_id = ast_type_alias.ty.to_type_id(&data, &mut infer)?;

        infer.solve_all()?;
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
            .map(|def| infer.instantiate_ty(&def.generics, def.body.ty(), region))
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
    fn to_hir(self: &SrcNode<Self>, infer: &mut InferCtx) -> Result<InferNode<Pat>, Error> {
        let (type_id, hir_pat) = match &**self {
            ast::Pat::Ident(ident) => (infer.insert(TypeInfo::Unknown(None), self.region()), Pat::Ident(*ident)),
            ast::Pat::Wildcard => (infer.insert(TypeInfo::Unknown(None), self.region()), Pat::Wildcard),
            // ast::Pat::Value(x) => {
            //     let type_info = x.get_type_info(infer);
            //     (infer.insert(type_info, self.region()), Pat::Value(x))
            // },
            _ => todo!(),
        };

        Ok(InferNode::new(hir_pat, (self.region(), type_id)))
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
            ast::Type::Unknown => TypeInfo::Unknown(None),
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
        let (type_id, hir_expr) = match &**self {
            ast::Expr::Literal(litr) => {
                let val = litr.to_hir()?;
                let ty_info = val.get_type_info(infer);
                (infer.insert(ty_info, self.region()), Expr::Value(val))
            },
            ast::Expr::Path(path) => if path.len() == 1 {
                if let Some(local_id) = scope
                    .get_local(path.base(), infer, self.region())
                {
                    (local_id, Expr::Local(path.base()))
                } else if let Some(global_id) = data.get_def_type(path.base(), infer, self.region()) {
                    (global_id, Expr::Global(path.base()))
                } else {
                    return Err(Error::no_such_binding(path.base().to_string(), self.region()));
                }
            } else {
                todo!("Complex paths")
            },
            ast::Expr::Unary(op, a) => {
                let a = a.to_hir(data, infer, scope)?;
                let type_id = infer.insert(TypeInfo::Unknown(None), self.region());
                infer.add_constraint(Constraint::Unary {
                    out: type_id,
                    op: op.clone(),
                    a: a.type_id(),
                });
                (type_id, Expr::Unary(op.clone(), a))
            },
            ast::Expr::Binary(op, a, b) => {
                let a = a.to_hir(data, infer, scope)?;
                let b = b.to_hir(data, infer, scope)?;
                let type_id = infer.insert(TypeInfo::Unknown(None), self.region());
                infer.add_constraint(Constraint::Binary {
                    out: type_id,
                    op: op.clone(),
                    a: a.type_id(),
                    b: b.type_id(),
                });
                (type_id, Expr::Binary(op.clone(), a, b))
            },
            ast::Expr::List(items) => {
                let items = items.iter().map(|item| item.to_hir(data, infer, scope)).collect::<Result<Vec<_>, _>>()?;
                let item_type_id = infer.insert(TypeInfo::Unknown(None), self.region());
                for item in items.iter() {
                    infer.unify(item_type_id, item.type_id())?;
                }
                let type_id = infer.insert(TypeInfo::List(item_type_id), self.region());
                (type_id, Expr::List(items))
            },
            ast::Expr::Tuple(items) => {
                let items = items.iter().map(|item| item.to_hir(data, infer, scope)).collect::<Result<Vec<_>, _>>()?;
                let type_id = infer.insert(TypeInfo::Tuple(items.iter().map(|item| item.type_id()).collect()), self.region());
                (type_id, Expr::Tuple(items))
            },
            ast::Expr::Func(param, param_ty, body) => {
                let param = param.to_hir(infer)?;

                // Unify param_ty with type hint
                if let Some(param_ty) = param_ty {
                    let param_ty_id = param_ty.to_type_id(data, infer)?;
                    infer.unify(param.type_id(), param_ty_id)?;
                }

                let body_scope = scope.with_many(param.ident_types());
                let body = body.to_hir(data, infer, &body_scope)?;

                let type_id = infer.insert(TypeInfo::Func(param.type_id(), body.type_id()), self.region());
                (type_id, Expr::Func(param, body))
            },
            ast::Expr::Apply(f, arg) => {
                let f = f.to_hir(data, infer, scope)?;
                let arg = arg.to_hir(data, infer, scope)?;
                let type_id = infer.insert(TypeInfo::Unknown(None), self.region());
                let f_type_id = infer.insert(TypeInfo::Func(arg.type_id(), type_id), f.region());
                infer.unify(f_type_id, f.type_id())?;
                (type_id, Expr::Apply(f, arg))
            },
            ast::Expr::Let(pat, pat_ty, val, then) => {
                // `let a = b in c` desugars to `b:(a -> c)`
                let pat = pat.to_hir(infer)?;

                // Unify pattern with type hint
                if let Some(pat_ty) = pat_ty {
                    let pat_ty_id = pat_ty.to_type_id(data, infer)?;
                    infer.unify(pat.type_id(), pat_ty_id)?;
                }

                let val = val.to_hir(data, infer, scope)?;
                infer.unify(pat.type_id(), val.type_id())?;

                let then_scope = scope.with_many(pat.ident_types());
                let then_body = then.to_hir(data, infer, &then_scope)?;
                let then_body_type_id = then_body.type_id();

                let then_func = InferNode::new(Expr::Func(pat, then_body), (then.region(), infer.insert(TypeInfo::Func(val.type_id(), then_body_type_id), then.region())));

                (then_body_type_id, Expr::Apply(then_func, val))
            },
            ast::Expr::If(pred, a, b) => {
                let pred_hir = pred.to_hir(data, infer, scope)?;
                let pred_type_id = infer.insert(TypeInfo::Primitive(Primitive::Boolean), pred.region());
                infer.unify(pred_hir.type_id(), pred_type_id)?;

                let a = a.to_hir(data, infer, scope)?;
                let b = b.to_hir(data, infer, scope)?;
                infer.unify(a.type_id(), b.type_id())?;

                (a.type_id(), Expr::Match(pred_hir, vec![
                    (InferNode::new(Pat::Value(Value::Boolean(true)), (pred.region(), pred_type_id)), a),
                    (InferNode::new(Pat::Value(Value::Boolean(false)), (pred.region(), pred_type_id)), b),
                ]))
            },
            ast_expr => todo!("HIR parsing of {:?}", ast_expr),
        };

        Ok(InferNode::new(hir_expr, (self.region(), type_id)))
    }
}

impl Pat {
    fn into_checked(self: InferNode<Self>, infer: &InferCtx) -> Result<TypedPat, Error> {
        let meta = infer.reconstruct(self.type_id())?;

        Ok(TypeNode::new(match self.into_inner() {
            Pat::Wildcard => Pat::Wildcard,
            Pat::Value(val) => Pat::Value(val),
            Pat::Ident(ident) => Pat::Ident(ident),
        }, meta))
    }
}

impl Expr<(SrcRegion, TypeId)> {
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

