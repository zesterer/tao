use std::collections::HashMap;
use internment::LocalIntern;
use crate::{
    ast,
    mir,
    error::Error,
    src::SrcRegion,
    ty::{TypeEngine, DataId, TypeId, TypeInfo, Type, TraitId, InnerTrait, InferCtx, Core},
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

pub enum Value {
    Number(f64),
    String(LocalIntern<String>),
    Boolean(bool),
}

impl Value {
    pub fn get_data_id(&self, core: &Core) -> DataId {
        match self {
            Value::Number(_) => core.primitives.number,
            Value::String(_) => core.primitives.string,
            Value::Boolean(_) => core.primitives.boolean,
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
    Path(ast::Path),
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
type TypeExpr = TypeNode<Expr<SrcNode<Type>>>;
type TypedPat = TypeNode<Pat>;

#[derive(Clone)]
pub enum Scope<'a> {
    Module(&'a Module),
    Local(Ident, TypeId, &'a Self),
    Many(HashMap<Ident, TypeId>, &'a Self),
}

impl<'a> Scope<'a> {
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

    fn get(&self, ident: Ident, ctx: &mut InferCtx, region: SrcRegion) -> Option<TypeId> {
        match self {
            Scope::Local(local, ty, parent) => Some(*ty)
                .filter(|_| local == &ident)
                .or_else(|| parent.get(ident, ctx, region)),
            Scope::Many(locals, parent) => locals
                .get(&ident)
                .copied()
                .or_else(|| parent.get(ident, ctx, region)),
            Scope::Module(module) => module.get_val_type(ident, ctx, region),
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
    fn get_named_type(&self, name: Ident, params: &[TypeId], infer: &mut InferCtx, region: SrcRegion) -> Result<TypeId, Error> {
        if let Some(ty_id) = self
            .generics
            .get(&name)
        {
            Ok(*ty_id)
        } else if let Some(ty_info) = match name.as_str() {
            "Num" => Some(TypeInfo::Data(self.core.primitives.number)),
            "Bool" => Some(TypeInfo::Data(self.core.primitives.boolean)),
            "Str" => Some(TypeInfo::Data(self.core.primitives.string)),
            _ => None,
        } {
            if params.len() == 0 {
                Ok(infer.insert(ty_info, region))
            } else {
                Err(Error::custom(format!("Primitive type '{}' cannot be parameterised", name))
                    .with_region(infer.region(params[0])))
            }
        } else {
            if let Some(res) = self.module.get_named_type(name, params, infer, region) {
                res
            } else {
                Err(Error::custom(format!("No such data type '{}'", name))
                    .with_region(region))
            }
        }
    }
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
    pub body: TypeExpr,
}

pub struct TypeAlias {
    pub generics: Vec<SrcNode<Ident>>,
    pub ty: SrcNode<Type>,
}

pub struct Program {
    type_engine: TypeEngine,
    core: Core,
    root: Module,
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
            }
        }
        Ok(this)
    }

    pub fn root(&self) -> &Module {
        &self.root
    }

    fn create_scope(&self) -> Scope {
        Scope::Module(&self.root)
    }

    pub fn insert_def(&mut self, ast_def: &ast::Def) -> Result<(), Error> {
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

        let body = {
            let mut body = ast_def.body.to_hir(&data, &mut infer)?;
            body.infer(&data, &mut infer, &scope)?;

            // Unify with optional type annotation
            if let Some(ty) = &ast_def.ty {
                let ty_id = ty.to_type_id(&data, &mut infer)?;
                infer.unify(body.type_id(), ty_id)?;
            }

            body.into_checked(&mut infer)?
        };

        let generics = ast_def.generics
            .iter()
            .map(|g| SrcNode::new(**g, g.region()))
            .collect();

        self.root.defs.insert(*ast_def.name, Def { generics, body });
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
            .collect();
        let ty_id = infer.insert(ty_info, region);
        let ty = infer.reconstruct(ty_id)?;

        self.root.type_aliases.insert(*ast_type_alias.name, TypeAlias { generics, ty });
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

    fn get_val_type(&self, ident: Ident, infer: &mut InferCtx, region: SrcRegion) -> Option<TypeId> {
        self.defs
            .get(&ident)
            .map(|def| infer.insert_ty(&def.generics, def.body.ty()))
    }

    fn get_named_type(&self, ident: Ident, params: &[TypeId], infer: &mut InferCtx, region: SrcRegion) -> Option<Result<TypeId, Error>> {
        self
            .type_aliases
            .get(&ident)
            .map(|type_alias| if params.len() != type_alias.generics.len() {
                Err(Error::custom(format!("Type '{}' expected {} parameters, found {}", ident, type_alias.generics.len(), params.len())))
            } else {
                Ok(infer.insert_ty_inner(&|ident| {
                    type_alias.generics
                        .iter()
                        .enumerate()
                        .find(|(_, g)| ***g == ident)
                        .map(|(i, _)| TypeInfo::Ref(params[i]))
                }, &type_alias.ty))
            })
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
                TypeInfo::Ref(data.get_named_type(*name, &params, infer, self.region())?)
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
    fn to_hir(self: &SrcNode<Self>, data: &DataCtx, infer: &mut InferCtx) -> Result<InferExpr, Error> {
        let hir_expr = match &**self {
            ast::Expr::Literal(litr) => Expr::Value(litr.to_hir()?),
            ast::Expr::Path(path) => Expr::Path(path.clone()),
            ast::Expr::Unary(op, a) => Expr::Unary(op.clone(), a.to_hir(data, infer)?),
            ast::Expr::Binary(op, a, b) => Expr::Binary(op.clone(), a.to_hir(data, infer)?, b.to_hir(data, infer)?),
            ast::Expr::List(items) => Expr::List(items.iter().map(|item| item.to_hir(data, infer)).collect::<Result<_, _>>()?),
            ast::Expr::Tuple(items) => Expr::Tuple(items.iter().map(|item| item.to_hir(data, infer)).collect::<Result<_, _>>()?),
            ast::Expr::Func(param, param_ty, body) => {
                let param = param.to_hir(infer)?;

                // Unify param_ty with type hint
                if let Some(param_ty) = param_ty {
                    let param_ty_id = param_ty.to_type_id(data, infer)?;
                    infer.unify(param.type_id(), param_ty_id)?;
                }

                Expr::Func(param, body.to_hir(data, infer)?)
            },
            ast::Expr::Apply(f, arg) => Expr::Apply(f.to_hir(data, infer)?, arg.to_hir(data, infer)?),
            ast::Expr::Let(pat, pat_ty, val, then) => {
                let pat = pat.to_hir(infer)?;

                // Unify pattern with type hint
                if let Some(pat_ty) = pat_ty {
                    let pat_ty_id = pat_ty.to_type_id(data, infer)?;
                    infer.unify(pat.type_id(), pat_ty_id)?;
                }

                // `let a = b in c` desugars to `b:(a -> c)`
                let then = InferNode::new(Expr::Func(pat, then.to_hir(data, infer)?), (then.region(), infer.insert(TypeInfo::Unknown, then.region())));
                Expr::Apply(then, val.to_hir(data, infer)?)
            },
            ast::Expr::If(pred, a, b) => {
                Expr::Match(pred.to_hir(data, infer)?, vec![
                    (InferNode::new(Pat::Value(Value::Boolean(true)), (a.region(), infer.insert(TypeInfo::Unknown, a.region()))), a.to_hir(data, infer)?),
                    (InferNode::new(Pat::Value(Value::Boolean(false)), (b.region(), infer.insert(TypeInfo::Unknown, b.region()))), b.to_hir(data, infer)?),
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
            Pat::Value(x) => infer.insert(TypeInfo::from(x.get_data_id(&data.core)), self.region()),
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
            Expr::Value(x) => infer.insert(TypeInfo::from(x.get_data_id(&data.core)), region),
            Expr::Path(path) => if path.len() == 1 {
                scope
                    .get(path.base(), infer, region)
                    .ok_or_else(|| Error::no_such_binding(path.base().to_string(), region))?
            } else {
                todo!("Complex paths")
            },
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
            Expr::Path(path) => Expr::Path(path),
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

