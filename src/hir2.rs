use std::{
    ops::Deref,
    collections::HashMap,
};
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

pub type TypedNode<T> = Node<T, (SrcRegion, Type)>;

impl<T> Node<T, (SrcRegion, Type)> {
    pub fn region(&self) -> SrcRegion {
        self.attr().0
    }

    pub fn ty(&self) -> &Type {
        &self.attr().1
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
    fn ident_types(&self) -> Vec<(Ident, TypeId)> {
        match &**self {
            Pat::Value(_) => Vec::new(),
            Pat::Ident(ident) => vec![(*ident, self.type_id())],
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
type TypedExpr = TypedNode<Expr<(SrcRegion, Type)>>;
type TypedPat = TypedNode<Pat>;

#[derive(Clone)]
pub enum Scope<'a> {
    Empty,
    Local(Ident, TypeId, &'a Self),
    Many(Vec<(Ident, TypeId)>, &'a Self),
}

impl<'a> Scope<'a> {
    fn with<'b>(&'b self, ident: Ident, ty: TypeId) -> Scope<'b>
        where 'a: 'b
    {
        Scope::Local(ident, ty, self)
    }

    fn with_many<'b>(&'b self, locals: Vec<(Ident, TypeId)>) -> Scope<'b>
        where 'a: 'b
    {
        if locals.len() == 1 {
            self.with(locals[0].0, locals[0].1)
        } else {
            Scope::Many(locals, self)
        }
    }

    fn get(&self, ident: Ident) -> Option<TypeId> {
        match self {
            Scope::Local(local, ty, parent) => if *local == ident {
                Some(*ty)
            } else {
                parent.get(ident)
            },
            Scope::Many(locals, parent) => locals
                .iter()
                .find(|(local, _)| *local == ident)
                .copied()
                .map(|(_, ty)| ty)
                .or_else(|| parent.get(ident)),
            Scope::Empty => None,
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
    pub body: TypedExpr,
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

    pub fn root(&self) -> &Module {
        &self.root
    }

    pub fn insert_def(&mut self, name: Ident, body: &SrcNode<ast::Expr>) -> Result<(), Error> {
        let mut ctx = InferCtx::default();
        self.root.defs.insert(name, Def {
            body: {
                let mut body = body.to_hir(&mut ctx)?;
                body.infer(&self.core, &mut ctx, &Scope::Empty)?;
                body.into_checked(&self.type_engine, &mut ctx)?
            },
        });
        Ok(())
    }
}

#[derive(Default)]
pub struct Module {
    modules: HashMap<Ident, Module>,
    traits: HashMap<Ident, Trait>,
    datatypes: HashMap<Ident, Data>,
    defs: HashMap<Ident, Def>,
}

impl Module {
    pub fn def(&self, name: Ident) -> Option<&Def> {
        self.defs.get(&name)
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

impl ast::Expr {
    fn to_hir(self: &SrcNode<Self>, ctx: &mut InferCtx) -> Result<InferExpr, Error> {
        let hir_expr = match &**self {
            ast::Expr::Literal(litr) => Expr::Value(litr.to_hir()?),
            ast::Expr::Path(path) => Expr::Path(path.clone()),
            ast::Expr::Unary(op, a) => Expr::Unary(op.clone(), a.to_hir(ctx)?),
            ast::Expr::Binary(op, a, b) => Expr::Binary(op.clone(), a.to_hir(ctx)?, b.to_hir(ctx)?),
            ast::Expr::List(items) => Expr::List(items.iter().map(|item| item.to_hir(ctx)).collect::<Result<_, _>>()?),
            ast::Expr::Tuple(items) => Expr::Tuple(items.iter().map(|item| item.to_hir(ctx)).collect::<Result<_, _>>()?),
            ast::Expr::Func(param, body) => Expr::Func(param.to_hir(ctx)?, body.to_hir(ctx)?),
            ast::Expr::Apply(f, arg) => Expr::Apply(f.to_hir(ctx)?, arg.to_hir(ctx)?),
            ast::Expr::Let(pat, _, expr, then) => {
                // `let a = b in c` desugars to `b:(a -> c)`
                let then = InferNode::new(Expr::Func(pat.to_hir(ctx)?, then.to_hir(ctx)?), (then.region(), ctx.insert(TypeInfo::Unknown, then.region())));
                Expr::Apply(then, expr.to_hir(ctx)?)
            },
            ast::Expr::If(pred, a, b) => {
                Expr::Match(pred.to_hir(ctx)?, vec![
                    (InferNode::new(Pat::Value(Value::Boolean(true)), (a.region(), ctx.insert(TypeInfo::Unknown, a.region()))), a.to_hir(ctx)?),
                    (InferNode::new(Pat::Value(Value::Boolean(false)), (b.region(), ctx.insert(TypeInfo::Unknown, b.region()))), b.to_hir(ctx)?),
                ])
            },
            ast_expr => todo!("HIR parsing of {:?}", ast_expr),
        };

        Ok(InferNode::new(hir_expr, (self.region(), ctx.insert(TypeInfo::Unknown, self.region()))))
    }
}

impl Pat {
    fn infer(self: &mut InferNode<Self>, core: &Core, ctx: &mut InferCtx) -> Result<(), Error> {
        let type_id = match &mut **self {
            Pat::Value(x) => ctx.insert(TypeInfo::from(x.get_data_id(core)), self.region()),
            Pat::Ident(ident) => ctx.insert(TypeInfo::Unknown, self.region()),
        };

        ctx.unify(self.type_id(), type_id)?;

        Ok(())
    }

    fn into_checked(self: InferNode<Self>, engine: &TypeEngine, ctx: &InferCtx) -> Result<TypedPat, Error> {
        let meta = (self.region(), ctx.reconstruct(self.type_id())?);

        Ok(TypedNode::new(match self.into_inner() {
            Pat::Value(val) => Pat::Value(val),
            Pat::Ident(ident) => Pat::Ident(ident),
        }, meta))
    }
}

impl Expr<(SrcRegion, TypeId)> {
    fn infer(self: &mut InferNode<Self>, core: &Core, ctx: &mut InferCtx, scope: &Scope) -> Result<(), Error> {
        let region = self.region();
        let type_id = match &mut **self {
            Expr::Value(x) => ctx.insert(TypeInfo::from(x.get_data_id(core)), region),
            Expr::Path(path) => if path.len() == 1 {
                scope
                    .get(path.base())
                    .ok_or_else(|| Error::no_such_binding(path.base().to_string(), region))?
            } else {
                todo!("Complex paths")
            },
            Expr::Unary(op, x) => {
                x.infer(core, ctx, scope)?;
                ctx.insert(TypeInfo::Associated(
                    op_to_unary_trait(core, **op).into(),
                    x.type_id(),
                    LocalIntern::new("Out".to_string()),
                ), region)
            },
            Expr::Binary(op, x, y) => {
                x.infer(core, ctx, scope)?;
                y.infer(core, ctx, scope)?;
                ctx.insert(TypeInfo::Associated(
                    InnerTrait::new(op_to_binary_trait(core, **op), vec![y.type_id().into()]),
                    x.type_id(),
                    LocalIntern::new("Out".to_string()),
                ), region)
            },
            Expr::List(items) => {
                let item_ty = ctx.insert(TypeInfo::Unknown, region);
                for item in items.iter_mut() {
                    item.infer(core, ctx, scope)?;
                    ctx.unify(item_ty, item.type_id())?;
                }
                ctx.insert(TypeInfo::List(item_ty), region)
            },
            Expr::Tuple(items) => {
                let items = items
                    .iter_mut()
                    .map(|item| {
                        item.infer(core, ctx, scope)?;
                        Ok(item.type_id())
                    })
                    .collect::<Result<_, _>>()?;
                ctx.insert(TypeInfo::Tuple(items), region)
            },
            Expr::Func(param, body) => {
                param.infer(core, ctx)?;
                let body_scope = scope.with_many(param.ident_types());
                body.infer(core, ctx, &body_scope)?;
                ctx.insert(TypeInfo::Func(param.type_id(), body.type_id()), region)
            },
            Expr::Apply(f, arg) => {
                f.infer(core, ctx, scope)?;
                arg.infer(core, ctx, scope)?;
                let out_ty = ctx.insert(TypeInfo::Unknown, region);
                let f_ty = ctx.insert(TypeInfo::Func(
                    arg.type_id(),
                    out_ty,
                ), f.region());
                ctx.unify(f.type_id(), f_ty)?;
                out_ty
            },
            Expr::Match(pred, arms) => {
                pred.infer(core, ctx, scope)?;
                let out_ty = ctx.insert(TypeInfo::Unknown, region);
                for (pat, body) in arms.iter_mut() {
                    pat.infer(core, ctx)?;
                    ctx.unify(pat.type_id(), pred.type_id())?;
                    let body_scope = scope.with_many(pat.ident_types());
                    body.infer(core, ctx, &body_scope)?;
                    ctx.unify(body.type_id(), out_ty)?;
                }
                out_ty
            },
            _ => todo!(),
        };

        ctx.unify(self.type_id(), type_id)?;

        Ok(())
    }

    fn into_checked(self: InferNode<Self>, engine: &TypeEngine, ctx: &InferCtx) -> Result<TypedExpr, Error> {
        let meta = (self.region(), ctx.reconstruct(self.type_id())?);

        Ok(TypedNode::new(match self.into_inner() {
            Expr::Value(val) => Expr::Value(val),
            Expr::Path(path) => Expr::Path(path),
            Expr::Unary(op, x) => Expr::Unary(
                op,
                x.into_checked(engine, ctx)?,
            ),
            Expr::Binary(op, x, y) => Expr::Binary(
                op,
                x.into_checked(engine, ctx)?,
                y.into_checked(engine, ctx)?,
            ),
            Expr::List(items) => Expr::List(
                items.into_iter().map(|item| item.into_checked(engine, ctx)).collect::<Result<_, _>>()?,
            ),
            Expr::Tuple(items) => Expr::Tuple(
                items.into_iter().map(|item| item.into_checked(engine, ctx)).collect::<Result<_, _>>()?,
            ),
            Expr::Func(param, body) => Expr::Func(
                param.into_checked(engine, ctx)?,
                body.into_checked(engine, ctx)?,
            ),
            Expr::Apply(f, arg) => Expr::Apply(
                f.into_checked(engine, ctx)?,
                arg.into_checked(engine, ctx)?,
            ),
            Expr::Match(pred, arms) => Expr::Match(
                pred.into_checked(engine, ctx)?,
                arms
                    .into_iter()
                    .map(|(pat, body)| Ok((
                        pat.into_checked(engine, ctx)?,
                        body.into_checked(engine, ctx)?,
                    )))
                    .collect::<Result<_, _>>()?,
            ),
            _ => todo!(),
        }, meta))
    }
}

