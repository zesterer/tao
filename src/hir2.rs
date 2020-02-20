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

pub type InferNode<T> = Node<T, (SrcRegion, TypeId, Option<Type>)>;

impl<T> Node<T, (SrcRegion, TypeId, Option<Type>)> {
    pub fn region(&self) -> SrcRegion {
        self.attr().0
    }

    pub fn type_id(&self) -> TypeId {
        self.attr().1
    }

    pub fn ty(&self) -> Option<&Type> {
        self.attr().2.as_ref()
    }

    pub fn give_type(&mut self, ty: Type) {
        self.attr_mut().2 = Some(ty);
    }
}

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

pub enum Pat {
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

pub enum Expr {
    Value(Value),
    Path(ast::Path),
    Unary(SrcNode<ast::UnaryOp>, InferNode<Expr>),
    Binary(SrcNode<ast::BinaryOp>, InferNode<Expr>, InferNode<Expr>),
    List(Vec<InferNode<Expr>>),
    Tuple(Vec<InferNode<Expr>>),
    Func(InferNode<Pat>, InferNode<Expr>),
    Apply(InferNode<Expr>, InferNode<Expr>), // TODO: Should application be a binary operator?
    Match(InferNode<Expr>, Vec<(InferNode<Pat>, InferNode<Expr>)>),
}

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

pub struct Engine {
    type_engine: TypeEngine,
    core: Core,
    defs: HashMap<LocalIntern<Path>, (InferCtx, InferNode<Expr>)>,
}

impl Engine {
    pub fn new() -> Self {
        let (type_engine, core) = TypeEngine::default().with_core();
        Self {
            type_engine,
            core,
            defs: HashMap::default(),
        }
    }

    fn make_litr(&self, litr: &ast::Literal) -> Result<Value, Error> {
        Ok(match litr {
            ast::Literal::Number(x) => Value::Number(*x),
            ast::Literal::String(x) => Value::String(*x),
            ast::Literal::Boolean(x) => Value::Boolean(*x),
        })
    }

    fn make_pat(&self, ctx: &mut InferCtx, pat: &SrcNode<ast::Pat>) -> Result<InferNode<Pat>, Error> {
        let hir_pat = match &**pat {
            ast::Pat::Ident(ident) => Pat::Ident(*ident),
            ast_pat => todo!("HIR parsing of {:?}", ast_pat),
        };

        Ok(InferNode::new(hir_pat, (pat.region(), ctx.insert(TypeInfo::Unknown, pat.region()), None)))
    }

    fn make_expr(&self, ctx: &mut InferCtx, expr: &SrcNode<ast::Expr>) -> Result<InferNode<Expr>, Error> {
        let hir_expr = match &**expr {
            ast::Expr::Literal(litr) => Expr::Value(self.make_litr(litr)?),
            ast::Expr::Path(path) => Expr::Path(path.clone()),
            ast::Expr::Unary(op, a) => Expr::Unary(op.clone(), self.make_expr(ctx, a)?),
            ast::Expr::Binary(op, a, b) => Expr::Binary(op.clone(), self.make_expr(ctx, a)?, self.make_expr(ctx, b)?),
            ast::Expr::List(items) => Expr::List(items.iter().map(|item| self.make_expr(ctx, item)).collect::<Result<_, _>>()?),
            ast::Expr::Tuple(items) => Expr::Tuple(items.iter().map(|item| self.make_expr(ctx, item)).collect::<Result<_, _>>()?),
            ast::Expr::Func(param, body) => Expr::Func(self.make_pat(ctx, param)?, self.make_expr(ctx, body)?),
            ast::Expr::Apply(f, arg) => Expr::Apply(self.make_expr(ctx, f)?, self.make_expr(ctx, arg)?),
            ast::Expr::Let(pat, _, expr, then) => {
                // `let a = b in c` desugars to `b:(a -> c)`
                let then = InferNode::new(Expr::Func(self.make_pat(ctx, pat)?, self.make_expr(ctx, then)?), (then.region(), ctx.insert(TypeInfo::Unknown, then.region()), None));
                Expr::Apply(then, self.make_expr(ctx, expr)?)
            },
            ast::Expr::If(pred, a, b) => {
                Expr::Match(self.make_expr(ctx, pred)?, vec![
                    (InferNode::new(Pat::Value(Value::Boolean(true)), (a.region(), ctx.insert(TypeInfo::Unknown, a.region()), None)), self.make_expr(ctx, a)?),
                    (InferNode::new(Pat::Value(Value::Boolean(false)), (b.region(), ctx.insert(TypeInfo::Unknown, b.region()), None)), self.make_expr(ctx, b)?),
                ])
            },
            ast_expr => todo!("HIR parsing of {:?}", ast_expr),
        };

        Ok(InferNode::new(hir_expr, (expr.region(), ctx.insert(TypeInfo::Unknown, expr.region()), None)))
    }

    // TODO: Allow passing in type parameters
    pub fn with_def(mut self, name: LocalIntern<Path>, body: &SrcNode<ast::Expr>) -> Result<Self, Error> {
        let mut ctx = InferCtx::default();
        let body = self.make_expr(&mut ctx, body)?;
        self.defs.insert(name, (ctx, body));
        Ok(self)
    }

    pub fn def(&self, name: LocalIntern<Path>) -> Option<&InferNode<Expr>> {
        self.defs.get(&name).map(|(_, expr)| expr)
    }

    fn infer_pat(core: &Core, ctx: &mut InferCtx, pat: &mut InferNode<Pat>) -> Result<(), Error> {
        let type_id = match &mut **pat {
            Pat::Value(x) => ctx.insert(TypeInfo::from(x.get_data_id(core)), pat.region()),
            Pat::Ident(ident) => ctx.insert(TypeInfo::Unknown, pat.region()),
        };

        ctx.unify(pat.type_id(), type_id)?;

        Ok(())
    }

    fn infer_expr(core: &Core, ctx: &mut InferCtx, scope: &Scope, expr: &mut InferNode<Expr>) -> Result<(), Error> {
        let region = expr.region();
        let type_id = match &mut **expr {
            Expr::Value(x) => ctx.insert(TypeInfo::from(x.get_data_id(core)), region),
            Expr::Path(path) => if path.len() == 1 {
                scope
                    .get(path.base())
                    .ok_or_else(|| Error::no_such_binding(path.base().to_string(), region))?
            } else {
                todo!("Complex paths")
            },
            Expr::Unary(op, x) => {
                Self::infer_expr(core, ctx, scope, x)?;
                ctx.insert(TypeInfo::Associated(
                    op_to_unary_trait(core, **op).into(),
                    x.type_id(),
                    LocalIntern::new("Out".to_string()),
                ), region)
            },
            Expr::Binary(op, x, y) => {
                Self::infer_expr(core, ctx, scope, x)?;
                Self::infer_expr(core, ctx, scope, y)?;
                ctx.insert(TypeInfo::Associated(
                    InnerTrait::new(op_to_binary_trait(core, **op), vec![y.type_id().into()]),
                    x.type_id(),
                    LocalIntern::new("Out".to_string()),
                ), region)
            },
            Expr::List(items) => {
                let item_ty = ctx.insert(TypeInfo::Unknown, region);
                for item in items.iter_mut() {
                    Self::infer_expr(core, ctx, scope, item)?;
                    ctx.unify(item_ty, item.type_id())?;
                }
                ctx.insert(TypeInfo::List(item_ty), region)
            },
            Expr::Tuple(items) => {
                let items = items
                    .iter_mut()
                    .map(|item| {
                        Self::infer_expr(core, ctx, scope, item)?;
                        Ok(item.type_id())
                    })
                    .collect::<Result<_, _>>()?;
                ctx.insert(TypeInfo::Tuple(items), region)
            },
            Expr::Func(param, body) => {
                Self::infer_pat(core, ctx, param)?;
                let body_scope = scope.with_many(param.ident_types());
                Self::infer_expr(core, ctx, &body_scope, body)?;
                ctx.insert(TypeInfo::Func(param.type_id(), body.type_id()), region)
            },
            Expr::Apply(f, arg) => {
                Self::infer_expr(core, ctx, scope, f)?;
                Self::infer_expr(core, ctx, scope, arg)?;
                let out_ty = ctx.insert(TypeInfo::Unknown, region);
                let f_ty = ctx.insert(TypeInfo::Func(
                    arg.type_id(),
                    out_ty,
                ), f.region());
                ctx.unify(f.type_id(), f_ty)?;
                out_ty
            },
            Expr::Match(pred, arms) => {
                Self::infer_expr(core, ctx, scope, pred)?;
                let out_ty = ctx.insert(TypeInfo::Unknown, region);
                for (pat, body) in arms.iter_mut() {
                    Self::infer_pat(core, ctx, pat)?;
                    ctx.unify(pat.type_id(), pred.type_id())?;
                    let body_scope = scope.with_many(pat.ident_types());
                    Self::infer_expr(core, ctx, &body_scope, body)?;
                    ctx.unify(body.type_id(), out_ty)?;
                }
                out_ty
            },
            _ => todo!(),
        };

        ctx.unify(expr.type_id(), type_id)?;

        Ok(())
    }

    pub fn infer_types(mut self) -> Result<Self, Vec<Error>> {
        let mut errs = Vec::new();
        for (_, (ctx, def)) in &mut self.defs {
            if let Err(err) = Self::infer_expr(&self.core, ctx, &Scope::Empty, def) {
                errs.push(err);
            }
        }

        if errs.len() == 0 {
            Ok(self)
        } else {
            Err(errs)
        }
    }

    fn check_pat(engine: &TypeEngine, ctx: &InferCtx, pat: &mut InferNode<Pat>) -> Result<(), Error> {
        match &mut **pat {
            Pat::Value(_) => {},
            Pat::Ident(_) => {},
        }

        pat.give_type(ctx.reconstruct(pat.type_id())?);

        Ok(())
    }

    fn check_expr(engine: &TypeEngine, ctx: &InferCtx, expr: &mut InferNode<Expr>) -> Result<(), Error> {
        match &mut **expr {
            Expr::Value(_) => {},
            Expr::Path(_) => {},
            Expr::Unary(_, x) => {
                Self::check_expr(engine, ctx, x)?;
            },
            Expr::Binary(_, x, y) => {
                Self::check_expr(engine, ctx, x)?;
                Self::check_expr(engine, ctx, y)?;
            },
            Expr::List(items) => {
                for item in items.iter_mut() {
                    Self::check_expr(engine, ctx, item)?;
                }
            },
            Expr::Tuple(items) => {
                for item in items.iter_mut() {
                    Self::check_expr(engine, ctx, item)?;
                }
            },
            Expr::Func(param, body) => {
                Self::check_pat(engine, ctx, param)?;
                Self::check_expr(engine, ctx, body)?;
            },
            Expr::Apply(f, arg) => {
                Self::check_expr(engine, ctx, f)?;
                Self::check_expr(engine, ctx, arg)?;
            },
            Expr::Match(pred, arms) => {
                Self::check_expr(engine, ctx, pred)?;
                for (pat, body) in arms.iter_mut() {
                    Self::check_pat(engine, ctx, pat)?;
                    Self::check_expr(engine, ctx, body)?;
                }
            },
            _ => todo!(),
        }

        expr.give_type(ctx.reconstruct(expr.type_id())?);

        Ok(())
    }

    pub fn check_types(mut self) -> Result<CheckedEngine, Vec<Error>> {
        let mut errs = Vec::new();
        for (_, (ctx, def)) in &mut self.defs {
            if let Err(err) = Self::check_expr(&self.type_engine, ctx, def) {
                errs.push(err);
            }
        }

        if errs.len() == 0 {
            Ok(CheckedEngine {
                engine: self,
            })
        } else {
            Err(errs)
        }
    }
}

pub struct CheckedEngine {
    engine: Engine,
}

impl Deref for CheckedEngine {
    type Target = Engine;

    fn deref(&self) -> &Self::Target { &self.engine }
}

impl CheckedEngine {
    pub fn generate_mir_expr(&self, expr: &InferNode<Expr>) -> Result<TypeNode<mir::Expr>, Error> {
        todo!()
    }

    pub fn generate_mir(&self) -> Result<mir::Module, Error> {
        self.defs
            .iter()
            .try_fold(
                mir::Module::default(),
                |module, (name, (ctx, body))| Ok(module.with_def(*name, self.generate_mir_expr(body)?)),
            )
    }
}


