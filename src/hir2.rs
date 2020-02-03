use std::collections::HashMap;
use internment::LocalIntern;
use crate::{
    ast,
    mir,
    error::Error,
    ty::{TypeEngine, TypeInfo, InferCtx, Core},
    node2::{SrcNode, SrcTypeNode, TypeNode},
};

type Ident = LocalIntern<String>;

pub enum Value {
    Number(f64),
    String(LocalIntern<String>),
    Boolean(bool),
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

pub enum Expr {
    Value(Value),
    Path(LocalIntern<Path>),
    Unary(SrcNode<ast::UnaryOp>, SrcTypeNode<Expr>),
    Binary(SrcNode<ast::BinaryOp>, SrcTypeNode<Expr>, SrcTypeNode<Expr>),
}

pub struct Engine {
    type_engine: TypeEngine,
    core: Core,
    defs: HashMap<LocalIntern<Path>, (InferCtx, SrcTypeNode<Expr>)>,
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

    fn make_expr(&self, ctx: &mut InferCtx, expr: &SrcNode<ast::Expr>) -> Result<SrcTypeNode<Expr>, Error> {
        let hir_expr = match &**expr {
            ast::Expr::Literal(litr) => Expr::Value(self.make_litr(litr)?),
            ast::Expr::Path(path) => Expr::Path(Path::from_path(path)),
            ast::Expr::Unary(op, a) => Expr::Unary(op.clone(), self.make_expr(ctx, a)?),
            ast::Expr::Binary(op, a, b) => Expr::Binary(op.clone(), self.make_expr(ctx, a)?, self.make_expr(ctx, b)?),
            ast_expr => todo!("HIR parsing of {:?}", ast_expr),
        };

        Ok(SrcTypeNode::new(hir_expr, (expr.region(), ctx.insert(TypeInfo::Unknown))))
    }

    // TODO: Allow passing in type parameters
    pub fn make_def(&mut self, name: LocalIntern<Path>, body: &SrcNode<ast::Expr>) -> Result<(), Error> {
        let mut ctx = InferCtx::default();
        let body = self.make_expr(&mut ctx, body)?;
        self.defs.insert(name, (ctx, body));
        Ok(())
    }

    pub fn generate_mir(&mut self) -> Result<mir::Module, Error> {
        todo!()
    }
}
