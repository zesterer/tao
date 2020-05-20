// pub mod val;
// pub mod data;
pub mod infer;

use std::collections::HashMap;
use internment::LocalIntern;
use crate::{
    ast,
    error::Error,
    src::Span,
    ty::{Primitive, Type},
    node::{Node, SrcNode, TypeNode},
};
use self::infer::{TypeId, TypeInfo, Constraint, InferCtx};

type Ident = LocalIntern<String>;

pub type InferNode<T> = Node<T, (Span, TypeId)>;

impl<T> Node<T, (Span, TypeId)> {
    pub fn span(&self) -> Span {
        self.attr().0
    }

    pub fn type_id(&self) -> TypeId {
        self.attr().1
    }
}

#[derive(Clone, Debug)]
pub enum Value {
    Number(f64),
    String(LocalIntern<String>),
    Boolean(bool),
}

impl Value {
    pub fn get_type_info(&self, _infer: &mut InferCtx) -> TypeInfo {
        match self {
            Value::Boolean(_) => TypeInfo::Primitive(Primitive::Boolean),
            Value::Number(_) => TypeInfo::Primitive(Primitive::Number),
            Value::String(_) => TypeInfo::Primitive(Primitive::String),
        }
    }
}

#[derive(Debug)]
pub enum Pat<M> {
    Wildcard,
    Value(Value),
    List(Vec<Node<Binding<M>, M>>),
    ListFront(Vec<Node<Binding<M>, M>>, Option<SrcNode<Ident>>),
    Tuple(Vec<Node<Binding<M>, M>>),
    Record(Vec<(SrcNode<Ident>, Node<Binding<M>, M>)>),
}

type InferBinding = InferNode<Binding<(Span, TypeId)>>;
type InferPat = SrcNode<Pat<(Span, TypeId)>>;
pub type TypeBinding = TypeNode<Binding<(Span, SrcNode<Type>)>>;
pub type TypePat = SrcNode<Pat<(Span, SrcNode<Type>)>>;

#[derive(Debug)]
pub struct Binding<M> {
    pub pat: SrcNode<Pat<M>>,
    pub binding: Option<SrcNode<Ident>>,
}

impl InferBinding {
    fn get_binding_idents(&self, idents: &mut HashMap<Ident, TypeId>) {
        match &*self.pat {
            Pat::Wildcard | Pat::Value(_) => {},
            Pat::List(items) => items
                .iter()
                .for_each(|item| item.get_binding_idents(idents)),
            Pat::ListFront(items, tail) => {
                items
                    .iter()
                    .for_each(|item| item.get_binding_idents(idents));
                if let Some(ident) = tail {
                    idents.insert(**ident, self.type_id());
                }
            },
            Pat::Tuple(items) => items
                .iter()
                .for_each(|item| item.get_binding_idents(idents)),
            Pat::Record(fields) => fields
                .iter()
                .for_each(|(_, field)| field.get_binding_idents(idents)),
        }

        if let Some(ident) = &self.binding {
            idents.insert(**ident, self.type_id());
        }
    }

    fn binding_idents(&self) -> HashMap<Ident, TypeId> {
        let mut idents = HashMap::default();
        self.get_binding_idents(&mut idents);
        idents
    }
}

impl<M> Pat<M> {
    fn is_refutable(&self) -> bool {
        match &*self {
            Pat::Wildcard => false,
            Pat::Value(_) => true,
            Pat::List(_) => true, // List could be different size
            Pat::ListFront(items, _) => items.len() > 0,
            Pat::Tuple(items) => items.iter().any(|item| item.pat.is_refutable()),
            Pat::Record(fields) => fields.iter().any(|(_, field)| field.pat.is_refutable()),
        }
    }
}

#[derive(Debug)]
pub enum Expr<M> {
    Value(Value),
    Local(Ident),
    Global(Ident, Vec<(SrcNode<Ident>, M)>),
    Unary(SrcNode<ast::UnaryOp>, Node<Self, M>),
    Binary(SrcNode<ast::BinaryOp>, Node<Self, M>, Node<Self, M>),
    List(Vec<Node<Self, M>>),
    Tuple(Vec<Node<Self, M>>),
    Record(Vec<(SrcNode<Ident>, Node<Self, M>)>),
    Func(Node<Binding<M>, M>, Node<Self, M>),
    Apply(Node<Self, M>, Node<Self, M>), // TODO: Should application be a binary operator?
    Match(Node<Self, M>, Vec<(Node<Binding<M>, M>, Node<Self, M>)>),
}

type InferExpr = InferNode<Expr<(Span, TypeId)>>;
pub type TypeExpr = TypeNode<Expr<(Span, SrcNode<Type>)>>;

#[derive(Clone)]
pub enum Scope<'a> {
    Root,
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

    fn get_local(&self, ident: Ident, infer: &mut InferCtx, span: Span) -> Option<TypeId> {
        match self {
            Scope::Local(local, ty, parent) => Some(*ty)
                .filter(|_| local == &ident)
                .or_else(|| parent.get_local(ident, infer, span)),
            Scope::Many(locals, parent) => locals
                .get(&ident)
                .copied()
                .or_else(|| parent.get_local(ident, infer, span)),
            Scope::Root => None,
        }
    }
}

#[derive(Copy, Clone)]
struct GlobalHints<'a>(&'a HashMap<Ident, (Span, Vec<SrcNode<Ident>>, &'a SrcNode<ast::Type>)>);

#[derive(Clone)]
pub struct DataCtx<'a> {
    module: &'a Module,
    generics: HashMap<Ident, TypeId>,

    // TODO: Include type hint information
    globals: GlobalHints<'a>,
}

impl<'a> DataCtx<'a> {
    fn get_data_type(&self, name: &SrcNode<Ident>, params: &[TypeId], infer: &mut InferCtx, span: Span) -> Result<TypeId, Error> {
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
                Ok(infer.insert(ty_info, span))
            } else {
                Err(Error::custom(format!("Primitive type '{}' cannot be parameterised", **name))
                    .with_span(name.span())
                    .with_span(infer.span(params[0]))
                    .with_hint(format!("Remove all type parameters from '{}'", **name)))
            }
        } else {
            if let Some(res) = self.module
                .get_data_type(**name)
                .map(|(ty, generics)| if params.len() != generics.len() {
                    Err(Error::custom(format!("Type '{}' expected {} parameters, found {}", **name, generics.len(), params.len()))
                        .with_span(ty.span())
                        .with_span(span))
                } else {
                    Ok(infer.instantiate_ty_inner(&|name| generics
                        .iter()
                        .zip(params.iter())
                        .find(|(gen, _)| ***gen == name)
                        .map(|(_, ty)| *ty), ty))
                })
            {
                res
            } else {
                Err(Error::custom(format!("No such data type '{}'", **name))
                    .with_span(name.span()))
            }
        }
    }

    fn get_def_type(&self, ident: Ident, data: &DataCtx, infer: &mut InferCtx, span: Span) -> Result<Option<(TypeId, Vec<(SrcNode<Ident>, TypeId)>)>, Error> {
        self.module
            .get_def_type(ident, infer)
            .map(|x| Ok(Some(x)))
            .unwrap_or_else(|| Ok(if let Some((global_span, generics, hint)) = self.globals.0.get(&ident) {
                let generics = generics.iter().map(|ident| (ident.clone(), infer.insert(TypeInfo::Unknown(Some(Type::GenParam(**ident))), span))).collect::<Vec<_>>();
                Some((
                    hint.to_type_id(data, infer, &|gen| generics.iter().find(|(name, _)| **name == gen).map(|(_, ty)| *ty))?,
                    generics,
                ))
            } else {
                None
            }))
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
    pub root: Module,
}

impl Program {
    pub fn new() -> Self {
        Self {
            root: Module::default(),
        }
    }

    pub fn new_root(module: &SrcNode<ast::Module>) -> Result<Self, Error> {
        let mut this = Self::new();
        // Collect list of globals
        let globals = module.decls
            .iter()
            .filter_map(|decl| match &**decl {
                ast::Decl::Def(def) => Some((*def.name, (def.name.span(), def.generics.clone(), &def.ty))),
                _ => None,
            })
            .collect();

        for decl in module.decls.iter() {
            match &**decl {
                ast::Decl::Def(def) => this.insert_def_inner(GlobalHints(&globals), def)?,
                ast::Decl::TypeAlias(type_alias) => this.insert_type_alias(GlobalHints(&globals), type_alias)?,
                ast::Decl::Data(_) => todo!("Data types"),
            }
        }

        this.type_check()?;

        Ok(this)
    }

    fn type_check(&self) -> Result<(), Error> {
        // Go through globals, ensuring they type-check recursively
        for (_, def) in self.root.defs.iter() {
            // Ensure that all self-references in the body match with the inferred definition
            // This is a halfway-house solution that allows recursion without proper damas-HM inference
            def.body
                .visit()
                .try_for_each(|expr| match &**expr {
                    Expr::Global(ident, _) => {
                        let mut infer = InferCtx::default();
                        let global = &self.root.defs[ident];
                        let definition = infer.instantiate_ty(&global.generics, global.body.ty(), global.body.ty().span()).0;
                        // TODO: Put the usage span in here
                        let usage = infer.instantiate_ty(&def.generics, expr.ty(), expr.ty().span()).0;
                        infer.unify(usage, definition)
                    },
                    _ => Ok(()),
                })?;

            // Type-check pattern refutability
            def.body
                .visit()
                .try_for_each(|expr| match &**expr {
                    Expr::Func(param, _) if param.pat.is_refutable() => Err(Error::custom(format!("Refutable pattern may not be used here"))
                        .with_span(param.pat.span())),
                    _ => Ok(()),
                })?;
        }

        Ok(())
    }

    pub fn root(&self) -> &Module {
        &self.root
    }

    fn create_scope(&self) -> Scope {
        Scope::Root
    }

    pub fn insert_def(&mut self, ast_def: &ast::Def) -> Result<(), Error> {
        self.insert_def_inner(GlobalHints(&HashMap::default()), ast_def)
    }

    pub fn insert_def_inner(&mut self, globals: GlobalHints, ast_def: &ast::Def) -> Result<(), Error> {
        // Check for double declaration
        if let Some(existing_def) = self.root.defs.get(&**ast_def.name) {
            return Err(Error::custom(format!("Definition with name '{}' already exists", **ast_def.name))
                .with_span(existing_def.name.span())
                .with_span(ast_def.name.span()));
        }

        let mut infer = InferCtx::default();
        let data = DataCtx {
            module: &self.root,
            generics: ast_def
                .generics
                .iter()
                .map(|ident| (**ident, infer.insert(TypeInfo::GenParam(**ident), ident.span())))
                .collect(),
            globals,
        };
        let scope = self.create_scope();

        let body = {
            let mut body = ast_def.body.to_hir(&data, &mut infer, &scope)?;

            // Unify with optional type annotation
            let ty_id = ast_def.ty.to_type_id(&data, &mut infer, &|_| None)?;

            infer.unify(body.type_id(), ty_id)?;

            infer.solve_all()?;
            body.into_checked(&mut infer)?
                // Use the def name as the type's span
                .map_meta(|(span, ty)| (span, ty.map_meta(|_| ast_def.name.span())))
        };

        let generics = ast_def.generics
            .iter()
            .map(|g| SrcNode::new(**g, g.span()))
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
                    .with_span(gen.span())
                    .with_span(body.ty().span())
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

    pub fn insert_type_alias(&mut self, globals: GlobalHints, ast_type_alias: &ast::TypeAlias) -> Result<(), Error> {
        let mut infer = InferCtx::default();
        let data = DataCtx {
            module: &self.root,
            generics: ast_type_alias
                .generics
                .iter()
                .map(|ident| (**ident, infer.insert(TypeInfo::GenParam(**ident), ident.span())))
                .collect(),
            globals,
        };

        let span = ast_type_alias.ty.span();
        let generics = ast_type_alias.generics
            .iter()
            .map(|g| SrcNode::new(**g, g.span()))
            .collect::<Vec<_>>();

        let ty_id = ast_type_alias.ty.to_type_id(&data, &mut infer, &|_| None)?;

        infer.solve_all()?;
        let ty = infer.reconstruct(ty_id, span)?;

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
                    .with_span(gen.span())
                    .with_span(ty.span())
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
    type_aliases: HashMap<Ident, TypeAlias>,
    defs: HashMap<Ident, Def>,
}

impl Module {
    pub fn def(&self, name: Ident) -> Option<&Def> {
        self.defs.get(&name)
    }

    fn get_def_type(&self, ident: Ident, infer: &mut InferCtx) -> Option<(TypeId, Vec<(SrcNode<Ident>, TypeId)>)> {
        self.defs
            .get(&ident)
            .map(|def| infer.instantiate_ty(&def.generics, def.body.ty(), def.name.span()))
    }

    fn get_data_type(&self, ident: Ident) -> Option<(&SrcNode<Type>, &[SrcNode<Ident>])> {
        self
            .type_aliases
            .get(&ident)
            .map(|type_alias| (&type_alias.ty, type_alias.generics.as_slice()))
    }
}

// AST to HIR conversions

impl ast::Binding {
    fn to_hir(self: &SrcNode<Self>, infer: &mut InferCtx) -> Result<InferBinding, Error> {
        let (type_id, pat) = match &*self.pat {
            ast::Pat::Wildcard => (infer.insert(TypeInfo::Unknown(None), self.span()), Pat::Wildcard),
            ast::Pat::Literal(litr) => {
                let val = litr.to_hir()?;
                let val_type_info = val.get_type_info(infer);
                (infer.insert(val_type_info, self.span()), Pat::Value(val))
            },
            ast::Pat::List(items) => {
                let item_type_id = infer.insert(TypeInfo::Unknown(None), self.span());
                let items = items
                    .iter()
                    .map(|item| {
                        let item = item.to_hir(infer)?;
                        infer.unify(item.type_id(), item_type_id)?;
                        Ok(item)
                    })
                    .collect::<Result<_, _>>()?;
                (infer.insert(TypeInfo::List(item_type_id), self.span()), Pat::List(items))
            },
            ast::Pat::ListFront(items, tail) => {
                let item_type_id = infer.insert(TypeInfo::Unknown(None), self.span());
                let items = items
                    .iter()
                    .map(|item| {
                        let item = item.to_hir(infer)?;
                        infer.unify(item.type_id(), item_type_id)?;
                        Ok(item)
                    })
                    .collect::<Result<_, _>>()?;
                (infer.insert(TypeInfo::List(item_type_id), self.span()), Pat::ListFront(items, tail.clone()))
            },
            ast::Pat::Tuple(items) => {
                let mut item_type_ids = Vec::new();
                let items = items
                    .iter()
                    .map(|item| {
                        let item = item.to_hir(infer)?;
                        item_type_ids.push(item.type_id());
                        Ok(item)
                    })
                    .collect::<Result<_, _>>()?;
                (infer.insert(TypeInfo::Tuple(item_type_ids), self.span()), Pat::Tuple(items))
            },
            ast::Pat::Record(fields) => {
                let mut field_type_ids = Vec::new();
                let fields = fields
                    .iter()
                    .map(|(name, field)| {
                        let field = field.to_hir(infer)?;
                        field_type_ids.push((name.clone(), field.type_id()));
                        Ok((name.clone(), field))
                    })
                    .collect::<Result<_, _>>()?;
                (infer.insert(TypeInfo::Record(field_type_ids), self.span()), Pat::Record(fields))
            },
        };

        Ok(InferNode::new(Binding {
            pat: SrcNode::new(pat, self.pat.span()),
            binding: self.binding.clone(),
        }, (self.span(), type_id)))
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
    fn to_type_id(self: &SrcNode<Self>, data: &DataCtx, infer: &mut InferCtx, get_generic: &impl Fn(Ident) -> Option<TypeId>) -> Result<TypeId, Error> {
        let info = match &**self {
            ast::Type::Unknown => TypeInfo::Unknown(None),
            ast::Type::Data(name, params) => {
                // Substitute generic
                if params.len() == 0 {
                    if let Some(ty_id) = get_generic(**name) {
                        return Ok(ty_id);
                    }
                }
                let params = params
                    .iter()
                    .map(|param| param.to_type_id(data, infer, get_generic))
                    .collect::<Result<Vec<_>, _>>()?;
                TypeInfo::Ref(data.get_data_type(name, &params, infer, self.span())?)
            },
            ast::Type::List(ty) => TypeInfo::List(ty.to_type_id(data, infer, get_generic)?),
            ast::Type::Tuple(items) => TypeInfo::Tuple(items
                .iter()
                .map(|item| item.to_type_id(data, infer, get_generic))
                .collect::<Result<_, _>>()?),
            ast::Type::Record(fields) => TypeInfo::Record(fields
                .iter()
                .map(|(name, field)| Ok((name.clone(), field.to_type_id(data, infer, get_generic)?)))
                .collect::<Result<_, _>>()?),
            ast::Type::Func(i, o) => TypeInfo::Func(
                i.to_type_id(data, infer, get_generic)?,
                o.to_type_id(data, infer, get_generic)?,
            ),
        };

        Ok(infer.insert(info, self.span()))
    }
}

impl ast::Expr {
    fn to_hir(self: &SrcNode<Self>, data: &DataCtx, infer: &mut InferCtx, scope: &Scope) -> Result<InferExpr, Error> {
        let (type_id, hir_expr) = match &**self {
            ast::Expr::Literal(litr) => {
                let val = litr.to_hir()?;
                let ty_info = val.get_type_info(infer);
                (infer.insert(ty_info, self.span()), Expr::Value(val))
            },
            ast::Expr::Path(path) => if path.len() == 1 {
                if let Some(local_id) = scope
                    .get_local(path.base(), infer, self.span())
                {
                    let type_id = infer.insert(TypeInfo::Unknown(None), self.span());
                    infer.unify(type_id, local_id)?;
                    (type_id, Expr::Local(path.base()))
                } else if let Some((global_id, generics)) = data.get_def_type(path.base(), data, infer, self.span())? {
                    let generics = generics.into_iter().map(|(ident, ty)| (ident.clone(), (ident.span(), ty))).collect();
                    (global_id, Expr::Global(path.base(), generics))
                } else {
                    return Err(Error::custom(format!("No such binding '{}' in scope", path.base().to_string()))
                        .with_span(self.span()));
                }
            } else {
                todo!("Complex paths")
            },
            ast::Expr::Unary(op, a) => {
                let a = a.to_hir(data, infer, scope)?;
                let type_id = infer.insert(TypeInfo::Unknown(None), self.span());
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
                let type_id = infer.insert(TypeInfo::Unknown(None), self.span());
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
                let item_type_id = infer.insert(TypeInfo::Unknown(None), self.span());
                for item in items.iter() {
                    infer.unify(item_type_id, item.type_id())?;
                }
                let type_id = infer.insert(TypeInfo::List(item_type_id), self.span());
                (type_id, Expr::List(items))
            },
            ast::Expr::Tuple(items) => {
                let items = items
                    .iter()
                    .map(|item| item.to_hir(data, infer, scope))
                    .collect::<Result<Vec<_>, _>>()?;
                let type_id = infer.insert(TypeInfo::Tuple(items.iter().map(|item| item.type_id()).collect()), self.span());
                (type_id, Expr::Tuple(items))
            },
            ast::Expr::Record(fields) => {
                let fields = fields
                    .iter()
                    .map(|(name, value)| Ok((name.clone(), value.to_hir(data, infer, scope)?)))
                    .collect::<Result<Vec<_>, _>>()?;
                let type_id = infer.insert(TypeInfo::Record(fields.iter().map(|(name, value)| (name.clone(), value.type_id())).collect()), self.span());
                (type_id, Expr::Record(fields))
            },
            ast::Expr::Func(param, param_ty, body) => {
                let param = param.to_hir(infer)?;

                // Unify param_ty with type hint
                if let Some(param_ty) = param_ty {
                    let param_ty_id = param_ty.to_type_id(data, infer, &|_| None)?;
                    infer.unify(param.type_id(), param_ty_id)?;
                }

                let body_scope = scope.with_many(param.binding_idents());
                let body = body.to_hir(data, infer, &body_scope)?;

                let type_id = infer.insert(TypeInfo::Func(param.type_id(), body.type_id()), self.span());
                (type_id, Expr::Func(param, body))
            },
            ast::Expr::Apply(f, arg) => {
                let f = f.to_hir(data, infer, scope)?;
                let arg = arg.to_hir(data, infer, scope)?;
                let type_id = infer.insert(TypeInfo::Unknown(None), self.span());
                let f_type_id = infer.insert(TypeInfo::Func(arg.type_id(), type_id), f.span());
                infer.unify(f_type_id, f.type_id())?;
                (type_id, Expr::Apply(f, arg))
            },
            ast::Expr::Let(pat, pat_ty, val, then) => {
                // `let a = b in c` desugars to `b:(a -> c)`
                // TODO: Desugar to `match b in a => c` instead?
                let pat = pat.to_hir(infer)?;

                // Unify pattern with type hint
                if let Some(pat_ty) = pat_ty {
                    let pat_ty_id = pat_ty.to_type_id(data, infer, &|_| None)?;
                    infer.unify(pat.type_id(), pat_ty_id)?;
                }

                let val = val.to_hir(data, infer, scope)?;
                infer.unify(pat.type_id(), val.type_id())?;

                let then_scope = scope.with_many(pat.binding_idents());
                let then_body = then.to_hir(data, infer, &then_scope)?;

                (then_body.type_id(), Expr::Match(val, vec![
                    (pat, then_body)
                ]))
            },
            ast::Expr::If(pred, a, b) => {
                let pred_hir = pred.to_hir(data, infer, scope)?;
                let pred_type_id = infer.insert(TypeInfo::Primitive(Primitive::Boolean), pred.span());
                infer.unify(pred_hir.type_id(), pred_type_id)?;

                let a = a.to_hir(data, infer, scope)?;
                let b = b.to_hir(data, infer, scope)?;
                infer.unify(a.type_id(), b.type_id())?;

                (a.type_id(), Expr::Match(pred_hir, vec![
                    (InferNode::new(Binding {
                        pat: SrcNode::new(Pat::Value(Value::Boolean(true)), Span::none()),
                        binding: None,
                    }, (pred.span(), pred_type_id)), a),
                    (InferNode::new(Binding {
                        pat: SrcNode::new(Pat::Value(Value::Boolean(false)), Span::none()),
                        binding: None,
                    }, (pred.span(), pred_type_id)), b),
                ]))
            },
            ast::Expr::Match(pred, arms) => {
                let pred = pred.to_hir(data, infer, scope)?;

                let match_type_id = infer.insert(TypeInfo::Unknown(None), self.span());

                let arms = arms
                    .iter()
                    .map(|((pat, pat_ty), body)| {
                        let pat = pat.to_hir(infer)?;

                        // Unify pattern with type hint
                        if let Some(pat_ty) = pat_ty {
                            let pat_ty_id = pat_ty.to_type_id(data, infer, &|_| None)?;
                            infer.unify(pat.type_id(), pat_ty_id)?;
                        }

                        infer.unify(pat.type_id(), pred.type_id())?;

                        let body_scope = scope.with_many(pat.binding_idents());
                        let body = body.to_hir(data, infer, &body_scope)?;

                        infer.unify(match_type_id, body.type_id())?;

                        Ok((pat, body))
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                if arms
                    .iter()
                    .all(|(binding, _)| binding.pat.is_refutable())
                {
                    return Err(Error::custom(format!("Match requires irrefutable pattern somewhere (TODO: Implement proper exhaustivity checks)"))
                        .with_span(self.span()));
                }

                (match_type_id, Expr::Match(pred, arms))
            },
            expr => todo!("HIR for {:?}", expr),
        };

        Ok(InferNode::new(hir_expr, (self.span(), type_id)))
    }
}

impl InferPat {
    fn into_checked(self, infer: &InferCtx) -> Result<TypePat, Error> {
        let span = self.span();

        let pat = match self.into_inner() {
            Pat::Wildcard => Pat::Wildcard,
            Pat::Value(val) => Pat::Value(val),
            Pat::List(items) => Pat::List(items
                .into_iter()
                .map(|item| item.into_checked(infer))
                .collect::<Result<_, _>>()?),
            Pat::ListFront(items, tail) => Pat::ListFront(items
                .into_iter()
                .map(|item| item.into_checked(infer))
                .collect::<Result<_, _>>()?, tail),
            Pat::Tuple(items) => Pat::Tuple(items
                .into_iter()
                .map(|item| item.into_checked(infer))
                .collect::<Result<_, _>>()?),
            Pat::Tuple(items) => Pat::Tuple(items
                .into_iter()
                .map(|item| item.into_checked(infer))
                .collect::<Result<_, _>>()?),
            Pat::Record(fields) => Pat::Record(fields
                .into_iter()
                .map(|(name, field)| Ok((name, field.into_checked(infer)?)))
                .collect::<Result<_, _>>()?),
        };

        Ok(SrcNode::new(pat, span))
    }
}

impl InferBinding {
    fn into_checked(self, infer: &InferCtx) -> Result<TypeBinding, Error> {
        let span = self.span();
        let meta = infer.reconstruct(self.type_id(), span)?;

        let this = self.into_inner();

        Ok(TypeNode::new(Binding {
            pat: this.pat.into_checked(infer)?,
            binding: this.binding,
        }, (span, meta)))
    }
}

impl InferExpr {
    fn into_checked(self, infer: &InferCtx) -> Result<TypeExpr, Error> {
        let span = self.span();
        let meta = infer.reconstruct(self.type_id(), span)?;

        Ok(TypeNode::new(match self.into_inner() {
            Expr::Value(val) => Expr::Value(val),
            Expr::Local(ident) => Expr::Local(ident),
            Expr::Global(ident, generics) => Expr::Global(
                ident,
                generics
                    .into_iter()
                    .map(|(ident, (span, type_id))| Ok((ident, (span, infer.reconstruct(type_id, span)?))))
                    .collect::<Result<_, _>>()?,
            ),
            Expr::Unary(op, x) => Expr::Unary(
                op,
                x.into_checked(infer)?,
            ),
            Expr::Binary(op, x, y) => Expr::Binary(
                op,
                x.into_checked(infer)?,
                y.into_checked(infer)?,
            ),
            Expr::List(items) => Expr::List(items
                .into_iter()
                .map(|item| item.into_checked(infer))
                .collect::<Result<_, _>>()?),
            Expr::Tuple(items) => Expr::Tuple(items
                .into_iter()
                .map(|item| item.into_checked(infer))
                .collect::<Result<_, _>>()?),
            Expr::Record(fields) => Expr::Record(fields
                .into_iter()
                .map(|(name, value)| Ok((name, value.into_checked(infer)?)))
                .collect::<Result<_, _>>()?),
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
        }, (span, meta)))
    }
}

impl TypeExpr {
    fn visit(&self) -> impl Iterator<Item=&Self> + '_ {
        let mut stack = vec![self];
        std::iter::from_fn(move || stack
            .pop()
            .map(|expr| {
                match &**expr {
                    Expr::Value(_) => {},
                    Expr::Local(_) => {},
                    Expr::Global(_, _) => {},
                    Expr::Unary(_, x) => stack.push(x),
                    Expr::Binary(_, x, y) => {
                        stack.push(x);
                        stack.push(y);
                    },
                    Expr::List(items) => {
                        for item in items.iter() {
                            stack.push(item);
                        }
                    },
                    Expr::Tuple(items) => {
                        for item in items.iter() {
                            stack.push(item);
                        }
                    },
                    Expr::Record(fields) => {
                        for (_, value) in fields.iter() {
                            stack.push(value);
                        }
                    },
                    Expr::Func(_, body) => {
                        stack.push(body);
                    },
                    Expr::Apply(f, i) => {
                        stack.push(f);
                        stack.push(i);
                    },
                    Expr::Match(pred, arms) => {
                        stack.push(pred);
                        for (_, arm) in arms.iter() {
                            stack.push(arm);
                        }
                    },
                }

                expr
            }))
    }
}
