// pub mod val;
pub mod data;
pub mod infer;

use std::collections::{HashMap, HashSet};
use internment::LocalIntern;
use crate::{
    ast::{self, Literal},
    error::Error,
    src::Span,
    ty::{Primitive, Type},
    node::{Node, SrcNode, TypeNode},
};
use self::{
    infer::{TypeId, TypeInfo, Constraint, InferCtx},
    data::DataId,
};

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

impl ast::Literal {
    pub fn get_type_info(&self, infer: &mut InferCtx, span: Span) -> TypeInfo {
        match self {
            ast::Literal::Boolean(_) => TypeInfo::Primitive(Primitive::Boolean),
            ast::Literal::Char(_) => TypeInfo::Primitive(Primitive::Char),
            ast::Literal::Number(_) => TypeInfo::Primitive(Primitive::Number),
            ast::Literal::String(_) => TypeInfo::List(infer.insert(TypeInfo::Primitive(Primitive::Char), span)),
        }
    }
}

#[derive(Debug)]
pub enum Pat<M> {
    Wildcard,
    Literal(Literal),
    List(Vec<Node<Binding<M>, M>>),
    ListFront(Vec<Node<Binding<M>, M>>, Option<SrcNode<Ident>>),
    Tuple(Vec<Node<Binding<M>, M>>),
    Record(Vec<(SrcNode<Ident>, Node<Binding<M>, M>)>),
    Deconstruct(SrcNode<(DataId, usize)>, Vec<(SrcNode<Ident>, M)>, Node<Binding<M>, M>),
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

impl<M> Node<Binding<M>, M> {
    fn get_binding_idents<'a>(&'a self, idents: &mut HashMap<Ident, &'a M>) {
        match &*self.pat {
            Pat::Wildcard | Pat::Literal(_) => {},
            Pat::List(items) => items
                .iter()
                .for_each(|item| item.get_binding_idents(idents)),
            Pat::ListFront(items, tail) => {
                items
                    .iter()
                    .for_each(|item| item.get_binding_idents(idents));
                if let Some(ident) = tail {
                    idents.insert(**ident, self.attr());
                }
            },
            Pat::Tuple(items) => items
                .iter()
                .for_each(|item| item.get_binding_idents(idents)),
            Pat::Record(fields) => fields
                .iter()
                .for_each(|(_, field)| field.get_binding_idents(idents)),
            Pat::Deconstruct(_, _, inner) => inner.get_binding_idents(idents),
        }

        if let Some(ident) = &self.binding {
            idents.insert(**ident, self.attr());
        }
    }

    pub fn binding_idents(&self) -> HashMap<Ident, &M> {
        let mut idents = HashMap::default();
        self.get_binding_idents(&mut idents);
        idents
    }
}

impl<M> Pat<M> {
    fn is_refutable(&self, data_ctx: &data::DataCtx) -> bool {
        match &*self {
            Pat::Wildcard => false,
            Pat::Literal(_) => true,
            Pat::List(_) => true, // List could be different size
            Pat::ListFront(items, _) => items.len() > 0,
            Pat::Tuple(items) => items.iter().any(|item| item.pat.is_refutable(data_ctx)),
            Pat::Record(fields) => fields.iter().any(|(_, field)| field.pat.is_refutable(data_ctx)),
            Pat::Deconstruct(data, _, inner) => data_ctx.get_data(data.0).variants.len() != 1
                || inner.pat.is_refutable(data_ctx),
        }
    }
}

fn arms_are_exhaustive<'a>(
    data_ctx: &data::DataCtx,
    ty: &Type,
    arms: impl Iterator<Item=&'a TypeBinding> + Clone,
) -> Result<(), Option<String>> {
    if arms.clone().any(|binding| !binding.pat.is_refutable(data_ctx)) {
        Ok(())
    } else {
        match ty {
            Type::List(_) => {
                let smallest_irrefutable = arms
                    .clone()
                    .fold(None, |a, binding| match &*binding.pat {
                        Pat::ListFront(items, _) if items
                            .iter()
                            .all(|item| !item.pat.is_refutable(data_ctx)) => Some(a
                                .unwrap_or(items.len())
                                .min(items.len())),
                        pat if !pat.is_refutable(data_ctx) => Some(0),
                        _ => a,
                    })
                    .ok_or_else(|| Some(format!("[...]")))?;
                let holes_patched = arms
                    .filter_map(|binding| match &*binding.pat {
                        Pat::List(items) if items
                            .iter()
                            .all(|item| !item.pat.is_refutable(data_ctx))
                            && items.len() < smallest_irrefutable => Some(items.len()),
                        _ => None,
                    })
                    .collect::<HashSet<_>>();
                if holes_patched.len() >= smallest_irrefutable {
                    Ok(())
                } else {
                    let case_n = (0..smallest_irrefutable)
                        .find(|i| !holes_patched.contains(&i))
                        .unwrap();
                    Err(Some(format!("[{}]", (0..case_n).map(|_| "_").collect::<Vec<_>>().join(", "))))
                }
            },
            Type::Primitive(Primitive::Boolean) => {
                let mut found = [false, false];
                arms
                    .for_each(|binding| match &*binding.pat {
                        Pat::Literal(Literal::Boolean(x)) => found[*x as usize] = true,
                        _ => {},
                    });
                if found == [true, true] {
                    Ok(())
                } else {
                    Err(Some(format!("{}", found[0])))
                }
            },
            Type::Data(data, _) => {
                let data = data_ctx.get_data(**data);
                let variants_matched = arms
                    .filter_map(|binding| match &*binding.pat {
                        Pat::Deconstruct(data, _, inner) if !inner.pat.is_refutable(data_ctx) =>
                            Some(data.1),
                        _ => None,
                    })
                    .collect::<HashSet<_>>();
                if variants_matched.len() == data.variants.len() {
                    Ok(())
                } else {
                    let missing = (0..data.variants.len())
                        .find(|v| !variants_matched.contains(&v))
                        .unwrap();
                    Err(Some(format!("{}", *data.variants[missing].0)))
                }
            },
            _ => Err(None),
        }
    }
}

#[derive(Copy, Clone, Debug)]
#[repr(u32)]
pub enum Intrinsic {
    Out,
    In,
}

#[derive(Debug)]
pub enum Expr<M> {
    Literal(Literal),
    Local(Ident),
    Intrinsic(Intrinsic, Vec<(SrcNode<Ident>, M)>, Vec<Node<Self, M>>),
    Global(Ident, Vec<(SrcNode<Ident>, M)>),
    Unary(SrcNode<ast::UnaryOp>, Node<Self, M>),
    Binary(SrcNode<ast::BinaryOp>, Node<Self, M>, Node<Self, M>),
    List(Vec<Node<Self, M>>),
    Tuple(Vec<Node<Self, M>>),
    Record(Vec<(SrcNode<Ident>, Node<Self, M>)>),
    Func(Node<Binding<M>, M>, Node<Self, M>),
    Apply(Node<Self, M>, Node<Self, M>), // TODO: Should application be a binary operator?
    Access(Node<Self, M>, SrcNode<Ident>),
    Update(Node<Self, M>, SrcNode<Ident>, Node<Self, M>),
    Match(Node<Self, M>, Vec<(Node<Binding<M>, M>, Node<Self, M>)>),
    Constructor(SrcNode<(DataId, usize)>, Vec<(SrcNode<Ident>, M)>, Node<Self, M>),
}

type InferExpr = InferNode<Expr<(Span, TypeId)>>;
pub type TypeExpr = TypeNode<Expr<(Span, SrcNode<Type>)>>;

#[derive(Clone)]
pub enum Scope<'a> {
    Root {
        module: &'a Module,
        current_def: Option<(Ident, TypeId, &'a [SrcNode<Ident>])>,
        globals: GlobalHints<'a>,
    },
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
            Scope::Root { .. } => None,
        }
    }

    fn get_def_type(
        &self,
        ident: Ident,
        infer: &mut InferCtx,
        span: Span,
    ) -> Result<Option<(TypeId, Vec<(SrcNode<Ident>, TypeId)>)>, Error> {
        match self {
            Scope::Local(_, _, parent) => parent.get_def_type(ident, infer, span),
            Scope::Many(_, parent) => parent.get_def_type(ident, infer, span),
            Scope::Root { module, current_def, globals } => current_def
                // Check current definition
                .as_ref()
                .filter(|(name, _, _)| *name == ident)
                .map(|(_, ty, generics)| Ok(Some((
                    *ty,
                    generics
                        .iter()
                        .map(|name| (name.clone(), infer.insert(TypeInfo::GenParam(**name), Span::none())))
                        .collect(),
                ))))
                // Check module
                .or_else(|| module
                    .get_def_type(ident, infer, span)
                    .map(|x| Ok(Some(x))))
                // Check uninitiated globals
                .unwrap_or_else(|| Ok(if let Some((global_span, generics, hint)) = globals.0.get(&ident) {
                    let generics = generics
                        .iter()
                        .map(|ident| (ident.clone(), infer.insert(TypeInfo::Unknown(Some(**ident)), span)))
                        .collect::<Vec<_>>();
                    Some((
                        hint.to_type_id(infer, &|gen| generics.iter().find(|(name, _)| **name == gen).map(|(_, ty)| *ty))?,
                        generics,
                    ))
                } else {
                    None
                })),
        }
    }

    fn get_intrinsic(
        &self,
        ident: Ident,
        infer: &mut InferCtx,
        span: Span,
    ) -> Result<Option<(TypeId, Vec<(SrcNode<Ident>, TypeId)>, Vec<TypeId>, Intrinsic)>, Error> {
        let universe = infer.insert(TypeInfo::Primitive(Primitive::Universe), span);

        match ident.as_str() {
            "out" => Ok(Some((
                universe,
                Vec::new(),
                vec![infer.insert(TypeInfo::Primitive(Primitive::Char), span), universe],
                Intrinsic::Out,
            ))),
            "in" => Ok(Some((
                {
                    let c = infer.insert(TypeInfo::Primitive(Primitive::Char), span);
                    infer.insert(TypeInfo::Tuple(vec![c, universe]), span)
                },
                Vec::new(),
                vec![universe],
                Intrinsic::In,
            ))),
            _ => Ok(None),
        }
    }
}

#[derive(Copy, Clone)]
pub struct GlobalHints<'a>(&'a HashMap<Ident, (Span, Vec<SrcNode<Ident>>, &'a SrcNode<ast::Type>)>);

#[derive(Debug)]
pub struct Def {
    pub generics: Vec<SrcNode<Ident>>,
    pub name: SrcNode<Ident>,
    pub body: TypeExpr,
}

pub struct Program {
    pub root: Module,
    pub data_ctx: data::DataCtx,
}

impl Program {
    pub fn new() -> Self {
        Self {
            root: Module::default(),
            data_ctx: data::DataCtx::default(),
        }
    }

    pub fn new_root(module: &SrcNode<ast::Module>) -> Result<Self, Vec<Error>> {
        let mut this = Self {
            root: Module::default(),
            data_ctx: data::DataCtx::from_ast_module(module)?,
        };
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
                ast::Decl::Def(def) => this.insert_def_inner(GlobalHints(&globals), def)
                    .map_err(|e| vec![e])?,
                _ => {},
            }
        }

        this.type_check()
            .map_err(|e| vec![e])?;

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
                        let mut infer = InferCtx::from_data_ctx(&self.data_ctx);
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
                    Expr::Func(param, _) if param.pat.is_refutable(&self.data_ctx) => Err(Error::custom(format!("Refutable pattern may not be used here"))
                        .with_span(param.pat.span())),
                    Expr::Match(pred, arms) => if let Err(missing_case) = arms_are_exhaustive(&self.data_ctx, pred.ty(), arms.iter().map(|(binding, _)| binding)) {
                        let hint = missing_case
                            .map(|case| format!("Case '{}' is not handled", case))
                            .unwrap_or_else(|| format!("Handle all possible cases"));
                        Err(Error::custom(format!("Match arms are not exhaustive"))
                            .with_span(expr.span())
                            .with_hint(hint))
                    } else {
                        Ok(())
                    },
                    _ => Ok(()),
                })?;
        }

        Ok(())
    }

    pub fn root(&self) -> &Module {
        &self.root
    }

    pub fn insert_def(&mut self, ast_def: &ast::Def) -> Result<(), Error> {
        self.insert_def_inner(GlobalHints(&HashMap::default()), ast_def)
    }

    fn insert_def_inner(&mut self, globals: GlobalHints, ast_def: &ast::Def) -> Result<(), Error> {
        // Check for double declaration
        if let Some(existing_def) = self.root.defs.get(&**ast_def.name) {
            return Err(Error::custom(format!("Definition with name '{}' already exists", **ast_def.name))
                .with_span(existing_def.name.span())
                .with_span(ast_def.name.span()));
        }

        let mut infer = InferCtx::from_data_ctx(&self.data_ctx);

        // Add the definition's generics to the infer context
        ast_def
            .generics
            .iter()
            .for_each(|name| infer.insert_generic(**name, name.span()));

        let generics = ast_def.generics
            .iter()
            .map(|g| SrcNode::new(**g, g.span()))
            .collect::<Vec<_>>();

        let current_def_ty = infer.insert(TypeInfo::Unknown(None), ast_def.body.span());

        let scope = Scope::Root {
            module: &self.root,
            current_def: Some((*ast_def.name, current_def_ty, &generics)),
            globals,
        };

        let body = {
            let mut body = ast_def.body.to_hir(&mut infer, &scope)?;

            infer.unify(current_def_ty, body.type_id())?;

            // Unify with optional type annotation
            let ty_id = ast_def.ty.to_type_id(&mut infer, &|_| None)?;

            infer.unify(body.type_id(), ty_id)?;

            infer.solve_all()?;
            body.into_checked(&mut infer)?
                // Use the def name as the type's span
                .map_meta(|(span, ty)| (span, ty.map_meta(|_| ast_def.name.span())))
        };

        // Ensure that all generic parameters are in use
        for gen in generics.iter() {
            let mut uses_gen = false;
            body.ty().visit(&mut |ty| {
                if **ty == Type::GenParam(**gen) {
                    uses_gen = true;
                }
            });
            if !uses_gen {
                return Err(Error::custom(format!("Type parameter '{}' must be mentioned by type '{}'", **gen, **body.ty()))
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
}

#[derive(Default)]
pub struct Module {
    defs: HashMap<Ident, Def>,
}

impl Module {
    pub fn def(&self, name: Ident) -> Option<&Def> {
        self.defs.get(&name)
    }

    fn get_def_type(&self, ident: Ident, infer: &mut InferCtx, span: Span) -> Option<(TypeId, Vec<(SrcNode<Ident>, TypeId)>)> {
        self.defs
            .get(&ident)
            .map(|def| infer.instantiate_ty(&def.generics, def.body.ty(), span))
    }
}

// AST to HIR conversions

impl ast::Binding {
    fn to_hir(self: &SrcNode<Self>, infer: &mut InferCtx) -> Result<InferBinding, Error> {
        // First, assume that a free identifier is a data constructor
        let (type_id, pat, binding) = if let ast::Binding::Ident(name) = &**self {
            let type_id = infer.insert(TypeInfo::Unknown(None), self.span());
            (type_id, SrcNode::new(Pat::Wildcard, self.span()), Some(SrcNode::new(*name, self.span())))
        } else {
            let (binding, pat, span) = match &**self {
                ast::Binding::Bound(binding, pat) => (Some(binding.clone()), &**pat, pat.span()),
                ast::Binding::Unbound(pat) => (None, pat, self.span()),
                ast::Binding::Ident(_) => unreachable!(),
            };

            let (type_id, pat) = match pat {
                ast::Pat::Wildcard => (infer.insert(TypeInfo::Unknown(None), self.span()), Pat::Wildcard),
                ast::Pat::Literal(litr) => {
                    let litr_type_info = litr.get_type_info(infer, self.span());
                    (infer.insert(litr_type_info, self.span()), Pat::Literal(litr.clone()))
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
                ast::Pat::Deconstruct(constructor, inner) => {
                    let inner = inner.to_hir(infer)?;

                    // Instantiate inner type with generics as free type terms
                    let (data_id, variant, type_id, params, inner_ty) = infer.data_ctx().get_constructor_type(**constructor, infer, self.span())?;

                    infer.unify(inner.type_id(), inner_ty)?;

                    (type_id, Pat::Deconstruct(
                        SrcNode::new((data_id, variant), constructor.span()),
                        params.into_iter().map(|(name, ty)| (name.clone(), (name.span(), ty))).collect(),
                        inner,
                    ))
                },
            };

            (type_id, SrcNode::new(pat, span), binding)
        };

        Ok(InferNode::new(Binding { pat, binding }, (self.span(), type_id)))
    }
}

impl ast::Type {
    fn to_type_id(self: &SrcNode<Self>, infer: &mut InferCtx, get_generic: &impl Fn(Ident) -> Option<TypeId>) -> Result<TypeId, Error> {
        let info = match &**self {
            ast::Type::Unknown => TypeInfo::Unknown(None),
            ast::Type::Data(name, params) => {
                // Substitute generic
                if let Some(ty_id) = get_generic(**name) {
                    if params.len() == 0 {
                        TypeInfo::Ref(ty_id)
                    } else {
                        return Err(Error::custom(format!("Generic types may not be parameterised"))
                            .with_span(self.span())
                            .with_span(params.iter().fold(Span::none(), |a, p| a.union(p.span()))));
                    }
                } else {
                    let params = params
                        .iter()
                        .map(|param| param.to_type_id(infer, get_generic))
                        .collect::<Result<Vec<_>, _>>()?;
                    TypeInfo::Ref(infer
                        .data_ctx()
                        .get_named_type(name, &params, infer, self.span())?)
                }
            },
            ast::Type::List(ty) => TypeInfo::List(ty.to_type_id(infer, get_generic)?),
            ast::Type::Tuple(items) => TypeInfo::Tuple(items
                .iter()
                .map(|item| item.to_type_id(infer, get_generic))
                .collect::<Result<_, _>>()?),
            ast::Type::Record(fields) => TypeInfo::Record(fields
                .iter()
                .map(|(name, field)| Ok((name.clone(), field.to_type_id(infer, get_generic)?)))
                .collect::<Result<_, _>>()?),
            ast::Type::Func(i, o) => TypeInfo::Func(
                i.to_type_id(infer, get_generic)?,
                o.to_type_id(infer, get_generic)?,
            ),
        };

        Ok(infer.insert(info, self.span()))
    }
}

impl ast::Expr {
    fn to_hir(self: &SrcNode<Self>, infer: &mut InferCtx, scope: &Scope) -> Result<InferExpr, Error> {
        let (type_id, hir_expr) = match &**self {
            ast::Expr::Literal(litr) => {
                let litr_ty_info = litr.get_type_info(infer, self.span());
                (infer.insert(litr_ty_info, self.span()), Expr::Literal(litr.clone()))
            },
            ast::Expr::Path(path) => if path.len() == 1 {
                if let Some(local_id) = scope
                    .get_local(path.base(), infer, self.span())
                {
                    let type_id = infer.insert(TypeInfo::Unknown(None), self.span());
                    infer.unify(type_id, local_id)?;
                    (type_id, Expr::Local(path.base()))
                } else if let Some((global_id, generics)) = scope.get_def_type(path.base(), infer, self.span())? {
                    let generics = generics
                        .into_iter()
                        .map(|(ident, ty)| (ident.clone(), (ident.span(), ty)))
                        .collect();
                    let type_id = infer.insert(TypeInfo::Unknown(None), self.span());
                    infer.unify(type_id, global_id)?;
                    (type_id, Expr::Global(path.base(), generics))
                } else {
                    return Err(Error::custom(format!("No such binding '{}' in scope", path.base().to_string()))
                        .with_span(self.span()));
                }
            } else {
                todo!("Complex paths")
            },
            ast::Expr::Intrinsic(name, args) => {
                if let Some((type_id, generics, arg_tys, intrinsic)) = scope.get_intrinsic(*name, infer, self.span())? {
                    if arg_tys.len() != args.len() {
                        return Err(Error::custom(format!("Wrong number of intrinsic arguments: expected {}, found {}", arg_tys.len(), args.len()))
                            .with_span(self.span()));
                    }
                    let args = args
                        .iter()
                        .zip(arg_tys.iter())
                        .map(|(expr, ty)| {
                            let expr = expr.to_hir(infer, scope)?;
                            infer.unify(expr.type_id(), *ty)?;
                            Ok(expr)
                        })
                        .collect::<Result<_, _>>()?;
                    let generics = generics
                        .into_iter()
                        .map(|(ident, ty)| (ident.clone(), (ident.span(), ty)))
                        .collect();
                    (type_id, Expr::Intrinsic(intrinsic, generics, args))
                } else {
                    return Err(Error::custom(format!("No such intrinsic value '{}'", &*name))
                        .with_span(self.span()));
                }
            }
            ast::Expr::Unary(op, a) => {
                let a = a.to_hir(infer, scope)?;
                let type_id = infer.insert(TypeInfo::Unknown(None), self.span());
                infer.add_constraint(Constraint::Unary {
                    out: type_id,
                    op: op.clone(),
                    a: a.type_id(),
                });
                (type_id, Expr::Unary(op.clone(), a))
            },
            ast::Expr::Binary(op, a, b) => {
                let a = a.to_hir(infer, scope)?;
                let b = b.to_hir(infer, scope)?;
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
                let items = items.iter().map(|item| item.to_hir(infer, scope)).collect::<Result<Vec<_>, _>>()?;
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
                    .map(|item| item.to_hir(infer, scope))
                    .collect::<Result<Vec<_>, _>>()?;
                let type_id = infer.insert(TypeInfo::Tuple(items.iter().map(|item| item.type_id()).collect()), self.span());
                (type_id, Expr::Tuple(items))
            },
            ast::Expr::Record(fields) => {
                let fields = fields
                    .iter()
                    .map(|(name, value)| Ok((name.clone(), value.to_hir(infer, scope)?)))
                    .collect::<Result<Vec<_>, _>>()?;
                let type_id = infer.insert(TypeInfo::Record(fields.iter().map(|(name, value)| (name.clone(), value.type_id())).collect()), self.span());
                (type_id, Expr::Record(fields))
            },
            ast::Expr::Func(param, param_ty, body) => {
                let param = param.to_hir(infer)?;

                // Unify param_ty with type hint
                if let Some(param_ty) = param_ty {
                    let param_ty_id = param_ty.to_type_id(infer, &|_| None)?;
                    infer.unify(param.type_id(), param_ty_id)?;
                }

                let body_scope = scope.with_many(param
                    .binding_idents()
                    .into_iter()
                    .map(|(ident, (_, ty))| (ident, *ty))
                    .collect());
                let body = body.to_hir(infer, &body_scope)?;

                let type_id = infer.insert(TypeInfo::Func(param.type_id(), body.type_id()), self.span());
                (type_id, Expr::Func(param, body))
            },
            ast::Expr::Apply(f, arg) => {
                let f = f.to_hir(infer, scope)?;
                let arg = arg.to_hir(infer, scope)?;
                let type_id = infer.insert(TypeInfo::Unknown(None), self.span());
                let f_type_id = infer.insert(TypeInfo::Func(arg.type_id(), type_id), f.span());
                infer.unify(f_type_id, f.type_id())?;
                (type_id, Expr::Apply(f, arg))
            },
            ast::Expr::Access(record, field) => {
                let record = record.to_hir(infer, scope)?;
                let type_id = infer.insert(TypeInfo::Unknown(None), self.span());
                infer.add_constraint(Constraint::Access {
                    out: type_id,
                    record: record.type_id(),
                    field: field.clone(),
                });
                (type_id, Expr::Access(record, field.clone()))
            },
            ast::Expr::Update(record, field, value) => {
                let record = record.to_hir(infer, scope)?;

                let field_ty = infer.insert(TypeInfo::Unknown(None), field.span());
                let value_scope = scope.with(**field, field_ty);
                let value = value.to_hir(infer, &value_scope)?;
                infer.unify(field_ty, value.type_id())?;

                infer.add_constraint(Constraint::Access {
                    out: value.type_id(),
                    record: record.type_id(),
                    field: field.clone(),
                });

                (record.type_id(), Expr::Update(record, field.clone(), value))
            },
            ast::Expr::Let(pat, pat_ty, val, then) => {
                // `let a = b in c` desugars to `b:(a -> c)`
                // TODO: Desugar to `match b in a => c` instead?
                let pat = pat.to_hir(infer)?;

                // Unify pattern with type hint
                if let Some(pat_ty) = pat_ty {
                    let pat_ty_id = pat_ty.to_type_id(infer, &|_| None)?;
                    infer.unify(pat.type_id(), pat_ty_id)?;
                }

                let val = val.to_hir(infer, scope)?;
                infer.unify(pat.type_id(), val.type_id())?;

                let then_scope = scope.with_many(pat
                    .binding_idents()
                    .into_iter()
                    .map(|(ident, (_, ty))| (ident, *ty))
                    .collect());
                let then_body = then.to_hir(infer, &then_scope)?;

                (then_body.type_id(), Expr::Match(val, vec![
                    (pat, then_body)
                ]))
            },
            ast::Expr::If(pred, a, b) => {
                let pred_hir = pred.to_hir(infer, scope)?;
                let pred_type_id = infer.insert(TypeInfo::Primitive(Primitive::Boolean), pred.span());
                infer.unify(pred_hir.type_id(), pred_type_id)?;

                let a = a.to_hir(infer, scope)?;
                let b = b.to_hir(infer, scope)?;
                infer.unify(a.type_id(), b.type_id())?;

                (a.type_id(), Expr::Match(pred_hir, vec![
                    (InferNode::new(Binding {
                        pat: SrcNode::new(Pat::Literal(Literal::Boolean(true)), Span::none()),
                        binding: None,
                    }, (pred.span(), pred_type_id)), a),
                    (InferNode::new(Binding {
                        pat: SrcNode::new(Pat::Literal(Literal::Boolean(false)), Span::none()),
                        binding: None,
                    }, (pred.span(), pred_type_id)), b),
                ]))
            },
            ast::Expr::Match(pred, arms) => {
                let pred = pred.to_hir(infer, scope)?;

                let match_type_id = infer.insert(TypeInfo::Unknown(None), self.span());

                let arms = arms
                    .iter()
                    .map(|((pat, pat_ty), body)| {
                        let pat = pat.to_hir(infer)?;

                        // Unify pattern with type hint
                        if let Some(pat_ty) = pat_ty {
                            let pat_ty_id = pat_ty.to_type_id(infer, &|_| None)?;
                            infer.unify(pat.type_id(), pat_ty_id)?;
                        }

                        infer.unify(pat.type_id(), pred.type_id())?;

                        let body_scope = scope.with_many(pat
                            .binding_idents()
                            .into_iter()
                            .map(|(ident, (_, ty))| (ident, *ty))
                            .collect());
                        let body = body.to_hir(infer, &body_scope)?;

                        infer.unify(match_type_id, body.type_id())?;

                        Ok((pat, body))
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                (match_type_id, Expr::Match(pred, arms))
            },
            ast::Expr::Constructor(constructor, inner) => {
                let inner = inner.to_hir(infer, scope)?;

                // Instantiate inner type with generics as free type terms
                let (data_id, variant, type_id, params, inner_ty) = infer.data_ctx().get_constructor_type(**constructor, infer, self.span())?;

                infer.unify(inner.type_id(), inner_ty)?;

                (type_id, Expr::Constructor(
                    SrcNode::new((data_id, variant), constructor.span()),
                    params.into_iter().map(|(name, ty)| (name.clone(), (name.span(), ty))).collect(),
                    inner,
                ))
            },
            // ast::Expr::Do(stmts) => {
            //     fn stmts_to_hir<'a>(mut stmts: impl Iterator<Item=&'a ast::DoStatement>, infer: &mut InferCtx, scope: &Scope) -> Result<Option<InferExpr>, Error> {
            //         match stmts.next() {
            //             None => Ok(None),
            //             Some(ast::DoStatement::Exec(expr)) => {
            //                 let expr = expr.to_hir(infer, scope)?;
            //                 if let Some(prev_stmts) = stmts_to_hir(stmts, infer, scope)? {
            //                     Ok(Some(Expr::Apply(Expr::Apply("next", expr), prev_stmts)))
            //                 } else {
            //                     Ok(Some(expr))
            //                 }
            //             },
            //             _ => todo!(),
            //         }
            //     }

            //     let expr = stmts_to_hir(stmts.iter().rev().map(|x| &**x), infer, scope)?.unwrap();
            //     (expr.type_id(), expr.into_inner())
            // },
        };

        Ok(InferNode::new(hir_expr, (self.span(), type_id)))
    }
}

impl InferPat {
    fn into_checked(self, infer: &InferCtx) -> Result<TypePat, Error> {
        let span = self.span();

        let pat = match self.into_inner() {
            Pat::Wildcard => Pat::Wildcard,
            Pat::Literal(litr) => Pat::Literal(litr),
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
            Pat::Record(fields) => Pat::Record(fields
                .into_iter()
                .map(|(name, field)| Ok((name, field.into_checked(infer)?)))
                .collect::<Result<_, _>>()?),
            Pat::Deconstruct(data, generics, inner) => Pat::Deconstruct(
                data,
                generics
                    .into_iter()
                    .map(|(ident, (span, type_id))| Ok((ident, (span, infer.reconstruct(type_id, span)?))))
                    .collect::<Result<_, _>>()?,
                inner.into_checked(infer)?,
            ),
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
        let type_id = self.type_id();
        Ok(TypeNode::new(match self.into_inner() {
            Expr::Literal(litr) => Expr::Literal(litr),
            Expr::Local(ident) => Expr::Local(ident),
            Expr::Intrinsic(intrinsic, generics, args) => Expr::Intrinsic(
                intrinsic,
                generics
                    .into_iter()
                    .map(|(ident, (span, type_id))| Ok((ident, (span, infer.reconstruct(type_id, span)?))))
                    .collect::<Result<_, _>>()?,
                args
                    .into_iter()
                    .map(|x| x.into_checked(infer))
                    .collect::<Result<_, _>>()?,
            ),
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
            Expr::Access(record, field) => Expr::Access(
                record.into_checked(infer)?,
                field,
            ),
            Expr::Update(record, field, value) => Expr::Update(
                record.into_checked(infer)?,
                field,
                value.into_checked(infer)?,
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
            Expr::Constructor(data, generics, inner) => Expr::Constructor (
                data,
                generics
                    .into_iter()
                    .map(|(ident, (span, type_id))| Ok((ident, (span, infer.reconstruct(type_id, span)?))))
                    .collect::<Result<_, _>>()?,
                inner.into_checked(infer)?,
            )
        }, (span, infer.reconstruct(type_id, span)?)))
    }
}

impl TypeExpr {
    fn visit(&self) -> impl Iterator<Item=&Self> + '_ {
        let mut stack = vec![self];
        std::iter::from_fn(move || stack
            .pop()
            .map(|expr| {
                match &**expr {
                    Expr::Literal(_) => {},
                    Expr::Local(_) => {},
                    Expr::Global(_, _) => {},
                    Expr::Intrinsic(_, _, args) => args
                        .iter()
                        .for_each(|x| stack.push(x)),
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
                    Expr::Access(record, _) => stack.push(record),
                    Expr::Update(record, _, value) => {
                        stack.push(record);
                        stack.push(value);
                    },
                    Expr::Match(pred, arms) => {
                        stack.push(pred);
                        for (_, arm) in arms.iter() {
                            stack.push(arm);
                        }
                    },
                    Expr::Constructor(_, _, inner) => stack.push(inner),
                }

                expr
            }))
    }
}
