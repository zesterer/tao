use std::collections::HashMap;
use internment::LocalIntern;
use crate::{
    Error,
    ty::{Type, Primitive},
    src::Span,
    node::SrcNode,
    ast,
};
use super::infer::{InferCtx, TypeId, TypeInfo};

type Ident = LocalIntern<String>;

pub type DataId = usize;

#[derive(Debug)]
pub struct Data {
    span: Span,
    pub generics: Vec<SrcNode<Ident>>,
    pub variants: Vec<(SrcNode<Ident>, SrcNode<Type>)>,
}

#[derive(Debug)]
pub struct TypeAlias {
    span: Span,
    generics: Vec<SrcNode<Ident>>,
    ty: SrcNode<Type>,
}

#[derive(Default, Debug)]
pub struct DataCtx {
    id_counter: usize,

    // TODO: Combine these. Bimap?
    data_names: HashMap<Ident, DataId>,
    data_names_rev: HashMap<DataId, Ident>,

    data: HashMap<DataId, Data>,
    constructors: HashMap<Ident, (DataId, usize)>,
    type_aliases: HashMap<Ident, TypeAlias>,
}

impl DataCtx {
    pub fn insert_data(&mut self, name: Ident, data: Data) -> Result<DataId, Error> {
        self.id_counter += 1;
        let id = self.id_counter;

        if let Some(other_span) = self.data_names
            .get(&name)
            .map(|data| self.data[data].span)
            .or_else(|| self.type_aliases
                .get(&name)
                .map(|alias| alias.span))
        {
            Err(Error::custom(format!("Conflict between types with the same name"))
                .with_span(data.span)
                .with_secondary_span(other_span))
        } else {
            self.data_names.insert(name, id);
            self.data_names_rev.insert(id, name);

            self.data.insert(id, data);

            Ok(id)
        }
    }

    pub fn from_ast_module(module: &SrcNode<ast::Module>) -> Result<Self, Vec<Error>> {
        let mut this = Self::default();

        // Do a first pass through the data types to build up their basic signatures
        module.decls
            .iter()
            .filter_map(|decl| if let ast::Decl::Data(data) = &**decl { Some(data) } else { None })
            .try_for_each(|data| {
                this.insert_data(*data.name, Data {
                    span: data.name.span(),
                    generics: data.generics.clone(),
                    variants: Vec::new(),
                })
                    .map(|_| ())
                    .map_err(|e| vec![e])
            })?;

        // Do a pass to discover type aliases
        module.decls
            .iter()
            .filter_map(|decl| if let ast::Decl::TypeAlias(alias) = &**decl { Some(alias) } else { None })
            .try_for_each(|alias| {
                let mut infer = InferCtx::from_data_ctx(&this);
                // Add this type alias' generic parameters to the infer context
                alias
                    .generics
                    .iter()
                    .for_each(|name| infer.insert_generic(**name, name.span()));
                // Turn the type alias into a concrete type
                let type_id = alias.ty.to_type_id(&mut infer, &|_| None)?;
                let ty = infer.reconstruct(type_id, alias.name.span())?;
                this.type_aliases.insert(*alias.name, TypeAlias {
                    span: alias.name.span(),
                    generics: alias.generics.clone(),
                    ty,
                });
                Ok(())
            })
            .map_err(|e| vec![e])?;

        // Do a second pass to properly collect datatype information
        module.decls
            .iter()
            .filter_map(|decl| if let ast::Decl::Data(data) = &**decl { Some(data) } else { None })
            .try_for_each(|data| {
                let id = this.data_names[&*data.name];
                let variants = match &*data.data_ty {
                    ast::DataType::Sum(variants) => variants
                        .iter()
                        .enumerate()
                        .map(|(variant, (name, ty))| {
                            this.constructors.insert(**name, (id, variant));
                            Ok((name.clone(), ty
                                .as_ref()
                                .map(|ty| {
                                    let mut infer = InferCtx::from_data_ctx(&this);
                                    // Add this datatype' generic parameters to the infer context
                                    data
                                        .generics
                                        .iter()
                                        .for_each(|name| infer.insert_generic(**name, name.span()));
                                    let type_id = ty.to_type_id(&mut infer, &|_| None)?;
                                    infer.reconstruct(type_id, data.name.span())
                                })
                                .transpose()?
                                .unwrap_or_else(|| SrcNode::new(Type::Tuple(Vec::new()), Span::none()))))
                        })
                        .collect::<Result<_, Error>>()?,
                    ast::DataType::Product(ty) => {
                        this.constructors.insert(*data.name, (id, 0));
                        let mut infer = InferCtx::from_data_ctx(&this);
                        // Add this datatype' generic parameters to the infer context
                        data
                            .generics
                            .iter()
                            .for_each(|name| infer.insert_generic(**name, name.span()));
                        let type_id = ty.to_type_id(&mut infer, &|_| None)?;
                        vec![(data.name.clone(), infer.reconstruct(type_id, data.name.span())?)]
                    },
                };
                this.data.get_mut(&id).unwrap().variants = variants;

                Ok(())
            })
            .map_err(|e| vec![e])?;

        Ok(this)
    }

    pub fn get_data_name(&self, id: DataId) -> Ident {
        *self.data_names_rev.get(&id).unwrap()
    }

    pub fn get_data(&self, id: DataId) -> &Data {
        self.data.get(&id).unwrap()
    }

    pub fn get_data_id(&self, constructor: Ident, span: Span) -> Result<(DataId, usize), Error> {
        self.constructors
            .get(&constructor)
            .copied()
            .ok_or_else(|| Error::custom(format!("No data type with constructor '{}' exists", constructor))
                .with_span(span))
    }

    pub fn get_constructor_type(
        &self,
        constructor: Ident,
        infer: &mut InferCtx,
        span: Span,
    ) -> Result<(DataId, usize, TypeId, Vec<(SrcNode<Ident>, TypeId)>, TypeId), Error> {
        let (data_id, variant) = self.get_data_id(constructor, span)?;

        let data = self.get_data(data_id);

        // let generics = data.generics
        //     .iter()
        //     .map(|ident| (ident.clone(), infer.insert(TypeInfo::Unknown(Some(Type::GenParam(**ident))), span)))
        //     .collect::<Vec<_>>();

        let (inner_ty, params) = infer.instantiate_ty(&data.generics, &data.variants[variant].1, span);

        Ok((
            data_id,
            variant,
            infer.insert(TypeInfo::Data(data_id, params.iter().map(|(_, ty)| *ty).collect()), span),
            params,
            inner_ty,
        ))
    }

    pub fn get_named_type(
        &self,
        name: &SrcNode<Ident>,
        params: &[TypeId],
        infer: &mut InferCtx,
        span: Span,
    ) -> Result<TypeId, Error> {
        if let Some(ty_id) = infer.generic(**name) {
            Ok(ty_id)
        } else if let Some(ty_info) = match name.as_str() {
            "Num" => Some(TypeInfo::Primitive(Primitive::Number)),
            "Bool" => Some(TypeInfo::Primitive(Primitive::Boolean)),
            "Char" => Some(TypeInfo::Primitive(Primitive::Char)),
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
            if let Some(res) = self.type_aliases // Search type aliases
                .get(&**name)
                .map(|alias| if params.len() != alias.generics.len() {
                    Err(Error::custom(format!("Type '{}' expected {} parameters, found {}", **name, alias.generics.len(), params.len()))
                        .with_span(span)
                        .with_secondary_span(alias.ty.span()))
                } else {
                    Ok(infer.instantiate_ty_inner(&|name| alias.generics
                        .iter()
                        .zip(params.iter())
                        .find(|(gen, _)| ***gen == name)
                        .map(|(_, param_ty)| *param_ty), &alias.ty))
                })
            {
                res
            } else if let Some(res) = self.data_names // Search data types
                .get(&**name)
                .map(|data_id| (*data_id, self.get_data(*data_id)))
                .map(|(data_id, data)| if params.len() != data.generics.len() {
                    Err(Error::custom(format!("Data '{}' expected {} parameters, found {}", **name, data.generics.len(), params.len()))
                        .with_span(span)
                        .with_secondary_span(data.span))
                } else {
                    Ok(infer.insert(TypeInfo::Data(data_id, params.to_vec()), span))
                })
            {
                res
            } else {
                Err(Error::custom(format!("No such type '{}'", **name))
                    .with_span(name.span()))
            }
        }
    }
}
