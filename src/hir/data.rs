use super::{ty::Ty, Ident, Item, TyGenerics};
use crate::util::SrcNode;
use bimap::BiMap;
use std::collections::HashMap;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct DataId(usize);

#[derive(Debug)]
pub struct Data {
    pub generics: TyGenerics,
    pub variants: HashMap<Ident, (SrcNode<()>, SrcNode<Ty>)>,
}

#[derive(Debug, Default)]
pub struct DataCtx {
    data: HashMap<DataId, Data>,
    names: BiMap<DataId, Item>,
}
