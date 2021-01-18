use super::{ty::Ty, Ident, Item, TyGenerics};
use crate::util::SrcNode;
use bimap::BiMap;
use std::collections::HashMap;

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct DataId(usize);

pub struct Data {
    pub generics: TyGenerics,
    pub variants: HashMap<Ident, (SrcNode<()>, SrcNode<Ty>)>,
}

pub struct DataCtx {
    data: HashMap<DataId, Data>,
    names: BiMap<DataId, Item>,
}
