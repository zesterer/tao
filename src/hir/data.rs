use std::collections::HashMap;
use internment::LocalIntern;
use crate::ty::Type;

type Ident = LocalIntern<String>;

pub type DataId = usize;

pub struct DataType {
    variants: Vec<Type>,
}

pub struct Data {
    id_counter: usize,
    data_types: HashMap<DataId, DataType>,
}
