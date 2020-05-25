use serde::Serialize;
use tao::{error::ErrorInSrc, run_module};
use wasm_bindgen::prelude::*;

#[derive(Serialize)]
pub struct Error<'a> {
    pub msg: String,
    pub src: ErrorInSrc<'a>,
}

#[wasm_bindgen]
pub fn run(src: &str) -> Result<String, JsValue> {
    match run_module(src) {
        Ok(val) => Ok(val.to_string()),
        Err(errs) => Err(JsValue::from_serde(
            &errs
                .iter()
                .map(|err| Error {
                    msg: err.in_source(&src).to_string(),
                    src: err.in_source(&src),
                })
                .collect::<Vec<_>>(),
        )
        .unwrap()),
    }
}
