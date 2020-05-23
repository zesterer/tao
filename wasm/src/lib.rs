use tao::run_module;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn run(src: &str) -> Result<String, JsValue> {
    match run_module(src) {
        Ok(val) => Ok(val.to_string()),
        Err(errs) => Err(JsValue::from_serde(
            &errs
                .iter()
                .map(|err| err.in_source(&src).to_string())
                .collect::<Vec<_>>(),
        )
        .unwrap()),
    }
}
