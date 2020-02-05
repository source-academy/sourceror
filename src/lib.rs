extern crate wasm_bindgen;

use wasm_bindgen::prelude::*;

mod wasmgen;

#[wasm_bindgen]
extern {
    pub fn alert(s: &str);
}

#[wasm_bindgen]
pub fn greet(name: &str) {
    alert(&format!("Hello, {}!", name));
}
