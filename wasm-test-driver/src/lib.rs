use wasm_bindgen::prelude::*;

use backend_wasm;
use wasmgen::*;

use wasm_test_harness::*;

#[wasm_bindgen(js_namespace = test, js_name = add_test)]
extern "C" {
    fn add_test(wasm_binary: Box<[u8]>);
}

/*
Returns a valid wasm binary that should be executed by invoking "main" function.
*/
#[wasm_bindgen]
pub fn build_tests() {
    let mut ctx = NormalContext::new(|binary| add_test(binary));
    backend_wasm::wasmtest(&mut ctx);
}
