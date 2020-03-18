use wasm_bindgen::prelude::*;

use backend_wasm;
use wasmgen::*;

use wasm_test_harness::*;

/*
Returns a valid wasm binary that should be executed by invoking "main" function.
*/
#[wasm_bindgen]
pub fn build_tests() -> Box<[u8]> {
    let mut ctx = NormalContext::new();
    backend_wasm::wasmtest(&mut ctx);
    let module = ctx.build();
    let mut receiver = std::vec::Vec::<u8>::new();
    module.wasm_serialize(&mut receiver);
    receiver.into_boxed_slice()
}
