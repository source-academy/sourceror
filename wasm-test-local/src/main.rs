use backend_wasm;
use wasmgen::*;

use wasm_test_harness::*;

pub fn main() {
    std::io::stdin().read_line(&mut String::new()).unwrap(); // wait so I can attach debugger to process
    let mut ctx = NormalContext::new();
    backend_wasm::wasmtest(&mut ctx);
    let module = ctx.build();
    let mut receiver = std::vec::Vec::<u8>::new();
    module.wasm_serialize(&mut receiver);
}
