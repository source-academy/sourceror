use wasm_bindgen::prelude::*;
use web_sys::console;
use js_sys;
use source_compiler;


// When the `wee_alloc` feature is enabled, this uses `wee_alloc` as the global
// allocator.
//
// If you don't want to use `wee_alloc`, you can safely delete this.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;


// This is like the `main` function, except for JavaScript.
#[wasm_bindgen(start)]
pub fn main_js() -> Result<(), JsValue> {
    // This provides better error messages in debug mode.
    // It's disabled in release mode so it doesn't bloat up the file size.
    #[cfg(debug_assertions)]
    console_error_panic_hook::set_once();


    // Your code goes here!
    // const SOURCE_CODE: &'static str = r#"
    // { "type": "Program", "start": 0, "end": 65, "body": [ { "type": "FunctionDeclaration", "start": 0, "end": 52, "id": { "type": "Identifier", "start": 9, "end": 11, "name": "ok" }, "expression": false, "generator": false, "async": false, "params": [ { "type": "Identifier", "start": 12, "end": 13, "name": "n" } ], "body": { "type": "BlockStatement", "start": 15, "end": 52, "body": [ { "type": "ReturnStatement", "start": 19, "end": 50, "argument": { "type": "ConditionalExpression", "start": 26, "end": 49, "test": { "type": "BinaryExpression", "start": 26, "end": 33, "left": { "type": "Identifier", "start": 26, "end": 27, "name": "n" }, "operator": "===", "right": { "type": "Literal", "start": 32, "end": 33, "value": 0, "raw": "0" } }, "consequent": { "type": "Literal", "start": 36, "end": 37, "value": 0, "raw": "0" }, "alternate": { "type": "CallExpression", "start": 40, "end": 49, "callee": { "type": "Identifier", "start": 40, "end": 42, "name": "ok" }, "arguments": [ { "type": "BinaryExpression", "start": 43, "end": 48, "left": { "type": "Identifier", "start": 43, "end": 44, "name": "n" }, "operator": "-", "right": { "type": "Literal", "start": 47, "end": 48, "value": 1, "raw": "1" } } ], "optional": false } } } ] } }, { "type": "ExpressionStatement", "start": 54, "end": 64, "expression": { "type": "CallExpression", "start": 54, "end": 63, "callee": { "type": "Identifier", "start": 54, "end": 56, "name": "ok" }, "arguments": [ { "type": "Literal", "start": 57, "end": 62, "value": 50000, "raw": "50000" } ], "optional": false } } ], "sourceType": "script" }
    // "#;
    // let result = compile(1, SOURCE_CODE);
    console::log_2(&JsValue::from_str("%cTESTING"), &JsValue::from_str("color: green"));
    Ok(())
}

#[wasm_bindgen]
pub async fn e2e_compile(context: i32, source_code: String) -> js_sys::Uint8Array {
    return source_compiler::compile(context, source_code).await
}
