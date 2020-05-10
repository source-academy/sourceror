use wasm_bindgen::prelude::*;
use wasm_bindgen_futures::JsFuture;

use wasmgen;

use backend_wasm;

use projstd;

// #[wasm_bindgen]
// extern {
//     pub fn alert(s: &str);
// }

// #[wasm_bindgen]
// pub fn greet(name: &str) {
//     alert(&format!("Hello, {}!", name));
// }

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_name = sourcerorLogCallback)]
    pub fn compiler_log(context: i32, message: String);

    #[wasm_bindgen(js_name = sourcerorFetchDepCallback)]
    pub fn fetch_dep(name: &str) -> JsFuture;
}

#[derive(Copy, Clone)]
pub struct MainLogger {
    context: i32,
}

impl MainLogger {
    fn new(context: i32) -> Self {
        Self { context: context }
    }
}

impl projstd::log::Logger for MainLogger {
    fn log(&self, message: String) {
        compiler_log(self.context, message);
    }
}

/**
 * The entry function for compilation.
 * `context` is an opaque value so that the host code can associate our calls to compiler_log() with the correct call to compile().
 * `source_code`: ESTree JSON representation of validated program
 * `import_spec`: list of imports following the import file format
 */
#[wasm_bindgen]
pub async fn compile(context: i32, source_code: &str) -> Box<[u8]> {
    // nice console errors in debug mode
    #[cfg(all(debug_assertions, target_arch = "wasm32"))]
    console_error_panic_hook::set_once();

    (|| {
        use wasmgen::WasmSerialize;

        //let ir_imports = frontend_estree::parse_imports(import_spec, MainLogger::new(context))?;
        let ir_program =
            frontend_estree::run_frontend(source_code, fetch_dep, MainLogger::new(context)).await?;
        let wasm_module = backend_wasm::run_backend(&ir_program, backend_wasm::Options::default());
        let mut receiver = std::vec::Vec::<u8>::new();
        wasm_module.wasm_serialize(&mut receiver);
        Ok(receiver.into_boxed_slice())
    })()
    .unwrap_or_else(|_: ()| Box::new([]))

    // for now we just generate a dummy function that returns 42
    /*use wasmgen::*;
    let mut module = WasmModule::new_builder().build();
    let functype = FuncType::new(Box::new([]), Box::new([ValType::I32]));
    let (_type_idx, func_idx) = module.register_func(&functype);
    let mut code_builder = CodeBuilder::new(functype);
    {
        let (_locals_builder, expr_builder) = code_builder.split();
        expr_builder.i32_const(42); // put 42 onto the stack
        expr_builder.end(); // return
    }
    module.commit_func(func_idx, code_builder);
    module.export_func(func_idx, "main".to_string());
    let mut receiver = std::vec::Vec::<u8>::new();
    module.wasm_serialize(&mut receiver);
    receiver.into_boxed_slice()*/
}

#[cfg(test)]
mod tests {
    #[test]
    fn gen() -> std::io::Result<()> {
        use crate::wasmgen::*;
        use std::fs::File;
        use std::io::prelude::*;
        let mut module = WasmModule::new_builder().build();
        let functype = FuncType::new(Box::new([ValType::I32]), Box::new([ValType::I32]));
        let (_type_idx, func_idx) = module.register_func(&functype);
        let mut code_builder = CodeBuilder::new(functype);
        {
            let (locals_builder, expr_builder) = code_builder.split();
            expr_builder.local_get(locals_builder.param(0)); // put the 0th parameter onto the stack
            expr_builder.end(); // return
        }
        module.commit_func(func_idx, code_builder);
        module.export_func(func_idx, "test".to_string());
        let mut file = File::create("test.wasm")?;
        let mut receiver = std::vec::Vec::<u8>::new();
        module.wasm_serialize(&mut receiver);
        file.write_all(receiver.as_slice())?;
        Ok(())
    }
}
