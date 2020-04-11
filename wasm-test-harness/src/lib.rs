use wasmgen::*;

use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    // Use `js_namespace` here to bind `console.log(..)` instead of just
    // `log(..)`
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);

    // The `console.log` is quite polymorphic, so we can bind it with multiple
    // signatures. Note that we need to use `js_name` to ensure we always call
    // `log` in JS.
    #[wasm_bindgen(js_namespace = console, js_name = log)]
    fn log_u32(a: u32);

    // Multiple arguments too!
    #[wasm_bindgen(js_namespace = console, js_name = log)]
    fn log_many(a: &str, b: &str);
}

pub trait TestContext {
    fn add_test<F: FnOnce(&mut CodeBuilder, &mut WasmModule, wasmgen::FuncIdx, &NormalTester)>(
        &mut self,
        name_: &str,
        f: F,
    );
}

pub trait Tester {
    // net wasm stack: [i32, i32] -> []
    fn i32_assert_eq(&self, scratch: &mut Scratch, expr_builder: &mut ExprBuilder);
}

pub struct NormalContext<A: Fn(Box<[u8]>) -> ()> {
    add_to_js: A,
}

pub struct NormalTester {
    assert_failed_i32_func: FuncIdx,
    globalidx_assert_count: GlobalIdx,
}

impl Tester for NormalTester {
    // net wasm stack: [i32, i32] -> []
    fn i32_assert_eq(&self, scratch: &mut Scratch, expr_builder: &mut ExprBuilder) {
        let localidx_1 = scratch.push_i32();
        let localidx_2 = scratch.push_i32();
        expr_builder.local_set(localidx_2);
        expr_builder.local_tee(localidx_1);
        expr_builder.local_get(localidx_2);
        expr_builder.i32_ne();
        expr_builder.if_(&[]);
        expr_builder.local_get(localidx_1);
        expr_builder.local_get(localidx_2);
        expr_builder.global_get(self.globalidx_assert_count);
        expr_builder.call(self.assert_failed_i32_func);
        expr_builder.end();
        expr_builder.global_get(self.globalidx_assert_count);
        expr_builder.i32_const(1);
        expr_builder.i32_add();
        expr_builder.global_set(self.globalidx_assert_count);
        scratch.pop_i32();
        scratch.pop_i32();
    }
}

impl<A: Fn(Box<[u8]>) -> ()> NormalContext<A> {
    pub fn new(add_to_js: A) -> NormalContext<A> {
        NormalContext {
            add_to_js: add_to_js,
        }
    }
}

impl<A: Fn(Box<[u8]>) -> ()> TestContext for NormalContext<A> {
    /*
    Adds a test.
    `f` should emit code that has net wasm stack [] -> [], even though the CodeBuilder might have a different signature.  This is because the test harness might return different bookkeeping information.
    It should not emit the end() instruction to end the function.
    */
    fn add_test<F: FnOnce(&mut CodeBuilder, &mut WasmModule, wasmgen::FuncIdx, &NormalTester)>(
        &mut self,
        name_: &str,
        f: F,
    ) {
        let mut wasm_builder = WasmModule::new_builder();
        // (assert_LHS, assert_RHS)
        let assert_failed_i32_func = wasm_builder.import_func(
            "platform".to_string(),
            "assert_fail".to_string(),
            &FuncType::new(
                Box::new([ValType::I32, ValType::I32, ValType::I32]),
                Box::new([]),
            ),
        );
        // (number of previously passed test cases)
        let test_failed_func = wasm_builder.import_func(
            "platform".to_string(),
            "test_fail".to_string(),
            &FuncType::new(Box::new([]), Box::new([])),
        );
        // generate the error function
        let error_func: wasmgen::FuncIdx = wasm_builder.import_func(
            "core".to_string(),
            "error".to_string(),
            &wasmgen::FuncType::new(
                Box::new([
                    wasmgen::ValType::I32,
                    wasmgen::ValType::I32,
                    wasmgen::ValType::I32,
                    wasmgen::ValType::I32,
                    wasmgen::ValType::I32,
                ]),
                Box::new([]),
            ),
        );
        let mut wasm_module = wasm_builder.build();
        let globalidx_assert_count = wasm_module.add_i32_global(Mut::Var, 0);

        // Create a function [] -> [i32], where the returned i32 is a boolean indicating number of assertions.
        let functype = FuncType::new(Box::new([]), Box::new([ValType::I32]));
        let (_type_idx, func_idx) = wasm_module.register_func(&functype);

        let mut code_builder = CodeBuilder::new(functype);
        let tester = NormalTester {
            assert_failed_i32_func: assert_failed_i32_func,
            globalidx_assert_count: globalidx_assert_count,
        };

        // net wasm stack: [] -> []
        f(&mut code_builder, &mut wasm_module, error_func, &tester);

        // net wasm stack: [] -> [ret(i32)]
        {
            let expr_builder: &mut ExprBuilder = code_builder.expr_builder();
            expr_builder.global_get(globalidx_assert_count);
            expr_builder.end();
        }

        wasm_module.commit_func(func_idx, code_builder);
        wasm_module.export_func(func_idx, "main".to_string());

        let mut receiver = std::vec::Vec::<u8>::new();
        wasm_module.wasm_serialize(&mut receiver);
        (self.add_to_js)(receiver.into_boxed_slice());
    }
}
