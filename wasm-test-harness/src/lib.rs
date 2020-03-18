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
    fn add_test<F: FnOnce(&mut CodeBuilder, &mut WasmModule, &NormalTester)>(
        &mut self,
        name_: &str,
        f: F,
    );
}

pub trait Tester {
    // net wasm stack: [i32, i32] -> []
    fn i32_assert_eq(&self, scratch: &mut Scratch, expr_builder: &mut ExprBuilder);
}

pub struct NormalContext {
    wasm_module: WasmModule,
    code_builder: CodeBuilder,
    main_func: FuncIdx,
    localidx_test_count: LocalIdx,
    assert_failed_i32_func: FuncIdx,
    test_failed_func: FuncIdx,
}

pub struct NormalTester {
    assert_failed_i32_func: FuncIdx,
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
        expr_builder.call(self.assert_failed_i32_func);
        expr_builder.end();
        scratch.pop_i32();
        scratch.pop_i32();
    }
}

impl NormalContext {
    pub fn new() -> NormalContext {
        let mut wasm_builder = WasmModule::new_builder();
        // (assert_LHS, assert_RHS)
        let assert_failed_i32_func = wasm_builder.import_func(
            "platform".to_string(),
            "assert_fail".to_string(),
            &FuncType::new(Box::new([ValType::I32, ValType::I32]), Box::new([])),
        );
        // (number of previously passed test cases)
        let test_failed_func = wasm_builder.import_func(
            "platform".to_string(),
            "test_fail".to_string(),
            &FuncType::new(Box::new([ValType::I32]), Box::new([])),
        );
        let mut wasm_module = wasm_builder.build();
        let functype = FuncType::new(Box::new([]), Box::new([ValType::I32]));
        let (_type_idx, func_idx) = wasm_module.register_func(&functype);
        let mut code_builder = CodeBuilder::new(functype);
        let localidx = code_builder.locals_builder().add(ValType::I32);
        NormalContext {
            wasm_module: wasm_module,
            code_builder: code_builder,
            main_func: func_idx,
            localidx_test_count: localidx,
            assert_failed_i32_func: assert_failed_i32_func,
            test_failed_func: test_failed_func,
        }
    }
    pub fn build(mut self) -> WasmModule {
        {
            let (_locals_builder, expr_builder) = self.code_builder.split();
            expr_builder.local_get(self.localidx_test_count);
            expr_builder.end();
        }
        self.wasm_module
            .commit_func(self.main_func, self.code_builder);
        self.wasm_module
            .export_func(self.main_func, "main".to_string());
        self.wasm_module
    }
}

impl TestContext for NormalContext {
    /*
    Adds a test.
    `f` should emit code that has net wasm stack [] -> [], even though the CodeBuilder might have a different signature.  This is because the test harness might return different bookkeeping information.
    It should not emit the end() instruction to end the function.
    */
    fn add_test<F: FnOnce(&mut CodeBuilder, &mut WasmModule, &NormalTester)>(
        &mut self,
        name_: &str,
        f: F,
    ) {
        // Create a function [] -> [i32], where the returned i32 is a boolean indicating success or failure.
        let functype = FuncType::new(Box::new([]), Box::new([ValType::I32]));
        let (_type_idx, func_idx) = self.wasm_module.register_func(&functype);
        {
            let mut code_builder = CodeBuilder::new(functype);
            let tester = NormalTester {
                assert_failed_i32_func: self.assert_failed_i32_func,
            };

            // net wasm stack: [] -> []
            f(&mut code_builder, &mut self.wasm_module, &tester);

            // net wasm stack: [] -> [ret(i32)]
            {
                let expr_builder: &mut ExprBuilder = code_builder.expr_builder();
                expr_builder.i32_const(1);
                expr_builder.end();
            }

            self.wasm_module.commit_func(func_idx, code_builder);
        }

        // add to the main function
        {
            let (_locals_builder, expr_builder) = self.code_builder.split();
            expr_builder.call(func_idx);
            expr_builder.if_(&[]);
            expr_builder.local_get(self.localidx_test_count);
            expr_builder.i32_const(1);
            expr_builder.i32_add();
            expr_builder.local_set(self.localidx_test_count);
            expr_builder.else_();
            expr_builder.local_get(self.localidx_test_count);
            expr_builder.call(self.test_failed_func);
            expr_builder.end();
        }
    }
}
