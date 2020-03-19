use super::*;
use wasm_test_harness::*;

pub fn wasmtest<C: TestContext>(c: &mut C) {
    c.add_test("no roots", |code_builder, wasm_module, t| {
        /*
        In this test we will create a Cheney with the initial size (1MiB usuable size),
        and add 32768 copies of 28-byte structs (which will exactly hit the maximum size (including 4-byte tag)).
        The memory should be located at increasing order at 32-byte offsets.
        Then add 32768 copies again (without holding references to the old memory).
        And do it a another time... for 8 times total.
        The memory should not grow, and it should be located as expected.
        */
        // 28 bytes struct
        let struct_types: [Box<[ir::VarType]>; 1] = [Box::new([
            ir::VarType::Any,
            ir::VarType::Any,
            ir::VarType::Boolean,
        ])];
        let struct_field_byte_offsets: [Box<[u32]>; 1] = [Box::new([0, 12, 24])];
        let struct_sizes: [u32; 1] = [28];
        let cheney = Cheney::new(
            &struct_types,
            &struct_field_byte_offsets,
            &struct_sizes,
            wasm_module.add_unbounded_memory(MEM_INITIAL_HEAP_SIZE),
            0,
            MEM_INITIAL_HEAP_SIZE,
            wasm_module,
        );

        let (locals_builder, expr_builder) = code_builder.split();
        let mut scratch = Scratch::new(locals_builder);

        for i in 0..4 {
            // add 32768 structs, ensuring that they are at proper positions
            // net wasm stack: [] -> []
            {
                let localidx_i = scratch.push_i32();

                // i = 0;
                expr_builder.i32_const(0);
                expr_builder.local_set(localidx_i);

                // do {..} while(..);
                expr_builder.loop_(&[]);
                {
                    // ret = new struct$0();
                    cheney.encode_fixed_allocation(
                        ir::VarType::StructT { typeidx: 0 },
                        &[],
                        &mut scratch,
                        expr_builder,
                    );

                    // assert(ret == i * 32 + 4); // this is where the Cheney GC stores the memory.
                    expr_builder.local_get(localidx_i);
                    expr_builder.i32_const(32);
                    expr_builder.i32_mul();
                    expr_builder.i32_const(4);
                    expr_builder.i32_add();
                    t.i32_assert_eq(&mut scratch, expr_builder);

                    // i = i + 1;
                    expr_builder.local_get(localidx_i);
                    expr_builder.i32_const(1);
                    expr_builder.i32_add();
                    expr_builder.local_set(localidx_i);

                    // while (i < 32768);
                    expr_builder.local_get(localidx_i);
                    expr_builder.i32_const(32768);
                    expr_builder.i32_lt_u();
                    expr_builder.br_if(0);
                }
                expr_builder.end();

                scratch.pop_i32();
            }

            // add 32768 structs, ensuring that they are at proper positions
            // net wasm stack: [] -> []
            {
                let localidx_i = scratch.push_i32();

                // i = 0;
                expr_builder.i32_const(0);
                expr_builder.local_set(localidx_i);

                // do {..} while(..);
                expr_builder.loop_(&[]);
                {
                    // ret = new struct$0();
                    cheney.encode_fixed_allocation(
                        ir::VarType::StructT { typeidx: 0 },
                        &[],
                        &mut scratch,
                        expr_builder,
                    );

                    // assert(ret == i * 32 + 4 + MEM_INITIAL_USABLE_SIZE); // this is where the Cheney GC stores the memory.
                    expr_builder.local_get(localidx_i);
                    expr_builder.i32_const(32);
                    expr_builder.i32_mul();
                    expr_builder.i32_const((4 + MEM_INITIAL_USABLE_SIZE * WASM_PAGE_SIZE) as i32);
                    expr_builder.i32_add();
                    t.i32_assert_eq(&mut scratch, expr_builder);

                    // i = i + 1;
                    expr_builder.local_get(localidx_i);
                    expr_builder.i32_const(1);
                    expr_builder.i32_add();
                    expr_builder.local_set(localidx_i);

                    // while (i < 32768);
                    expr_builder.local_get(localidx_i);
                    expr_builder.i32_const(32768);
                    expr_builder.i32_lt_u();
                    expr_builder.br_if(0);
                }
                expr_builder.end();

                scratch.pop_i32();
            }
        }
    });
}
