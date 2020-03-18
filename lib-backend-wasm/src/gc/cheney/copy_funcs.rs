use wasmgen::Scratch;

pub fn make_copy_funcs(
    wasm_module: &mut wasmgen::WasmModule,
    struct_sizes: &[u32],
    free_mem_ptr: wasmgen::GlobalIdx,
) -> Box<[Option<wasmgen::FuncIdx>]> {
    let funcidx_copy_string: wasmgen::FuncIdx = {
        let functype = wasmgen::FuncType::new(
            Box::new([wasmgen::ValType::I32]),
            Box::new([wasmgen::ValType::I32]),
        );
        let (_type_idx, func_idx) = wasm_module.register_func(&functype);
        let mut code_builder = wasmgen::CodeBuilder::new(functype);
        {
            let (locals_builder, expr_builder) = code_builder.split();
            let mut scratch = Scratch::new(locals_builder);
            let localidx_param = wasmgen::LocalIdx { idx: 0 };

            // Algorithm
            /*
            let new_ptr = free_mem_ptr + 4; // skip the tag
            let str_end = ptr + 4 + round_up_to_multiple_of_4(*ptr);
            ptr -= 4;
            free_mem_ptr = move(ptr, str_end, free_mem_ptr); // move everything, including the tag.
            (*ptr) = I32_MIN | (new_ptr >> 1); // say that we already copied it.
            return new_ptr;
            */
            {
                let localidx_free_mem_ptr = scratch.push_i32();
                let localidx_new_ptr = scratch.push_i32();
                let localidx_str_end = scratch.push_i32();

                // let new_ptr = free_mem_ptr + 4;
                // net wasm stack: [] -> [new_ptr]
                {
                    expr_builder.global_get(free_mem_ptr);
                    expr_builder.local_tee(localidx_free_mem_ptr);
                    expr_builder.i32_const(4);
                    expr_builder.i32_add();
                    expr_builder.local_tee(localidx_new_ptr);
                }

                // let str_end = ptr + 4 + round_up_to_multiple_of_4(*ptr);
                // Actually, we do:
                // let str_end = ptr + ((*ptr + 7) & (~3));
                // net wasm stack: [] -> []
                {
                    expr_builder.local_get(localidx_param);
                    expr_builder.local_get(localidx_param);
                    expr_builder.i32_load(wasmgen::MemArg::new4(0));
                    expr_builder.i32_const(7);
                    expr_builder.i32_add();
                    expr_builder.i32_const(-4);
                    expr_builder.i32_and();
                    expr_builder.i32_add();
                    expr_builder.local_set(localidx_str_end);
                }

                // ptr -= 4;
                // free_mem_ptr = move(ptr - 4, str_end, free_mem_ptr);
                // we actually do:
                /*
                ptr -= 4;
                let it = ptr;
                do {
                    *free_mem_ptr = *it;
                    ++free_mem_ptr;
                    ++it;
                } while (it != str_end);
                // rmb to assign the local free_mem_ptr back to global
                */
                // net wasm stack: [] -> []
                {
                    let localidx_it: wasmgen::LocalIdx = scratch.push_i32();

                    // net wasm stack: [] -> []
                    {
                        expr_builder.local_get(localidx_param);
                        expr_builder.i32_const(4);
                        expr_builder.i32_sub();
                        expr_builder.local_tee(localidx_param);
                        expr_builder.local_set(localidx_it);
                    }

                    // net wasm stack: [] -> []
                    {
                        expr_builder.loop_(&[]);

                        // *free_mem_ptr = *it;
                        // net wasm stack: [] -> []
                        expr_builder.local_get(localidx_free_mem_ptr);
                        expr_builder.local_get(localidx_it);
                        expr_builder.i32_load(wasmgen::MemArg::new4(0));
                        expr_builder.i32_store(wasmgen::MemArg::new4(0));

                        // ++free_mem_ptr;
                        // net wasm stack: [] -> []
                        expr_builder.local_get(localidx_free_mem_ptr);
                        expr_builder.i32_const(1);
                        expr_builder.i32_add();
                        expr_builder.local_set(localidx_free_mem_ptr);

                        // ++it;
                        // net wasm stack: [] -> [it]
                        expr_builder.local_get(localidx_it);
                        expr_builder.i32_const(1);
                        expr_builder.i32_add();
                        expr_builder.local_tee(localidx_it);

                        // while (it != str_end);
                        // net wasm stack: [it] -> []
                        expr_builder.local_get(localidx_str_end);
                        expr_builder.i32_ne();
                        expr_builder.br_if(0); // conditional jump to start of innermost loop

                        expr_builder.end();
                    }

                    scratch.pop_i32();
                }

                // (*ptr) = I32_MIN | (new_ptr >> 1);
                // net wasm stack: [] -> []
                {
                    expr_builder.local_get(localidx_param);
                    expr_builder.i32_const(i32::min_value());
                    expr_builder.local_get(localidx_new_ptr);
                    expr_builder.i32_const(1);
                    expr_builder.i32_shr_u();
                    expr_builder.i32_or();
                    expr_builder.i32_store(wasmgen::MemArg::new4(0));
                }

                // currently stack is [new_ptr], which automatically gets returned
                expr_builder.end();

                scratch.pop_i32();
                scratch.pop_i32();
                scratch.pop_i32();
            }
        }
        wasm_module.commit_func(func_idx, code_builder);
        func_idx
    };

    // Generate functions for copy_$i
    // Since copy_$i only depends on sizeof($i), we can combine all structs with the same size.
    std::iter::empty()
        .chain(std::iter::once(None)) // Unassigned
        .chain(std::iter::once(None)) // Undefined
        .chain(std::iter::once(None)) // Number
        .chain(std::iter::once(None)) // Boolean
        .chain(std::iter::once(Some(funcidx_copy_string))) // String
        .chain(std::iter::once(None)) // Func
        .chain(
            struct_sizes
                .iter()
                .cloned()
                .scan(
                    std::collections::hash_map::HashMap::<u32, wasmgen::FuncIdx>::new(),
                    |hm, size| {
                        Some(
                            *(hm.entry(size).or_insert_with(|| {
                                // encode copy_$i for StructT with `size`
                                let functype = wasmgen::FuncType::new(
                                    Box::new([wasmgen::ValType::I32]),
                                    Box::new([wasmgen::ValType::I32]),
                                );
                                let (_type_idx, func_idx) = wasm_module.register_func(&functype);
                                let mut code_builder = wasmgen::CodeBuilder::new(functype);
                                {
                                    let (locals_builder, expr_builder) = code_builder.split();
                                    let mut scratch = Scratch::new(locals_builder);
                                    let localidx_param = wasmgen::LocalIdx { idx: 0 };

                                    {
                                        let localidx_free_mem_ptr = scratch.push_i32();
                                        let localidx_new_ptr = scratch.push_i32();

                                        // let new_ptr = free_mem_ptr + 4;
                                        // net wasm stack: [] -> [new_ptr]
                                        {
                                            expr_builder.global_get(free_mem_ptr);
                                            expr_builder.local_tee(localidx_free_mem_ptr);
                                            expr_builder.i32_const(4);
                                            expr_builder.i32_add();
                                            expr_builder.local_tee(localidx_new_ptr);
                                        }

                                        // ptr-=4;
                                        // net wasm stack: [] -> []
                                        {
                                            expr_builder.local_get(localidx_param);
                                            expr_builder.i32_const(4);
                                            expr_builder.i32_sub();
                                            expr_builder.local_set(localidx_param);
                                        }

                                        // free_mem_ptr = move(ptr, ptr + 4 + sizeof($i), free_mem_ptr);
                                        // we actually do:
                                        /*
                                        *free_mem_ptr = *ptr;
                                        *(free_mem_ptr+4) = *(ptr+4);
                                        *(free_mem_ptr+8) = *(ptr+8);
                                        ...
                                        */
                                        // net wasm stack: [] -> []
                                        {
                                            assert!(size % 4 == 0);
                                            for offset in (0..(4 + size)).step_by(4) {
                                                expr_builder.local_get(localidx_free_mem_ptr);
                                                expr_builder.local_get(localidx_param);
                                                expr_builder
                                                    .i32_load(wasmgen::MemArg::new4(offset));
                                                expr_builder
                                                    .i32_store(wasmgen::MemArg::new4(offset));
                                            }
                                            expr_builder.local_get(localidx_free_mem_ptr);
                                            expr_builder.i32_const((4 + size) as i32);
                                            expr_builder.i32_add();
                                            expr_builder.global_set(free_mem_ptr);
                                        }

                                        // (*ptr) = I32_MIN | (new_ptr >> 1);
                                        // net wasm stack: [] -> []
                                        {
                                            expr_builder.local_get(localidx_param);
                                            expr_builder.i32_const(i32::min_value());
                                            expr_builder.local_get(localidx_new_ptr);
                                            expr_builder.i32_const(1);
                                            expr_builder.i32_shr_u();
                                            expr_builder.i32_or();
                                            expr_builder.i32_store(wasmgen::MemArg::new4(0));
                                        }

                                        // currently stack is [new_ptr], which automatically gets returned
                                        expr_builder.end();

                                        scratch.pop_i32();
                                        scratch.pop_i32();
                                    }
                                }
                                wasm_module.commit_func(func_idx, code_builder);
                                func_idx
                            })),
                        )
                    },
                )
                .map(|x| Some(x)),
        )
        .collect()
}
