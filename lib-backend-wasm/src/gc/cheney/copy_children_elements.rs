use wasmgen::Scratch;

use super::WASM_PAGE_BITS;

// returns the base table element index from which indirect access should be calculated (i.e. the "table offset")
// e.g. if we want to access copy_children_$i, we should call_indirect with index = (table_offset+i)
pub fn make_copy_children_elements(
    wasm_module: &mut wasmgen::WasmModule,
    struct_types: &[Box<[ir::VarType]>],
    struct_field_byte_offsets: &[Box<[u32]>],
    struct_sizes: &[u32],
    tableidx: wasmgen::TableIdx,
    copy_indirect_table_offset: u32,
    copy_funcs: &[Option<wasmgen::FuncIdx>],
    heap_begin: u32,
) -> u32 {
    // make the string version of copy_children
    // it doesn't call any other function; just returns the ptr past-the-end of the string
    fn make_string_function(wasm_module: &mut wasmgen::WasmModule) -> wasmgen::FuncIdx {
        let functype = wasmgen::FuncType::new(
            Box::new([wasmgen::ValType::I32]),
            Box::new([wasmgen::ValType::I32]),
        );
        let (_type_idx, func_idx) = wasm_module.register_func(&functype);
        let mut code_builder = wasmgen::CodeBuilder::new(functype);
        {
            let (_locals_builder, expr_builder) = code_builder.split();
            let localidx_param = wasmgen::LocalIdx { idx: 0 };

            // Algorithm: return ptr + 4 + round_up_to_multiple_of_4(*ptr);
            // Equivalent to: return ptr + ((*ptr + 7) & (~3));
            // Equivalent to: return ((ptr + *ptr + 7) & (~3)); // since ptr is guaranteed divisible by 4
            // Equivalent to: return ((ptr + *ptr + 7) & (-4));

            // net wasm stack: [] -> [ret(i32)]
            expr_builder.local_get(localidx_param);
            expr_builder.local_get(localidx_param);
            expr_builder.i32_load(wasmgen::MemArg::new4(0));
            expr_builder.i32_add();
            expr_builder.i32_const(7);
            expr_builder.i32_add();
            expr_builder.i32_const(-4);
            expr_builder.i32_and();

            expr_builder.end(); // return it
        }
        wasm_module.commit_func(func_idx, code_builder);
        func_idx
    }

    // make the struct version of copy_children
    fn make_struct_function(
        wasm_module: &mut wasmgen::WasmModule,
        ir_vartypes: &[ir::VarType],
        byte_offsets: &[u32],
        struct_size: u32,
        tableidx: wasmgen::TableIdx,
        copy_indirect_table_offset: u32,
        copy_funcs: &[Option<wasmgen::FuncIdx>],
        heap_begin: u32,
    ) -> wasmgen::FuncIdx {
        let functype = wasmgen::FuncType::new(
            Box::new([wasmgen::ValType::I32]),
            Box::new([wasmgen::ValType::I32]),
        );
        let (_type_idx, func_idx) = wasm_module.register_func(&functype);
        let mut code_builder = wasmgen::CodeBuilder::new(functype);
        {
            let (locals_builder, expr_builder) = code_builder.split();
            let localidx_param = wasmgen::LocalIdx { idx: 0 };
            let mut scratch = Scratch::new(locals_builder);

            ir_vartypes
                .iter()
                .cloned()
                .zip(byte_offsets.iter().cloned())
                .for_each(|(ir_vartype, byte_offset)| {
                    match ir_vartype {
                        ir::VarType::Any => {
                            // f.data = (*(GC_TABLE_PTR_COPY_INDIRECT_OFFSET + f.tag))(f.data);
                            // net wasm stack: [] -> []
                            expr_builder.local_get(localidx_param);
                            expr_builder.local_get(localidx_param);
                            expr_builder.i64_load(wasmgen::MemArg::new4(byte_offset + 4)); // the `data` of the Any is at offset 4
                            expr_builder.local_get(localidx_param);
                            expr_builder.i32_load(wasmgen::MemArg::new4(byte_offset)); // the `tag` of the Any is at offset 0
                            if copy_indirect_table_offset != 0 {
                                expr_builder.i32_const(copy_indirect_table_offset as i32);
                                expr_builder.i32_add();
                            }
                            expr_builder.call_indirect(
                                wasm_module.insert_type_into(wasmgen::FuncType::new(
                                    Box::new([wasmgen::ValType::I64]),
                                    Box::new([wasmgen::ValType::I64]),
                                )),
                                tableidx,
                            );
                            expr_builder.i64_store(wasmgen::MemArg::new4(byte_offset + 4));
                            // the `data` of the Any is at offset 4
                        }
                        ir::VarType::Unassigned => {}
                        ir::VarType::Undefined => {}
                        ir::VarType::Number => {}
                        ir::VarType::Boolean => {}
                        ir::VarType::String => {
                            // net wasm stack: [] -> []
                            gen(
                                expr_builder,
                                &mut scratch,
                                localidx_param,
                                byte_offset,
                                tableidx,
                                copy_funcs[ir::VarType::String.tag() as usize].unwrap(),
                                heap_begin,
                                true,
                            );
                        }
                        ir::VarType::Func => {
                            /*
                            if (f.closure != -1) {
                                if (*(f.closure-4)) & I32_MIN {
                                    f.closure = (*(f.closure-4)) << 1; // we don't know the closure type, but we point to the new one anyway.
                                } else {
                                    f.closure = i32_wrap_i64((*(GC_TABLE_PTR_COPY_INDIRECT_OFFSET + *(f.closure-4)))(i64_extend_i32(f.closure)));
                                }
                            }
                            */
                            let localidx_closure = scratch.push_i32(); // f.closure
                            let localidx_val = scratch.push_i32(); // *(f.closure-4)

                            // net wasm stack: [] -> [f.closure(i32)]
                            expr_builder.local_get(localidx_param);
                            expr_builder.i32_load(wasmgen::MemArg::new4(byte_offset + 4)); // the `closure` of the Func is at offset 4
                            expr_builder.local_tee(localidx_closure);

                            // net wasm stack: [closure(i32)] -> [cond(i32)]
                            expr_builder.i32_const(-1);
                            expr_builder.i32_ne();

                            // net wasm stack: [cond(i32)] -> []
                            expr_builder.if_(&[]);
                            {
                                // net wasm stack: [] -> [ptr(i32)]
                                expr_builder.local_get(localidx_param);

                                // net wasm stack: [] -> [closure_minus_4(i32)]
                                expr_builder.local_get(localidx_closure);
                                expr_builder.i32_const(4);
                                expr_builder.i32_sub();

                                // net wasm stack: [closure_minus_4(i32)] -> [val(i32)]
                                expr_builder.i32_load(wasmgen::MemArg::new4(0));
                                expr_builder.local_tee(localidx_val);

                                // net wasm stack: [val(i32)] -> [cond(i32)]
                                expr_builder.i32_const(i32::min_value());
                                expr_builder.i32_and();

                                // net wasm stack: [cond(i32)] -> [new_closure(i32)]
                                expr_builder.if_(&[wasmgen::ValType::I32]);
                                expr_builder.local_get(localidx_val);
                                expr_builder.i32_const(1);
                                expr_builder.i32_shl();
                                expr_builder.else_();
                                expr_builder.local_get(localidx_closure);
                                expr_builder.i64_extend_i32_u();
                                expr_builder.local_get(localidx_val);
                                if copy_indirect_table_offset != 0 {
                                    expr_builder.i32_const(copy_indirect_table_offset as i32);
                                    expr_builder.i32_add();
                                }
                                expr_builder.call_indirect(
                                    wasm_module.insert_type_into(wasmgen::FuncType::new(
                                        Box::new([wasmgen::ValType::I64]),
                                        Box::new([wasmgen::ValType::I64]),
                                    )),
                                    tableidx,
                                );
                                expr_builder.i32_wrap_i64();
                                expr_builder.end();

                                // net wasm stack: [ptr(i32), new_closure(i32)] -> []
                                expr_builder.i32_store(wasmgen::MemArg::new4(byte_offset + 4));
                            }
                            expr_builder.end();

                            scratch.pop_i32();
                            scratch.pop_i32();
                        }
                        ir::VarType::StructT { typeidx } => {
                            // net wasm stack: [] -> []
                            gen(
                                expr_builder,
                                &mut scratch,
                                localidx_param,
                                byte_offset,
                                tableidx,
                                copy_funcs[ir::VarType::StructT { typeidx }.tag() as usize]
                                    .unwrap(),
                                heap_begin,
                                false,
                            );
                        }
                    }
                });

            fn gen(
                expr_builder: &mut wasmgen::ExprBuilder,
                scratch: &mut Scratch,
                localidx_param: wasmgen::LocalIdx,
                byte_offset: u32,
                tableidx: wasmgen::TableIdx,
                copy_func: wasmgen::FuncIdx,
                heap_begin: u32,
                is_string: bool,
            ) {
                /*
                if (ptr != -1 && (f is not String || ptr > heap_begin * WASM_PAGE_SIZE)) {
                    if (*(ptr-4)) & I32_MIN { // already copied (we multiplex the MSB of the tag field, since there shouldn't be more than 2^31 types)
                        f.ptr = (*(ptr-4)) << 1; // we store the ptr in the tag, but shifted right by one bit position (valid since ptr are all multiple of 4)
                    } else {
                        f.ptr = copy_${tag of f}(f.ptr);
                    }
                }
                */
                let localidx_ptr = scratch.push_i32(); // from_any_data(data)
                let localidx_val = scratch.push_i32(); // *(from_any_data(data)-4)

                // net wasm stack: [] -> [ptr(i32)]
                expr_builder.local_get(localidx_param);
                expr_builder.i32_load(wasmgen::MemArg::new4(byte_offset));
                expr_builder.local_tee(localidx_ptr);

                // net wasm stack: [ptr(i32)] -> [cond(i32)]
                expr_builder.i32_const(-1);
                expr_builder.i32_ne();
                if is_string {
                    expr_builder.local_get(localidx_ptr);
                    expr_builder.i32_const((heap_begin << WASM_PAGE_BITS) as i32);
                    expr_builder.i32_gt_u();
                    expr_builder.i32_and();
                }

                // net wasm stack: [cond(i32)] -> []
                expr_builder.if_(&[]);
                {
                    // net wasm stack: [] -> [param(i32)]
                    expr_builder.local_get(localidx_param);

                    // net wasm stack: [] -> [ptr_minus_4(i32)]
                    expr_builder.local_get(localidx_ptr);
                    expr_builder.i32_const(4);
                    expr_builder.i32_sub();

                    // net wasm stack: [ptr_minus_4(i32)] -> [val(i32)]
                    expr_builder.i32_load(wasmgen::MemArg::new4(0));
                    expr_builder.local_tee(localidx_val);

                    // net wasm stack: [val(i32)] -> [cond(i32)]
                    expr_builder.i32_const(i32::min_value());
                    expr_builder.i32_and();

                    // net wasm stack: [cond(i32)] -> [ret(i32)]
                    expr_builder.if_(&[wasmgen::ValType::I32]);
                    expr_builder.local_get(localidx_val);
                    expr_builder.i32_const(1);
                    expr_builder.i32_shl();
                    expr_builder.else_();
                    expr_builder.local_get(localidx_ptr);
                    expr_builder.call(copy_func);
                    expr_builder.end();

                    // net wasm stack: [param(i32), ret(i32)] -> []
                    expr_builder.i32_store(wasmgen::MemArg::new4(byte_offset));
                }
                expr_builder.end();

                scratch.pop_i32();
                scratch.pop_i32();
            }

            // net wasm stack: [] -> [i32(ptr to past-the-end)]
            expr_builder.local_get(localidx_param);
            expr_builder.i32_const(struct_size as i32);
            expr_builder.i32_add();

            expr_builder.end(); // return it
        }
        wasm_module.commit_func(func_idx, code_builder);
        func_idx
    }

    let copy_children_table_offset: u32 = wasm_module.reserve_table_elements(
        tableidx,
        (ir::NUM_PRIMITIVE_TAG_TYPES + struct_types.len()) as u32,
    );

    // Note: some reserved table elements are left uncommitted.  They will automatically trap if called at runtime.  (If that happens, then the compiler has a bug.)

    let funcidx_string: wasmgen::FuncIdx = make_string_function(wasm_module);
    wasm_module.commit_table_elements(
        tableidx,
        copy_children_table_offset + ir::VarType::String.tag() as u32,
        Box::new([funcidx_string]),
    );
    let funcidxs_structs: Box<[wasmgen::FuncIdx]> = struct_types
        .iter()
        .zip(struct_field_byte_offsets.iter())
        .zip(struct_sizes.iter().cloned())
        .map(|((ir_vartypes, byte_offsets), struct_size)| {
            make_struct_function(
                wasm_module,
                ir_vartypes,
                byte_offsets,
                struct_size,
                tableidx,
                copy_indirect_table_offset,
                copy_funcs,
                heap_begin,
            )
        })
        .collect();
    wasm_module.commit_table_elements(
        tableidx,
        copy_children_table_offset + ir::NUM_PRIMITIVE_TAG_TYPES as u32,
        funcidxs_structs,
    );

    copy_children_table_offset
}
