use super::WASM_PAGE_BITS;
use wasmgen::Scratch;

// returns the base table element index from which indirect access should be calculated (i.e. the "table offset")
// e.g. if we want to access copy_indirect_$i, we should call_indirect with index = (table_offset+i)
pub fn make_copy_indirect_elements(
    wasm_module: &mut wasmgen::WasmModule,
    num_structs: usize,
    copy_funcs: &[Option<wasmgen::FuncIdx>],
    tableidx: wasmgen::TableIdx,
    heap_begin: u32,
) -> u32 {
    /*
    // copy_indirect_$i shall exist for all types (except Any)
    // returns the new data to be stored in place of the old one
    fn copy_indirect_$i(data: i64) -> i64 { // note: this will be no-op if type is not ptr
        // it is the same as copy_$i, just that we cast between the i64(data) and the actual i32(ptr), and back.
        if constexpr $i is Func {
            if (f.closure != -1) {
                if (*(f.closure-4)) & I32_MIN {
                    return make_func(f.idx, (*(f.closure-4)) << 1);
                } else {
                    return make_func(f.idx, i32_wrap_i64((*(GC_TABLE_PTR_COPY_INDIRECT_OFFSET + *(f.closure-4)))(i64_extend_i32(f.closure))));
                }
            }
        } else if constexpr $i is not a ptr (i.e. not StructT or string) {
            // NO-OP
        } else {
            if (ptr != -1 && (f is not String || ptr > heap_begin * WASM_PAGE_SIZE)) {
                if (*(ptr-4)) & I32_MIN { // already copied (we multiplex the MSB of the tag field, since there shouldn't be more than 2^31 types)
                    return to_any_data((*(ptr-4)) << 1); // we store the ptr in the tag, but shifted right by one bit position (valid since ptr are all multiple of 4)
                } else {
                    return to_any_data(copy_$i(from_any_data(data)));
                }
            }
        }
    }
    */

    // make the dummy (no-op function)
    fn make_no_op_function(wasm_module: &mut wasmgen::WasmModule) -> wasmgen::FuncIdx {
        let functype = wasmgen::FuncType::new(
            Box::new([wasmgen::ValType::I64]),
            Box::new([wasmgen::ValType::I64]),
        );
        let (_type_idx, func_idx) = wasm_module.register_func(&functype);
        let mut code_builder = wasmgen::CodeBuilder::new(functype);
        {
            let (_locals_builder, expr_builder) = code_builder.split();
            let localidx_param = wasmgen::LocalIdx { idx: 0 };
            expr_builder.local_get(localidx_param); // put the param onto the stack
            expr_builder.end(); // return it
        }
        wasm_module.commit_func(func_idx, code_builder);
        func_idx
    }

    // make the function for funcs
    fn make_func_function(
        wasm_module: &mut wasmgen::WasmModule,
        tableoffset: u32,
        tableidx: wasmgen::TableIdx,
    ) -> wasmgen::FuncIdx {
        let functype = wasmgen::FuncType::new(
            Box::new([wasmgen::ValType::I64]),
            Box::new([wasmgen::ValType::I64]),
        );
        let (_type_idx, func_idx) = wasm_module.register_func(&functype);
        let mut code_builder = wasmgen::CodeBuilder::new(functype);
        {
            let (locals_builder, expr_builder) = code_builder.split();
            let localidx_param = wasmgen::LocalIdx { idx: 0 };
            let mut scratch = Scratch::new(locals_builder);

            /*
            // Algorithm:
            if (f.closure != -1) {
                if (*(f.closure-4)) & I32_MIN {
                    return make_func(f.idx, (*(f.closure-4)) << 1);
                } else {
                    return make_func(f.idx, i32_wrap_i64((*(GC_TABLE_PTR_COPY_INDIRECT_OFFSET + *(f.closure-4)))(i64_extend_i32(f.closure))));
                }
            }
            */

            let localidx_closure = scratch.push_i32(); // f.closure
            let localidx_val = scratch.push_i32(); // *(f.closure-4)

            // net wasm stack: [] -> [f.idx(i64)]
            expr_builder.local_get(localidx_param);
            expr_builder.i32_wrap_i64();
            expr_builder.i64_extend_i32_u();

            // net wasm stack: [f.idx(i64)] -> [f.idx(i64), closure(i32)]
            expr_builder.local_get(localidx_param);
            expr_builder.i64_const(32);
            expr_builder.i64_shr_u();
            expr_builder.i32_wrap_i64();
            expr_builder.local_tee(localidx_closure);

            // net wasm stack: [f.idx(i64), closure(i32)] -> [f.idx(i64), cond(i32)]
            expr_builder.i32_const(-1);
            expr_builder.i32_ne();

            // net wasm stack: [f.idx(i64), cond(i32)] -> [f.idx(i64), ret_closure(i64)]
            expr_builder.if_(&[wasmgen::ValType::I64]);
            {
                // net wasm stack: [f.idx(i64)] -> [f.idx(i64), closure_minus_4(i32)]
                expr_builder.local_get(localidx_closure);
                expr_builder.i32_const(4);
                expr_builder.i32_sub();

                // net wasm stack: [f.idx(i64), closure_minus_4(i32)] -> [f.idx(i64), val(i32)]
                expr_builder.i32_load(wasmgen::MemArg::new4(0));
                expr_builder.local_tee(localidx_val);

                // net wasm stack: [f.idx(i64), val(i32)] -> [f.idx(i64), cond(i32)]
                expr_builder.i32_const(i32::min_value());
                expr_builder.i32_and();

                // net wasm stack: [f.idx(i64), cond(i32)] -> [f.idx(i64), ret_closure(i64)]
                expr_builder.if_(&[wasmgen::ValType::I64]);
                expr_builder.local_get(localidx_val);
                expr_builder.i64_extend_i32_u();
                expr_builder.i64_const(33);
                expr_builder.i64_shl();
                expr_builder.else_();
                expr_builder.local_get(localidx_closure);
                expr_builder.i64_extend_i32_u();
                expr_builder.local_get(localidx_val);
                if tableoffset != 0 {
                    expr_builder.i32_const(tableoffset as i32);
                    expr_builder.i32_add();
                }
                expr_builder.call_indirect(
                    wasm_module.insert_type_into(wasmgen::FuncType::new(
                        Box::new([wasmgen::ValType::I64]),
                        Box::new([wasmgen::ValType::I64]),
                    )),
                    tableidx,
                );
                expr_builder.i64_const(32);
                expr_builder.i64_shl();
                expr_builder.end();
            }
            expr_builder.else_();
            {
                expr_builder.i64_const((-1) << 32);
            }
            expr_builder.end();

            // net wasm stack: [f.idx(i64), ret_closure(i64)] -> [actual_ret(i64)]
            expr_builder.i64_or();

            scratch.pop_i32();
            scratch.pop_i32();

            expr_builder.end(); // return it
        }
        wasm_module.commit_func(func_idx, code_builder);
        func_idx
    }

    // `copy_func` is the function copy_$i.
    fn make_struct_function(
        wasm_module: &mut wasmgen::WasmModule,
        copy_func: wasmgen::FuncIdx,
        heap_begin: u32,
        is_string: bool,
    ) -> wasmgen::FuncIdx {
        let functype = wasmgen::FuncType::new(
            Box::new([wasmgen::ValType::I64]),
            Box::new([wasmgen::ValType::I64]),
        );
        let (_type_idx, func_idx) = wasm_module.register_func(&functype);
        let mut code_builder = wasmgen::CodeBuilder::new(functype);
        {
            let (locals_builder, expr_builder) = code_builder.split();
            let localidx_param = wasmgen::LocalIdx { idx: 0 };
            let mut scratch = Scratch::new(locals_builder);

            /*
            // Algorithm:
            if (ptr != -1 && (f is not String || ptr > heap_begin * WASM_PAGE_SIZE)) {
                if (*(ptr-4)) & I32_MIN { // already copied (we multiplex the MSB of the tag field, since there shouldn't be more than 2^31 types)
                    return to_any_data((*(ptr-4)) << 1); // we store the ptr in the tag, but shifted right by one bit position (valid since ptr are all multiple of 4)
                } else {
                    return to_any_data(copy_$i(from_any_data(data)));
                }
            } else return -1;
            */

            let localidx_ptr = scratch.push_i32(); // from_any_data(data)
            let localidx_val = scratch.push_i32(); // *(from_any_data(data)-4)

            // net wasm stack: [] -> [ptr(i32)]
            expr_builder.local_get(localidx_param);
            expr_builder.i32_wrap_i64();
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

            // net wasm stack: [cond(i32)] -> [actual_ret(i64)]
            expr_builder.if_(&[wasmgen::ValType::I64]);
            {
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

                // net wasm stack: [ret(i32)] -> [actual_ret(i64)]
                expr_builder.i64_extend_i32_u();
            }
            expr_builder.else_();
            {
                expr_builder.i64_const(((((-1) as i32) as u32) as u64) as i64); // weird conversion so that the high bits will be zero, not technically necessary since we are allowed to have arbitrary high bits, but it is nicer.
            }
            expr_builder.end();

            scratch.pop_i32();
            scratch.pop_i32();

            expr_builder.end(); // return it
        }
        wasm_module.commit_func(func_idx, code_builder);
        func_idx
    }

    let copy_indirect_table_offset: u32 = wasm_module
        .reserve_table_elements(tableidx, (ir::NUM_PRIMITIVE_TAG_TYPES + num_structs) as u32);

    let no_op_funcidx: wasmgen::FuncIdx = make_no_op_function(wasm_module);
    let func_funcidx: wasmgen::FuncIdx =
        make_func_function(wasm_module, copy_indirect_table_offset, tableidx);
    let string_funcidx: wasmgen::FuncIdx = make_struct_function(
        wasm_module,
        copy_funcs[ir::VarType::String.tag() as usize].unwrap(),
        heap_begin,
        true,
    );

    let copy_indirect_elements: Box<[wasmgen::FuncIdx]> = std::iter::empty()
        .chain(std::iter::once(no_op_funcidx)) // Unassigned
        .chain(std::iter::once(no_op_funcidx)) // Undefined
        .chain(std::iter::once(no_op_funcidx)) // Number
        .chain(std::iter::once(no_op_funcidx)) // Boolean
        .chain(std::iter::once(string_funcidx)) // String
        .chain(std::iter::once(func_funcidx)) // Func
        .chain((0..num_structs).map(|n| {
            make_struct_function(
                wasm_module,
                copy_funcs[ir::NUM_PRIMITIVE_TAG_TYPES + n].unwrap(),
                heap_begin,
                false,
            )
        }))
        .collect();
    assert!(copy_indirect_elements.len() == ir::NUM_PRIMITIVE_TAG_TYPES + num_structs);
    wasm_module.commit_table_elements(tableidx, copy_indirect_table_offset, copy_indirect_elements);
    copy_indirect_table_offset
}
