use crate::global_var::GlobalVarManagerRef;
use wasmgen::Scratch;

use super::WASM_PAGE_BITS;

pub fn make_do_cheney<'a>(
    wasm_module: &mut wasmgen::WasmModule,
    tableidx: wasmgen::TableIdx,
    copy_indirect_table_offset: u32,
    copy_children_table_offset: u32,
    memidx: wasmgen::MemIdx,
    globalidx_free_mem_ptr: wasmgen::GlobalIdx,
    globalidx_end_mem_ptr: wasmgen::GlobalIdx,
    globalidx_gc_roots_stack_base_ptr: wasmgen::GlobalIdx,
    globalidx_gc_roots_stack_ptr: wasmgen::GlobalIdx,
    copy_funcs: &[Option<wasmgen::FuncIdx>],
    global_var_manager: GlobalVarManagerRef<'a>,
    heap_begin: u32,
) -> wasmgen::FuncIdx {
    // Guaranteed to synchronise localidx_free_mem_ptr and globalidx_free_mem_ptr before returning.
    // net wasm stack: [] -> []
    fn generate_common_portion<'a>(
        wasm_module: &mut wasmgen::WasmModule,
        tableidx: wasmgen::TableIdx,
        copy_indirect_table_offset: u32,
        copy_children_table_offset: u32,
        globalidx_free_mem_ptr: wasmgen::GlobalIdx,
        localidx_free_mem_ptr: wasmgen::LocalIdx,
        localidx_gc_roots_stack_base_ptr: wasmgen::LocalIdx,
        localidx_gc_roots_stack_ptr: wasmgen::LocalIdx,
        copy_funcs: &[Option<wasmgen::FuncIdx>],
        global_var_manager: GlobalVarManagerRef<'a>,
        heap_begin: u32,
        expr_builder: &mut wasmgen::ExprBuilder,
        scratch: &mut Scratch,
    ) {
        let localidx_scan = scratch.push_i32();

        // let scan = free_mem_ptr;
        // net wasm stack: [] -> []
        expr_builder.local_get(localidx_free_mem_ptr);
        expr_builder.local_set(localidx_scan);

        // net wasm stack: [] -> []
        {
            // Pseudocode:
            // for each global g {
            //     copy_field_impl_$i(&mut g);
            // }
            for (ir_vartype, wasm_globalidxs) in global_var_manager {
                // note: similar to copying struct fields in copy_children_elements()
                match ir_vartype {
                    ir::VarType::Any => {
                        // f.data = (*(GC_TABLE_PTR_COPY_INDIRECT_OFFSET + f.tag))(f.data);
                        // net wasm stack: [] -> []
                        expr_builder.global_get(wasm_globalidxs[1]); // the `data` of the Any
                        expr_builder.global_get(wasm_globalidxs[0]); // the `tag` of the Any
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
                        expr_builder.global_set(wasm_globalidxs[1]); // store the `data` of the Any
                    }
                    ir::VarType::Unassigned => {}
                    ir::VarType::Undefined => {}
                    ir::VarType::Number => {}
                    ir::VarType::Boolean => {}
                    ir::VarType::String => {
                        // net wasm stack: [] -> []
                        gen(
                            expr_builder,
                            scratch,
                            wasm_globalidxs[0],
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
                        expr_builder.global_get(wasm_globalidxs[1]); // the `closure` of the Func
                        expr_builder.local_tee(localidx_closure);

                        // net wasm stack: [closure(i32)] -> [cond(i32)]
                        expr_builder.i32_const(-1);
                        expr_builder.i32_ne();

                        // net wasm stack: [cond(i32)] -> []
                        expr_builder.if_(&[]);
                        {
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

                            // net wasm stack: [new_closure(i32)] -> []
                            expr_builder.global_set(wasm_globalidxs[1]);
                        }
                        expr_builder.end();

                        scratch.pop_i32();
                        scratch.pop_i32();
                    }
                    ir::VarType::StructT { typeidx } => {
                        // net wasm stack: [] -> []
                        gen(
                            expr_builder,
                            scratch,
                            wasm_globalidxs[0],
                            tableidx,
                            copy_funcs[ir::VarType::StructT { typeidx }.tag() as usize].unwrap(),
                            heap_begin,
                            false,
                        );
                    }
                }
            }

            fn gen(
                expr_builder: &mut wasmgen::ExprBuilder,
                scratch: &mut Scratch,
                wasm_globalidx: wasmgen::GlobalIdx,
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
                expr_builder.global_get(wasm_globalidx);
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

                    // net wasm stack: [ret(i32)] -> []
                    expr_builder.global_set(wasm_globalidx);
                }
                expr_builder.end();

                scratch.pop_i32();
                scratch.pop_i32();
            }
        }

        // net wasm stack: [] -> []
        {
            let localidx_gc_roots_it = scratch.push_i32();

            // net wasm stack: [] -> [gc_roots_it(i32)]
            expr_builder.local_get(localidx_gc_roots_stack_base_ptr);
            expr_builder.local_tee(localidx_gc_roots_it);

            // while loop turns into this:
            /*
            if gc_roots_it != gc_roots_stack_ptr {
                do {
                    ...
                } while (gc_roots_it != gc_roots_stack_ptr);
            }
            */

            // net wasm stack: [gc_roots_it(i32)] -> [cond(i32)]
            expr_builder.local_get(localidx_gc_roots_stack_ptr);
            expr_builder.i32_ne();

            // net wasm stack: [cond(i32)] -> []
            expr_builder.if_(&[]);
            {
                // net wasm stack: [] -> []
                expr_builder.loop_(&[]);
                {
                    // let f = *gc_roots_it;
                    // f.data = (*(GC_TABLE_PTR_COPY_INDIRECT_OFFSET + f.tag))(f.data);
                    // net wasm stack: [] -> []
                    expr_builder.local_get(localidx_gc_roots_it);
                    expr_builder.local_get(localidx_gc_roots_it);
                    expr_builder.i64_load(wasmgen::MemArg::new4(4)); // load f.data
                    expr_builder.local_get(localidx_gc_roots_it);
                    expr_builder.i32_load(wasmgen::MemArg::new4(0)); // load f.tag
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
                    expr_builder.i64_store(wasmgen::MemArg::new4(4)); // store f.data

                    // gc_roots_it += 12;
                    // net wasm stack: [] -> [gc_roots_it(i32)]
                    expr_builder.local_get(localidx_gc_roots_it);
                    expr_builder.i32_const(12);
                    expr_builder.i32_add();
                    expr_builder.local_tee(localidx_gc_roots_it);

                    // do a conditional jump back
                    // net wasm stack: [gc_roots_it(i32)] -> []
                    expr_builder.local_get(localidx_gc_roots_stack_ptr);
                    expr_builder.i32_ne();
                    expr_builder.br_if(0);
                }
                expr_builder.end();
            }
            expr_builder.end();

            // reload free_mem_ptr
            // net wasm stack: [] -> []
            expr_builder.global_get(globalidx_free_mem_ptr);
            expr_builder.local_set(localidx_free_mem_ptr);

            scratch.pop_i32();
        }

        {
            // Pseudocode:
            /*
            while (scan != free_mem_ptr) {
                scan = (*(GC_TABLE_PTR_COPY_CHILDREN_OFFSET + *scan))(scan+4);
            }
            */
            // while loop turns into this:
            /*
            if scan != free_mem_ptr {
                do {
                    ...
                } while (scan != free_mem_ptr);
            }
            */

            // net wasm stack: [] -> [cond(i32)]
            expr_builder.local_get(localidx_scan);
            expr_builder.local_get(localidx_free_mem_ptr);
            expr_builder.i32_ne();

            // net wasm stack: [cond(i32)] -> []
            expr_builder.if_(&[]);
            {
                // net wasm stack: [] -> []
                expr_builder.loop_(&[]);
                {
                    // net wasm stack: [] -> [scan(i32)]
                    expr_builder.local_get(localidx_scan);
                    expr_builder.i32_const(4);
                    expr_builder.i32_add();
                    expr_builder.local_get(localidx_scan);
                    expr_builder.i32_load(wasmgen::MemArg::new4(0));
                    if copy_children_table_offset != 0 {
                        expr_builder.i32_const(copy_children_table_offset as i32);
                        expr_builder.i32_add();
                    }
                    expr_builder.call_indirect(
                        wasm_module.insert_type_into(wasmgen::FuncType::new(
                            Box::new([wasmgen::ValType::I32]),
                            Box::new([wasmgen::ValType::I32]),
                        )),
                        tableidx,
                    );
                    expr_builder.local_tee(localidx_scan);

                    // need to reload free_mem_ptr from global, because the called function might have changed it.
                    // net wasm stack: [scan(i32)] -> []
                    expr_builder.global_get(globalidx_free_mem_ptr);
                    expr_builder.local_tee(localidx_free_mem_ptr);
                    expr_builder.i32_ne();
                    expr_builder.br_if(0);
                }
                expr_builder.end();
            }
            expr_builder.end();
        }

        scratch.pop_i32();
    }

    let functype = wasmgen::FuncType::new(
        Box::new([wasmgen::ValType::I32]),
        Box::new([wasmgen::ValType::I32]),
    );
    let (_type_idx, func_idx) = wasm_module.register_func(&functype);
    let mut code_builder = wasmgen::CodeBuilder::new(functype);
    {
        let (locals_builder, expr_builder) = code_builder.split();
        let localidx_bytes_required = wasmgen::LocalIdx { idx: 0 };
        let mut scratch = Scratch::new(locals_builder);

        let localidx_end_mem_ptr = scratch.push_i32();
        let localidx_gc_roots_stack_base_ptr = scratch.push_i32();
        let localidx_gc_roots_stack_ptr = scratch.push_i32();

        // if (end_mem_ptr != gc_roots_stack_base_ptr)

        // net wasm stack: [] -> []
        expr_builder.global_get(globalidx_gc_roots_stack_ptr);
        expr_builder.local_set(localidx_gc_roots_stack_ptr);

        // net wasm stack: [] -> [end_mem_ptr(i32)]
        expr_builder.global_get(globalidx_end_mem_ptr);
        expr_builder.local_tee(localidx_end_mem_ptr);

        // net wasm stack: [end_mem_ptr(i32)] -> [end_mem_ptr(i32), gc_roots_stack_base_ptr(i32)]
        expr_builder.global_get(globalidx_gc_roots_stack_base_ptr);
        expr_builder.local_tee(localidx_gc_roots_stack_base_ptr);
        // at this point, localidx_gc_roots_stack_ptr, localidx_end_mem_ptr, localidx_gc_roots_stack_base_ptr have all been assigned

        // net wasm stack: [end_mem_ptr(i32), gc_roots_stack_base_ptr(i32)] -> [cond(i32)]
        expr_builder.i32_ne();

        // net wasm stack: [cond(i32)] -> []
        expr_builder.if_(&[]);
        {
            // We are shifting forward
            let localidx_free_mem_ptr = scratch.push_i32();

            // free_mem_ptr = end_mem_ptr;
            // net wasm stack: [] -> []
            expr_builder.local_get(localidx_end_mem_ptr);
            expr_builder.local_tee(localidx_free_mem_ptr);
            expr_builder.global_set(globalidx_free_mem_ptr);

            // end_mem_ptr = gc_roots_stack_base_ptr;
            // net wasm stack: [] -> []
            expr_builder.local_get(localidx_gc_roots_stack_base_ptr);
            expr_builder.local_tee(localidx_end_mem_ptr);
            expr_builder.global_set(globalidx_end_mem_ptr);

            // net wasm stack: [] -> []
            generate_common_portion(
                wasm_module,
                tableidx,
                copy_indirect_table_offset,
                copy_children_table_offset,
                globalidx_free_mem_ptr,
                localidx_free_mem_ptr,
                localidx_gc_roots_stack_base_ptr,
                localidx_gc_roots_stack_ptr,
                copy_funcs,
                global_var_manager,
                heap_begin,
                expr_builder,
                &mut scratch,
            );

            // if (end_mem_ptr - free_mem_ptr >= bytes_required) return 1;
            // net wasm stack: [] -> []
            expr_builder.local_get(localidx_end_mem_ptr);
            expr_builder.local_get(localidx_free_mem_ptr);
            expr_builder.i32_sub();
            expr_builder.local_get(localidx_bytes_required);
            expr_builder.i32_ge_u();
            expr_builder.if_(&[]);
            expr_builder.i32_const(1);
            expr_builder.return_();
            expr_builder.end();

            scratch.pop_i32();
        }
        expr_builder.end();

        // We are shifting backward
        {
            let localidx_free_mem_ptr = scratch.push_i32();

            let constant_base_mem_ptr: u32 = heap_begin << WASM_PAGE_BITS;

            // end_mem_ptr = (gc_roots_stack_base_ptr >> 1) + (base_mem_ptr >> 1);
            // net wasm stack: [] -> []
            expr_builder.local_get(localidx_gc_roots_stack_base_ptr);
            expr_builder.i32_const(1);
            expr_builder.i32_shr_u();
            expr_builder.i32_const((constant_base_mem_ptr >> 1) as i32);
            expr_builder.i32_add();
            expr_builder.local_tee(localidx_end_mem_ptr);
            expr_builder.global_set(globalidx_end_mem_ptr);

            // free_mem_ptr = base_mem_ptr;
            // net wasm stack: [] -> []
            expr_builder.i32_const(constant_base_mem_ptr as i32);
            expr_builder.local_tee(localidx_free_mem_ptr);
            expr_builder.global_set(globalidx_free_mem_ptr);

            // net wasm stack: [] -> []
            generate_common_portion(
                wasm_module,
                tableidx,
                copy_indirect_table_offset,
                copy_children_table_offset,
                globalidx_free_mem_ptr,
                localidx_free_mem_ptr,
                localidx_gc_roots_stack_base_ptr,
                localidx_gc_roots_stack_ptr,
                copy_funcs,
                global_var_manager,
                heap_begin,
                expr_builder,
                &mut scratch,
            );

            // Portion to grow memory if necessary
            {
                let localidx_current_amt = scratch.push_i32();
                let localidx_remaining_amt = scratch.push_i32();

                // let current_amt = free_mem_ptr - base_mem_ptr;
                // net wasm stack: [] -> [current_amt(i32)]
                expr_builder.local_get(localidx_free_mem_ptr);
                expr_builder.i32_const(constant_base_mem_ptr as i32);
                expr_builder.i32_sub();
                expr_builder.local_tee(localidx_current_amt);

                // let remaining_amt = end_mem_ptr - free_mem_ptr;
                // net wasm stack: [current_amt(i32)] -> [current_amt(i32), remaining_amt(i32)]
                expr_builder.local_get(localidx_end_mem_ptr);
                expr_builder.local_get(localidx_free_mem_ptr);
                expr_builder.i32_sub();
                expr_builder.local_tee(localidx_remaining_amt);

                // prepare condition for if-stmt
                // (current_amt > remaining_amt || remaining_amt < bytes_required)
                // net wasm stack: [current_amt(i32), remaining_amt(i32)] -> [cond(i32)]
                expr_builder.i32_gt_u();
                expr_builder.local_get(localidx_remaining_amt);
                expr_builder.local_get(localidx_bytes_required);
                expr_builder.i32_lt_u();
                expr_builder.i32_or();

                // net wasm stack: [cond(i32)] -> []
                expr_builder.if_(&[]);
                {
                    let localidx_required_amt = scratch.push_i32();
                    let localidx_request_delta = scratch.push_i32();

                    {
                        let localidx_max1 = scratch.push_i32();
                        // let required_amt = max(gc_roots_stack_base_ptr - base_mem_ptr, round_up_to_power_of_2(bytes_required + current_amt));
                        // net wasm stack: [] -> [required_amt(i32)]
                        {
                            let localidx_max2 = scratch.push_i32();

                            // net wasm stack: [] -> [max1(i32)]
                            expr_builder.local_get(localidx_gc_roots_stack_base_ptr);
                            expr_builder.i32_const(constant_base_mem_ptr as i32);
                            expr_builder.i32_sub();
                            expr_builder.local_tee(localidx_max1);

                            // we are actually doing: max2 = 1 << (32 - clz(bytes_required + current_amt - 1))
                            // net wasm stack: [max1(i32)] -> [max1(i32), max2(i32)]
                            expr_builder.i32_const(1);
                            expr_builder.i32_const(32);
                            expr_builder.local_get(localidx_bytes_required);
                            expr_builder.local_get(localidx_current_amt);
                            expr_builder.i32_add();
                            expr_builder.i32_const(1);
                            expr_builder.i32_sub();
                            expr_builder.i32_clz();
                            expr_builder.i32_sub();
                            expr_builder.i32_shl();
                            expr_builder.local_tee(localidx_max2);

                            // net wasm stack: [max1(i32), max2(i32)] -> [required_amt(i32)]
                            expr_builder.local_get(localidx_max1);
                            expr_builder.local_get(localidx_max2);
                            expr_builder.i32_ge_u();
                            expr_builder.select();
                            expr_builder.local_tee(localidx_required_amt);

                            scratch.pop_i32();
                        }

                        // let request_delta = (required_amt << 1) - (gc_roots_stack_base_ptr - base_mem_ptr);
                        // net wasm stack: [required_amt(i32)] -> [request_delta(i32)]
                        expr_builder.i32_const(1);
                        expr_builder.i32_shl();
                        expr_builder.local_get(localidx_max1);
                        expr_builder.i32_sub();
                        expr_builder.local_tee(localidx_request_delta);

                        scratch.pop_i32();
                    }

                    // prepare condition for if-stmt
                    // (memory_grow(request_delta >> WASM_PAGE_BITS) != -1)
                    // net wasm stack: [request_delta(i32)] -> [cond(i32)]
                    expr_builder.i32_const(WASM_PAGE_BITS as i32);
                    expr_builder.i32_shr_u();
                    expr_builder.memory_grow(memidx);
                    expr_builder.i32_const(-1);
                    expr_builder.i32_ne();

                    // net wasm stack: [cond(i32)] -> []
                    expr_builder.if_(&[]);
                    {
                        let localidx_it = scratch.push_i32();
                        let localidx_tmp = scratch.push_i32();

                        // end_mem_ptr = base_mem_ptr + required_amt;
                        // net wasm stack: [] -> []
                        expr_builder.local_get(localidx_required_amt);
                        expr_builder.i32_const(constant_base_mem_ptr as i32);
                        expr_builder.i32_add();
                        expr_builder.local_tee(localidx_end_mem_ptr);
                        expr_builder.global_set(globalidx_end_mem_ptr);

                        // let it = gc_roots_stack_base_ptr;
                        // net wasm stack: [] -> [it(i32)]
                        expr_builder.local_get(localidx_gc_roots_stack_base_ptr);
                        expr_builder.local_tee(localidx_it);

                        // gc_roots_stack_base_ptr += request_delta;
                        // let tmp = gc_roots_stack_base_ptr;
                        // note: we don't write back to local cache of `gc_roots_stack_base_ptr` because it will never be used again.
                        // net wasm stack: [] -> []
                        expr_builder.local_get(localidx_gc_roots_stack_base_ptr);
                        expr_builder.local_get(localidx_request_delta);
                        expr_builder.i32_add();
                        expr_builder.local_tee(localidx_tmp);
                        expr_builder.global_set(globalidx_gc_roots_stack_base_ptr);

                        /*
                        while (it != gc_roots_stack_ptr) {
                            tmp->tag = it->tag;
                            tmp->data = it->data;
                            it += 12;
                            tmp += 12;
                        }
                        */
                        // Actually we encode it as:
                        /*
                        if (it != gc_roots_stack_ptr) {
                            do {
                                tmp->tag = it->tag;
                                tmp->data = it->data;
                                tmp += 12;
                                it += 12;
                            } while(it != gc_roots_stack_ptr);
                        }
                        */
                        // net wasm stack: [it(i32)] -> []
                        expr_builder.local_get(localidx_gc_roots_stack_ptr);
                        expr_builder.i32_ne();
                        expr_builder.if_(&[]);
                        {
                            expr_builder.loop_(&[]);
                            {
                                // tmp->tag = it->tag;
                                // net wasm stack: [] -> []
                                expr_builder.local_get(localidx_tmp);
                                expr_builder.local_get(localidx_it);
                                expr_builder.i32_load(wasmgen::MemArg::new4(0));
                                expr_builder.i32_store(wasmgen::MemArg::new4(0));

                                // tmp->data = it->data;
                                // net wasm stack: [] -> []
                                expr_builder.local_get(localidx_tmp);
                                expr_builder.local_get(localidx_it);
                                expr_builder.i64_load(wasmgen::MemArg::new4(4));
                                expr_builder.i64_store(wasmgen::MemArg::new4(4));

                                // tmp += 12;
                                // net wasm stack: [] -> []
                                expr_builder.local_get(localidx_tmp);
                                expr_builder.i32_const(12);
                                expr_builder.i32_add();
                                expr_builder.local_set(localidx_tmp);

                                // it += 12;
                                // net wasm stack: [] -> [it(i32)]
                                expr_builder.local_get(localidx_it);
                                expr_builder.i32_const(12);
                                expr_builder.i32_add();
                                expr_builder.local_tee(localidx_it);

                                // ... while(it != gc_roots_stack_ptr);
                                // net wasm stack: [it(i32)] -> []
                                expr_builder.local_get(localidx_gc_roots_stack_ptr);
                                expr_builder.i32_ne();
                                expr_builder.br_if(0);
                            }
                            expr_builder.end();
                        }
                        expr_builder.end();

                        // gc_roots_stack_ptr = tmp;
                        // no need to write to local cache, because it will never be used again.
                        // net wasm stack: [] -> []
                        expr_builder.local_get(localidx_tmp);
                        expr_builder.global_set(globalidx_gc_roots_stack_ptr);

                        scratch.pop_i32();
                        scratch.pop_i32();
                    }
                    expr_builder.end();

                    scratch.pop_i32();
                    scratch.pop_i32();
                }
                expr_builder.end();

                scratch.pop_i32();
                scratch.pop_i32();
            }

            // return end_mem_ptr - free_mem_ptr >= bytes_required;
            // net wasm stack: [] -> [ret(i32)]
            expr_builder.local_get(localidx_end_mem_ptr);
            expr_builder.local_get(localidx_free_mem_ptr);
            expr_builder.i32_sub();
            expr_builder.local_get(localidx_bytes_required);
            expr_builder.i32_ge_u();
            // automatic return by falling off end of function, no need explicit return instruction.

            scratch.pop_i32();
        }

        scratch.pop_i32();
        scratch.pop_i32();
        scratch.pop_i32();

        expr_builder.end(); // return it
    }
    wasm_module.commit_func(func_idx, code_builder);
    func_idx
}
