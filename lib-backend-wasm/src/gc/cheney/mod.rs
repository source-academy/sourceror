use super::HeapManager;
use super::WASM_PAGE_BITS;
use super::WASM_PAGE_SIZE;
use wasmgen::Scratch;

use crate::var_conv::*;

mod copy_children_elements;
mod copy_funcs;
mod copy_indirect_elements;
mod do_cheney;

#[cfg(feature = "wasmtest")]
pub mod wasmtest;

/**
 * Cheney is a GC implementation that uses Cheney's algorithm.
 * At every alternate run of the GC, it will ensure that it has as much free space as used space (not including the swap space).
 *
 * Layout of heap (when using the lower half):
 * [.....(allocated space).....|.....(free space).....|.....(swap space).....|.....(gc roots).....]
 * Layout of heap (when using the higher half):
 * [.....(swap space).....|.....(allocated space).....|.....(free space).....|.....(gc roots).....]
 * `allocated space`: the memory that has been allocated to the program via encode_fixed_allocation() or encode_dynamic_allocation().
 * `free space`: memory that has not been allocated to the program yet.
 * `swap space`: the half of the memory that shall never be used until the GC runs.
 * Invariant: allocated_space + free_space = swap_space.
 * `gc_roots`: stack containing roots of the GC breadth-first search algorithm, these variables are declared to be 'alive' by the program.
 * * This stack grow upwards, so that it will trap automatically if the stack overflows.
 * * Values are stored as 'Any' format.
 * After a GC run that changes the heap from higher-half to lower-half, the algorithm will check if (free_space >= allocated_space).
 * * If not, it will grow the memory and move gc_roots rightward in order to ensure (free_space >= allocated_space).
 * When allocating memory, a tag is placed at *(ptr-4) to specify the type of content being contained there.  It is used by the BFS in do_cheney() to call indirectly the correct function.
 *
 * Two functions will be generated for each type:
 * * Direct function
 * * Indirect function
 * The direct function will take parameters with types as defined in the backend-wasm documentation (e.g. Number will take one f64, Func will take two i32s).
 * The indirect function is meant for Anys, it will take one i64(data).
 */
pub struct Cheney<'a, 'b, 'c> {
    struct_types: &'a [Box<[ir::VarType]>], // types of the fields of each struct type
    struct_field_byte_offsets: &'b [Box<[u32]>], // byte offsets of the fields of each struct type (each Box has same lengths as that of `struct_types`)
    struct_sizes: &'c [u32], // map from typeidx to struct_sizes.  Note: typeidx is not VarType::tag()!  It is the typeidx used in VarType::StructT
    memidx: wasmgen::MemIdx, // MemIdx of the heap that this GC manages (for wasm 1.0, this is always 0)
    free_mem_ptr: wasmgen::GlobalIdx, // Global that stores pointer to start of free space
    end_mem_ptr: wasmgen::GlobalIdx, // Global that stores pointer to past-the-end of free space
    gc_roots_stack_base_ptr: wasmgen::GlobalIdx, // Global that stores pointer to beginning of gc_roots stack
    gc_roots_stack_ptr: wasmgen::GlobalIdx, // Global that stores pointer to past-the-end of gc_roots stack
    heap_begin: u32,                        // in page units
    do_cheney_funcidx: wasmgen::FuncIdx,    // funcidx of do_cheney() function
}

// Note: Currently  MEM_INITIAL_USABLE_SIZE * 2 should be at least as large as the gc_roots size (1 << 4).
//   Otherwise, we must rewrite the part in do_cheney() to move the gc_stack with move_backward() instead of move().
const MEM_INITIAL_USABLE_SIZE: u32 = 1 << 4; // the allocated_space+free_space
const MEM_INITIAL_HEAP_SIZE: u32 = MEM_INITIAL_USABLE_SIZE * 2 + (1 << 4); // 2 MiB of initial heap space (1 MiB usable at a time) and 1 MiB of gc_roots stack space

impl<'a, 'b, 'c> Cheney<'a, 'b, 'c> {
    // Constructs a new Cheney GC, and initializes it appropriately.

    pub fn new(
        struct_types: &'a [Box<[ir::VarType]>],
        struct_field_byte_offsets: &'b [Box<[u32]>],
        struct_sizes: &'c [u32],
        memidx: wasmgen::MemIdx,
        heap_begin: u32,
        heap_initial_end: u32,
        wasm_module: &mut wasmgen::WasmModule,
    ) -> Self {
        assert!(heap_begin + MEM_INITIAL_HEAP_SIZE == heap_initial_end);
        //todo! generate the gc functions for all struct types
        /*
        // copy_children_$i shall only exist for pointer types (i.e. types that reside on heap), i.e. StructT or String.
        Encoded function (for struct):
        // ptr is the start of the struct (excluding the tag)
        // returns a ptr to past-the-end of the struct (which is the tag of the next struct)
        // $i is the VarType::tag() (so Any doesn't have any of these functions)
        fn copy_children_$i(ptr: i32) -> i32 {
            for each field f in *ptr {
                if constexpr f has type Any {
                    f.data = (*(GC_TABLE_PTR_COPY_INDIRECT_OFFSET + f.tag))(f.data);
                } else if constexpr f is Func {
                    if (f.closure != -1) {
                        if (*(f.closure-4)) & I32_MIN {
                            f.closure = (*(f.closure-4)) << 1; // we don't know the closure type, but we point to the new one anyway.
                        } else {
                            f.closure = i32_wrap_i64((*(GC_TABLE_PTR_COPY_INDIRECT_OFFSET + *(f.closure-4)))(i64_extend_i32(f.closure)));
                        }
                    }
                } else if constexpr f is a ptr type {
                    if (ptr != -1) {
                        if (*(ptr-4)) & I32_MIN { // already copied (we multiplex the MSB of the tag field, since there shouldn't be more than 2^31 types)
                            f.ptr = (*(ptr-4)) << 1; // we store the ptr in the tag, but shifted right by one bit position (valid since ptr are all multiple of 4)
                        } else {
                            f.ptr = copy_${tag of f}(f.ptr);
                        }
                    }
                }
            }
        }
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
                } else return make_func(f.idx, -1);
            } else if constexpr $i is not a ptr (i.e. not StructT or string) {
                // NO-OP
            } else {
                if (ptr != -1) {
                    if (*(ptr-4)) & I32_MIN { // already copied (we multiplex the MSB of the tag field, since there shouldn't be more than 2^31 types)
                        return to_any_data((*(ptr-4)) << 1); // we store the ptr in the tag, but shifted right by one bit position (valid since ptr are all multiple of 4)
                    } else {
                        return to_any_data(copy_$i(from_any_data(data)));
                    }
                } else return -1;
            }
        }
        // copy_children_$i shall only exist for pointer types (i.e. types that reside on heap), i.e. StructT or String.
        // returns the new location
        // todo! maybe inline this function.  Copying most stuff (except strings perhaps) will be short.
        fn copy_$i(ptr: i32) -> i32 { // don't generate function for non-ptr types.
            let new_ptr = free_mem_ptr + 4; // skip the tag
            ptr-=4; // rewind to get the tag
            free_mem_ptr = move(ptr, ptr + 4 + sizeof($i), free_mem_ptr); // move everything, including the tag.
            (*ptr) = I32_MIN | (new_ptr >> 1); // say that we already copied it.
            return new_ptr;
        }
        // `bytes_required`: min number of bytes we want (including the tag!)
        // returns nonzero if successfully made enough space, otherwise zero.
        fn do_cheney(bytes_required: u32) -> i32 {
            if (end_mem_ptr != gc_roots_stack_base_ptr) {
                // shift forwards
                free_mem_ptr = end_mem_ptr; // note: must write back free_mem_ptr
                end_mem_ptr = gc_roots_stack_base_ptr; // note: must write back end_mem_ptr
                // note: ensure free_mem_ptr is written to global before the following lines (because copy_$i modifies it)

                // common section (start)
                let scan = free_mem_ptr;

                let gc_roots_it = gc_roots_stack_base_ptr;
                while (gc_roots_it != gc_roots_stack_ptr) {
                    let f = *gc_roots_it;
                    f.data = (*(GC_TABLE_PTR_COPY_INDIRECT_OFFSET + f.tag))(f.data);
                    gc_roots_it += 12; // 12 is the size of Any
                }
                // note: must reload free_mem_ptr after every iteration, it is modified in the function call
                while (scan != free_mem_ptr) {
                    scan = (*(GC_TABLE_PTR_COPY_CHILDREN_OFFSET + *scan))(scan+4);
                }
                // common section (end)

                if (end_mem_ptr - free_mem_ptr >= bytes_required) return 1;
            }
            {
                // shift backwards
                let base_mem_ptr = heap_begin * WASM_PAGE_SIZE;
                end_mem_ptr = (gc_roots_stack_base_ptr >> 1) + (base_mem_ptr >> 1); // average of gc_roots_stack_base_ptr and base_mem_ptr.
                free_mem_ptr = base_mem_ptr;
                // note: ensure free_mem_ptr is written to global before the following lines (because copy_$i modifies it)

                // common section (start)
                let scan = free_mem_ptr;

                let gc_roots_it = gc_roots_stack_base_ptr;
                while (gc_roots_it != gc_roots_stack_ptr) {
                    let f = *gc_roots_it;
                    f.data = (*(GC_TABLE_PTR_COPY_INDIRECT_OFFSET + f.tag))(f.data);
                    gc_roots_it += 12; // 12 is the size of Any
                }
                // note: must reload free_mem_ptr every iteration, it is modified in the function call
                while (scan != free_mem_ptr) {
                    scan = (*(GC_TABLE_PTR_COPY_CHILDREN_OFFSET + *scan))(scan+4);
                }
                // common section (end)

                // if memory is still more than half full, reserve more memory:
                let current_amt = free_mem_ptr - base_mem_ptr;
                let remaining_amt = end_mem_ptr - free_mem_ptr;
                if (current_amt > remaining_amt || remaining_amt < bytes_required) {
                    // get twice the existing amount of heap memory if possible (note: both are powers of two, so max() can be optimised)
                    let required_amt = max(gc_roots_stack_base_ptr - base_mem_ptr, round_up_to_power_of_2(bytes_required + current_amt));
                    let request_delta = (required_amt << 1) - (end_mem_ptr - base_mem_ptr);
                    if (memory_grow(request_delta >> WASM_PAGE_BITS) != -1) {
                        end_mem_ptr = base_mem_ptr + required_amt; // rmb to write back to global
                        // copy over the gc_roots stack
                        let it = gc_roots_stack_base_ptr;
                        gc_roots_stack_base_ptr += request_delta; // note: existing stack will not be overwritten, so move()/move_backward() both work. // rmb to write back to global
                        let tmp = gc_roots_stack_base_ptr;
                        while (it != gc_roots_stack_ptr) {
                            tmp->tag = it->tag;
                            tmp->data = it->data;
                            it += 12;
                            tmp += 12;
                        }
                        gc_roots_stack_ptr = tmp; // rmb to write back to global
                    }
                }
                return end_mem_ptr - free_mem_ptr >= bytes_required;
            }
        }
        Note: For type like Unassigned, Undefined, Number, Boolean that are not a ptr, the indirect function is a no-op, and it doesn't have copy_$i or copy_children_$i.
        For Func, it has copy_indirect_$i (which will simply forward to the closure struct), but not copy_$i (copy_children should invoke the closure directly).  It also doesn't have copy_children_$i.
        Any does not have copy_$i and copy_indirect_$i (since copy_indirect_$i is suppose to indirectly determine the type of the any)
        */

        let free_mem_ptr: wasmgen::GlobalIdx =
            wasm_module.add_i32_global(wasmgen::Mut::Var, (heap_begin * WASM_PAGE_SIZE) as i32);
        let end_mem_ptr: wasmgen::GlobalIdx = wasm_module.add_i32_global(
            wasmgen::Mut::Var,
            ((heap_begin + MEM_INITIAL_USABLE_SIZE) * WASM_PAGE_SIZE) as i32,
        );
        let gc_roots_stack_base_ptr: wasmgen::GlobalIdx = wasm_module.add_i32_global(
            wasmgen::Mut::Var,
            ((heap_begin + MEM_INITIAL_USABLE_SIZE * 2) * WASM_PAGE_SIZE) as i32,
        );
        let gc_roots_stack_ptr: wasmgen::GlobalIdx = wasm_module.add_i32_global(
            wasmgen::Mut::Var,
            ((heap_begin + MEM_INITIAL_USABLE_SIZE * 2) * WASM_PAGE_SIZE) as i32,
        );

        // copy_$i functions, indexed by VarType::tag().
        let copy_funcs: Box<[Option<wasmgen::FuncIdx>]> =
            copy_funcs::make_copy_funcs(wasm_module, struct_sizes, free_mem_ptr);
        assert!(copy_funcs.len() == ir::NUM_PRIMITIVE_TAG_TYPES + struct_sizes.len());

        let tableidx: wasmgen::TableIdx = wasm_module.get_or_add_table();

        let copy_indirect_table_offset: u32 = copy_indirect_elements::make_copy_indirect_elements(
            wasm_module,
            struct_sizes.len(),
            &copy_funcs,
            tableidx,
        );

        let copy_children_table_offset: u32 = copy_children_elements::make_copy_children_elements(
            wasm_module,
            struct_types,
            struct_field_byte_offsets,
            struct_sizes,
            tableidx,
            copy_indirect_table_offset,
            &copy_funcs,
        );

        let do_cheney_funcidx: wasmgen::FuncIdx = do_cheney::make_do_cheney(
            wasm_module,
            tableidx,
            copy_indirect_table_offset,
            copy_children_table_offset,
            memidx,
            free_mem_ptr,
            end_mem_ptr,
            gc_roots_stack_base_ptr,
            gc_roots_stack_ptr,
            heap_begin,
        );

        Cheney {
            struct_types: struct_types,
            struct_field_byte_offsets: struct_field_byte_offsets,
            struct_sizes: struct_sizes,
            memidx: memidx,
            free_mem_ptr: free_mem_ptr,
            end_mem_ptr: end_mem_ptr,
            gc_roots_stack_base_ptr: gc_roots_stack_base_ptr,
            gc_roots_stack_ptr: gc_roots_stack_ptr,
            heap_begin: heap_begin,
            do_cheney_funcidx: do_cheney_funcidx,
        }
    }

    fn filter_roots(
        local_roots: &[(ir::VarType, wasmgen::LocalIdx)],
    ) -> Box<[(ir::VarType, wasmgen::LocalIdx)]> {
        local_roots
            .iter()
            .cloned()
            .filter(|(ir_vartype, wasm_localidx)| match ir_vartype {
                ir::VarType::Unassigned
                | ir::VarType::Undefined
                | ir::VarType::Number
                | ir::VarType::Boolean => false,
                _ => true,
            })
            .collect()
    }

    // Helper function used to encode heap allocation.
    // `f` should be a function that has net wasm stack [] -> [i32(size)], it pushes the bytes required (including tag) on the stack.
    // net wasm stack: [] -> [i32(ptr)]
    fn encode_allocation<F: Fn(&mut wasmgen::ExprBuilder) -> ()>(
        &self,
        encode_size: F,
        local_roots: &[(ir::VarType, wasmgen::LocalIdx)],
        scratch: &mut Scratch,
        expr_builder: &mut wasmgen::ExprBuilder,
    ) {
        // Algorithm:
        /*
        if (end_mem_ptr - free_mem_ptr < size) {
            for local in local_roots {
                if(local is Any, String, Func, or StructT) {
                    *gc_roots_stack_ptr = to_any(local);
                    gc_roots_stack_ptr += 12;
                }
            }
            // note: `do_cheney` might change all the global variables, so all cache must be reloaded after calling it.
            if(do_cheney(size)) {
                for local in local_roots.reversed() {
                    if(local is Any, String, Func, or StructT) {
                        gc_roots_stack_ptr -= 12;
                        local = from_any(*gc_roots_stack_ptr);
                    }
                }
                goto label;
            }
            abort();
        }
        label:
        ret = free_mem_ptr; // ret is the value that is left on the stack
        free_mem_ptr += size;
        ret += 4;
        */

        // (end_mem_ptr - free_mem_ptr < size)
        // net wasm stack: [] -> [cond(i32)]
        expr_builder.global_get(self.end_mem_ptr);
        expr_builder.global_get(self.free_mem_ptr);
        expr_builder.i32_sub();
        encode_size(expr_builder);
        expr_builder.i32_lt_u();

        // net wasm stack: [cond(i32)] -> []
        expr_builder.if_(&[]);
        {
            // save the new values of all the filtered roots on the gc_roots stack
            // net wasm stack: [] -> []
            self.encode_local_roots_prologue(local_roots, scratch, expr_builder);

            // net wasm stack: [] -> [size(i32)]
            encode_size(expr_builder);

            // net wasm stack: [size(i32)] -> [success(i32)]
            expr_builder.call(self.do_cheney_funcidx);

            // net wasm stack: [success(i32)] -> []
            expr_builder.if_(&[]);
            {
                // load back the new values of all the filtered roots
                // net wasm stack: [] -> []
                self.encode_local_roots_epilogue(local_roots, scratch, expr_builder);

                // net wasm stack: [] -> []
                expr_builder.br(1);
            }
            expr_builder.end();

            // todo!(call a proper error handler with error code)
            // net wasm stack: [] -> []
            expr_builder.unreachable();
        }
        expr_builder.end();

        // net wasm stack: [] -> [res(i32)]
        expr_builder.global_get(self.free_mem_ptr);
        expr_builder.global_get(self.free_mem_ptr);
        encode_size(expr_builder);
        expr_builder.i32_add();
        expr_builder.global_set(self.free_mem_ptr);
        expr_builder.i32_const(4);
        expr_builder.i32_add();
    }
}

impl<'a, 'b, 'c> HeapManager for Cheney<'a, 'b, 'c> {
    // Returns the initial number of pages required by this heap HeapManager.
    fn initial_heap_size() -> u32 {
        MEM_INITIAL_HEAP_SIZE
    }

    // Encodes instructions to get a chunk of memory suitable for the given struct type specified by ir_vartype.
    // It is guaranteed to be 4-byte aligned.
    // net wasm stack: [] -> [i32(ptr)]
    fn encode_fixed_allocation(
        &self,
        ir_vartype: ir::VarType,
        local_roots: &[(ir::VarType, wasmgen::LocalIdx)],
        scratch: &mut Scratch,
        expr_builder: &mut wasmgen::ExprBuilder,
    ) {
        match ir_vartype {
            ir::VarType::StructT { typeidx } => {
                let size = self.struct_sizes[typeidx];
                assert!((size & 3) == 0, "struct size must be multiple of 4");
                // net wasm stack: [] -> [i32(ptr)]
                self.encode_allocation(
                    |expr_builder| {
                        // net wasm stack: [] -> [i32(size)]
                        expr_builder.i32_const((size + 4) as i32);
                    },
                    local_roots,
                    scratch,
                    expr_builder,
                );

                // Write Undefined to all Any fields in the struct
                // and write nullptr (i.e. -1) to all String, Func::closure, StructT
                // todo!: String should eventually be set to an empty string in the constant string pool.
                // net wasm stack: [i32(ptr)] -> [i32(ptr)]
                {
                    let localidx_ptr: wasmgen::LocalIdx = scratch.push_i32();
                    expr_builder.local_tee(localidx_ptr);
                    self.struct_types[typeidx]
                        .iter()
                        .zip(self.struct_field_byte_offsets[typeidx].iter())
                        .for_each(|(ir_vartype, byte_offset)| match ir_vartype {
                            ir::VarType::Any => {
                                expr_builder.local_get(localidx_ptr);
                                expr_builder.i32_const(ir::VarType::Unassigned.tag());
                                expr_builder.i32_store(wasmgen::MemArg::new4(*byte_offset));
                            }
                            ir::VarType::String | ir::VarType::StructT { typeidx: _ } => {
                                expr_builder.local_get(localidx_ptr);
                                expr_builder.i32_const(-1);
                                expr_builder.i32_store(wasmgen::MemArg::new4(*byte_offset));
                            }
                            ir::VarType::Func => {
                                expr_builder.local_get(localidx_ptr);
                                expr_builder.i32_const(-1);
                                expr_builder.i32_store(wasmgen::MemArg::new4(*byte_offset + 4));
                                // Note: "+4" above to access the closure
                            }
                            _ => {}
                        });
                    scratch.pop_i32();
                }
            }
            _ => panic!("incorrect VarType, expected StructT"),
        }
    }

    // Encodes instructions to get a chunk of memory for an string/array of unknown size.  See `encode_fixed_allocation` for more detauls.
    // The size need not be a multiple of 4.
    // net wasm stack: [i32(num_bytes)] -> [i32(ptr)]
    fn encode_dynamic_allocation(
        &self,
        ir_vartype: ir::VarType,
        local_roots: &[(ir::VarType, wasmgen::LocalIdx)],
        scratch: &mut Scratch,
        expr_builder: &mut wasmgen::ExprBuilder,
    ) {
        match ir_vartype {
            ir::VarType::String => {
                let localidx_mem_size: wasmgen::LocalIdx = scratch.push_i32();
                // Algorithm: mem_size = ((num_bytes + 11) & (~3))   // equivalent to (4 + round_up_to_multiple_of_4(num_bytes))
                // net wasm stack: [i32(num_bytes)] -> []
                expr_builder.i32_const(11);
                expr_builder.i32_add();
                expr_builder.i32_const(-4); // equivalent to (~3) in two's complement
                expr_builder.i32_and();
                expr_builder.local_set(localidx_mem_size);

                // net wasm stack: [] -> [i32(ptr)]
                self.encode_allocation(
                    |expr_builder| {
                        // net wasm stack: [] -> [i32(size)]
                        expr_builder.local_get(localidx_mem_size);
                    },
                    local_roots,
                    scratch,
                    expr_builder,
                );

                // write the string length
                // net wasm stack: [i32(ptr)] -> [i32(ptr)]
                {
                    let localidx_ret: wasmgen::LocalIdx = scratch.push_i32();
                    expr_builder.local_tee(localidx_ret);
                    expr_builder.local_get(localidx_ret);
                    expr_builder.local_get(localidx_mem_size);
                    expr_builder.i32_store(wasmgen::MemArg::new4(0));
                    scratch.pop_i32();
                }

                scratch.pop_i32();
            }
            _ => panic!("incorrect VarType, expected String"),
        }
    }

    // todo!: Return the correct u32 relative offset.  This might need a function-specific wrapper on this GC class, or to pass it a local stack, or to disable filtering of locals.
    type RootsStackHandle = (); //should be `u32` eventually;

    // Encodes instructions to push local variables to gc_roots stack.
    // This should be called before a function which might allocate memory is called.
    // It should be paired with a call to `encode_local_roots_elilogue()`.
    // net wasm stack: [] -> []
    fn encode_local_roots_prologue(
        &self,
        local_roots: &[(ir::VarType, wasmgen::LocalIdx)],
        scratch: &mut Scratch,
        expr_builder: &mut wasmgen::ExprBuilder,
    ) -> Self::RootsStackHandle {
        let filtered_roots: Box<[(ir::VarType, wasmgen::LocalIdx)]> =
            Self::filter_roots(local_roots);

        // if there are no roots to add, then we don't need to load the gc_roots_stack_ptr.
        // net wasm stack: [] -> []
        if !filtered_roots.is_empty() {
            let localidx_gc_roots_stack_ptr = scratch.push_i32();

            // net wasm stack: [] -> [gc_roots_stack_ptr(i32)]
            expr_builder.global_get(self.gc_roots_stack_ptr);

            for (ir_vartype, wasm_localidx) in filtered_roots.into_iter().cloned() {
                // net wasm stack: [gc_roots_stack_ptr(i32)] -> []
                expr_builder.local_tee(localidx_gc_roots_stack_ptr);
                encode_load_local(wasm_localidx, ir_vartype, ir_vartype, expr_builder);
                encode_store_memory(0, ir::VarType::Any, ir_vartype, scratch, expr_builder);

                // net wasm stack: [] -> [gc_roots_stack_ptr(i32)]
                expr_builder.local_get(localidx_gc_roots_stack_ptr);
                expr_builder.i32_const(12);
                expr_builder.i32_add();
            }

            // net wasm stack: [gc_roots_stack_ptr(i32)] -> []
            expr_builder.global_set(self.gc_roots_stack_ptr);

            scratch.pop_i32();
        }
    }

    // Encodes instructions to pop local variables from gc_roots stack.
    // This should be called after a function which might allocate memory is called.
    // It should be paired with a call to `encode_local_roots_prologue()`.
    // net wasm stack: [] -> []
    fn encode_local_roots_epilogue(
        &self,
        local_roots: &[(ir::VarType, wasmgen::LocalIdx)],
        scratch: &mut Scratch,
        expr_builder: &mut wasmgen::ExprBuilder,
    ) {
        let filtered_roots: Box<[(ir::VarType, wasmgen::LocalIdx)]> =
            Self::filter_roots(local_roots);

        if !filtered_roots.is_empty() {
            let localidx_gc_roots_stack_ptr = scratch.push_i32();

            // net wasm stack: [] -> [gc_roots_stack_ptr(i32)]
            expr_builder.global_get(self.gc_roots_stack_ptr);

            for (ir_vartype, wasm_localidx) in filtered_roots.into_iter().cloned().rev() {
                // net wasm stack: [gc_roots_stack_ptr(i32)] -> [gc_roots_stack_ptr(i32)]
                expr_builder.i32_const(12);
                expr_builder.i32_sub();

                // net wasm stack: [gc_roots_stack_ptr(i32)] -> []
                expr_builder.local_tee(localidx_gc_roots_stack_ptr);
                encode_load_memory(0, ir::VarType::Any, ir_vartype, scratch, expr_builder);
                encode_store_local(wasm_localidx, ir_vartype, ir_vartype, expr_builder);

                // net wasm stack: [] -> [gc_roots_stack_ptr(i32)]
                expr_builder.local_get(localidx_gc_roots_stack_ptr);
            }

            // net wasm stack: [gc_roots_stack_ptr(i32)] -> []
            expr_builder.global_set(self.gc_roots_stack_ptr);

            scratch.pop_i32();
        }
    }

    // Encodes instructions to read a local variable from an arbitary position in the gc_roots stack, relative to the past-the-top position.
    // net wasm stack: [] -> []
    fn encode_local_root_read(
        &self,
        _local_root: (ir::VarType, wasmgen::LocalIdx),
        _handle: Self::RootsStackHandle,
        _scratch: &mut Scratch,
        _expr_builder: &mut wasmgen::ExprBuilder,
    ) {
        todo!();
    }

    // Encodes instructions to write a local variable to an arbitary position in the gc_roots stack, relative to the past-the-top position.
    // net wasm stack: [] -> []
    fn encode_local_root_write(
        &self,
        _local_root: (ir::VarType, wasmgen::LocalIdx),
        _handle: Self::RootsStackHandle,
        _scratch: &mut Scratch,
        _expr_builder: &mut wasmgen::ExprBuilder,
    ) {
        todo!();
    }
}
