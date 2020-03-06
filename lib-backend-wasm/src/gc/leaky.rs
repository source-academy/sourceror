use super::Scratch;
use super::WASM_PAGE_BITS;
use super::WASM_PAGE_SIZE;

/**
 * Leaky is a GC implementation that does not clean up anything.  It simply allocates more memory every time a request is made.
 * It is very fast, but will keep eating memory.
 * It doubles the total heap space every time it runs out of free space, in order to maintain amortised constant time complexity.
 *
 * Layout of heap:
 * [.....(allocated space).....|.....(free space).....]
 */
pub struct Leaky<'a, 'b, 'c> {
    // Invariant: free_mem_ptr <= end_mem_ptr
    // Invariant: end_mem_ptr % WASM_PAGE_SIZE == 0
    struct_types: &'a [Box<[ir::VarType]>], // types of the fields of each struct type
    struct_field_byte_offsets: &'b [Box<[u32]>], // byte offsets of the fields of each struct type (each Box has same lengths as that of `struct_types`)
    struct_sizes: &'c [u32], // map from typeidx to struct_sizes.  Note: typeidx is not VarType::tag()!  It is the typeidx used in VarType::StructT
    memidx: wasmgen::MemIdx, // MemIdx of the heap that this GC manages (for wasm 1.0, this is always 0)
    free_mem_ptr: wasmgen::GlobalIdx, // Global that stores pointer to start of free space
    end_mem_ptr: wasmgen::GlobalIdx, // Global that stores pointer to past-the-end of heap
    heap_begin: u32,         // in page units
}

impl<'a, 'b, 'c> Leaky<'a, 'b, 'c> {
    // Constructs a new leaky GC, and initializes it appropriately.
    pub fn new(
        struct_types: &'a [Box<[ir::VarType]>],
        struct_field_byte_offsets: &'b [Box<[u32]>],
        struct_sizes: &'c [u32],
        memidx: wasmgen::MemIdx,
        heap_begin: u32,
        heap_initial_end: u32,
        wasm_module: &mut wasmgen::WasmModule,
    ) -> Self {
        Leaky {
            struct_types: struct_types,
            struct_field_byte_offsets: struct_field_byte_offsets,
            struct_sizes: struct_sizes,
            memidx: memidx,
            free_mem_ptr: wasm_module
                .add_i32_global(wasmgen::Mut::Var, (heap_begin * WASM_PAGE_SIZE) as i32),
            end_mem_ptr: wasm_module.add_i32_global(
                wasmgen::Mut::Var,
                (heap_initial_end * WASM_PAGE_SIZE) as i32,
            ),
            heap_begin: heap_begin,
        }
    }

    // Helper function used to encode heap allocation.
    // `f` should be a function that has net wasm stack [] -> [i32(size)]
    // net wasm stack: [] -> [i32(ptr)]
    fn encode_allocation<F: Fn(&mut wasmgen::ExprBuilder) -> ()>(
        &self,
        encode_size: F,
        scratch: &mut Scratch,
        expr_builder: &mut wasmgen::ExprBuilder,
    ) {
        // Algorithm:
        // let ret = free_mem_ptr;
        // free_mem_ptr += size;
        // if (free_mem_ptr > end_mem_ptr) {
        //     let curr_pages = (end_mem_ptr >> WASM_PAGE_BITS) - heap_begin; // i.e. we want to double the heap size
        //     let required_pages = (free_mem_ptr - end_mem_ptr + (WASM_PAGE_SIZE - 1)) >> WASM_PAGE_BITS;
        //     let opt_pages = curr_pages > required_pages ? curr_pages : required_pages;
        //     if(memory_grow(opt_pages) == -1) trap(ERR_OUT_OF_MEMORY);
        //     end_mem_ptr += opt_pages << WASM_PAGE_BITS;
        // }
        // return ret;

        let localidx_free_mem_ptr: wasmgen::LocalIdx = scratch.push_i32();
        let localidx_end_mem_ptr: wasmgen::LocalIdx = scratch.push_i32();
        // net wasm stack: [] -> [ret]
        expr_builder.global_get(self.free_mem_ptr);
        // net wasm stack: [ret] -> [ret]
        expr_builder.local_tee(localidx_free_mem_ptr);
        // net wasm stack: [ret] -> [ret, ret]
        expr_builder.local_get(localidx_free_mem_ptr);
        // net wasm stack: [ret, ret] -> [ret, ret, size]
        encode_size(expr_builder);
        // net wasm stack: [ret, ret, size] -> [ret, free_mem_ptr]
        expr_builder.i32_add();
        // net wasm stack: [ret, free_mem_ptr] -> [ret, free_mem_ptr]
        expr_builder.local_tee(localidx_free_mem_ptr);
        // net wasm stack: [ret, free_mem_ptr] -> [ret, free_mem_ptr, end_mem_ptr]
        expr_builder.global_get(self.end_mem_ptr);
        // net wasm stack: [ret, free_mem_ptr, end_mem_ptr] -> [ret, free_mem_ptr, end_mem_ptr]
        expr_builder.local_tee(localidx_end_mem_ptr);
        // at this point, all the locals have fixed values
        // net wasm stack: [ret, free_mem_ptr, end_mem_ptr] -> [ret, boolean]
        expr_builder.i32_gt_u();
        // net wasm stack: [ret, boolean] -> [ret]
        expr_builder.if_(&[]);
        // START IF
        {
            let localidx_curr_pages: wasmgen::LocalIdx = scratch.push_i32();
            let localidx_required_pages: wasmgen::LocalIdx = scratch.push_i32();
            let localidx_opt_pages: wasmgen::LocalIdx = scratch.push_i32();
            // net wasm stack (for following 6 instructions) : [] -> [curr_pages]
            expr_builder.local_get(localidx_end_mem_ptr);
            expr_builder.i32_const(WASM_PAGE_BITS as i32);
            expr_builder.i32_shr_u();
            expr_builder.i32_const(self.heap_begin as i32);
            expr_builder.i32_sub();
            expr_builder.local_tee(localidx_curr_pages);
            // net wasm stack (for following 8 instructions) : [curr_pages] -> [curr_pages, required_pages]
            expr_builder.local_get(localidx_free_mem_ptr);
            expr_builder.local_get(localidx_end_mem_ptr);
            expr_builder.i32_sub();
            expr_builder.i32_const((WASM_PAGE_SIZE - 1) as i32);
            expr_builder.i32_add();
            expr_builder.i32_const(WASM_PAGE_BITS as i32);
            expr_builder.i32_shr_u();
            expr_builder.local_tee(localidx_required_pages);
            // net wasm stack (for following 4 instructions) : [curr_pages, required_pages] -> [opt_pages]
            expr_builder.local_get(localidx_curr_pages);
            expr_builder.local_get(localidx_required_pages);
            expr_builder.i32_gt_u();
            expr_builder.select();
            expr_builder.local_tee(localidx_opt_pages);
            // net wasm stack (for following 6 instructions) : [opt_pages] -> []
            expr_builder.memory_grow(self.memidx);
            expr_builder.i32_const(-1);
            expr_builder.i32_eq();
            expr_builder.if_(&[]);
            expr_builder.unreachable(); // todo!("should pass an error code to the host environment")
            expr_builder.end();
            // net wasm stack (for following 6 instructions) : [] -> []
            expr_builder.local_get(localidx_end_mem_ptr);
            expr_builder.local_get(localidx_opt_pages);
            expr_builder.i32_const(WASM_PAGE_BITS as i32);
            expr_builder.i32_shl();
            expr_builder.i32_add();
            expr_builder.global_set(self.end_mem_ptr);

            scratch.pop_i32();
            scratch.pop_i32();
            scratch.pop_i32();
        }
        // END IF
        expr_builder.end();

        // write the free_mem_ptr back:
        // net wasm stack (for following 2 instructions) : [] -> []
        expr_builder.local_get(localidx_free_mem_ptr);
        expr_builder.global_set(self.free_mem_ptr);

        scratch.pop_i32();
        scratch.pop_i32();
    }
}

impl<'a, 'b, 'c> super::HeapManager for Leaky<'a, 'b, 'c> {
    // Encodes instructions to get a chunk of memory suitable for the given struct type specified by ir_vartype.
    // It is guaranteed to be 4-byte aligned.
    // net wasm stack: [] -> [i32(ptr)]
    fn encode_fixed_allocation(
        &self,
        ir_vartype: ir::VarType,
        _local_roots: &[(ir::VarType, wasmgen::LocalIdx)],
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
                        expr_builder.i32_const(size as i32);
                    },
                    scratch,
                    expr_builder,
                );

                // Write Undefined to all Any fields in the struct
                // net wasm stack: [i32(ptr)] -> [i32(ptr)]
                {
                    let localidx_ptr: wasmgen::LocalIdx = scratch.push_i32();
                    expr_builder.local_tee(localidx_ptr);
                    self.struct_types[typeidx]
                        .iter()
                        .zip(self.struct_field_byte_offsets[typeidx].iter())
                        .for_each(|(ir_vartype, byte_offset)| {
                            if *ir_vartype == ir::VarType::Any {
                                expr_builder.local_get(localidx_ptr);
                                expr_builder.i32_const(ir::VarType::Unassigned.tag());
                                expr_builder.i32_store(wasmgen::MemArg::new4(*byte_offset));
                            }
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
        _local_roots: &[(ir::VarType, wasmgen::LocalIdx)],
        scratch: &mut Scratch,
        expr_builder: &mut wasmgen::ExprBuilder,
    ) {
        match ir_vartype {
            ir::VarType::String => {
                let localidx_mem_size: wasmgen::LocalIdx = scratch.push_i32();
                // Algorithm: mem_size = ((num_bytes + 7) & (~3))   // equivalent to (4 + round_up_to_multiple_of_4(num_bytes))
                // net wasm stack: [i32(num_bytes)] => []
                expr_builder.i32_const(7);
                expr_builder.i32_add();
                expr_builder.i32_const(-4); // equivalent to (~3) in two's complement
                expr_builder.i32_and();
                expr_builder.local_set(localidx_mem_size);

                self.encode_allocation(
                    |expr_builder| {
                        // net wasm stack: [] -> [i32(size)]
                        expr_builder.local_get(localidx_mem_size);
                    },
                    scratch,
                    expr_builder,
                );

                scratch.pop_i32();

                // Note: Can leave the memory undefined, since there is no garbage collection in Leaky.
            }
            _ => panic!("incorrect VarType, expected String"),
        }
    }

    // Encodes instructions to push local variables to gc_roots stack.
    // This should be called before a function which might allocate memory is called.
    // It should be paired with a call to `encode_local_roots_elilogue()`.
    // net wasm stack: [] -> []
    fn encode_local_roots_prologue(
        &self,
        _local_roots: &[(ir::VarType, wasmgen::LocalIdx)],
        _scratch: &mut Scratch,
        _expr_builder: &mut wasmgen::ExprBuilder,
    ) {
        // Do nothing - because our memory manager will never collect garbage.  The garbage will leak.
    }

    // Encodes instructions to pop local variables from gc_roots stack.
    // This should be called after a function which might allocate memory is called.
    // It should be paired with a call to `encode_local_roots_prologue()`.
    // net wasm stack: [] -> []
    fn encode_local_roots_epilogue(
        &self,
        _local_roots: &[(ir::VarType, wasmgen::LocalIdx)],
        _scratch: &mut Scratch,
        _expr_builder: &mut wasmgen::ExprBuilder,
    ) {
        // Do nothing - because our memory manager will never collect garbage.  The garbage will leak.
    }

    // Encodes instructions to read a local variable from an arbitary position in the gc_roots stack, relative to the past-the-top position.
    // net wasm stack: [] -> []
    fn encode_local_root_read(
        &self,
        _local_root: (ir::VarType, wasmgen::LocalIdx),
        _depth: u32,
        _scratch: &mut Scratch,
        _expr_builder: &mut wasmgen::ExprBuilder,
    ) {
        // Do nothing - because our memory manager will never collect garbage.  The garbage will leak.
	}

    // Encodes instructions to write a local variable to an arbitary position in the gc_roots stack, relative to the past-the-top position.
    // net wasm stack: [] -> []
    fn encode_local_root_write(
        &self,
        _local_root: (ir::VarType, wasmgen::LocalIdx),
        _depth: u32,
        _scratch: &mut Scratch,
        _expr_builder: &mut wasmgen::ExprBuilder,
    ) {
        // Do nothing - because our memory manager will never collect garbage.  The garbage will leak.
	}
}
