pub mod cheney;
pub mod leaky;

use crate::WASM_PAGE_BITS;
use crate::WASM_PAGE_SIZE;

use wasmgen::Scratch;

/**
 * Trait that all heap managers (i.e. garbage collectors) should implement.
 */
pub trait HeapManager {
    // For exposition only (implementers to follow this syntax, but it is not in the trait)
    // Constructs a new HeapManager.
    // This function might add things to the wasm_module (e.g. globals) for use by the GC.
    // `heap_begin`: the lowest index of the heap, in WASM_PAGE_SIZE
    // `heap_initial_end`: initial past-the-end (highest) index of the heap, in WASM_PAGE_SIZE
    // The memory is guaranteed to be constructed in unbounded mode (but might still fail with -1 if the host refuses to give more memory)
    // Note: Some (or maybe most) GCs will maintain a stack called `gc_roots`, where locals that contain pointers will be pushed before calling another function and popped after that function returns.
    // * This allows the GC to know what the roots are, when it needs to run.
    // * This `gc_roots` stack is an implementation detail of the GC.  External code should not assume its existence.
    // `struct_sizes` must be all multiples of 4 bytes.
    /*
    fn new(
        struct_types: &'a [Box<[ir::VarType]>],
        struct_field_byte_offsets: &'b [Box<[u32]>],
        struct_sizes: &'c [u32],
        memidx: wasmgen::MemIdx,
        heap_begin: u32,
        heap_initial_end: u32,
        wasm_module: &mut wasmgen::WasmModule,
    ) -> Self;
    */

    // Returns the initial number of pages required by this heap HeapManager.
    fn initial_heap_size() -> u32;

    // Encodes instructions to get a chunk of memory suitable for the given vartype.
    // Caller is responsible for putting this object on the gc_roots stack if necessary.
    // `ir_vartype`: vartype of the object we want (must be a pointer type, cannot be Any, Boolean, Number, Func).
    // `local_roots`: List of local variables and their corresponding indices in wasm that might hold pointers (i.e. Any and all pointer types).  Guaranteed non-pointer types will be ignored.
    // * These types are only written to the gc_roots stack after we realise that we really need to run the gc.
    // The returned ptr guaranteed to be 4-byte aligned.
    //
    // If `local_roots` is not empty, this function generates code equivalent to, but possibly more efficient to doing this:
    // self.encode_local_roots_prologue(local_roots, expr_builder);
    // self.encode_fixed_allocation(ir_vartype, &[], expr_builder);
    // self.encode_local_roots_elilogue(local_roots, expr_builder);
    //
    // Note: the values in all the variables in `local_roots` must be valid!  This means that the caller should only add it if it has been assigned a value!
    //
    // net wasm stack: [] -> [i32(ptr)]
    fn encode_fixed_allocation(
        &self,
        ir_vartype: ir::VarType,
        local_types: &[ir::VarType],
        local_map: &[usize],
        wasm_local_map: &[wasmgen::LocalIdx],
        scratch: &mut Scratch,
        expr_builder: &mut wasmgen::ExprBuilder,
    );

    // Encodes instructions to get a chunk of memory for an string/array of unknown size.  See `encode_fixed_allocation` for more details.
    // The size need not be a multiple of 4.  (But the allocator will round up to nearest 4-byte boundary.)
    //
    // This function generates code equivalent to, but possibly more efficient to doing this:
    // self.encode_local_roots_prologue(local_roots, expr_builder);
    // self.encode_dynamic_allocation(ir_vartype, &[], expr_builder);
    // self.encode_local_roots_epilogue(local_roots, expr_builder);
    //
    // net wasm stack: [i32(num_bytes)] -> [i32(ptr)]
    fn encode_dynamic_allocation(
        &self,
        ir_vartype: ir::VarType,
        local_types: &[ir::VarType],
        local_map: &[usize],
        wasm_local_map: &[wasmgen::LocalIdx],
        scratch: &mut Scratch,
        expr_builder: &mut wasmgen::ExprBuilder,
    );

    type RootsStackHandle;

    // Encodes instructions to push local variables to gc_roots stack.
    // This should be called before a function which might allocate memory is called.
    // It should be paired with a call to `encode_local_roots_epilogue()`.
    // It is safe to make multiple calls to this function (with different `local_roots`), but the corresponding calls to `encode_local_roots_epilogue()` must be made in the reverse order.  In other words, it works like a stack.
    // Note: the values in all the variables in `local_roots` must be valid!  This means that the caller should only add it if it has been assigned a value!
    // net wasm stack: [] -> []
    fn encode_local_roots_prologue(
        &self,
        local_types: &[ir::VarType],
        local_map: &[usize],
        wasm_local_map: &[wasmgen::LocalIdx],
        scratch: &mut Scratch,
        expr_builder: &mut wasmgen::ExprBuilder,
    ) -> Self::RootsStackHandle;

    // Encodes instructions to pop local variables from gc_roots stack.
    // This should be called after a function which might allocate memory is called.
    // It should be paired with a call to `encode_local_roots_prologue()`.
    // net wasm stack: [] -> []
    fn encode_local_roots_epilogue(
        &self,
        local_types: &[ir::VarType],
        local_map: &[usize],
        wasm_local_map: &[wasmgen::LocalIdx],
        scratch: &mut Scratch,
        expr_builder: &mut wasmgen::ExprBuilder,
    );

    // Encodes instructions to read a local variable from an arbitary position in the gc_roots stack, relative to the past-the-top position.
    // The stack size and content is unchanged.
    // This is not strictly necessary, but may help with optimisations to minimise the number of reads/writes to the stack.
    // Note: the local variable must contain the same data that was there at the previous call to encode_local_root_write or encode_local_roots_prologue, because this will not do anything if the local variable is guaranteed to have it's value unchanged (e.g. mark and sweep GC).
    // Note: `handle` is the value returned from a previous call to encode_local_roots_prologue() with the same local_root array.
    // todo!: This is outdated, `local_root` should be updated to be more like the above functions.  This should be modified if we want to do optimisations.
    // net wasm stack: [] -> []
    fn encode_local_root_read(
        &self,
        local_root: (ir::VarType, wasmgen::LocalIdx),
        handle: Self::RootsStackHandle,
        scratch: &mut Scratch,
        expr_builder: &mut wasmgen::ExprBuilder,
    );

    // Encodes instructions to write a local variable to an arbitary position in the gc_roots stack, relative to the past-the-top position.
    // The stack size is unchanged.
    // This is not strictly necessary, but may help with optimisations to minimise the number of reads/writes to the stack.
    // todo!: This is outdated, `local_root` should be updated to be more like the above functions.  This should be modified if we want to do optimisations.
    // net wasm stack: [] -> []
    fn encode_local_root_write(
        &self,
        local_root: (ir::VarType, wasmgen::LocalIdx),
        handle: Self::RootsStackHandle,
        scratch: &mut Scratch,
        expr_builder: &mut wasmgen::ExprBuilder,
    );

    // Encodes the the conversion of a expr representing a closure to the actual closure i32 value.
    // Typically GCs will want to be able to assume that this is a pointer (or null).
    // net wasm stack: [] -> [i32]
    fn encode_closure_conversion(
        &self,
        vartype: ir::VarType,
        expr_builder: &mut wasmgen::ExprBuilder,
    );

    // Encodes instructions to initialize locals that could potentially go onto the gc_roots stack.
    // For Cheney, this would set all pointers to -1.  Anys are set to unassigned (Note: although wasm zero-initializes things, the local variable might be reused (due to the way Scratch works), so make any assumptions on the existing value.).
    // This is necessary because the first memory allocation might happen before these locals are initialized.
    // Eventually, this function should not be used once there is support for proper local lifetimes - then we should only need to have uninitialized locals for recursion.
    // net wasm stack: [] -> []
    fn encode_local_roots_init(
        &self,
        local_types: &[ir::VarType],
        local_map: &[usize],
        wasm_local_map: &[wasmgen::LocalIdx],
        scratch: &mut Scratch,
        expr_builder: &mut wasmgen::ExprBuilder,
    );
}
