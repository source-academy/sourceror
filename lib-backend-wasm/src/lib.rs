/**
 * This module converts IR to wasm bytecode.
 * It requires the `ir` and `wasmgen` crates.
 * It also emits the GC code.
 *
 * Encoding notes (note: i32 has no signedness in wasm):
 * Note: Things are pushed onto the protected wasm stack from right to left (so left is on top)
 * Note: When in locals or globals, the left has smaller index
 * Note: When in linear memory, the left has smaller index
 * Note: for i32 widening as i64, only the low bits of the i64 are used
 * Note: When encoding multiple things in the i64 (e.g. Func), left uses lower bits
 * Note: The closure must be a pointer type (i.e. StructT or String) allocated at the binding site, in order to make function equality work.
 *       Function equality is simply closure reference equality.  No need to compare the function ptr.
 * Note: The type of a closure must be known at compile time.  It is an i32 (tag) stored in closures[func.tableidx] (generated in data section).
 * Note: When the target of a function is not known (i.e. non-direct appl), all parameters must be Any.
 * * At the call site of call_indirect, we need to specify the list of parameter types.  It will all be Any, but we will exploit wasm
 * Note: Unused bytes may not be zero.  You should not assume it.
 * Unassigned -> <nothing>
 * Undefined -> <nothing>
 * Number -> f64
 * Boolean -> i32 (1: true; 0: false)
 * String -> i32 (ptr to unsized mem)
 * Func -> i32 (index in wasm table) + i32 (closure)
 * StructT -> i32 (ptr to data)
 * Any -> i32 (tag) + i64 (data, reinterpret as the concrete type specified in the tag)
 *
 * Note on String (and in the future Array should be similar):
 * * The content of a String is: length(4 bytes) followed by the content(length bytes).
 * * The pointer returned points to the `length` field.
 * * The actual size of the memory used is (length+4) bytes rounded up to nearest 4-byte boundary.
 *
 * Most functions have a comment that looks like: net wasm stack: [...] -> [...]
 * This refers to net change to the wasm protected stack (top of stack on the right side, which agrees with the webassembly specification).
 * Stack elements in quotes (e.g. <ir_vartype>) means that that position of the stack contains a value (or values) of the given `ir_vartype` (not necessarily Any).
 *
 * Booleans are encoded as 0 (false) or 1 (true), which is the same as wasm representation.
 *
 * Note: When comparing Anys for ===, the unused space might be anything!  So we need to switch on the vartype first.
 *
 * todo! unimplemented! Indirect function calls and functions returning functions are probably broken now because they need to be able to return a tuple of values.
 * The fix (not yet implemented) is to allocate space on the stack to transfer those values.
 * We need to define a proper calling convention for this (i.e. when to put it on the stack, and when to use the return value field).
 *
 * Memory management:
 * WebAssembly has one linear memory, growable at the right end (largest index).
 * We divide the memory as such (from 0 (left) to memory.size (right)):
 * [.....(stack).....|.....(global data).....|.....(heap).....]
 * stack: Grows leftward (toward smaller indices).  Contains stuff owned by a function, that needs to have its address taken.
 * global data: Bulk data needed by the whole program.  Stores things like string constants (for pooling).  Size of this partition depends on the program being compiled.
 * heap:  Managed by the GC.  Memory can be increased on the right side with wasm memory.grow instruction.  Only the GC knows how to read the stuff inside here.
 * There is one pre-added global:
 * * global#0 is the stack pointer (points to the last memory address that is filled).
 * * * Note: By convention, arguments and return values on the stack go **on top** of the stack pointer.
 * * * So if we have a 12-byte value on the stack that is a return value, it will be at location (global#0 - 12).
 * * The GC might add more globals.  So the funcs should not make any assumption about the starting globalidx that they can use.
 */
use ir;
use wasmgen;

mod func;
mod gc;
mod scratch;

use gc::leaky::Leaky;
use gc::HeapManager;

use projstd::iter::*;
use projstd::tuple::*;

const IR_FUNCIDX_TABLE_OFFSET: u32 = 0; // If ir::FuncIdx == x, then wasmgen::TableIdx == IR_FUNCIDX_TABLE_OFFSET + x as u32

const WASM_PAGE_SIZE: u32 = 65536;

// In units of WASM_PAGE_SIZE
const MEM_STACK_SIZE: u32 = 1 << 4; // 1 MiB of stack space

/**
 * This is the main function that invokes everything in the backend.
 * Call it, and everything will work.
 */
pub fn run_backend(ir_program: &ir::Program) -> wasmgen::WasmModule {
    encode_program(ir_program)
}

fn encode_program(ir_program: &ir::Program) -> wasmgen::WasmModule {
    // todo! Emit struct_types
    // todo! Emit globals
    // todo! Emit entry point
    // (note: not the same was the wasm entry point!
    // By convention, this is a normal function exported as "main")
    let mut wasm_module = wasmgen::WasmModule::new_builder().build();

    // add stack ptr
    let globalidx_stackptr =
        wasm_module.add_i32_global(wasmgen::Mut::Var, (MEM_STACK_SIZE * WASM_PAGE_SIZE) as i32);

    let (struct_field_byte_offsets, struct_sizes): (Box<[Box<[u32]>]>, Box<[u32]>) = ir_program
        .struct_types
        .iter()
        .map(|struct_type| {
            let mut total: u32 = 0;
            (
                struct_type
                    .iter()
                    .map(|vartype| size_in_memory(*vartype))
                    .scan_ref(&mut total, |st, elem| {
                        let ret: u32 = *st;
                        *st += elem;
                        Some(ret)
                    })
                    .collect(),
                total,
            )
        })
        .unzip()
        .into_boxed_slices();

    // todo!(figure out the size of the global data before processing the functions, so that the GC knows where its heap starts from)
    // in terms of WASM_PAGE_SIZE
    let global_data_size: u32 = 0;

    // add linear memory
    let memidx: wasmgen::MemIdx = encode_mem(global_data_size, &mut wasm_module);

    // garbage collector
    let heap = Leaky::new(
        &ir_program.struct_types,
        &struct_field_byte_offsets,
        &struct_sizes,
        memidx,
        MEM_STACK_SIZE + global_data_size,
        MEM_STACK_SIZE + global_data_size + Leaky::initial_heap_size(),
        &mut wasm_module,
    );

    func::encode_funcs(
        &ir_program.funcs,
        &ir_program.struct_types,
        &struct_field_byte_offsets,
        ir_program.entry_point,
        &heap,
        &mut wasm_module,
    );

    wasm_module
}

fn size_in_memory(ir_vartype: ir::VarType) -> u32 {
    match ir_vartype {
        ir::VarType::Any => 4 + 8,
        ir::VarType::Unassigned => 0,
        ir::VarType::Undefined => 0,
        ir::VarType::Number => 8,
        ir::VarType::Boolean => 4,
        ir::VarType::String => 4,
        ir::VarType::Func => 4 + 4,
        ir::VarType::StructT { typeidx: _ } => 4,
    }
}

// encodes the linear memory
// currently it will not reserve any space for global memory
fn encode_mem(globals_num_pages: u32, wasm_module: &mut wasmgen::WasmModule) -> wasmgen::MemIdx {
    wasm_module
        .add_unbounded_memory(MEM_STACK_SIZE + globals_num_pages + Leaky::initial_heap_size())
}
