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
 * stack: Grows leftward (toward smaller indices), so that a stack overflow will trigger a hard error (instead of silently overwritting our global data).  Contains stuff owned by a function, that needs to have its address taken.
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
mod pre_traverse;
mod string_prim_inst;
mod var_conv;

use gc::cheney::Cheney;
use gc::leaky::Leaky;
use gc::HeapManager;

use projstd::iter::*;
use projstd::tuple::*;

use wasmgen::Scratch;

const IR_FUNCIDX_TABLE_OFFSET: u32 = 0; // If ir::FuncIdx == x, then wasmgen::TableIdx == IR_FUNCIDX_TABLE_OFFSET + x as u32

const WASM_PAGE_SIZE: u32 = 65536;
const WASM_PAGE_BITS: u32 = WASM_PAGE_SIZE.trailing_zeros();

// In units of WASM_PAGE_SIZE
const MEM_STACK_SIZE: u32 = 1 << 4; // 1 MiB of stack space

// Struct containing compilation options
#[derive(Default, Copy, Clone)]
pub struct Options {
    wasm_multi_value: bool, // Whether we can generate code that uses the WebAssembly multi-valued returns proposal
    wasm_bulk_memory: bool, // Whether we can generate code that uses the WebAssembly bulk memory proposal
}

/**
 * This is the main function that invokes everything in the backend.
 * Call it, and everything will work.
 */
pub fn run_backend(ir_program: &ir::Program, options: Options) -> wasmgen::WasmModule {
    encode_program(ir_program, options)
}

fn encode_program(ir_program: &ir::Program, options: Options) -> wasmgen::WasmModule {
    // todo! Emit struct_types
    // todo! Emit globals
    // (note: not the same was the wasm entry point!)
    // By convention, this is a normal function exported as "main")

    let mut wasm_module_builder = wasmgen::WasmModule::new_builder();

    // generate the error function
    let error_func: wasmgen::FuncIdx = wasm_module_builder.import_func(
        "core".to_string(),
        "error".to_string(),
        &wasmgen::FuncType::new(
            Box::new([
                wasmgen::ValType::I32,
                wasmgen::ValType::I32,
                wasmgen::ValType::I32,
                wasmgen::ValType::I32,
                wasmgen::ValType::I32,
            ]),
            Box::new([]),
        ),
    );

    // import all the other functions
    let imported_funcs: Box<[wasmgen::FuncIdx]> = ir_program
        .imports
        .iter()
        .map(|ir_import| {
            let import_param_list = encode_import_params(&ir_import.params);
            let import_result = encode_import_param(ir_import.result);
            wasm_module_builder.import_func(
                ir_import.module_name.clone(),
                ir_import.entity_name.clone(),
                &wasmgen::FuncType::new(import_param_list, import_result.into()),
            )
        })
        .collect();

    let mut wasm_module = wasm_module_builder.build();

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
                    .map(|vartype| var_conv::size_in_memory(*vartype))
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

    // make the string pool from all string constants in the program
    // and the list of addressable funcs and their funcidxs
    let pre_traverse::TraverseResult {
        string_pool,
        addressed_funcs,
    } = pre_traverse::pre_traverse_funcs(&ir_program.funcs);

    let (shifted_string_pool, mut pool_data) =
        string_pool.into_shifted_and_buffer(MEM_STACK_SIZE << WASM_PAGE_BITS);
    // round up to multiple of WASM_PAGE_SIZE
    let required_data_size =
        ((pool_data.len() as u32) + (WASM_PAGE_SIZE - 1)) & (!(WASM_PAGE_SIZE - 1));
    pool_data.resize(required_data_size as usize, 0);
    // in terms of WASM_PAGE_SIZE
    let globals_num_pages: u32 = required_data_size >> WASM_PAGE_BITS;

    // add linear memory
    let memidx: wasmgen::MemIdx = encode_mem(
        MEM_STACK_SIZE + globals_num_pages + Cheney::initial_heap_size(),
        &mut wasm_module,
    );
    /*let memidx: wasmgen::MemIdx = encode_mem(
        MEM_STACK_SIZE + globals_num_pages + Cheney::initial_heap_size(),
        &mut wasm_module,
    );*/

    // export the memory (so that the host can read the return value)
    wasm_module.export_mem(memidx, "linear_memory".to_string());

    // initialize pool data
    encode_pool_data(
        &pool_data,
        MEM_STACK_SIZE << WASM_PAGE_BITS,
        memidx,
        &mut wasm_module,
    );

    // garbage collector
    let heap = Cheney::new(
        &ir_program.struct_types,
        &struct_field_byte_offsets,
        &struct_sizes,
        memidx,
        MEM_STACK_SIZE + globals_num_pages,
        MEM_STACK_SIZE + globals_num_pages + Cheney::initial_heap_size(),
        error_func,
        &mut wasm_module,
    );
    /*let heap = Cheney::new(
        &ir_program.struct_types,
        &struct_field_byte_offsets,
        &struct_sizes,
        memidx,
        MEM_STACK_SIZE + globals_num_pages,
        MEM_STACK_SIZE + globals_num_pages + Cheney::initial_heap_size(),
        error_func,
        &mut wasm_module,
    );*/

    func::encode_funcs(
        &ir_program.funcs,
        &ir_program.struct_types,
        &struct_field_byte_offsets,
        imported_funcs,
        ir_program.entry_point,
        globalidx_stackptr,
        memidx,
        addressed_funcs,
        &heap,
        &shifted_string_pool,
        error_func,
        options,
        &mut wasm_module,
    );

    wasm_module
}

fn encode_import_params(ivts: &[ir::ImportValType]) -> Box<[wasmgen::ValType]> {
    ivts.iter()
        .copied()
        .flat_map(|ivt| encode_import_param(ivt))
        .copied()
        .collect()
}

fn encode_import_param(ivt: ir::ImportValType) -> &'static [wasmgen::ValType] {
    match ivt {
        ir::ImportValType::Undefined => &[],
        ir::ImportValType::Number => &[wasmgen::ValType::F64],
        ir::ImportValType::String => &[wasmgen::ValType::I32],
    }
}

// encodes the linear memory
// currently it will not reserve any space for global memory
fn encode_mem(num_pages: u32, wasm_module: &mut wasmgen::WasmModule) -> wasmgen::MemIdx {
    wasm_module.add_unbounded_memory(num_pages)
}

fn encode_pool_data(
    pool_data: &[u8],
    offset: u32,
    memidx: wasmgen::MemIdx,
    wasm_module: &mut wasmgen::WasmModule,
) {
    wasm_module.add_data(memidx, offset, pool_data);
}

#[cfg(feature = "wasmtest")]
pub fn wasmtest<C: wasm_test_harness::TestContext>(c: &mut C) {
    gc::cheney::wasmtest::wasmtest(c);
}
