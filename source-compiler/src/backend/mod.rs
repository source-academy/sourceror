/**
 * This module converts IR to wasm bytecode.
 * It requires the `ir` and `wasmgen` crates.
 * It also emits the GC code.
 *
 * Encoding notes (note: i32 has no signedness in wasm):
 * Note: Things are pushed onto the protected wasm stack from right to left (so left is on top)
 * Note: When in locals or globals, the left has smaller index
 * Note: for i32 widening as i64, only the low bits of the i64 are used
 * Note: When encoding multiple things in the i64 (e.g. Func), left uses lower bits
 * Note: The closure must be a pointer type (i.e. StructT or String) allocated at the binding site, in order to make function equality work.
 *       Function equality is simply closure reference equality.  No need to compare the function ptr.
 * Note: The type of a closure must be known at compile time.  It is an i32 (tag) stored in closures[func.tableidx] (generated in data section).
 * Undefined -> <nothing>
 * Unassigned -> <nothing>
 * Number -> f64
 * Boolean -> i32 (1: true; 0: false)
 * String -> i32 (ptr to unsized mem)
 * Func -> i32 (index in wasm table) + i32 (closure)
 * StructT -> i32 (ptr to data)
 * Any -> i32 (tag) + i64 (data, reinterpret as the concrete type specified in the tag)
 *
 * Most functions have a comment that looks like: net wasm stack: [...] -> [...]
 * This refers to net change to the wasm protected stack (top of stack on the right side, which agrees with the webassembly specification).
 * Stack elements in quotes (e.g. <ir_vartype>) means that that position of the stack contains a value (or values) of the given `ir_vartype` (not necessarily Any).
 */
use ir;
use wasmgen;

const IR_FUNCIDX_TABLE_OFFSET: u32 = 0; // If ir::FuncIdx == x, then wasmgen::TableIdx == IR_FUNCIDX_TABLE_OFFSET + x as u32

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

    let struct_field_byte_offsets: Box<[Box<[u32]>]> = ir_program
        .struct_types
        .iter()
        .map(|struct_type| {
            struct_type
                .iter()
                .map(|vartype| size_in_memory(*vartype))
                .scan(0, |st, elem| {
                    let ret: u32 = *st;
                    *st += elem;
                    Some(ret)
                })
                .collect()
        })
        .collect();

    encode_funcs(
        &ir_program.funcs,
        &ir_program.struct_types,
        &struct_field_byte_offsets,
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

#[derive(Copy, Clone)]
struct EncodeContext<'a, 'b, 'c, 'd, 'e> {
    var_map: &'a [wasmgen::LocalIdx],
    params_locals_types: &'b [ir::VarType],
    struct_types: &'c [Box<[ir::VarType]>],
    struct_field_byte_offsets: &'d [Box<[u32]>], // has same sizes as `struct_types`, but instead stores the byte offset of each field from the beginning of the struct
    funcs: &'e [ir::Func], // ir functions, so that callers can check the param type of return type
                           // will also include function indices
}

struct MutContext<'a> {
    scratch: Scratch<'a>,
    // will also include function indices
}

// scratch space (additional locals) that any encoder might want
// generally, push and pop should match up within every function
// (callers should not generally push stuff without popping them in the same function)
struct Scratch<'a> {
    locals_builder: &'a mut wasmgen::LocalsManager,
    i32_buffer: Vec<wasmgen::LocalIdx>,
    i64_buffer: Vec<wasmgen::LocalIdx>,
    f32_buffer: Vec<wasmgen::LocalIdx>,
    f64_buffer: Vec<wasmgen::LocalIdx>,
    i32_idx: usize,
    i64_idx: usize,
    f32_idx: usize,
    f64_idx: usize,
}

impl<'a> Scratch<'a> {
    fn new(locals_builder: &'a mut wasmgen::LocalsManager) -> Scratch<'a> {
        Scratch {
            locals_builder: locals_builder,
            i32_buffer: Default::default(),
            i64_buffer: Default::default(),
            f32_buffer: Default::default(),
            f64_buffer: Default::default(),
            i32_idx: 0,
            i64_idx: 0,
            f32_idx: 0,
            f64_idx: 0,
        }
    }
    fn push_i32(&mut self) -> wasmgen::LocalIdx {
        Self::push_impl(
            &mut self.locals_builder,
            wasmgen::ValType::I32,
            &mut self.i32_buffer,
            &mut self.i32_idx,
        )
    }
    fn push_i64(&mut self) -> wasmgen::LocalIdx {
        Self::push_impl(
            &mut self.locals_builder,
            wasmgen::ValType::I64,
            &mut self.i64_buffer,
            &mut self.i64_idx,
        )
    }
    fn push_f32(&mut self) -> wasmgen::LocalIdx {
        Self::push_impl(
            &mut self.locals_builder,
            wasmgen::ValType::F32,
            &mut self.f32_buffer,
            &mut self.f32_idx,
        )
    }
    fn push_f64(&mut self) -> wasmgen::LocalIdx {
        Self::push_impl(
            &mut self.locals_builder,
            wasmgen::ValType::F64,
            &mut self.f64_buffer,
            &mut self.f64_idx,
        )
    }
    fn pop_i32(&mut self) {
        Self::pop_impl(&mut self.i32_idx)
    }
    fn pop_i64(&mut self) {
        Self::pop_impl(&mut self.i64_idx)
    }
    fn pop_f32(&mut self) {
        Self::pop_impl(&mut self.f32_idx)
    }
    fn pop_f64(&mut self) {
        Self::pop_impl(&mut self.f64_idx)
    }
    fn push_impl(
        locals_builder: &mut wasmgen::LocalsManager,
        valtype: wasmgen::ValType,
        buffer: &mut Vec<wasmgen::LocalIdx>,
        idx: &mut usize,
    ) -> wasmgen::LocalIdx {
        if *idx == buffer.len() {
            buffer.push(locals_builder.add(valtype));
        }
        let ret: wasmgen::LocalIdx = buffer[*idx];
        *idx += 1;
        ret
    }
    fn pop_impl(idx: &mut usize) {
        assert!(*idx > 0);
        *idx -= 1;
    }
}

fn encode_funcs(
    ir_funcs: &[ir::Func],
    ir_struct_types: &[Box<[ir::VarType]>],
    ir_struct_field_byte_offsets: &[Box<[u32]>],
    wasm_module: &mut wasmgen::WasmModule,
) {
    struct WasmRegistry {
        typeidx: wasmgen::TypeIdx,
        funcidx: wasmgen::FuncIdx,
        var_map: Box<[wasmgen::LocalIdx]>, // varmap converts ir param/local index to wasm param/local index
        code_builder: wasmgen::CodeBuilder,
        params_locals_types: Box<[ir::VarType]>,
    }

    // register all the funcs first, in order of ir::funcidx
    // also encode the params and the locals and the return type
    let mut registry_list: Box<[WasmRegistry]> = ir_funcs
        .iter()
        .map(|ir_func| {
            let (wasm_params, param_map) = encode_param_list(&ir_func.params);
            let num_wasm_params = wasm_params.len() as u32;
            let wasm_functype = wasmgen::FuncType::new(wasm_params, encode_result(&ir_func.result));
            let (wasm_typeidx, wasm_funcidx) = wasm_module.register_func(&wasm_functype);
            let (wasm_locals, local_map) = encode_param_list(&ir_func.locals);
            let mut code_builder = wasmgen::CodeBuilder::new(wasm_functype);
            let locals_builder = code_builder.locals_builder();
            for local in wasm_locals.into_iter() {
                locals_builder.add(*local);
            }
            let var_map = param_map
                .iter()
                .cloned()
                .chain(local_map.into_iter().map(|x| num_wasm_params + x))
                .map(|x| wasmgen::LocalIdx { idx: x })
                .collect();
            let params_locals_types = ir_func
                .params
                .iter()
                .chain(ir_func.locals.iter())
                .cloned()
                .collect();
            WasmRegistry {
                typeidx: wasm_typeidx,
                funcidx: wasm_funcidx,
                var_map: var_map,
                code_builder: code_builder,
                params_locals_types: params_locals_types,
            }
        })
        .collect();

    // use the wasmgen::codewriter to encode the function body
    ir_funcs
        .iter()
        .enumerate()
        .for_each(|(ir_funcidx, ir_func)| {
            let registry: &mut WasmRegistry = &mut registry_list[ir_funcidx];
            let code_builder: &mut wasmgen::CodeBuilder = &mut registry.code_builder;
            let (locals_builder, expr_builder) = code_builder.split();
            let scratch: Scratch = Scratch::new(locals_builder);
            let ctx = EncodeContext {
                var_map: &registry.var_map,
                params_locals_types: &registry.params_locals_types,
                struct_types: ir_struct_types,
                struct_field_byte_offsets: ir_struct_field_byte_offsets,
                funcs: ir_funcs,
            };
            let mut mutctx = MutContext { scratch: scratch };
            encode_statements(
                ir_func.statements.as_slice(),
                ctx,
                &mut mutctx,
                expr_builder,
            );
            expr_builder.end(); // append the end instruction to end the function
        });
}

// returns (wasm_param_list, param_map)
// where param_map[i] is the wasm param index of the beginning of the ith ir param
fn encode_param_list(ir_params: &[ir::VarType]) -> (Box<[wasmgen::ValType]>, Box<[u32]>) {
    let converted_params: Box<[&'static [wasmgen::ValType]]> = ir_params
        .iter()
        .map(|ir_param| encode_vartype(*ir_param))
        .collect();
    let param_map = converted_params
        .iter()
        .cloned()
        .map(|param| param.len() as u32)
        .collect();
    let wasm_params = converted_params
        .iter()
        .flat_map(|param| param.iter())
        .cloned()
        .collect();
    (wasm_params, param_map)
}

fn encode_result(ir_results: &Option<ir::VarType>) -> Box<[wasmgen::ValType]> {
    ir_results
        .iter()
        .flat_map(|ir_result| encode_vartype(*ir_result).iter())
        .cloned()
        .collect()
}

fn encode_vartype(ir_vartype: ir::VarType) -> &'static [wasmgen::ValType] {
    match ir_vartype {
        ir::VarType::Any => &[wasmgen::ValType::I32, wasmgen::ValType::I64],
        ir::VarType::Unassigned => panic!("ICE: IR->Wasm: Unassigned type may not be encoded"),
        ir::VarType::Undefined => &[],
        ir::VarType::Number => &[wasmgen::ValType::F64],
        ir::VarType::Boolean => &[wasmgen::ValType::I32],
        ir::VarType::String => &[wasmgen::ValType::I32],
        ir::VarType::Func => &[wasmgen::ValType::I32, wasmgen::ValType::I32],
        ir::VarType::StructT { typeidx } => &[wasmgen::ValType::I32],
    }
}

// net wasm stack: [] -> []
fn encode_statements(
    statements: &[ir::Statement],
    ctx: EncodeContext,
    mutctx: &mut MutContext,
    expr_builder: &mut wasmgen::ExprBuilder,
) {
    for statement in statements {
        encode_statement(statement, ctx, mutctx, expr_builder);
    }
}

// net wasm stack: [] -> []
fn encode_statement(
    statement: &ir::Statement,
    ctx: EncodeContext,
    mutctx: &mut MutContext,
    expr_builder: &mut wasmgen::ExprBuilder,
) {
    match statement {
        ir::Statement::Assign { target, expr } => {
            // encode stuff needed before expr (e.g. compute addresses):
            encode_target_addr_pre(target, expr.vartype, ctx, mutctx, expr_builder);
            // encode the expr (net wasm stack: [] -> [t...] where `t...` is a valid encoding of expr.vartype)
            encode_expr(expr, ctx, mutctx, expr_builder);
            // write the value from the stack to the target
            encode_target_addr_post(target, expr.vartype, ctx, mutctx, expr_builder);
        }
        ir::Statement::Return { expr } => unimplemented!(),
        ir::Statement::If {
            cond,
            true_stmts,
            false_stmts,
        } => unimplemented!(),
        ir::Statement::Expr { expr } => unimplemented!(),
        ir::Statement::Void { expr_kind } => unimplemented!(),
    }
}

// together, pre+expr+post fns should have net wasm stack: [] -> []
// the pre and post fns should be read and understood together
fn encode_target_addr_pre(
    target: &ir::TargetExpr,
    incoming_vartype: ir::VarType,
    ctx: EncodeContext,
    mutctx: &mut MutContext,
    expr_builder: &mut wasmgen::ExprBuilder,
) {
    match target {
        ir::TargetExpr::Global { globalidx, next } => {
            unimplemented!("Targetting a global is not implemented yet");
        }
        ir::TargetExpr::Local { localidx, next } => {
            match next {
                None => {}
                Some(struct_field) => {
                    // we are actually writing to linear memory
                    // so we place the address of the target struct on the stack
                    // (note: the address is to the struct, not the variable within the struct,
                    // the post fn encodes the offset using the memarg immediate)
                    // net wasm stack: [] -> [ptr]

                    // For Source1, structs can only arise from closures and local variables, so their types must already be known at compilation time.
                    assert!(
                        ctx.params_locals_types[*localidx]
                            == ir::VarType::StructT {
                                typeidx: struct_field.typeidx
                            },
                        "ICE: IR->Wasm: Struct type incorrect or not proved at compilation time"
                    );
                    // net wasm stack: [] -> [struct_ptr]
                    expr_builder.local_get(ctx.var_map[*localidx]);

                    fn follow_nested_struct(
                        outer_struct_field: &ir::StructField,
                        ctx: EncodeContext,
                        expr_builder: &mut wasmgen::ExprBuilder,
                    ) {
                        if let Some(inner_struct_field) = &outer_struct_field.next {
                            // If there is an inner struct, it must be known at compilation time
                            assert!(ctx.struct_types[outer_struct_field.typeidx][outer_struct_field.fieldidx] == ir::VarType::StructT{typeidx: inner_struct_field.typeidx}, "ICE: IR->Wasm: Struct type incorrect or not proved at compilation time");
                            // net wasm stack: [struct_ptr] -> [struct_ptr]
                            expr_builder.i32_load(wasmgen::MemArg::new4(
                                ctx.struct_field_byte_offsets[outer_struct_field.typeidx]
                                    [outer_struct_field.fieldidx],
                            ));
                            // recurse if there are even more inner structs
                            follow_nested_struct(&inner_struct_field, ctx, expr_builder);
                        }
                    }

                    follow_nested_struct(struct_field, ctx, expr_builder);
                }
            }
        }
    }
}
fn encode_target_addr_post(
    target: &ir::TargetExpr,
    incoming_vartype: ir::VarType,
    ctx: EncodeContext,
    mutctx: &mut MutContext,
    expr_builder: &mut wasmgen::ExprBuilder,
) {
    match target {
        ir::TargetExpr::Global { globalidx, next } => {
            unimplemented!("Targetting a global is not implemented yet");
        }
        ir::TargetExpr::Local { localidx, next } => {
            match next {
                None => {
                    encode_store_local(
                        ctx.var_map[*localidx],
                        ctx.params_locals_types[*localidx],
                        incoming_vartype,
                        expr_builder,
                    );
                }
                Some(struct_field) => {
                    // net wasm stack: [ptr, <incoming_vartype>] -> []

                    // returns the offset, as well as the static type of the variable in memory (so we know how to encode the value in memory)
                    fn get_innermost_offset(
                        sf: &ir::StructField,
                        ctx: EncodeContext,
                    ) -> (u32, ir::VarType) {
                        if let Some(inner_struct_field) = &sf.next {
                            return get_innermost_offset(inner_struct_field, ctx);
                        } else {
                            return (
                                ctx.struct_field_byte_offsets[sf.typeidx][sf.fieldidx],
                                ctx.struct_types[sf.typeidx][sf.fieldidx],
                            );
                        }
                    }

                    let (offset, ir_dest_vartype) = get_innermost_offset(struct_field, ctx);
                    encode_store_memory(
                        offset,
                        ir_dest_vartype,
                        incoming_vartype,
                        &mut mutctx.scratch,
                        expr_builder,
                    );
                }
            }
        }
    }
}

// stores a ir variable from the protected stack to local variable(s)
// net wasm stack: [<ir_source_vartype>] -> []
fn encode_store_local(
    wasm_localidx: wasmgen::LocalIdx,
    ir_dest_vartype: ir::VarType,
    ir_source_vartype: ir::VarType,
    expr_builder: &mut wasmgen::ExprBuilder,
) {
    if ir_dest_vartype == ir_source_vartype {
        match ir_dest_vartype {
            ir::VarType::Any | ir::VarType::Func => {
                expr_builder.local_set(wasm_localidx);
                expr_builder.local_set(wasm_localidx + 1);
            }
            ir::VarType::Number | ir::VarType::Boolean | ir::VarType::String => {
                expr_builder.local_set(wasm_localidx);
            }
            ir::VarType::StructT { typeidx: _ } => {
                expr_builder.local_set(wasm_localidx);
            }
            ir::VarType::Undefined => {}
            ir::VarType::Unassigned => {
                panic!("ICE: IR->Wasm: Local static vartype cannot be unassigned");
            }
        }
    } else if ir_dest_vartype == ir::VarType::Any {
        // writing from a specific type to the Any type
        match ir_source_vartype {
            ir::VarType::Any => {
                panic!("ICE");
            }
            ir::VarType::Undefined => {
                expr_builder.i32_const(ir_source_vartype.tag());
                expr_builder.local_set(wasm_localidx);
            }
            ir::VarType::Unassigned => {
                panic!("ICE: IR->Wasm: Cannot assign to local from unassigned value");
            }
            ir::VarType::Number => {
                expr_builder.i32_const(ir_source_vartype.tag());
                expr_builder.local_set(wasm_localidx);
                expr_builder.i64_reinterpret_f64(); // convert f64 to i64
                expr_builder.local_set(wasm_localidx + 1);
            }
            ir::VarType::Boolean | ir::VarType::String => {
                expr_builder.i32_const(ir_source_vartype.tag());
                expr_builder.local_set(wasm_localidx);
                expr_builder.i64_extend_i32_u(); // convert i32 to i64
                expr_builder.local_set(wasm_localidx + 1);
            }
            ir::VarType::StructT { typeidx: _ } => {
                expr_builder.i32_const(ir_source_vartype.tag());
                expr_builder.local_set(wasm_localidx);
                expr_builder.i64_extend_i32_u(); // convert i32 to i64
                expr_builder.local_set(wasm_localidx + 1);
            }
            ir::VarType::Func => {
                expr_builder.i32_const(ir_source_vartype.tag());
                expr_builder.local_set(wasm_localidx);
                // the rest of the instructions concats the two i32s from the stack into the i64 local
                expr_builder.i64_extend_i32_u(); // convert i32 to i64 (index in table)
                expr_builder.local_set(wasm_localidx + 1);
                expr_builder.i64_extend_i32_u(); // convert i32 to i64 (ptr to closure)
                expr_builder.i64_const(32);
                expr_builder.i64_shl();
                expr_builder.local_get(wasm_localidx + 1);
                expr_builder.i64_or();
                expr_builder.local_set(wasm_localidx + 1);
            }
        }
    } else {
        panic!("ICE: IR->Wasm: Assignment to local is not equivalent or widening conversion");
    }
}

// stores a ir variable from the protected stack to a location in memory
// net wasm stack: [struct_ptr, <irvartype>] -> []
fn encode_store_memory(
    wasm_struct_offset: u32,
    ir_dest_vartype: ir::VarType,
    ir_source_vartype: ir::VarType,
    scratch: &mut Scratch,
    expr_builder: &mut wasmgen::ExprBuilder,
) {
    if ir_dest_vartype == ir_source_vartype {
        match ir_dest_vartype {
            ir::VarType::Any => {
                let localidx_tag: wasmgen::LocalIdx = scratch.push_i32();
                let localidx_data: wasmgen::LocalIdx = scratch.push_i64();
                let localidx_ptr: wasmgen::LocalIdx = scratch.push_i32();
                expr_builder.local_set(localidx_tag);
                expr_builder.local_set(localidx_data);
                expr_builder.local_tee(localidx_ptr);
                expr_builder.local_get(localidx_tag);
                expr_builder.i32_store(wasmgen::MemArg::new4(wasm_struct_offset));
                expr_builder.local_get(localidx_ptr);
                expr_builder.local_get(localidx_data);
                expr_builder.i64_store(wasmgen::MemArg::new4(wasm_struct_offset + 4));
                scratch.pop_i32();
                scratch.pop_i64();
                scratch.pop_i32();
            }
            ir::VarType::Unassigned => {
                panic!("ICE: IR->Wasm: Cannot assign from unassigned value");
            }
            ir::VarType::Undefined => {}
            ir::VarType::Number => {
                expr_builder.f64_store(wasmgen::MemArg::new4(wasm_struct_offset));
            }
            ir::VarType::Boolean => {
                expr_builder.i32_store(wasmgen::MemArg::new4(wasm_struct_offset));
            }
            ir::VarType::String => {
                expr_builder.i32_store(wasmgen::MemArg::new4(wasm_struct_offset));
            }
            ir::VarType::Func => {
                let localidx_tableidx: wasmgen::LocalIdx = scratch.push_i32();
                let localidx_closure: wasmgen::LocalIdx = scratch.push_i32();
                let localidx_ptr: wasmgen::LocalIdx = scratch.push_i32();
                expr_builder.local_set(localidx_tableidx);
                expr_builder.local_set(localidx_closure);
                expr_builder.local_tee(localidx_ptr);
                expr_builder.local_get(localidx_tableidx);
                expr_builder.i32_store(wasmgen::MemArg::new4(wasm_struct_offset));
                expr_builder.local_get(localidx_ptr);
                expr_builder.local_get(localidx_closure);
                expr_builder.i32_store(wasmgen::MemArg::new4(wasm_struct_offset + 4));
                scratch.pop_i32();
                scratch.pop_i32();
                scratch.pop_i32();
            }
            ir::VarType::StructT { typeidx: _ } => {
                expr_builder.i32_store(wasmgen::MemArg::new4(wasm_struct_offset));
            }
        }
    } else if ir_dest_vartype == ir::VarType::Any {
        // writing from a specific type to the Any type
        match ir_source_vartype {
            ir::VarType::Any => {
                panic!("ICE");
            }
            ir::VarType::Unassigned => {
                panic!("ICE: IR->Wasm: Cannot assign from unassigned value");
            }
            ir::VarType::Undefined => {
                expr_builder.i32_const(ir_source_vartype.tag());
                expr_builder.i32_store(wasmgen::MemArg::new4(wasm_struct_offset));
            }
            ir::VarType::Number => {
                let localidx_val: wasmgen::LocalIdx = scratch.push_f64();
                let localidx_ptr: wasmgen::LocalIdx = scratch.push_i32();
                expr_builder.local_set(localidx_val);
                expr_builder.local_tee(localidx_ptr);
                expr_builder.i32_const(ir_source_vartype.tag());
                expr_builder.i32_store(wasmgen::MemArg::new4(wasm_struct_offset));
                expr_builder.local_get(localidx_ptr);
                expr_builder.local_get(localidx_val);
                expr_builder.f64_store(wasmgen::MemArg::new4(wasm_struct_offset + 4));
                scratch.pop_i32();
                scratch.pop_f64();
            }
            ir::VarType::Boolean | ir::VarType::String => {
                let localidx_val: wasmgen::LocalIdx = scratch.push_i32();
                let localidx_ptr: wasmgen::LocalIdx = scratch.push_i32();
                expr_builder.local_set(localidx_val);
                expr_builder.local_tee(localidx_ptr);
                expr_builder.i32_const(ir_source_vartype.tag());
                expr_builder.i32_store(wasmgen::MemArg::new4(wasm_struct_offset));
                expr_builder.local_get(localidx_ptr);
                expr_builder.local_get(localidx_val);
                expr_builder.i32_store(wasmgen::MemArg::new4(wasm_struct_offset + 4)); // note: high bytes of memory not used
                scratch.pop_i32();
                scratch.pop_i32();
            }
            ir::VarType::StructT { typeidx: _ } => {
                let localidx_val: wasmgen::LocalIdx = scratch.push_i32();
                let localidx_ptr: wasmgen::LocalIdx = scratch.push_i32();
                expr_builder.local_set(localidx_val);
                expr_builder.local_tee(localidx_ptr);
                expr_builder.i32_const(ir_source_vartype.tag());
                expr_builder.i32_store(wasmgen::MemArg::new4(wasm_struct_offset));
                expr_builder.local_get(localidx_ptr);
                expr_builder.local_get(localidx_val);
                expr_builder.i32_store(wasmgen::MemArg::new4(wasm_struct_offset + 4)); // note: high bytes of memory not used
                scratch.pop_i32();
                scratch.pop_i32();
            }
            ir::VarType::Func => {
                let localidx_tableidx: wasmgen::LocalIdx = scratch.push_i32();
                let localidx_closure: wasmgen::LocalIdx = scratch.push_i32();
                let localidx_ptr: wasmgen::LocalIdx = scratch.push_i32();
                expr_builder.local_set(localidx_tableidx);
                expr_builder.local_set(localidx_closure);
                expr_builder.local_tee(localidx_ptr);
                expr_builder.i32_const(ir_source_vartype.tag());
                expr_builder.i32_store(wasmgen::MemArg::new4(wasm_struct_offset));
                expr_builder.local_get(localidx_ptr);
                expr_builder.local_get(localidx_tableidx);
                expr_builder.i32_store(wasmgen::MemArg::new4(wasm_struct_offset + 4));
                expr_builder.local_get(localidx_ptr);
                expr_builder.local_get(localidx_closure);
                expr_builder.i32_store(wasmgen::MemArg::new4(wasm_struct_offset + 8));
                scratch.pop_i32();
                scratch.pop_i32();
                scratch.pop_i32();
            }
        }
    } else {
        panic!("ICE: IR->Wasm: Assignment not equivalent or widening conversion");
    }
}

// net wasm stack: [] -> [<irvartype>] where `<irvartype>` is a valid encoding of an object with IR type expr.vartype
fn encode_expr(
    expr: &ir::Expr,
    ctx: EncodeContext,
    mutctx: &mut MutContext,
    expr_builder: &mut wasmgen::ExprBuilder,
) {
    match &expr.kind {
        ir::ExprKind::PrimUndefined => {
            assert!(
                expr.vartype == ir::VarType::Undefined,
                "ICE: IR->Wasm: PrimUndefined does not have type undefined"
            );
            // Don't do anything, because undefined is encoded as <nothing>
        }
        ir::ExprKind::PrimNumber { val } => {
            assert!(
                expr.vartype == ir::VarType::Number,
                "ICE: IR->Wasm: PrimNumber does not have type number"
            );
            expr_builder.f64_const(*val);
        }
        ir::ExprKind::PrimBoolean { val } => {
            assert!(
                expr.vartype == ir::VarType::Boolean,
                "ICE: IR->Wasm: PrimBoolean does not have type boolean"
            );
            expr_builder.i32_const(if *val { 1 } else { 0 });
        }
        ir::ExprKind::PrimString { val } => {
            assert!(
                expr.vartype == ir::VarType::String,
                "ICE: IR->Wasm: PrimString does not have type string"
            );
            unimplemented!("PrimString needs GC support, unimplemented");
        }
        ir::ExprKind::PrimStructT { typeidx } => {
            assert!(
                expr.vartype == ir::VarType::StructT { typeidx: *typeidx },
                "ICE: IR->Wasm: PrimStructT does not have correct type, or typeidx is incorrect"
            );
            unimplemented!("PrimStructT needs GC support, unimplemented");
        }
        ir::ExprKind::PrimFunc { funcidx, closure } => {
            assert!(
                expr.vartype == ir::VarType::Func,
                "ICE: IR->Wasm: PrimFunc does not have type func"
            );
            assert!(ctx.funcs[*funcidx].params.first().cloned() == Some(closure.vartype)); // make sure the closure type given to us is indeed what the function will expect

            // encode the closure expr (might contain sub-exprs in general)
            // net wasm stack: [] -> [<closure_irvartype>] (note: this might not be i32, if there is an empty closure)
            encode_expr(&closure, ctx, mutctx, expr_builder);

            // encode the funcidx
            // net wasm stack: [] -> [funcidx]
            expr_builder.i32_const((IR_FUNCIDX_TABLE_OFFSET + *funcidx as u32) as i32);
        }
        ir::ExprKind::TypeOf { expr, expected } => {
            assert!(
                expr.vartype == ir::VarType::Any,
                "ICE: IR->Wasm: LHS of TypeOf builtin should have static type Any"
            );

            // encode the inner expr
            // net wasm stack: [] -> [i64 data, i32 tag]
            encode_expr(&expr, ctx, mutctx, expr_builder);

            // push `true` if the tag of the Any is equal to `expected`, `false` otherwise
            // net wasm stack: [] -> [i32 constant]
            expr_builder.i32_const(expected.tag());
            // net wasm stack: [i32 tag, i32 constant] -> [i32 result]
            expr_builder.i32_eq(); // compare two arguments for equality (1=true,0=false)

            // now, the stack still contains the i64 data beneath our result... we have to get rid of it before returning
            // net wasm stack: [i64 data, i32 result] -> [i32 result]
            let localidx_result: wasmgen::LocalIdx = mutctx.scratch.push_i32();
            expr_builder.local_set(localidx_result);
            expr_builder.drop(); // remove the top thing (which is now the i64 data) from the stack
            expr_builder.local_get(localidx_result);
            mutctx.scratch.pop_i32();
        }
        ir::ExprKind::VarName { source } => {
            // net wasm stack: [] -> [<source_vartype>]
            encode_target_value(source, expr.vartype, ctx, mutctx, expr_builder);
        }
        ir::ExprKind::PrimAppl { prim_inst, args } => {
            unimplemented!();
        }
        ir::ExprKind::Appl { func, args } => {
            unimplemented!();
        }
        ir::ExprKind::DirectAppl { funcidx, args } => {
            unimplemented!();
        }
    }
}

// Loads the eventual value of `source`, following all struct fields, onto the stack, encoded as `outgoing_vartype`.
// `outgoing_vartype` is required to be equivalent or subtype of the source vartype.  (Otherwise it means the optimiser is broken.)
// net wasm stack: [] -> [<outgoing_vartype>]
fn encode_target_value(
    source: &ir::TargetExpr,
    outgoing_vartype: ir::VarType,
    ctx: EncodeContext,
    mutctx: &mut MutContext,
    expr_builder: &mut wasmgen::ExprBuilder,
) {
    match source {
        ir::TargetExpr::Global { globalidx, next } => {
            unimplemented!("Globals are not supported... yet");
        }
        ir::TargetExpr::Local { localidx, next } => {
            //expr_builder.local_get(ctx.var_map[*localidx]); // wrong!!!

            match next {
                None => {
                    // `outgoing_vartype` is required to be equivalent or subtype of the source vartype
                    encode_load_local(
                        ctx.var_map[*localidx],
                        ctx.params_locals_types[*localidx],
                        outgoing_vartype,
                        expr_builder,
                    );
                }
                Some(struct_field) => {
                    // For Source1, structs can only arise from closures and local variables, so their types must already be known at compilation time.
                    assert!(
                        ctx.params_locals_types[*localidx]
                            == ir::VarType::StructT {
                                typeidx: struct_field.typeidx
                            },
                        "ICE: IR->Wasm: Struct type incorrect or not proved at compilation time"
                    );
                    // net wasm stack: [] -> [struct_ptr]
                    expr_builder.local_get(ctx.var_map[*localidx]);

                    // net wasm stack: [struct_ptr] -> [<outgoing_vartype>]
                    fn load_inner_local(
                        outer_struct_field: &ir::StructField,
                        outgoing_vartype: ir::VarType,
                        ctx: EncodeContext,
                        mutctx: &mut MutContext,
                        expr_builder: &mut wasmgen::ExprBuilder,
                    ) {
                        if let Some(inner_struct_field) = &outer_struct_field.next {
                            // If there is an inner struct, it must be known at compilation time
                            assert!(ctx.struct_types[outer_struct_field.typeidx][outer_struct_field.fieldidx] == ir::VarType::StructT{typeidx: inner_struct_field.typeidx}, "ICE: IR->Wasm: Struct type incorrect or not proved at compilation time");
                            // net wasm stack: [struct_ptr] -> [struct_ptr]
                            expr_builder.i32_load(wasmgen::MemArg::new4(
                                ctx.struct_field_byte_offsets[outer_struct_field.typeidx]
                                    [outer_struct_field.fieldidx],
                            ));
                            // recurse if there are even more inner structs
                            load_inner_local(
                                &inner_struct_field,
                                outgoing_vartype,
                                ctx,
                                mutctx,
                                expr_builder,
                            );
                        } else {
                            let offset: u32 = ctx.struct_field_byte_offsets
                                [outer_struct_field.typeidx][outer_struct_field.fieldidx];
                            let ir_source_vartype: ir::VarType = ctx.struct_types
                                [outer_struct_field.typeidx][outer_struct_field.fieldidx];
                            encode_load_memory(
                                offset,
                                ir_source_vartype,
                                outgoing_vartype,
                                &mut mutctx.scratch,
                                expr_builder,
                            );
                        }
                    }

                    load_inner_local(struct_field, outgoing_vartype, ctx, mutctx, expr_builder);
                }
            }
        }
    }
}

// loads a ir variable into the protected stack from local variable(s)
// net wasm stack: [] -> [<outgoing_vartype>]
fn encode_load_local(
    wasm_localidx: wasmgen::LocalIdx,
    ir_local_vartype: ir::VarType,
    ir_outgoing_vartype: ir::VarType,
    expr_builder: &mut wasmgen::ExprBuilder,
) {
    if ir_local_vartype == ir_outgoing_vartype {
        match ir_local_vartype {
            ir::VarType::Any | ir::VarType::Func => {
                expr_builder.local_get(wasm_localidx + 1);
                expr_builder.local_get(wasm_localidx);
            }
            ir::VarType::Number | ir::VarType::Boolean | ir::VarType::String => {
                expr_builder.local_get(wasm_localidx);
            }
            ir::VarType::StructT { typeidx: _ } => {
                expr_builder.local_get(wasm_localidx);
            }
            ir::VarType::Undefined => {}
            ir::VarType::Unassigned => {
                panic!("ICE: IR->Wasm: Local static vartype cannot be unassigned");
            }
        }
    } else if ir_local_vartype == ir::VarType::Any {
        // loading from Any type to a specific type
        match ir_outgoing_vartype {
            ir::VarType::Any => {
                panic!("ICE");
            }
            ir::VarType::Undefined => {}
            ir::VarType::Unassigned => {
                panic!("ICE: IR->Wasm: Cannot load from unassigned local");
            }
            ir::VarType::Number => {
                expr_builder.local_get(wasm_localidx + 1);
                expr_builder.f64_reinterpret_i64(); // convert i64 to f64
            }
            ir::VarType::Boolean | ir::VarType::String => {
                expr_builder.local_get(wasm_localidx + 1);
                expr_builder.i32_wrap_i64(); // convert i64 to i32
            }
            ir::VarType::StructT { typeidx: _ } => {
                expr_builder.local_get(wasm_localidx + 1);
                expr_builder.i32_wrap_i64(); // convert i64 to i32
            }
            ir::VarType::Func => {
                // get high bits into i32
                expr_builder.local_get(wasm_localidx + 1);
                expr_builder.i64_const(32);
                expr_builder.i64_shr_u();
                expr_builder.i32_wrap_i64();
                // get low bits into i32
                expr_builder.local_get(wasm_localidx + 1);
                expr_builder.i32_wrap_i64();
            }
        }
    } else {
        panic!("ICE: IR->Wasm: Load from local is not equivalent or narrowing conversion");
    }
}

// net wasm stack: [struct_ptr] -> [<outgoing_vartype>]
fn encode_load_memory(
    wasm_struct_offset: u32,
    ir_local_vartype: ir::VarType,
    ir_outgoing_vartype: ir::VarType,
    scratch: &mut Scratch,
    expr_builder: &mut wasmgen::ExprBuilder,
) {
    if ir_local_vartype == ir_outgoing_vartype {
        match ir_local_vartype {
            ir::VarType::Any => {
                let localidx_ptr: wasmgen::LocalIdx = scratch.push_i32();
                expr_builder.local_tee(localidx_ptr);
                expr_builder.i64_load(wasmgen::MemArg::new4(wasm_struct_offset + 4));
                expr_builder.local_get(localidx_ptr);
                expr_builder.i32_load(wasmgen::MemArg::new4(wasm_struct_offset));
                scratch.pop_i32();
            }
            ir::VarType::Undefined => {}
            ir::VarType::Unassigned => {
                panic!("ICE: IR->Wasm: Cannot load from unassigned memory");
            }
            ir::VarType::Number => {
                expr_builder.f64_load(wasmgen::MemArg::new4(wasm_struct_offset));
            }
            ir::VarType::Boolean => {
                expr_builder.i32_load(wasmgen::MemArg::new4(wasm_struct_offset));
            }
            ir::VarType::String => {
                expr_builder.i32_load(wasmgen::MemArg::new4(wasm_struct_offset));
            }
            ir::VarType::Func => {
                let localidx_ptr: wasmgen::LocalIdx = scratch.push_i32();
                expr_builder.local_tee(localidx_ptr);
                expr_builder.i32_load(wasmgen::MemArg::new4(wasm_struct_offset + 4));
                expr_builder.local_get(localidx_ptr);
                expr_builder.i32_load(wasmgen::MemArg::new4(wasm_struct_offset));
                scratch.pop_i32();
            }
            ir::VarType::StructT { typeidx: _ } => {
                expr_builder.i32_load(wasmgen::MemArg::new4(wasm_struct_offset));
            }
        }
    } else if ir_local_vartype == ir::VarType::Any {
        match ir_outgoing_vartype {
            ir::VarType::Any => {
                panic!("ICE");
            }
            ir::VarType::Unassigned => {
                panic!("ICE: IR->Wasm: Cannot load from unassigned memory");
            }
            ir::VarType::Undefined => {}
            ir::VarType::Number => {
                expr_builder.f64_load(wasmgen::MemArg::new4(wasm_struct_offset + 4));
            }
            ir::VarType::Boolean | ir::VarType::String => {
                expr_builder.i32_load(wasmgen::MemArg::new4(wasm_struct_offset + 4));
                // note: high bytes of memory not used
            }
            ir::VarType::StructT { typeidx: _ } => {
                expr_builder.i32_load(wasmgen::MemArg::new4(wasm_struct_offset + 4));
                // note: high bytes of memory not used
            }
            ir::VarType::Func => {
                let localidx_ptr: wasmgen::LocalIdx = scratch.push_i32();
                expr_builder.local_tee(localidx_ptr);
                expr_builder.i32_load(wasmgen::MemArg::new4(wasm_struct_offset + 8));
                expr_builder.local_get(localidx_ptr);
                expr_builder.i32_load(wasmgen::MemArg::new4(wasm_struct_offset + 4));
                scratch.pop_i32();
            }
        }
    } else {
        panic!("ICE: IR->Wasm: Load from memory is not equivalent or narrowing conversion");
    }
}
