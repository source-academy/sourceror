use crate::mutcontext::MutContext;

use super::gc::HeapManager;
/**
 * Contains stuff related to encoding of function bodies.
 */
use wasmgen::Scratch;

use crate::global_var::*;
use crate::multi_value_polyfill;
use crate::pre_traverse::ShiftedStringPool;
use crate::string_prim_inst;
use crate::Options;

use super::opt_var_conv::*;
use super::var_conv::*;

use projstd::searchablevec::SearchableVec;

use boolinator::*;

use std::collections::HashMap;

struct EncodeContext<'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, Heap: HeapManager> {
    // Local to this function
    return_type: Option<ir::VarType>,

    // Global for whole program
    struct_types: &'a [Box<[ir::VarType]>],
    struct_field_byte_offsets: &'b [Box<[u32]>], // has same sizes as `struct_types`, but instead stores the byte offset of each field from the beginning of the struct
    ir_signature_list: &'c [Signature], // mapping from ir::FuncIdx, for callers to check the param type or return type
    wasm_funcidxs: &'d [wasmgen::FuncIdx], // mapping from ir::FuncIdx to wasmgen::FuncIdx (used when we need to invoke a DirectAppl), this includes imports too

    // Global var management (does not include special globals like the stackptr)
    globals: GlobalVarManagerRef<'e>,

    // Other things
    stackptr: wasmgen::GlobalIdx,
    memidx: wasmgen::MemIdx,
    thunk_map: &'f HashMap<Box<[ir::OverloadEntry]>, u32>, // map from overloads to elemidx
    appl_data_encoder: &'g HashMap<ir::SourceLocation, u32>, // map from source location to the location in memory of the args
    heap: &'h Heap,
    string_pool: &'i ShiftedStringPool,
    error_func: wasmgen::FuncIdx, // imported function to call to error out (e.g. runtime type errors)
    options: Options,             // Compilation options (it implements Copy)
}

// Have to implement Copy and Clone manually, because #[derive(Copy, Clone)] doesn't work for generic types like Heap
impl<'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, Heap: HeapManager> Copy
    for EncodeContext<'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, Heap>
{
}
impl<'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, Heap: HeapManager> Clone
    for EncodeContext<'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, Heap>
{
    fn clone(&self) -> Self {
        *self
    }
}

/**
 * Wraps the WasmModule, to only allow operations that are allows while encoding a function body.
 * (Because a raw WasmModule might allow a lot of other things which shouldn't be done midway encoding the function body.)
 * todo!: Refactor ModuleEncodeWrapper out of this module, because MutContext depends on it, and the gc depends on MutContext.
 */
pub struct ModuleEncodeWrapper<'a> {
    wasm_module: &'a mut wasmgen::WasmModule,
}
impl<'a> ModuleEncodeWrapper<'a> {
    fn add_wasm_type(&mut self, wasm_functype: wasmgen::FuncType) -> wasmgen::TypeIdx {
        self.wasm_module.insert_type_into(wasm_functype)
    }
    fn add_ir_type_with_closure(
        &mut self,
        ir_params: &[ir::VarType],
        ir_result: Option<ir::VarType>,
        use_wasm_multi_value_feature: bool,
    ) -> wasmgen::TypeIdx {
        let (wasm_params, _, _) = encode_param_list(ir_params);
        let params_with_closure: Box<[wasmgen::ValType]> = std::iter::once(wasmgen::ValType::I32)
            .chain(wasm_params.into_iter().copied())
            .collect();
        let wasm_functype = wasmgen::FuncType::new(
            params_with_closure,
            encode_result(ir_result, use_wasm_multi_value_feature),
        );
        self.add_wasm_type(wasm_functype)
    }
    /*fn register_and_commit_func(codebuilder: wasmgen::CodeBuilder) -> ir::FuncIdx {
        wasm_module.commit_func(registry.funcidx, code_builder);
    }*/
}

pub struct Signature {
    pub params: Box<[ir::VarType]>,
    pub result: Option<ir::VarType>,
}

pub fn encode_funcs<'a, Heap: HeapManager>(
    ir_signature_list: &[Signature], // direct mapping from ir::FuncIdx: includes both imports and funcs
    ir_funcs: &[ir::Func],
    ir_struct_types: &[Box<[ir::VarType]>],
    ir_struct_field_byte_offsets: &[Box<[u32]>],
    imported_funcs: Box<[wasmgen::FuncIdx]>,
    ir_entry_point_funcidx: ir::FuncIdx,
    global_var_manager: GlobalVarManagerRef<'a>,
    globalidx_stackptr: wasmgen::GlobalIdx,
    memidx: wasmgen::MemIdx,
    thunk_sv: SearchableVec<Box<[ir::OverloadEntry]>>,
    appl_data_encoder: HashMap<ir::SourceLocation, u32>,
    heap: &Heap,
    string_pool: &ShiftedStringPool,
    error_func: wasmgen::FuncIdx,
    options: Options,
    wasm_module: &mut wasmgen::WasmModule,
) {
    struct WasmRegistry {
        funcidx: wasmgen::FuncIdx,
        wasm_param_map: Box<[wasmgen::LocalIdx]>, // map converts wasm_param_map index to actual wasm param index
        param_map: Box<[usize]>, // map converts ir param index to wasm_param_map index
        param_types: Box<[ir::VarType]>, // map converts ir param index to ir param type
    }

    let (thunk_list, thunk_map) = thunk_sv.into_parts();

    // reserve space for the indirect function table
    let tableidx = wasm_module.get_or_add_table();
    let thunk_table_offset = wasm_module.reserve_table_elements(tableidx, thunk_list.len() as u32);

    // register all the funcs first, in order of ir::funcidx
    // also encode the params and the locals and the return type
    // note: must register the funcs before any code (including thunks) can be generated,
    // so we can make calls to these funcs.
    let (registry_list, code_builder_list): (Vec<WasmRegistry>, Vec<wasmgen::CodeBuilder>) =
        ir_funcs
            .iter()
            .map(|ir_func| {
                let (wasm_param_valtypes, wasm_param_map, param_map) =
                    encode_param_list(&ir_func.params);
                let wasm_functype = wasmgen::FuncType::new(
                    wasm_param_valtypes,
                    encode_result(ir_func.result, options.wasm_multi_value),
                );
                let (_, wasm_funcidx) = wasm_module.register_func(&wasm_functype);
                let code_builder = wasmgen::CodeBuilder::new(wasm_functype);
                (
                    WasmRegistry {
                        funcidx: wasm_funcidx,
                        wasm_param_map: wasm_param_map,
                        param_map: param_map,
                        param_types: ir_func.params.clone(),
                    },
                    code_builder,
                )
            })
            .unzip();

    let wasm_funcidxs: Box<[wasmgen::FuncIdx]> = imported_funcs
        .into_iter()
        .copied()
        .chain(registry_list.iter().map(|x| x.funcidx))
        .collect();

    let new_thunk_map: HashMap<Box<[ir::OverloadEntry]>, u32> = thunk_map
        .into_iter()
        .map(|(oe, i)| (oe, thunk_table_offset + (i as u32)))
        .collect();

    let thunk_funcidxs: Box<[wasmgen::FuncIdx]> = thunk_list
        .into_iter()
        .map(|overload_entries| {
            let wasm_functype = wasmgen::FuncType::new(
                // closure, num_params, callerid
                Box::new([
                    wasmgen::ValType::I32,
                    wasmgen::ValType::I32,
                    wasmgen::ValType::I32,
                ]),
                encode_result(Some(ir::VarType::Any), options.wasm_multi_value),
            );
            let (_, wasm_funcidx) = wasm_module.register_func(&wasm_functype);
            let mut code_builder = wasmgen::CodeBuilder::new(wasm_functype);
            {
                let (locals_builder, expr_builder) = code_builder.split();
                let scratch: Scratch = Scratch::new(locals_builder);
                let ctx = EncodeContext {
                    return_type: Some(ir::VarType::Any),
                    struct_types: ir_struct_types,
                    struct_field_byte_offsets: ir_struct_field_byte_offsets,
                    ir_signature_list: ir_signature_list,
                    wasm_funcidxs: &wasm_funcidxs,
                    globals: global_var_manager,
                    stackptr: globalidx_stackptr,
                    memidx: memidx,
                    heap: heap,
                    thunk_map: &new_thunk_map,
                    appl_data_encoder: &appl_data_encoder,
                    string_pool: string_pool,
                    error_func: error_func,
                    options: options,
                };
                let mut mutctx = MutContext::new(
                    scratch,
                    &[],
                    &[],
                    &[], // doesn't contain the two real i32 params
                    ModuleEncodeWrapper { wasm_module },
                );
                encode_thunk(&overload_entries, ctx, &mut mutctx, expr_builder);

                // append the end instruction to end of the function
                expr_builder.end();
            }

            // commit the function:
            wasm_module.commit_func(wasm_funcidx, code_builder);

            wasm_funcidx
        })
        .collect();
    wasm_module.commit_table_elements(tableidx, thunk_table_offset, thunk_funcidxs);

    // use the wasmgen::codewriter to encode the function body
    ir_funcs
        .iter()
        .zip(code_builder_list.into_iter())
        .enumerate()
        .for_each(|(ir_funcidx, (ir_func, mut code_builder))| {
            let registry: &WasmRegistry = &registry_list[ir_funcidx];
            {
                let (locals_builder, expr_builder) = code_builder.split();
                let scratch: Scratch = Scratch::new(locals_builder);
                let ctx = EncodeContext {
                    return_type: ir_func.result,
                    struct_types: ir_struct_types,
                    struct_field_byte_offsets: ir_struct_field_byte_offsets,
                    ir_signature_list: ir_signature_list,
                    wasm_funcidxs: &wasm_funcidxs,
                    globals: global_var_manager,
                    stackptr: globalidx_stackptr,
                    memidx: memidx,
                    heap: heap,
                    thunk_map: &new_thunk_map,
                    appl_data_encoder: &appl_data_encoder,
                    string_pool: string_pool,
                    error_func: error_func,
                    options: options,
                };
                let mut mutctx = MutContext::new(
                    scratch,
                    &registry.wasm_param_map,
                    &registry.param_map,
                    &registry.param_types,
                    ModuleEncodeWrapper { wasm_module },
                );
                let wasm_reachable = encode_expr(&ir_func.expr, ctx, &mut mutctx, expr_builder);

                if let Some(vartype) = ir_func.expr.vartype {
                    assert!(wasm_reachable);
                    encode_return_calling_conv(
                        ir_func.result.unwrap(),
                        vartype,
                        options.wasm_multi_value,
                        globalidx_stackptr,
                        mutctx.scratch_mut(),
                        expr_builder,
                    );
                } else if wasm_reachable {
                    expr_builder.unreachable();
                }
                // if !wasm_reachable then wasm knows that this point is unreachable, so we don't need to emit the `unreachable` instruction

                // append the end instruction to end of the function
                expr_builder.end();
            }
            // commit the function:
            wasm_module.commit_func(registry.funcidx, code_builder);
        });

    // encode the entry point
    // Note: this is not the wasm start function (the wasm start function is invoked immediately on instantiation, before exported functions are callable)
    // By our convention this function is exported as "main"
    wasm_module.export_func(wasm_funcidxs[ir_entry_point_funcidx], "main".to_string());
}

// returns (wasm_param_valtypes, wasm_param_map, param_map)
// where param_map[i] is an index into wasm_param_map; it is the wasm param index of the beginning of the ith ir param
// e.g. if param_map[i] == 5 and this ir param actually converts to two wasm params, then the wasm params are at wasm_param_map[5] and wasm_param_map[6].
// (they may not actually be placed contiguously in the real wasm param indices, due to the coalescing allocations provided by scratch)
fn encode_param_list(
    ir_params: &[ir::VarType],
) -> (
    Box<[wasmgen::ValType]>,
    Box<[wasmgen::LocalIdx]>,
    Box<[usize]>,
) {
    let mut wasm_param_valtypes = Vec::new();
    let mut wasm_param_map = Vec::new();
    let mut param_map = Vec::new();
    for ir_param in ir_params {
        let encoded = encode_vartype(*ir_param);
        param_map.push(wasm_param_map.len());
        for wasm_valtype in encoded.iter().rev() {
            wasm_param_valtypes.push(*wasm_valtype);
        }
        let len = wasm_param_valtypes.len();
        for (i, _) in encoded.iter().enumerate() {
            wasm_param_map.push(wasmgen::LocalIdx {
                idx: (len - 1 - i) as u32,
            });
        }
    }
    (
        wasm_param_valtypes.into_boxed_slice(),
        wasm_param_map.into_boxed_slice(),
        param_map.into_boxed_slice(),
    )
}

fn encode_result(
    ir_results: Option<ir::VarType>,
    use_wasm_multi_value_feature: bool,
) -> Box<[wasmgen::ValType]> {
    let ret: Box<[wasmgen::ValType]> = ir_results
        .into_iter()
        .flat_map(|ir_result| encode_vartype(ir_result).iter())
        .copied()
        .collect();
    if ret.len() <= 1 || use_wasm_multi_value_feature {
        ret
    } else {
        Box::new([])
    }
}

fn encode_load_dummies(
    wasm_valtypes: &[wasmgen::ValType],
    expr_builder: &mut wasmgen::ExprBuilder,
) {
    for wasm_valtype in wasm_valtypes {
        match *wasm_valtype {
            wasmgen::ValType::I32 => expr_builder.i32_const(0),
            wasmgen::ValType::I64 => expr_builder.i64_const(0),
            wasmgen::ValType::F32 => expr_builder.f32_const(0.0),
            wasmgen::ValType::F64 => expr_builder.f64_const(0.0),
        };
    }
}

// net wasm stack: [<vartype>] -> []
fn encode_drop_value(ir_vartype: ir::VarType, expr_builder: &mut wasmgen::ExprBuilder) {
    match ir_vartype {
        ir::VarType::Any | ir::VarType::Func => {
            expr_builder.drop();
            expr_builder.drop();
        }
        ir::VarType::Number | ir::VarType::Boolean | ir::VarType::String => expr_builder.drop(),
        ir::VarType::StructT { typeidx: _ } => expr_builder.drop(),
        ir::VarType::Undefined => {}
        ir::VarType::Unassigned => panic!("Unassigned variable must not exist on the stack"),
    }
}

// together, pre+expr+post fns should have net wasm stack: [] -> []
// the pre and post fns should be read and understood together
fn encode_target_addr_pre<H: HeapManager>(
    target: &ir::TargetExpr,
    _incoming_vartype: ir::VarType,
    ctx: EncodeContext<H>,
    mutctx: &mut MutContext,
    expr_builder: &mut wasmgen::ExprBuilder,
) {
    fn follow_nested_struct<H: HeapManager>(
        outer_struct_field: &ir::StructField,
        ctx: EncodeContext<H>,
        expr_builder: &mut wasmgen::ExprBuilder,
    ) {
        if let Some(inner_struct_field) = &outer_struct_field.next {
            // If there is an inner struct, it must be known at compilation time
            assert!(
                ctx.struct_types[outer_struct_field.typeidx][outer_struct_field.fieldidx]
                    == ir::VarType::StructT {
                        typeidx: inner_struct_field.typeidx
                    },
                "ICE: IR->Wasm: Struct type incorrect or not proved at compilation time"
            );
            // net wasm stack: [struct_ptr] -> [struct_ptr]
            expr_builder.i32_load(wasmgen::MemArg::new4(
                ctx.struct_field_byte_offsets[outer_struct_field.typeidx]
                    [outer_struct_field.fieldidx],
            ));
            // recurse if there are even more inner structs
            follow_nested_struct(&inner_struct_field, ctx, expr_builder);
        }
    }

    match target {
        ir::TargetExpr::Global { globalidx, next } => {
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
                        ctx.globals.global_types[*globalidx]
                            == ir::VarType::StructT {
                                typeidx: struct_field.typeidx
                            },
                        "ICE: IR->Wasm: Struct type incorrect or not proved at compilation time"
                    );
                    // net wasm stack: [] -> [struct_ptr]
                    expr_builder.global_get(ctx.globals.wasm_global_slice(*globalidx)[0]);

                    follow_nested_struct(struct_field, ctx, expr_builder);
                }
            }
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
                        mutctx.named_local_types_elem(*localidx)
                            == ir::VarType::StructT {
                                typeidx: struct_field.typeidx
                            },
                        "ICE: IR->Wasm: Struct type incorrect or not proved at compilation time"
                    );
                    // net wasm stack: [] -> [struct_ptr]
                    expr_builder.local_get(mutctx.named_wasm_local_slice(*localidx)[0]);

                    follow_nested_struct(struct_field, ctx, expr_builder);
                }
            }
        }
    }
}
fn encode_target_addr_post<H: HeapManager>(
    target: &ir::TargetExpr,
    incoming_vartype: ir::VarType,
    ctx: EncodeContext<H>,
    mutctx: &mut MutContext,
    expr_builder: &mut wasmgen::ExprBuilder,
) {
    // returns the offset, as well as the static type of the variable in memory (so we know how to encode the value in memory)
    fn get_innermost_offset<H: HeapManager>(
        sf: &ir::StructField,
        ctx: EncodeContext<H>,
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

    match target {
        ir::TargetExpr::Global { globalidx, next } => {
            match next {
                None => {
                    encode_store_global(
                        ctx.globals.wasm_global_slice(*globalidx),
                        ctx.globals.global_types[*globalidx],
                        incoming_vartype,
                        expr_builder,
                    );
                }
                Some(struct_field) => {
                    // net wasm stack: [ptr, <incoming_vartype>] -> []

                    let (offset, ir_dest_vartype) = get_innermost_offset(struct_field, ctx);
                    encode_store_memory(
                        offset,
                        ir_dest_vartype,
                        incoming_vartype,
                        mutctx.scratch_mut(),
                        expr_builder,
                    );
                }
            }
        }
        ir::TargetExpr::Local { localidx, next } => {
            match next {
                None => {
                    encode_store_local(
                        mutctx.named_wasm_local_slice(*localidx),
                        mutctx.named_local_types_elem(*localidx),
                        incoming_vartype,
                        expr_builder,
                    );
                }
                Some(struct_field) => {
                    // net wasm stack: [ptr, <incoming_vartype>] -> []

                    let (offset, ir_dest_vartype) = get_innermost_offset(struct_field, ctx);
                    encode_store_memory(
                        offset,
                        ir_dest_vartype,
                        incoming_vartype,
                        mutctx.scratch_mut(),
                        expr_builder,
                    );
                }
            }
        }
    }
}

// Returns false if WebAssembly knows that the expr returns Void, true otherwise
// // Note: This is subset of IR::Expr that are Void, for example if-statements are never Void in WebAssembly, even though both branches return Void.
// net wasm stack: [] -> [<irvartype>] where `<irvartype>` is a valid encoding of an object with IR type expr.vartype
fn encode_expr<H: HeapManager>(
    expr: &ir::Expr,
    ctx: EncodeContext<H>,
    mutctx: &mut MutContext,
    expr_builder: &mut wasmgen::ExprBuilder,
) -> bool {
    match &expr.kind {
        ir::ExprKind::PrimUndefined => {
            // encodes the 'undefined' value
            assert!(
                expr.vartype == Some(ir::VarType::Undefined),
                "ICE: IR->Wasm: PrimUndefined does not have type undefined"
            );
            // Don't do anything, because undefined is encoded as <nothing>
            true
        }
        ir::ExprKind::PrimNumber { val } => {
            // encodes a literal number
            assert!(
                expr.vartype == Some(ir::VarType::Number),
                "ICE: IR->Wasm: PrimNumber does not have type number"
            );
            expr_builder.f64_const(*val);
            true
        }
        ir::ExprKind::PrimBoolean { val } => {
            // encodes a literal boolean
            assert!(
                expr.vartype == Some(ir::VarType::Boolean),
                "ICE: IR->Wasm: PrimBoolean does not have type boolean"
            );
            expr_builder.i32_const(if *val { 1 } else { 0 });
            true
        }
        ir::ExprKind::PrimString { val } => {
            // encodes a literal string
            assert!(
                expr.vartype == Some(ir::VarType::String),
                "ICE: IR->Wasm: PrimString does not have type string"
            );
            expr_builder.i32_const(ctx.string_pool.lookup(val) as i32);
            true
        }
        ir::ExprKind::PrimStructT { typeidx } => {
            // encodes a literal struct (semantically this is like the `new` keyword in Java), it will do a heap allocation
            assert!(
                expr.vartype == Some(ir::VarType::StructT { typeidx: *typeidx }),
                "ICE: IR->Wasm: PrimStructT does not have correct type, or typeidx is incorrect"
            );
            mutctx.heap_encode_fixed_allocation(ctx.heap, expr.vartype.unwrap(), expr_builder);
            true
        }
        ir::ExprKind::PrimFunc { funcidxs, closure } => {
            // encodes a function pointer and associated closure struct
            assert!(
                expr.vartype == Some(ir::VarType::Func),
                "ICE: IR->Wasm: PrimFunc does not have type func"
            );
            assert!(
                closure.vartype.is_some(),
                "ICE: IR->Wasm: Closure is not allowed to be noreturn"
            );

            // encode the closure expr (might contain sub-exprs in general)
            // net wasm stack: [] -> [<closure_irvartype>] (note: this might not be i32, if there is an empty closure)
            encode_expr(&closure, ctx, mutctx, expr_builder);

            // [<closure_irvartype>] -> [i32(closure)]
            ctx.heap
                .encode_closure_conversion(closure.vartype.unwrap(), expr_builder);

            // get the target tableidx of this thunk
            let tableidx: u32 = *ctx.thunk_map.get(funcidxs).unwrap();

            // encode the funcidx
            // net wasm stack: [] -> [funcidx]
            expr_builder.i32_const(tableidx as i32);

            true
        }
        ir::ExprKind::TypeCast {
            test: arg,
            expected,
            create_narrow_local,
            true_expr,
            false_expr,
        } => {
            // encodes a narrowing type cast

            assert!(
                arg.vartype == Some(ir::VarType::Any),
                "ICE: IR->Wasm: LHS of TypeOf builtin should have static type Any"
            );

            // encode the inner expr
            // net wasm stack: [] -> [i64 data, i32 tag]
            encode_expr(&arg, ctx, mutctx, expr_builder);

            // push `true` if the tag of the Any is equal to `expected`, `false` otherwise
            // net wasm stack: [] -> [i32 constant]
            expr_builder.i32_const(expected.tag());
            // net wasm stack: [i32 tag, i32 constant] -> [i32 result]
            expr_builder.i32_eq(); // compare two arguments for equality (1=true,0=false)

            // net wasm stack: [i32 result] -> [<expr.vartype>]
            if *create_narrow_local {
                // temporarily store the i64 data in a local, because we might need it later
                mutctx.with_scratch_i64(|mutctx, tmp_i64_data| {
                    // net wasm stack: [i64 data, i32 result] -> [i32 result]
                    mutctx.with_scratch_i32(|_mutctx, tmp_i32_result| {
                        expr_builder.local_set(tmp_i32_result);
                        expr_builder.local_set(tmp_i64_data);
                        expr_builder.local_get(tmp_i32_result);
                    });
                    // net wasm stack: [i32 result] -> [<expr.vartype>]
                    mutctx.with_unused_landing(|mutctx| {
                        multi_value_polyfill::if_with_opt_else(
                        encode_opt_vartype(expr.vartype),
                        ctx.options.wasm_multi_value,
                        mutctx,
                        expr_builder,
                        |mutctx, expr_builder| {
                            mutctx.with_uninitialized_named_local(
                                *expected,
                                |mutctx, ir_localidx| {
                                    // net wasm stack: [] -> []
                                    {
                                        let (named_wasm_local_slice, scratch) =
                                            mutctx.named_wasm_local_slice_and_scratch(ir_localidx);
                                        encode_unchecked_local_conv_any_narrowing(
                                            tmp_i64_data,
                                            named_wasm_local_slice,
                                            *expected,
                                            scratch,
                                            expr_builder,
                                        );
                                    }
                                    // net wasm stack: [] -> [<true_expr.vartype>]
                                    let wasm_reachable =
                                        encode_expr(true_expr, ctx, mutctx, expr_builder);
                                    // [<true_expr.vartype>] -> [<expr.vartype>]
                                    encode_opt_result_widening_operation(
                                        expr.vartype,
                                        true_expr.vartype,
                                        wasm_reachable,
                                        mutctx.scratch_mut(),
                                        expr_builder,
                                    );
                                },
                            );
                        },
                        (!false_expr.is_prim_undefined()).as_some(
                            |mutctx: &mut MutContext, expr_builder: &mut wasmgen::ExprBuilder| {
                                // net wasm stack: [] -> [<false_expr.vartype>]
                                let wasm_reachable =
                                    encode_expr(false_expr, ctx, mutctx, expr_builder);
                                // [<false_expr.vartype>] -> [<expr.vartype>]
                                encode_opt_result_widening_operation(
                                    expr.vartype,
                                    false_expr.vartype,
                                    wasm_reachable,
                                    mutctx.scratch_mut(),
                                    expr_builder,
                                );
                            },
                        ),
                    );
                    });
                });
            } else {
                // we don't want a narrow local

                // now, the stack still contains the i64 data beneath our result... we get rid of it first
                // net wasm stack: [i64 data, i32 result] -> [i32 result]
                mutctx.scratch_mut().with_i32(|_, localidx_result| {
                    expr_builder.local_set(localidx_result);
                    expr_builder.drop(); // remove the top thing (which is now the i64 data) from the stack
                    expr_builder.local_get(localidx_result);
                });

                // then we encode the true_expr and false_expr
                // net wasm stack:  [i32 result] -> [<expr.vartype>]
                mutctx.with_unused_landing(|mutctx| {
                    multi_value_polyfill::if_with_opt_else(
                        encode_opt_vartype(expr.vartype),
                        ctx.options.wasm_multi_value,
                        mutctx,
                        expr_builder,
                        |mutctx, expr_builder| {
                            // net wasm stack: [] -> [<true_expr.vartype>]
                            let wasm_reachable = encode_expr(true_expr, ctx, mutctx, expr_builder);
                            // [<true_expr.vartype>] -> [<expr.vartype>]
                            encode_opt_result_widening_operation(
                                expr.vartype,
                                true_expr.vartype,
                                wasm_reachable,
                                mutctx.scratch_mut(),
                                expr_builder,
                            );
                        },
                        (!false_expr.is_prim_undefined()).as_some(
                            |mutctx: &mut MutContext, expr_builder: &mut wasmgen::ExprBuilder| {
                                // net wasm stack: [] -> [<false_expr.vartype>]
                                let wasm_reachable =
                                    encode_expr(false_expr, ctx, mutctx, expr_builder);
                                // [<false_expr.vartype>] -> [<expr.vartype>]
                                encode_opt_result_widening_operation(
                                    expr.vartype,
                                    false_expr.vartype,
                                    wasm_reachable,
                                    mutctx.scratch_mut(),
                                    expr_builder,
                                );
                            },
                        ),
                    );
                });
            };
            true // since if-statements are never known by WebAssembly to be unreachable
        }
        ir::ExprKind::VarName { source } => {
            // encodes a read of a variable
            assert!(
                expr.vartype != None,
                "ICE: IR->Wasm: VarName expression cannot be Void"
            );
            // net wasm stack: [] -> [<source_vartype>]
            encode_target_value(source, expr.vartype.unwrap(), ctx, mutctx, expr_builder);
            true
        }
        ir::ExprKind::PrimAppl { prim_inst, args } => {
            // encodes a primitive (builtin) instruction, e.g. '<number>+<number>'
            encode_prim_inst(expr.vartype, *prim_inst, args, ctx, mutctx, expr_builder);
            true
        }
        ir::ExprKind::Appl {
            is_tail,
            func,
            args,
            location,
        } => {
            // encodes an indirect function call
            encode_appl(
                is_tail,
                expr.vartype,
                func,
                args,
                location,
                ctx,
                mutctx,
                expr_builder,
            );
            true
        }
        ir::ExprKind::DirectAppl { is_tail,  funcidx, args } => {
            // encodes a function call
            encode_direct_appl(is_tail, expr.vartype, *funcidx, args, ctx, mutctx, expr_builder);
            true
        }
        ir::ExprKind::Conditional {
            cond,
            true_expr,
            false_expr,
        } => {
            // encodes a conditional (i.e. a?b:c or an if-statement)

            assert!(
                cond.vartype == Some(ir::VarType::Boolean),
                "ICE: IR->Wasm: condition of cond_expr must be boolean"
            );

            // encode the condition
            // net wasm stack: [] -> [<cond.vartype>(bool)]
            encode_expr(cond, ctx, mutctx, expr_builder);
            // emit the if statement
            // net wasm stack:  [i32 result] -> [<expr.vartype>]
            mutctx.with_unused_landing(|mutctx| {
                multi_value_polyfill::if_with_opt_else(
                    encode_opt_vartype(expr.vartype),
                    ctx.options.wasm_multi_value,
                    mutctx,
                    expr_builder,
                    |mutctx, expr_builder| {
                        // net wasm stack: [] -> [<true_expr.vartype>]
                        let wasm_reachable = encode_expr(true_expr, ctx, mutctx, expr_builder);
                        // net wasm stack: [<true_expr.vartype>] -> [<expr.vartype>]
                        encode_opt_result_widening_operation(
                            expr.vartype,
                            true_expr.vartype,
                            wasm_reachable,
                            mutctx.scratch_mut(),
                            expr_builder,
                        );
                    },
                    (!false_expr.is_prim_undefined()).as_some(
                        |mutctx: &mut MutContext, expr_builder: &mut wasmgen::ExprBuilder| {
                            // net wasm stack: [] -> [<false_expr.vartype>]
                            let wasm_reachable = encode_expr(false_expr, ctx, mutctx, expr_builder);
                            // net wasm stack: [<false_expr.vartype>] -> [<expr.vartype>]
                            encode_opt_result_widening_operation(
                                expr.vartype,
                                false_expr.vartype,
                                wasm_reachable,
                                mutctx.scratch_mut(),
                                expr_builder,
                            );
                        },
                    ),
                );
            });
            true
        }
        ir::ExprKind::Declaration {
            local,
            init,
            contained_expr,
        } => {
            assert!(expr.vartype == contained_expr.vartype, "ICE: IR->Wasm: the expression in a Declaration must have the same type as the Declaration itself");
            if let Some(init_expr) = init {
                assert!(
                    init_expr.vartype.is_some(),
                    "ICE: IR->Wasm: init expr of a declaration cannot be noreturn"
                );
                // net wasm stack: [] -> [<init_expr.vartype>]
                encode_expr(init_expr, ctx, mutctx, expr_builder);
                mutctx.with_uninitialized_named_local(*local, |mutctx, named_localidx| {
                    // net wasm stack: [<init_expr.vartype>] -> []
                    encode_store_local(
                        mutctx.named_wasm_local_slice(named_localidx),
                        *local,
                        init_expr.vartype.unwrap(),
                        expr_builder,
                    );
                    // net wasm stack: [] -> [<contained_expr.vartype>]
                    encode_expr(contained_expr, ctx, mutctx, expr_builder)
                })
            } else {
                mutctx.with_named_local(
                    *local,
                    ctx.heap,
                    expr_builder,
                    |mutctx, expr_builder, _| {
                        encode_expr(contained_expr, ctx, mutctx, expr_builder)
                    },
                )
            }
            // note: we automatically propagate the WebAssembly-Voidness from the inner expr by returning it
        }
        ir::ExprKind::Assign {
            target,
            expr: rhs_expr,
        } => {
            if let Some(actual_vartype) = rhs_expr.vartype {
                // Note: JavaScript uses left-to-right evaluation order, so following of pointers should be done in encode_target_addr_pre().
                // encode stuff needed before expr (e.g. compute addresses):
                encode_target_addr_pre(target, actual_vartype, ctx, mutctx, expr_builder);
                // encode the expr (net wasm stack: [] -> [<actual_vartype>] where `<actual_vartype>` is a valid encoding of actual_vartype)
                encode_expr(rhs_expr, ctx, mutctx, expr_builder);
                // write the value from the stack to the target
                encode_target_addr_post(target, actual_vartype, ctx, mutctx, expr_builder);
                true
            } else {
                panic!("ICE: IR->Wasm: expression in assignment statement cannot be Void");
            }
        }
        ir::ExprKind::Return { expr: inner_expr } => {
            // For return expressions, we assume that any type inference engine already ensures that:
            // - the return type of this function is not Void
            // - the inner expr type is not Void
            // - the return type is at least as wide as the inner expr type
            match inner_expr.vartype {
                None => panic!("ICE: IR->Wasm: expression in return statement cannot be Void"),
                Some(inner_type) => {
                    match ctx.return_type {
                        None => panic!("ICE: IR->Wasm: cannot have return expression in a function that returns Void"),
                        Some(ret_type) => {
                            // net wasm stack: [] -> [<expr.vartype>]
                            encode_expr(inner_expr, ctx, mutctx, expr_builder);

                            // net wasm stack: [<expr.vartype>] -> [<return_calling_conv(ctx.return_type.unwrap())>]
                            encode_return_calling_conv(
                                ret_type,
                                inner_type,
                                ctx.options.wasm_multi_value,
                                ctx.stackptr,
                                mutctx.scratch_mut(),
                                expr_builder,
                            );
                            // return the value on the stack (or in the unprotected stack) (which now has the correct type)
                            expr_builder.return_();
                        }
                    };
                }
            };

            // returns false, because WebAssembly knows that anything after a return instruction is unreachable
            false
        }
        ir::ExprKind::Break {
            num_frames,
            expr: inner_expr,
        } => {
            assert!(inner_expr.vartype.is_some());

            // encode the inner expr
            // net wasm stack: [] -> [<inner_expr.vartype>]
            encode_expr(inner_expr, ctx, mutctx, expr_builder);

            // get the landing site properties
            let (landing_idx, landing_vartype, landing_ctx) = mutctx.get_wasm_landing(*num_frames);

            // net wasm stack: [<inner_expr.vartype>] -> [<landing_vartype>]
            encode_opt_result_widening_operation(
                Some(landing_vartype),
                inner_expr.vartype,
                true,
                mutctx.scratch_mut(),
                expr_builder,
            );

            // encode the unconditional branch
            multi_value_polyfill::break_(
                landing_idx,
                &landing_ctx,
                encode_vartype(landing_vartype),
                ctx.options.wasm_multi_value,
                mutctx,
                expr_builder,
            );

            // returns true, because WebAssembly branch instruction is stack-polymorphic
            false
        }
        ir::ExprKind::Block { expr: inner_expr } => {
            // register that a Break can land here
            multi_value_polyfill::block(
                encode_opt_vartype(expr.vartype),
                ctx.options.wasm_multi_value,
                mutctx,
                expr_builder,
                |mutctx, landing_ctx, expr_builder| {
                    mutctx.with_landing(expr.vartype.unwrap(), landing_ctx, |mutctx| {
                        encode_expr(inner_expr, ctx, mutctx, expr_builder);
                    })
                },
            );

            // returns true, because WebAssembly never regards a block as stack-polymorphic even if it is actually the case
            true
        }
        ir::ExprKind::Sequence { content } => {
            if content.is_empty() {
                assert!(
                    expr.vartype == Some(ir::VarType::Undefined),
                    "ICE: IR->Wasm: an empty sequence expression must return undefined"
                );
                // Don't do anything, because undefined is encoded as <nothing>
                true
            } else {
                let (others, last_slice) = content.split_at(content.len() - 1);
                assert!(last_slice.len() == 1);
                let last = &last_slice[0];
                assert!(
                    last.vartype == expr.vartype,
                    "sequence expression must return the type of its last expression"
                );
                for inner_expr in others {
                    encode_expr(inner_expr, ctx, mutctx, expr_builder);
                    if let Some(actual_vartype) = inner_expr.vartype {
                        encode_drop_value(actual_vartype, expr_builder);
                    } else {
                        panic!("ICE: IR->Wasm: Void expression can only be the last expression in a sequence");
                    }
                }
                encode_expr(last, ctx, mutctx, expr_builder)
            }
        }
        ir::ExprKind::Trap { code, location } => {
            // Calls the predefined imported function, which must never return.
            expr_builder.i32_const(*code as i32);
            expr_builder.i32_const(0);
            expr_builder.i32_const(location.file as i32);
            expr_builder.i32_const(location.start.line as i32);
            expr_builder.i32_const(location.start.column as i32);
            expr_builder.i32_const(location.end.line as i32);
            expr_builder.i32_const(location.end.column as i32);
            expr_builder.call(ctx.error_func);
            expr_builder.unreachable();
            // in the future, PrimInst::Trap should take a error code parameter, and maybe source location
            // and call a noreturn function to the embedder (JavaScript).
            false
        }
    }
}

// Loads the eventual value of `source`, following all struct fields, onto the stack, encoded as `outgoing_vartype`.
// `outgoing_vartype` is required to be equivalent or subtype of the source vartype.  (Otherwise it means the optimiser is broken.)
// net wasm stack: [] -> [<outgoing_vartype>]
fn encode_target_value<H: HeapManager>(
    source: &ir::TargetExpr,
    outgoing_vartype: ir::VarType,
    ctx: EncodeContext<H>,
    mutctx: &mut MutContext,
    expr_builder: &mut wasmgen::ExprBuilder,
) {
    // net wasm stack: [struct_ptr] -> [<outgoing_vartype>]
    fn load_inner_local<H: HeapManager>(
        outer_struct_field: &ir::StructField,
        outgoing_vartype: ir::VarType,
        ctx: EncodeContext<H>,
        mutctx: &mut MutContext,
        expr_builder: &mut wasmgen::ExprBuilder,
    ) {
        if let Some(inner_struct_field) = &outer_struct_field.next {
            // If there is an inner struct, it must be known at compilation time
            assert!(
                ctx.struct_types[outer_struct_field.typeidx][outer_struct_field.fieldidx]
                    == ir::VarType::StructT {
                        typeidx: inner_struct_field.typeidx
                    },
                "ICE: IR->Wasm: Struct type incorrect or not proved at compilation time"
            );
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
            let offset: u32 = ctx.struct_field_byte_offsets[outer_struct_field.typeidx]
                [outer_struct_field.fieldidx];
            let ir_source_vartype: ir::VarType =
                ctx.struct_types[outer_struct_field.typeidx][outer_struct_field.fieldidx];
            encode_load_memory(
                offset,
                ir_source_vartype,
                outgoing_vartype,
                mutctx.scratch_mut(),
                expr_builder,
            );
        }
    }

    match source {
        ir::TargetExpr::Global { globalidx, next } => {
            match next {
                None => {
                    // `outgoing_vartype` is required to be equivalent or subtype of the source vartype
                    encode_load_global(
                        ctx.globals.wasm_global_slice(*globalidx),
                        ctx.globals.global_types[*globalidx],
                        outgoing_vartype,
                        expr_builder,
                    );
                }
                Some(struct_field) => {
                    // For Source1, structs can only arise from closures and local variables, so their types must already be known at compilation time.
                    assert!(
                        ctx.globals.global_types[*globalidx]
                            == ir::VarType::StructT {
                                typeidx: struct_field.typeidx
                            },
                        "ICE: IR->Wasm: Struct type incorrect or not proved at compilation time"
                    );

                    // net wasm stack: [] -> [struct_ptr]
                    expr_builder.global_get(ctx.globals.wasm_global_slice(*globalidx)[0]);

                    // net wasm stack: [struct_ptr] -> [<outgoing_vartype>]
                    load_inner_local(struct_field, outgoing_vartype, ctx, mutctx, expr_builder);
                }
            }
        }
        ir::TargetExpr::Local { localidx, next } => {
            match next {
                None => {
                    // `outgoing_vartype` is required to be equivalent or subtype of the source vartype
                    encode_load_local(
                        mutctx.named_wasm_local_slice(*localidx),
                        mutctx.named_local_types_elem(*localidx),
                        outgoing_vartype,
                        expr_builder,
                    );
                }
                Some(struct_field) => {
                    // For Source1, structs can only arise from closures and local variables, so their types must already be known at compilation time.
                    assert!(
                        mutctx.named_local_types_elem(*localidx)
                            == ir::VarType::StructT {
                                typeidx: struct_field.typeidx
                            },
                        "ICE: IR->Wasm: Struct type incorrect or not proved at compilation time"
                    );

                    // net wasm stack: [] -> [struct_ptr]
                    expr_builder.local_get(mutctx.named_wasm_local_slice(*localidx)[0]);

                    // net wasm stack: [struct_ptr] -> [<outgoing_vartype>]
                    load_inner_local(struct_field, outgoing_vartype, ctx, mutctx, expr_builder);
                }
            }
        }
    }
}

// Requires: the primitive instruction actually has the correct number of parameters,
// and the primitive instruction requires parameters with type equal to or wider than the types in `args`.
// and the return type is exactly the correct type of the primitive instruction.
// (so we allow widening for params, and it must be exact for returns)
// net wasm stack: [] -> [<return_type>]
fn encode_prim_inst<H: HeapManager>(
    return_type: Option<ir::VarType>,
    prim_inst: ir::PrimInst,
    args: &[ir::Expr],
    ctx: EncodeContext<H>,
    mutctx: &mut MutContext,
    expr_builder: &mut wasmgen::ExprBuilder,
) {
    let (prim_param_types, prim_return_type) = prim_inst.signature();
    assert!(
        prim_return_type == return_type,
        "Return type of PrimInst must match the type of Expr"
    );
    // net wasm stack: [] -> [<prim_param_types[0]>, <prim_param_types[1]>, ...]
    encode_args_to_call_function(prim_param_types, args, ctx, mutctx, expr_builder);
    // net wasm stack: [<prim_param_types[0]>, <prim_param_types[1]>, ...] -> [<return_type>]
    match prim_inst {
        ir::PrimInst::NumberAdd => expr_builder.f64_add(),
        ir::PrimInst::NumberSub => expr_builder.f64_sub(),
        ir::PrimInst::NumberMul => expr_builder.f64_mul(),
        ir::PrimInst::NumberDiv => expr_builder.f64_div(),
        ir::PrimInst::NumberRem => {
            // webassembly has no floating point remainder operation...
            // we have to manually do it
            // https://stackoverflow.com/questions/26342823/implementation-of-fmod-function
            // note: not very numerically rigourous, there might be some rounding errors
            // algorithm used here for `a % b`:
            // a - (trunc(a / b) * b)
            mutctx.with_scratch_f64(|mutctx, localidx_first| {
                mutctx.with_scratch_f64(|_mutctx, localidx_second| {
                    expr_builder.local_set(localidx_second);
                    expr_builder.local_tee(localidx_first);
                    expr_builder.local_get(localidx_first);
                    expr_builder.local_get(localidx_second);
                    expr_builder.f64_div();
                    expr_builder.f64_trunc();
                    expr_builder.local_get(localidx_second);
                    expr_builder.f64_mul();
                    expr_builder.f64_sub();
                });
            });
        }
        ir::PrimInst::NumberEq => expr_builder.f64_eq(),
        ir::PrimInst::NumberNeq => expr_builder.f64_ne(),
        ir::PrimInst::NumberGt => expr_builder.f64_gt(),
        ir::PrimInst::NumberLt => expr_builder.f64_lt(),
        ir::PrimInst::NumberGe => expr_builder.f64_ge(),
        ir::PrimInst::NumberLe => expr_builder.f64_le(),
        ir::PrimInst::BooleanEq => expr_builder.i32_eq(),
        ir::PrimInst::BooleanNeq => expr_builder.i32_ne(),
        ir::PrimInst::BooleanAnd => expr_builder.i32_and(),
        ir::PrimInst::BooleanOr => expr_builder.i32_or(),
        ir::PrimInst::BooleanNot => {
            // webassembly has no boolean negation operation...
            // we have to manually do it
            // not(0) -> 1
            // not(1) -> 0
            // algorithm used here for `!a`:
            // a xor 1
            expr_builder.i32_const(1);
            expr_builder.i32_xor();
        }
        ir::PrimInst::NumberNegate => expr_builder.f64_neg(),
        ir::PrimInst::StringAdd => {
            string_prim_inst::encode_string_add(
                ctx.memidx,
                ctx.heap,
                ctx.options.wasm_bulk_memory,
                mutctx,
                expr_builder,
            );
        }
        ir::PrimInst::StringEq => {
            string_prim_inst::encode_string_eq(mutctx.scratch_mut(), expr_builder);
        }
        ir::PrimInst::StringNeq => {
            string_prim_inst::encode_string_ne(mutctx.scratch_mut(), expr_builder);
        }
        ir::PrimInst::StringGt => {
            string_prim_inst::encode_string_gt(mutctx.scratch_mut(), expr_builder);
        }
        ir::PrimInst::StringLt => {
            string_prim_inst::encode_string_lt(mutctx.scratch_mut(), expr_builder);
        }
        ir::PrimInst::StringGe => {
            string_prim_inst::encode_string_ge(mutctx.scratch_mut(), expr_builder);
        }
        ir::PrimInst::StringLe => {
            string_prim_inst::encode_string_le(mutctx.scratch_mut(), expr_builder);
        }
    }
}

// Requires: the callee actually has the correct number of parameters,
// and the func_expr has type VarType::Func or VarType::Any
// and the callee must have all params of type Any, and return type must also be Any.
// (to use more specific types, we must know the target function at compilation time, and hence use the DirectAppl)
// net wasm stack: [] -> [<return_type>]
fn encode_appl<H: HeapManager>(
    is_tail: &bool,
    return_type: Option<ir::VarType>,
    func_expr: &ir::Expr,
    args: &[ir::Expr],
    location: &ir::SourceLocation,
    ctx: EncodeContext<H>,
    mutctx: &mut MutContext,
    expr_builder: &mut wasmgen::ExprBuilder,
) {
    // An Appl (indirect call) should be encoded using the uniform calling convention, placing actual arguments onto the unprotected stack.

    // Assert that the return type is Any (calling a function indirectly should return Any)
    assert!(return_type == Some(ir::VarType::Any));

    // Assert that the func_expr has static type Func (ir should have used a TypeCast expr otherwise)
    assert!(func_expr.vartype == Some(ir::VarType::Func));

    // Evaluate the `func_expr`:
    // net wasm stack: [] -> [i32(closure ptr), i32(func ptr)]
    encode_expr(func_expr, ctx, mutctx, expr_builder);

    // todo!: this is currently bad codegen, we should only stuff things into locals if
    // there is a later param whose allocation is may_allocate

    // todo!: this should be just the closure, but mutctx doesn't support that yet
    // (it wouldn't put it on the gc roots stack if it's just the closure)
    mutctx.with_uninitialized_shadow_local(ir::VarType::Func, |mutctx, localidx_func| {
        // net wasm stack: [<Func>] -> []
        encode_store_local(
            mutctx.wasm_local_slice(localidx_func),
            ir::VarType::Func,
            ir::VarType::Func,
            expr_builder,
        );

        // Encode all the arguments
        // This will ensure that all args are evaluated before placing them onto the unprotected stack
        // and be careful that if the GC is triggered while evaluating an arg, we will be using the new value of the pointer-type args.
        // net wasm stack: [] -> [i32(closure), i32(num_args)]
        encode_args_to_call_indirect_function(
            args,
            mutctx.wasm_local_slice(localidx_func)[1],
            ctx,
            mutctx,
            expr_builder,
        );

        // encode the proper caller id (which is the memory location of the SourceLocation)
        // net wasm stack: [] -> [i32(callerid)]
        expr_builder.i32_const(*ctx.appl_data_encoder.get(location).unwrap() as i32);

        // push the tableidx onto the stack
        // net wasm stack: [] -> [tableidx]
        expr_builder.local_get(mutctx.wasm_local_slice(localidx_func)[0]);

        // todo!(For optimisation, heap_encode_prologue_epilogue should only be called if the callee might allocate)
        // Note: encode_args_to_call_function should be *before* encode_local_roots_prologue, since the args themselves might make function calls.
        if true {
            // This function might allocate memory, so we need to store the locals in the gc_roots stack first.

            // call the function with gc prologue and epilogue
            mutctx.heap_encode_prologue_epilogue(ctx.heap, expr_builder, |mutctx, expr_builder| {
                // call the function (indirectly, using uniform calling convention)
                if *is_tail {
                    return expr_builder.return_call_indirect(
                        mutctx
                            .module_wrapper()
                            .add_wasm_type(wasmgen::FuncType::new(
                                Box::new([
                                    wasmgen::ValType::I32,
                                    wasmgen::ValType::I32,
                                    wasmgen::ValType::I32,
                                ]),
                                encode_result(Some(ir::VarType::Any), ctx.options.wasm_multi_value),
                            )),
                        wasmgen::TableIdx { idx: 0 },
                    );
                } else {
                    return expr_builder.call_indirect(
                        mutctx
                            .module_wrapper()
                            .add_wasm_type(wasmgen::FuncType::new(
                                Box::new([
                                    wasmgen::ValType::I32,
                                    wasmgen::ValType::I32,
                                    wasmgen::ValType::I32,
                                ]),
                                encode_result(Some(ir::VarType::Any), ctx.options.wasm_multi_value),
                            )),
                        wasmgen::TableIdx { idx: 0 },
                    );
                }
            });
        } else {
            // This function is guaranteed not to allocate memory, so we don't need to put the locals on the gc_roots stack.

            // call the function (indirectly)
            if *is_tail {
                return expr_builder.return_call_indirect(
                    mutctx
                        .module_wrapper()
                        .add_wasm_type(wasmgen::FuncType::new(
                            Box::new([
                                wasmgen::ValType::I32,
                                wasmgen::ValType::I32,
                                wasmgen::ValType::I32,
                            ]),
                            encode_result(Some(ir::VarType::Any), ctx.options.wasm_multi_value),
                        )),
                    wasmgen::TableIdx { idx: 0 },
                );
            } else {
                return expr_builder.call_indirect(
                    mutctx
                        .module_wrapper()
                        .add_wasm_type(wasmgen::FuncType::new(
                            Box::new([
                                wasmgen::ValType::I32,
                                wasmgen::ValType::I32,
                                wasmgen::ValType::I32,
                            ]),
                            encode_result(Some(ir::VarType::Any), ctx.options.wasm_multi_value),
                        )),
                    wasmgen::TableIdx { idx: 0 },
                );
            }

        }

        // fetch return values from the location prescribed by the calling convention back to the stack
        encode_post_appl_calling_conv(
            Some(ir::VarType::Any),
            ctx.options.wasm_multi_value,
            ctx.stackptr,
            mutctx.scratch_mut(),
            expr_builder,
        );
    });
}

// Requires: the callee actually has the correct number of parameters,
// and the each parameter of the callee must have a type at least as wide as (i.e. be a supertype of) the corresponding args[i].vartype,
// and the return type of the callee is exactly return_type.
// net wasm stack: [] -> [<return_type>]
fn encode_direct_appl<H: HeapManager>(
    is_tail: &bool,
    return_type: Option<ir::VarType>,
    funcidx: ir::FuncIdx,
    args: &[ir::Expr],
    ctx: EncodeContext<H>,
    mutctx: &mut MutContext,
    expr_builder: &mut wasmgen::ExprBuilder,
) {
    // Assert that this funcidx exists
    assert!(funcidx < ctx.ir_signature_list.len(), "invalid funcidx");

    let signature: &Signature = &ctx.ir_signature_list[funcidx];

    // Assert that the function has correct return type
    assert!(return_type == signature.result);

    // Encode all the arguments
    encode_args_to_call_function(&signature.params, args, ctx, mutctx, expr_builder);

    // todo!(For optimisation, heap_encode_prologue_epilogue should only be called if the callee might allocate)
    // Note: encode_args_to_call_function should be *before* encode_local_roots_prologue, since the args themselves might make function calls.
    if true {
        // This function might allocate memory, so we need to store the locals in the gc_roots stack first.

        // call the function with gc prologue and epilogue
        mutctx.heap_encode_prologue_epilogue(ctx.heap, expr_builder, |_mutctx, expr_builder| {
            // call the function
            if *is_tail {
                expr_builder.return_call(ctx.wasm_funcidxs[funcidx]);
            } else {
                expr_builder.call(ctx.wasm_funcidxs[funcidx]);
            }
        });
    } else {
        // This function is guaranteed not to allocate memory, so we don't need to put the locals on the gc_roots stack.

        // call the function
        if *is_tail {
            expr_builder.return_call(ctx.wasm_funcidxs[funcidx]);
        } else {
            expr_builder.call(ctx.wasm_funcidxs[funcidx]);
        }
    }

    // fetch return values from the location prescribed by the calling convention back to the stack
    encode_post_appl_calling_conv(
        return_type,
        ctx.options.wasm_multi_value,
        ctx.stackptr,
        mutctx.scratch_mut(),
        expr_builder,
    );
}

// This function prepares subexpressions when calling a function.
// It evaluates arguments in left-to-right order, which is required for Source.
// Each `expected_param_types` must be at least as wide as each `[args[i].vartype, ...]`
// Otherwise we will panic.
// net wasm stack: [] -> [<expected_param_types[0]>, <expected_param_types[1]>, ...]
fn encode_args_to_call_function<H: HeapManager>(
    expected_param_types: &[ir::VarType],
    args: &[ir::Expr],
    ctx: EncodeContext<H>,
    mutctx: &mut MutContext,
    expr_builder: &mut wasmgen::ExprBuilder,
) {
    // todo! there might be a bug here - if a GC event happens midway encoding args, the old args that are references will break.
    // See encode_args_to_call_indirect_function() for reference.
    //
    assert!(
        expected_param_types.len() == args.len(),
        "expected_param_types and args must be same length when encoding args to call function"
    );
    for (expected_type, arg) in expected_param_types.iter().zip(args.iter()) {
        if let Some(unwrapped_arg_vartype) = arg.vartype {
            // net wasm stack: [] -> [<arg.vartype>]
            encode_expr(arg, ctx, mutctx, expr_builder);
            // net wasm stack: [<arg.vartype>] -> [<expected_type>]
            encode_widening_operation(
                *expected_type,
                unwrapped_arg_vartype,
                mutctx.scratch_mut(),
                expr_builder,
            );
        } else {
            panic!("argument type cannot be Void");
        }
    }
}

// This function prepares subexpressions when calling an indirect function.
// It is like encode_args_to_call_function(), but instead it calls a function indirectly,
// and uses the uniform calling convention for it.
// As such, all the parameters are implicitly encoded as Any.
// The closure is an i32.  It should be written *after* evaluating all the may_allocate params.
// net wasm stack: [] -> [i32(num_params)]
fn encode_args_to_call_indirect_function<H: HeapManager>(
    args: &[ir::Expr],
    closure_local: wasmgen::LocalIdx,
    ctx: EncodeContext<H>,
    mutctx: &mut MutContext,
    expr_builder: &mut wasmgen::ExprBuilder,
) {
    // encode the args
    // (we do some things that only work when there is at least one arg)
    if !args.is_empty() {
        // load the adjusted stackptr
        // net wasm stack: [] -> [i32(stackptr)]
        expr_builder.global_get(ctx.stackptr);
        expr_builder.i32_const((args.len() as u32 * size_in_memory(ir::VarType::Any)) as i32);
        expr_builder.i32_sub();

        // net wasm stack: [stackptr] -> []
        if args.len() > 1 {
            mutctx.with_scratch_i32(|mutctx, stackptr_localidx| {
                // note: we only write the args to memory after evaluating all of them
                // because calling a function may overwrite the parameter area on the unprotected stack.

                // note: have to manually push/pop locals from mutctx
                // so that we don't need to arbitrary nest the closures

                // net wasm stack: [stackptr] -> [stackptr]
                expr_builder.local_tee(stackptr_localidx);

                let (last_arg, other_args) = args.split_last().unwrap();

                let mut localidxs = Vec::new();

                // net wasm stack: [stackptr] -> [stackptr]
                for arg in other_args {
                    // net wasm stack: [] -> [<arg.vartype>]
                    encode_expr(arg, ctx, mutctx, expr_builder);

                    let localidx = mutctx.add_uninitialized_shadow_local(arg.vartype.unwrap());
                    localidxs.push(localidx);

                    // net wasm stack: [<arg.vartype>] -> []
                    encode_store_local(
                        mutctx.wasm_local_slice(localidx),
                        arg.vartype.unwrap(),
                        arg.vartype.unwrap(),
                        expr_builder,
                    );
                }

                // net wasm stack: [stackptr] -> []
                {
                    // net wasm stack: [] -> [<last_arg.vartype>]
                    encode_expr(last_arg, ctx, mutctx, expr_builder);

                    // net wasm stack: [stackptr, <arg.vartype>] -> []
                    encode_store_memory(
                        0,
                        ir::VarType::Any,
                        last_arg.vartype.unwrap(),
                        mutctx.scratch_mut(),
                        expr_builder,
                    );
                }

                // net wasm stack: [] -> []
                for ((i, arg), localidx) in other_args
                    .iter()
                    .enumerate()
                    .zip(localidxs.into_iter())
                    .rev()
                {
                    // net wasm stack: [] -> [stackptr]
                    expr_builder.local_get(stackptr_localidx);

                    // net wasm stack: [] -> [<arg.vartype>]
                    encode_load_local(
                        mutctx.wasm_local_slice(localidx),
                        arg.vartype.unwrap(),
                        arg.vartype.unwrap(),
                        expr_builder,
                    );

                    mutctx.remove_shadow_local(arg.vartype.unwrap());

                    // net wasm stack: [stackptr, <arg.vartype>] -> []
                    encode_store_memory(
                        (other_args.len() - i) as u32 * size_in_memory(ir::VarType::Any),
                        ir::VarType::Any,
                        arg.vartype.unwrap(),
                        mutctx.scratch_mut(),
                        expr_builder,
                    );
                }
            });
        } else {
            // net wasm stack: [] -> [<arg.vartype>]
            encode_expr(&args[0], ctx, mutctx, expr_builder);
            // net wasm stack: [i32(stackptr), <arg.vartype>] -> []
            encode_store_memory(
                0,
                ir::VarType::Any,
                args[0].vartype.unwrap(),
                mutctx.scratch_mut(),
                expr_builder,
            );
        }
    }

    // net wasm stack: [] -> [i32(closure)]
    expr_builder.local_get(closure_local);

    // net wasm stack: [] -> [i32(num_params)]
    expr_builder.i32_const(args.len() as i32);
}

// Encodes a small function that uses uniform calling convention and
// determines the correct function to actually call based on the signature.
// The correct function is called using a direct call.
// Returns the tableidx of this thunk.
// The context also deduplicates identical thunks using a hashmap stored in the mutctx.
// todo! the actual functions should be inlined into the thunk (probably all the time)
// net wasm stack: [] -> [stack-polymorphic]
fn encode_thunk<H: HeapManager>(
    overload_entries: &[ir::OverloadEntry],
    ctx: EncodeContext<H>,
    mutctx: &mut MutContext,
    expr_builder: &mut wasmgen::ExprBuilder,
) {
    // extract all possible number of params
    let mut param_counts: Vec<u32> = overload_entries
        .iter()
        .rev() // for consistency, but it doesn't affect it because we sort later
        .map(|o| {
            let num_actual_params = ctx.ir_signature_list[o.funcidx].params.len() as u32;
            let closure_subtract = if o.has_closure_param { 1 } else { 0 };
            assert!(num_actual_params >= closure_subtract);
            num_actual_params - closure_subtract
        })
        .collect();

    // sort and dedup
    param_counts.sort();
    param_counts.dedup();

    // the params
    let closure = wasmgen::LocalIdx { idx: 0 };
    let num_ir_params = wasmgen::LocalIdx { idx: 1 };
    let sourceloc_ref = wasmgen::LocalIdx { idx: 2 };

    // firstly, look at the number of params
    // if there are no options - no branching needed
    // if there is only one option - use 'if' statement
    // otherwise, use br_table
    match param_counts.len() {
        0 => {
            // trap immediately
            raise_trap(ctx.error_func, sourceloc_ref, expr_builder);
        }
        1 => {
            // {
            //   if (num_ir_params != param_counts[0]) break;
            //   <stuff>
            //   return <ret>
            // }
            // <trap>
            expr_builder.block(&[]);
            {
                expr_builder.local_get(num_ir_params);
                expr_builder.i32_const(param_counts[0] as i32);
                expr_builder.i32_ne();
                expr_builder.br_if(0);
                emit_thunk_impl_num_params(
                    param_counts[0],
                    0,
                    closure,
                    overload_entries,
                    ctx,
                    mutctx,
                    expr_builder,
                );
            }
            expr_builder.end();
            // wrong number of params
            raise_trap(ctx.error_func, sourceloc_ref, expr_builder);
        }
        _ => {
            // more than one case... we need a br_table.
            //   {
            //     {
            //       {
            //         br_table [0 1 ...] N
            //       }
            //       <stuff for case 0>
            //       return_call <...> (or br to default case if it doesn't exist)
            //     }
            //     <stuff for case 1>
            //     return_call <...> (or br to default case if it doesn't exist)
            //   }
            //   <stuff for case default>
            //   <trap>
            for _ in 0..=param_counts.len() {
                expr_builder.block(&[]);
            }
            let mut list: Vec<u32> = Vec::new();
            for _ in 0..=*param_counts.last().unwrap() {
                list.push(param_counts.len() as u32);
            }
            for (i, x) in param_counts.iter().copied().enumerate() {
                list[x as usize] = i as u32;
            }
            expr_builder.local_get(num_ir_params);
            expr_builder.br_table(&list, param_counts.len() as u32);
            for (i, x) in param_counts.iter().copied().enumerate() {
                expr_builder.end();
                emit_thunk_impl_num_params(
                    x,
                    param_counts.len() as u32 - i as u32 - 1,
                    closure,
                    overload_entries,
                    ctx,
                    mutctx,
                    expr_builder,
                );
            }

            expr_builder.end();
            // wrong number of params
            raise_trap(ctx.error_func, sourceloc_ref, expr_builder);
        }
    }

    // only returns if it traps and trap_depth == 0
    // when trap_depth == 0 we have an optimisation - just fall off the length of the block to get to the trap statement
    // net wasm stack: [] -> []
    fn emit_thunk_impl_num_params<H: HeapManager>(
        num_params: u32, // without the closure
        trap_depth: u32, // how far we need to break to get to the error_func call (zero := next '}')
        closure: wasmgen::LocalIdx,
        overload_entries: &[ir::OverloadEntry],
        ctx: EncodeContext<H>,
        mutctx: &mut MutContext,
        expr_builder: &mut wasmgen::ExprBuilder,
    ) {
        // now we know that there are exactly `num_params` on the unprotected stack

        // load the params into locals
        let param_types: Box<[ir::VarType]> = (0..num_params).map(|_| ir::VarType::Any).collect();
        mutctx.with_uninitialized_shadow_locals(&param_types, |mutctx, localidx_params_begin| {
            // load all the params from the unprotected stack into locals
            if num_params > 0 {
                // net wasm stack: [] -> [i32(localidx_stackptr)]
                expr_builder.global_get(ctx.stackptr);
                expr_builder.i32_const((size_in_memory(ir::VarType::Any) * num_params) as i32);
                expr_builder.i32_sub();

                if num_params > 1 {
                    // copy of the stackptr
                    // net wasm stack: [i32(localidx_stackptr)] -> []
                    mutctx.with_scratch_i32(|mutctx, localidx_stackptr| {
                        // adjust the stackptr to the end of the param list
                        expr_builder.local_tee(localidx_stackptr);

                        // first param
                        // net wasm stack: [i32(localidx_stackptr)] -> []
                        encode_load_memory(
                            size_in_memory(ir::VarType::Any) * (num_params - 1),
                            ir::VarType::Any,
                            ir::VarType::Any,
                            mutctx.scratch_mut(),
                            expr_builder,
                        );
                        encode_store_local(
                            mutctx.wasm_local_slice(localidx_params_begin),
                            ir::VarType::Any,
                            ir::VarType::Any,
                            expr_builder,
                        );

                        // remaining params
                        // net wasm stack: [] -> []
                        for i in 1..num_params {
                            // net wasm stack: [] -> []
                            expr_builder.local_get(localidx_stackptr);
                            encode_load_memory(
                                size_in_memory(ir::VarType::Any) * (num_params - i - 1),
                                ir::VarType::Any,
                                ir::VarType::Any,
                                mutctx.scratch_mut(),
                                expr_builder,
                            );
                            encode_store_local(
                                mutctx.wasm_local_slice(localidx_params_begin + i as usize),
                                ir::VarType::Any,
                                ir::VarType::Any,
                                expr_builder,
                            );
                        }
                    });
                } else {
                    // 1 param only
                    // no need the extra local
                    // net wasm stack: [i32(localidx_stackptr)] -> []
                    encode_load_memory(
                        0,
                        ir::VarType::Any,
                        ir::VarType::Any,
                        mutctx.scratch_mut(),
                        expr_builder,
                    );
                    encode_store_local(
                        mutctx.wasm_local_slice(localidx_params_begin),
                        ir::VarType::Any,
                        ir::VarType::Any,
                        expr_builder,
                    );
                }
            }

            // now we do a match param-by-param
            // using if.. else if.. else if.. etc
            // because there isn't always a nice way to use br_table that can enforce the order requirement of overload resolution
            // if (...) {
            //   <...> // this is noreturn
            // }
            // if (...) {
            //   <...> // this is noreturn
            // }
            // if (...) {
            //   <...> // this is noreturn
            // }
            // br <trap_depth>; only necessary if the last else-if is not trivially true
            let mut has_catch_all: bool = false;
            for (params, result, oe) in overload_entries
                .iter()
                .rev() // according to ir spec we match from back to front
                .filter_map(|oe| {
                    let params: &[ir::VarType] = if !oe.has_closure_param {
                        &ctx.ir_signature_list[oe.funcidx].params
                    } else {
                        assert!(!ctx.ir_signature_list[oe.funcidx].params.is_empty());
                        &ctx.ir_signature_list[oe.funcidx].params[1..]
                    };
                    if params.len() as u32 == num_params {
                        Some((params, ctx.ir_signature_list[oe.funcidx].result, oe))
                    } else {
                        None
                    }
                })
            {
                if params
                    .iter()
                    .copied()
                    .all(|ir_vartype| ir_vartype == ir::VarType::Any)
                {
                    // this is a catch-all overload
                    // no need to emit 'if' statement
                    emit_thunk_impl_tail_call(
                        oe,
                        closure,
                        localidx_params_begin,
                        params,
                        result,
                        ctx,
                        mutctx,
                        expr_builder,
                    );

                    has_catch_all = true;
                    break;
                } else {
                    let mut non_any_it = params
                        .iter()
                        .copied()
                        .enumerate()
                        .filter(|(_, ir_vartype)| *ir_vartype != ir::VarType::Any);

                    // emit the first condition
                    // net wasm stack: [] -> [i32(cond)]
                    {
                        let (i, ir_vartype) = non_any_it.next().unwrap();
                        // put the i32 tag onto the stack
                        expr_builder
                            .local_get(mutctx.wasm_local_slice(localidx_params_begin + i)[0]);
                        // put the constant to compare with
                        expr_builder.i32_const(ir_vartype.tag());
                        // compare them
                        expr_builder.i32_eq();
                    }

                    // emit the remaining conditions
                    // net wasm stack: [i32(cond)] -> [i32(cond)]
                    for (i, ir_vartype) in non_any_it {
                        // put the i32 tag onto the stack
                        expr_builder
                            .local_get(mutctx.wasm_local_slice(localidx_params_begin + i)[0]);
                        // put the constant to compare with
                        expr_builder.i32_const(ir_vartype.tag());
                        // compare them
                        expr_builder.i32_eq();
                        // apply AND with the previous condition
                        expr_builder.i32_and();
                    }

                    // test the condition (this is actually noreturn)
                    // net wasm stack: [i32(cond)] -> []
                    expr_builder.if_(&[]);
                    {
                        emit_thunk_impl_tail_call(
                            oe,
                            closure,
                            localidx_params_begin,
                            params,
                            result,
                            ctx,
                            mutctx,
                            expr_builder,
                        );
                    }
                    expr_builder.end();
                }
            }

            // emit the branch to the trap (if necessary)
            if !has_catch_all && trap_depth > 0 {
                expr_builder.br(trap_depth);
            }
        });
    }

    // net wasm stack: [] -> [stack-polymorphic]
    fn emit_thunk_impl_tail_call<H: HeapManager>(
        oe: &ir::OverloadEntry,
        closure: wasmgen::LocalIdx,
        localidx_params_begin: usize,
        params: &[ir::VarType],
        result: Option<ir::VarType>,
        ctx: EncodeContext<H>,
        mutctx: &mut MutContext,
        expr_builder: &mut wasmgen::ExprBuilder,
    ) {
        // put the closure (if desired)
        if oe.has_closure_param {
            expr_builder.local_get(closure);
        }
        // narrow and put the params
        for (i, param) in params.iter().copied().enumerate() {
            // narrow the param (unchecked)
            encode_load_local(
                mutctx.wasm_local_slice(localidx_params_begin + i),
                ir::VarType::Any,
                param,
                expr_builder,
            );
        }

        // make the direct function call
        if ctx.options.wasm_tail_call && result == Some(ir::VarType::Any) {
            // can do a tail call
            expr_builder.return_call(ctx.wasm_funcidxs[oe.funcidx]);
        } else {
            expr_builder.call(ctx.wasm_funcidxs[oe.funcidx]);
            if result == Some(ir::VarType::Any) {
                // we don't need to do conversion so just let it return back
                expr_builder.return_();
            } else {
                // need to do a type conversion
                if let Some(res) = result {
                    // net wasm stack: [return_calling_conv(vartype)] -> [vartype]
                    encode_post_appl_calling_conv(
                        result,
                        ctx.options.wasm_multi_value,
                        ctx.stackptr,
                        mutctx.scratch_mut(),
                        expr_builder,
                    );
                    // net wasm stack: [vartype] -> [return_callin_conv(Any)]
                    encode_return_calling_conv(
                        ir::VarType::Any,
                        res,
                        ctx.options.wasm_multi_value,
                        ctx.stackptr,
                        mutctx.scratch_mut(),
                        expr_builder,
                    );
                    // return it
                    expr_builder.return_();
                } else {
                    expr_builder.unreachable();
                }
            }
        }
    }

    // net wasm stack: [] -> [stack-polymorphic]
    fn raise_trap(
        error_func: wasmgen::FuncIdx,
        sourceloc_ref: wasmgen::LocalIdx,
        expr_builder: &mut wasmgen::ExprBuilder,
    ) {
        // we need to fetch the actual source location from the static memory to set as arguments of the error_func
        expr_builder.i32_const(ir::error::ERROR_CODE_FUNCTION_PARAM_TYPE as i32);
        expr_builder.i32_const(0);
        // the source location is 5 of u32s
        expr_builder.local_get(sourceloc_ref);
        expr_builder.i32_load(wasmgen::MemArg::new4(0));
        expr_builder.local_get(sourceloc_ref);
        expr_builder.i32_load(wasmgen::MemArg::new4(4));
        expr_builder.local_get(sourceloc_ref);
        expr_builder.i32_load(wasmgen::MemArg::new4(8));
        expr_builder.local_get(sourceloc_ref);
        expr_builder.i32_load(wasmgen::MemArg::new4(12));
        expr_builder.local_get(sourceloc_ref);
        expr_builder.i32_load(wasmgen::MemArg::new4(16));
        // call the error_func with 7 arguments
        expr_builder.call(error_func);
        // the error func is noreturn, so we want to tell wasm that it is indeed noreturn
        expr_builder.unreachable();
    }
}

// Widens the ir value on the stack into the value for returning.
// Then places the value for returning into the correct location as specified by the calling convention:
// If the wasm representation of this value only uses one wasm ValType, or multi-valued returns are enabled, then leave it on the stack
// Otherwise, write it into the unprotected stack
// net wasm stack: [source_type] -> [return_calling_conv(target_type)]
fn encode_return_calling_conv(
    target_type: ir::VarType,
    source_type: ir::VarType,
    use_wasm_multi_value_feature: bool,
    stackptr: wasmgen::GlobalIdx,
    scratch: &mut Scratch,
    expr_builder: &mut wasmgen::ExprBuilder,
) {
    if encode_vartype(target_type).len() <= 1 || use_wasm_multi_value_feature {
        // Should put on stack

        // net wasm stack: [source_type] -> [target_type]
        encode_widening_operation(target_type, source_type, scratch, expr_builder);
    } else {
        // Should put on unprotected stack
        // Recall that the unprotected stack grows downward
        // The convention is to put the data at [stackptr-size, stackptr) where `size` is the size of the target type

        // We need to retrieve the source value(s) into locals so that we can put the pointer under it on the stack.

        // temporary storage for source value
        let source_val: Box<[wasmgen::LocalIdx]> = encode_vartype(source_type)
            .iter()
            .copied()
            .map(|valtype| scratch.push(valtype))
            .collect();

        // pop the source value from the stack
        // net wasm stack: [source_type] -> []
        encode_store_local(&source_val, source_type, source_type, expr_builder);

        // return_ptr = stackptr - size
        // net wasm stack: [] -> [return_ptr]
        expr_builder.global_get(stackptr);
        expr_builder.i32_const(size_in_memory(target_type) as i32);
        expr_builder.i32_sub();

        // push the source value back onto the stack
        // net wasm stack: [] -> [source_type]
        encode_load_local(&source_val, source_type, source_type, expr_builder);

        // delete temporary storage for source value... (backwards because it is a stack)
        encode_vartype(source_type)
            .iter()
            .copied()
            .rev()
            .for_each(|valtype| scratch.pop(valtype));

        // write the source value to memory
        // net wasm stack: [return_ptr, source_type] -> []
        encode_store_memory(0, target_type, source_type, scratch, expr_builder);
    }
}

// Loads the return value from the previously-called function onto the stack.
// If the wasm representation of this value only uses one wasm ValType, or multi-valued returns are enabled, then this function does nothing.
// Otherwise, this function loads the return value from the unprotected stack.
// net wasm stack: [return_calling_conv(vartype)] -> [vartype]
fn encode_post_appl_calling_conv(
    opt_vartype: Option<ir::VarType>,
    use_wasm_multi_value_feature: bool,
    stackptr: wasmgen::GlobalIdx,
    scratch: &mut Scratch,
    expr_builder: &mut wasmgen::ExprBuilder,
) {
    if let Some(vartype) = opt_vartype {
        if encode_vartype(vartype).len() <= 1 || use_wasm_multi_value_feature {
            // Return value is on the stack already

            // Do nothing
        } else {
            // Return value is on the unprotected stack
            // Recall that the unprotected stack grows downward
            // The convention is to put the data at [stackptr-size, stackptr) where `size` is the size of the target type

            // return_ptr = stackptr - size
            // net wasm stack: [] -> [return_ptr]
            expr_builder.global_get(stackptr);
            expr_builder.i32_const(size_in_memory(vartype) as i32);
            expr_builder.i32_sub();

            // load the source value from memory
            // net wasm stack: [return_ptr] -> [source_type]
            encode_load_memory(0, vartype, vartype, scratch, expr_builder);
        }
    }
}
