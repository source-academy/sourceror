use super::gc::HeapManager;
/**
 * Contains stuff related to encoding of function bodies.
 */
use super::scratch::Scratch;

use super::var_conv::*;

struct EncodeContext<'a, 'b, 'c, 'd, 'e, 'f, 'g, Heap: HeapManager> {
    // Local to this function
    var_map: &'a [wasmgen::LocalIdx],
    params_locals_types: &'b [ir::VarType],
    return_type: Option<ir::VarType>,
    // Global for whole program
    struct_types: &'c [Box<[ir::VarType]>],
    struct_field_byte_offsets: &'d [Box<[u32]>], // has same sizes as `struct_types`, but instead stores the byte offset of each field from the beginning of the struct
    funcs: &'e [ir::Func], // ir functions, so that callers can check the param type of return type
    wasm_funcidxs: &'f [wasmgen::FuncIdx], // mapping from ir::FuncIdx to wasmgen::FuncIdx (used when we need to invoke a DirectAppl)
    heap: &'g Heap,
}

// Have to implement Copy and Clone manually, because #[derive(Copy, Clone)] doesn't work for generic types like Heap
impl<'a, 'b, 'c, 'd, 'e, 'f, 'g, Heap: HeapManager> Copy
    for EncodeContext<'a, 'b, 'c, 'd, 'e, 'f, 'g, Heap>
{
}
impl<'a, 'b, 'c, 'd, 'e, 'f, 'g, Heap: HeapManager> Clone
    for EncodeContext<'a, 'b, 'c, 'd, 'e, 'f, 'g, Heap>
{
    fn clone(&self) -> Self {
        *self
    }
}

struct MutContext<'a, 'b> {
    // Local to this function
    scratch: Scratch<'a>,
    // Global for whole program
    module_wrapper: ModuleEncodeWrapper<'b>,
    // will also include function indices
}

/**
 * Wraps the WasmModule, to only allow operations that are allows while encoding a function body.
 * (Because a raw WasmModule might allow a lot of other things which shouldn't be done midway encoding the function body.)
 */
struct ModuleEncodeWrapper<'a> {
    wasm_module: &'a mut wasmgen::WasmModule,
}
impl<'a> ModuleEncodeWrapper<'a> {
    fn add_wasm_type(&mut self, wasm_functype: wasmgen::FuncType) -> wasmgen::TypeIdx {
        self.wasm_module.insert_type_into(wasm_functype)
    }
    fn add_ir_type(
        &mut self,
        ir_params: &[ir::VarType],
        ir_result: Option<ir::VarType>,
    ) -> wasmgen::TypeIdx {
        let (wasm_params, _) = encode_param_list(ir_params);
        let wasm_functype = wasmgen::FuncType::new(wasm_params, encode_result(ir_result));
        self.add_wasm_type(wasm_functype)
    }
}

pub fn encode_funcs<Heap: HeapManager>(
    ir_funcs: &[ir::Func],
    ir_struct_types: &[Box<[ir::VarType]>],
    ir_struct_field_byte_offsets: &[Box<[u32]>],
    ir_entry_point_funcidx: ir::FuncIdx,
    heap: &Heap,
    wasm_module: &mut wasmgen::WasmModule,
) {
    struct WasmRegistry {
        typeidx: wasmgen::TypeIdx,
        funcidx: wasmgen::FuncIdx,
        var_map: Box<[wasmgen::LocalIdx]>, // varmap converts ir param/local index to wasm param/local index
        //code_builder: wasmgen::CodeBuilder,
        params_locals_types: Box<[ir::VarType]>,
    }

    // register all the funcs first, in order of ir::funcidx
    // also encode the params and the locals and the return type
    let (registry_list, code_builder_list): (Vec<WasmRegistry>, Vec<wasmgen::CodeBuilder>) =
        ir_funcs
            .iter()
            .map(|ir_func| {
                let (wasm_params, param_map) = encode_param_list(&ir_func.params);
                let num_wasm_params = wasm_params.len() as u32;
                let wasm_functype =
                    wasmgen::FuncType::new(wasm_params, encode_result(ir_func.result));
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
                (
                    WasmRegistry {
                        typeidx: wasm_typeidx,
                        funcidx: wasm_funcidx,
                        var_map: var_map,
                        //code_builder: code_builder,
                        params_locals_types: params_locals_types,
                    },
                    code_builder,
                )
            })
            .unzip();

    let wasm_funcidxs: Box<[wasmgen::FuncIdx]> = registry_list.iter().map(|x| x.funcidx).collect();

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
                    var_map: &registry.var_map,
                    params_locals_types: &registry.params_locals_types,
                    return_type: ir_func.result,
                    struct_types: ir_struct_types,
                    struct_field_byte_offsets: ir_struct_field_byte_offsets,
                    funcs: ir_funcs,
                    wasm_funcidxs: &wasm_funcidxs,
                    heap: heap,
                };
                let mut mutctx = MutContext {
                    scratch: scratch,
                    module_wrapper: ModuleEncodeWrapper { wasm_module },
                };
                encode_statements(
                    ir_func.statements.as_slice(),
                    ctx,
                    &mut mutctx,
                    expr_builder,
                );
                expr_builder.end(); // append the end instruction to end the function
            }
            // commit the function:
            wasm_module.commit_func(registry.funcidx, code_builder);
        });

    // encode the entry point
    // Note: this is not the wasm start function (the wasm start function is invoked immediately on instantiation, before exported functions are callable)
    // By our convention this function is exported as "main"
    wasm_module.export_func(
        registry_list[ir_entry_point_funcidx].funcidx,
        "main".to_string(),
    );
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

fn encode_result(ir_results: Option<ir::VarType>) -> Box<[wasmgen::ValType]> {
    ir_results
        .into_iter()
        .flat_map(|ir_result| encode_vartype(ir_result).iter())
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
        ir::VarType::StructT { typeidx: _ } => &[wasmgen::ValType::I32],
    }
}

// net wasm stack: [] -> []
fn encode_statements<H: HeapManager>(
    statements: &[ir::Statement],
    ctx: EncodeContext<H>,
    mutctx: &mut MutContext,
    expr_builder: &mut wasmgen::ExprBuilder,
) {
    for statement in statements {
        encode_statement(statement, ctx, mutctx, expr_builder);
    }
}

// net wasm stack: [] -> []
fn encode_statement<H: HeapManager>(
    statement: &ir::Statement,
    ctx: EncodeContext<H>,
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
        ir::Statement::Return { expr } => {
            match ctx.return_type {
                None => panic!("Cannot have return expression in a function that returns Void"),
                Some(ret) => {
                    // net wasm stack: [] -> [<expr.vartype>]
                    encode_expr(expr, ctx, mutctx, expr_builder);
                    // net wasm stack: [<expr.vartype>] -> [<ctx.return_type.unwrap()>]
                    encode_widening_operation(ret, expr.vartype, &mut mutctx.scratch, expr_builder);
                    // return the value on the stack (which now has the correct type)
                    expr_builder.return_();
                }
            }
        }
        ir::Statement::If {
            cond,
            true_stmts,
            false_stmts,
        } => {
            // encode the condition
            // net wasm stack: [] -> [<cond.vartype>]
            encode_expr(cond, ctx, mutctx, expr_builder);
            // convert the condition
            // net wasm stack: [<cond.vartype>] -> [i32 (condition)]
            encode_narrowing_operation(
                ir::VarType::Boolean,
                cond.vartype,
                &mut mutctx.scratch,
                expr_builder,
            );
            // emit the if statement
            expr_builder.if_(&[]);
            encode_statements(true_stmts, ctx, mutctx, expr_builder);
            expr_builder.else_();
            encode_statements(false_stmts, ctx, mutctx, expr_builder);
            expr_builder.end();
        }
        ir::Statement::Expr { expr } => {
            // encode the expression
            encode_expr(expr, ctx, mutctx, expr_builder);
            // remove the value from the stack
            encode_drop_value(expr.vartype, expr_builder);
        }
        ir::Statement::Void { expr_kind } => {
            encode_void_expr(expr_kind, ctx, mutctx, expr_builder);
        }
    }
}

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
    _mutctx: &mut MutContext,
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

                    fn follow_nested_struct<H: HeapManager>(
                        outer_struct_field: &ir::StructField,
                        ctx: EncodeContext<H>,
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
fn encode_target_addr_post<H: HeapManager>(
    target: &ir::TargetExpr,
    incoming_vartype: ir::VarType,
    ctx: EncodeContext<H>,
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

// temporary function until proper local scoping is available to the IR
fn get_local_roots_for_allocation<H: HeapManager>(
    ctx: EncodeContext<H>,
) -> Box<[(ir::VarType, wasmgen::LocalIdx)]> {
    ctx.params_locals_types
        .iter()
        .cloned()
        .zip(ctx.var_map.iter().cloned())
        .collect()
}

// net wasm stack: [] -> [<irvartype>] where `<irvartype>` is a valid encoding of an object with IR type expr.vartype
fn encode_expr<H: HeapManager>(
    expr: &ir::Expr,
    ctx: EncodeContext<H>,
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
            // todo!(Only store locals that are in scope at this point, instead of all of them.  This requires modifications ot the IR to get scoped locals.)
            ctx.heap.encode_fixed_allocation(
                expr.vartype,
                &get_local_roots_for_allocation(ctx),
                &mut mutctx.scratch,
                expr_builder,
            );
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
            expr_builder.i32_const((super::IR_FUNCIDX_TABLE_OFFSET + *funcidx as u32) as i32);
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
            encode_returnable_prim_inst(expr.vartype, *prim_inst, args, ctx, mutctx, expr_builder);
        }
        ir::ExprKind::Appl { func, args } => {
            encode_returnable_appl(expr.vartype, func, args, ctx, mutctx, expr_builder);
        }
        ir::ExprKind::DirectAppl { funcidx, args } => {
            encode_returnable_direct_appl(expr.vartype, *funcidx, args, ctx, mutctx, expr_builder);
        }
    }
}

// Encodes a function that never returns
// net wasm stack: [] -> []
// Note: for validation, we still need to ensure stack consistency.
fn encode_void_expr<H: HeapManager>(
    expr_kind: &ir::ExprKind,
    ctx: EncodeContext<H>,
    mutctx: &mut MutContext,
    expr_builder: &mut wasmgen::ExprBuilder,
) {
    match expr_kind {
        ir::ExprKind::PrimAppl {
            prim_inst: ir::PrimInst::Trap,
            args,
        } => {
            if !args.is_empty() {
                panic!("Trap should currently not take any arguments");
            } else {
                expr_builder.unreachable();
            }
        }
        ir::ExprKind::DirectAppl { funcidx, args } => {
            encode_void_direct_appl(*funcidx, args, ctx, mutctx, expr_builder);
        }
        _ => {
            panic!("This expr_kind is not allowed for void expr");
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
                    fn load_inner_local<H: HeapManager>(
                        outer_struct_field: &ir::StructField,
                        outgoing_vartype: ir::VarType,
                        ctx: EncodeContext<H>,
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

// Requires: the primitive instruction actually has the correct number of parameters,
// and the primitive instruction requires parameters with type equal to or wider than the types in `args`.
// and the return type is exactly the correct type of the primitive instruction.
// (so we allow widening for params, and it must be exact for returns)
// net wasm stack: [] -> [<return_type>]
fn encode_returnable_prim_inst<H: HeapManager>(
    return_type: ir::VarType,
    prim_inst: ir::PrimInst,
    args: &[ir::Expr],
    ctx: EncodeContext<H>,
    mutctx: &mut MutContext,
    expr_builder: &mut wasmgen::ExprBuilder,
) {
    match prim_inst.signature() {
        (prim_param_types, Some(prim_return_type)) => {
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
                    // webassembly has not floating point remainder operation...
                    // we have to manually do it
                    // https://stackoverflow.com/questions/26342823/implementation-of-fmod-function
                    // note: not very numerically rigourous, there might be some rounding errors
                    // algorithm used here for `a % b`:
                    // a - (trunc(a / b) * b)
                    let localidx_first: wasmgen::LocalIdx = mutctx.scratch.push_f64();
                    let localidx_second: wasmgen::LocalIdx = mutctx.scratch.push_f64();
                    expr_builder.local_set(localidx_second);
                    expr_builder.local_tee(localidx_first);
                    expr_builder.local_get(localidx_first);
                    expr_builder.local_get(localidx_second);
                    expr_builder.f64_div();
                    expr_builder.f64_trunc();
                    expr_builder.local_get(localidx_second);
                    expr_builder.f64_mul();
                    expr_builder.f64_sub();
                    mutctx.scratch.pop_f64();
                    mutctx.scratch.pop_f64();
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
                    // webassembly has not boolean negation operation...
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
                    // todo!(this function needs the dynamic allocation, so if it is in a separate function it will need to encode_local_roots_prologue/encode_local_roots_epilogue)
                    unimplemented!("String needs GC support, not implemented yet");
                }
                ir::PrimInst::Trap => {
                    // causes a trap (crashes the webassembly instance):
                    expr_builder.unreachable();
                    // in the future, PrimInst::Trap should take a error code parameter, and maybe source location
                    // and call a noreturn function to the embedder (JavaScript).
                }
            }
        }
        _ => {
            panic!("Cannot encode noreturn function in an expression");
        }
    }
}

// Requires: the callee actually has the correct number of parameters,
// and the func_expr has type VarType::Func or VarType::Any
// and the callee must have all params of type Any, and return type must also be Any.
// (to use more specific types, we must know the target function at compilation time, and hence use the DirectAppl)
// net wasm stack: [] -> [<return_type>]
fn encode_returnable_appl<H: HeapManager>(
    return_type: ir::VarType,
    func_expr: &ir::Expr,
    args: &[ir::Expr],
    ctx: EncodeContext<H>,
    mutctx: &mut MutContext,
    expr_builder: &mut wasmgen::ExprBuilder,
) {
    // todo! How to figure out at runtime if a function call has the correct number of arguments?
    // For now, we will just let wasm trap automatically for us if we call the function wrongly (but we might not get a proper error message)
    // Eventually we might want to check the signature manually.

    // Assert that the return type is Any (calling a function indirectly should return Any)
    assert!(return_type == ir::VarType::Any);

    // Evaluate the `func_expr`:
    // net wasm stack: [] -> [<func_expr.vartype>]
    encode_expr(func_expr, ctx, mutctx, expr_builder);

    // Emit the convertion to function if necessary, storing the tableidx in a i32 local variable
    // net wasm stack: [<func_expr.vartype>] -> [i32(closure ptr)]
    let localidx_tableidx: wasmgen::LocalIdx = mutctx.scratch.push_i32();
    {
        match func_expr.vartype {
            ir::VarType::Func => {
                // already a function, just pop out the tableidx from the stack
                expr_builder.local_set(localidx_tableidx);
            }
            ir::VarType::Any => {
                // emit the check if it is a function type
                expr_builder.i32_const(ir::VarType::Func.tag());
                expr_builder.i32_ne();
                expr_builder.if_(&[]);
                expr_builder.unreachable(); // todo! change to a call to javascript to specify the kind of trap
                expr_builder.end();
                // now it is a function type, we have to reinterpret the i64 as two i32s
                {
                    let localidx_data: wasmgen::LocalIdx = mutctx.scratch.push_i64();
                    expr_builder.local_tee(localidx_data);
                    expr_builder.local_get(localidx_data);
                    mutctx.scratch.pop_i64();
                    expr_builder.i32_wrap_i64();
                    expr_builder.local_set(localidx_tableidx);
                    expr_builder.i64_const(32);
                    expr_builder.i32_shr_u();
                    expr_builder.i32_wrap_i64();
                }
            }
            _ => {
                panic!("Cannot call function with static VarType that is not Func or Any");
            }
        }
    }

    // Encode all the arguments
    let anys: Box<[ir::VarType]> = args.iter().map(|_| ir::VarType::Any).collect();
    encode_args_to_call_function(&anys, args, ctx, mutctx, expr_builder);

    // push the tableidx onto the stack
    expr_builder.local_get(localidx_tableidx);

    if true {
        // This function might allocate memory, so we need to store the locals in the gc_roots stack first.

        // encode local roots prologue
        ctx.heap.encode_local_roots_prologue(
            &get_local_roots_for_allocation(ctx),
            &mut mutctx.scratch,
            expr_builder,
        );

        // call the function (indirectly)
        expr_builder.call_indirect(
            mutctx
                .module_wrapper
                .add_ir_type(&anys, Some(ir::VarType::Any)),
            wasmgen::TableIdx { idx: 0 },
        );

        // encode local roots prologue
        ctx.heap.encode_local_roots_epilogue(
            &get_local_roots_for_allocation(ctx),
            &mut mutctx.scratch,
            expr_builder,
        );
    } else {
        // This function is guaranteed not to allocate memory, so we don't need to put the locals on the gc_roots stack.

        // call the function (indirectly)
        expr_builder.call_indirect(
            mutctx
                .module_wrapper
                .add_ir_type(&anys, Some(ir::VarType::Any)),
            wasmgen::TableIdx { idx: 0 },
        );
    }

    mutctx.scratch.pop_i32();
}

// Requires: the callee actually has the correct number of parameters,
// and the each parameter of the callee must have a type at least as wide as (i.e. be a supertype of) the corresponding args[i].vartype,
// and the return type of the callee is exactly return_type.
// net wasm stack: [] -> [<return_type>]
fn encode_returnable_direct_appl<H: HeapManager>(
    return_type: ir::VarType,
    funcidx: ir::FuncIdx,
    args: &[ir::Expr],
    ctx: EncodeContext<H>,
    mutctx: &mut MutContext,
    expr_builder: &mut wasmgen::ExprBuilder,
) {
    // Assert that this funcidx exists
    assert!(funcidx < ctx.funcs.len(), "invalid funcidx");

    let func: &ir::Func = &ctx.funcs[funcidx];

    // Assert that the function has correct return type
    assert!(Some(return_type) == func.result);

    // Encode all the arguments
    encode_args_to_call_function(&func.params, args, ctx, mutctx, expr_builder);

    // todo!(For optimisation, encode_local_roots_prologue and encode_local_roots_epilogue should only be called if the callee might allocate)
    // Note: encode_args_to_call_function should be *before* encode_local_roots_prologue, since the args themselves might make function calls.
    if true {
        // This function might allocate memory, so we need to store the locals in the gc_roots stack first.

        // encode local roots prologue
        ctx.heap.encode_local_roots_prologue(
            &get_local_roots_for_allocation(ctx),
            &mut mutctx.scratch,
            expr_builder,
        );

        // call the function
        expr_builder.call(ctx.wasm_funcidxs[funcidx]);

        // encode local roots prologue
        ctx.heap.encode_local_roots_epilogue(
            &get_local_roots_for_allocation(ctx),
            &mut mutctx.scratch,
            expr_builder,
        );
    } else {
        // This function is guaranteed not to allocate memory, so we don't need to put the locals on the gc_roots stack.

        // call the function
        expr_builder.call(ctx.wasm_funcidxs[funcidx]);
    }
}

// Like `encode_returnable_direct_appl`, but the encoded function is guaranteed to never return.
// Requires: the callee actually has the correct number of parameters,
// and the each parameter of the callee must have a type at least as wide as (i.e. be a supertype of) the corresponding args[i].vartype,
// and the callee is specified to not return.
// net wasm stack: [] -> []
fn encode_void_direct_appl<H: HeapManager>(
    funcidx: ir::FuncIdx,
    args: &[ir::Expr],
    ctx: EncodeContext<H>,
    mutctx: &mut MutContext,
    expr_builder: &mut wasmgen::ExprBuilder,
) {
    // Assert that this funcidx exists
    assert!(funcidx < ctx.funcs.len(), "invalid funcidx");

    let func: &ir::Func = &ctx.funcs[funcidx];

    // Assert that the function does not return
    assert!(None == func.result);

    // Encode all the arguments
    encode_args_to_call_function(&func.params, args, ctx, mutctx, expr_builder);

    // todo!(For optimisation, encode_local_roots_prologue and encode_local_roots_epilogue should only be called if the callee might allocate)
    if true {
        // This function might allocate memory, so we need to store the locals in the gc_roots stack first.

        // encode local roots prologue
        ctx.heap.encode_local_roots_prologue(
            &get_local_roots_for_allocation(ctx),
            &mut mutctx.scratch,
            expr_builder,
        );

        // call the function
        expr_builder.call(ctx.wasm_funcidxs[funcidx]);

        // encode local roots prologue
        ctx.heap.encode_local_roots_epilogue(
            &get_local_roots_for_allocation(ctx),
            &mut mutctx.scratch,
            expr_builder,
        );
    } else {
        // This function is guaranteed not to allocate memory, so we don't need to put the locals on the gc_roots stack.

        // call the function
        expr_builder.call(ctx.wasm_funcidxs[funcidx]);
    }
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
    assert!(
        expected_param_types.len() == args.len(),
        "expected_param_types and args must be same length when encoding args to call function"
    );
    for (expected_type, arg) in expected_param_types.iter().zip(args.iter()) {
        // net wasm stack: [] -> [<arg.vartype>]
        encode_expr(arg, ctx, mutctx, expr_builder);
        // net wasm stack: [<arg.vartype>] -> [<expected_type>]
        encode_widening_operation(
            *expected_type,
            arg.vartype,
            &mut mutctx.scratch,
            expr_builder,
        );
    }
}

// net wasm stack: [<source_type>] -> [<target_type>]
fn encode_widening_operation(
    target_type: ir::VarType,
    source_type: ir::VarType,
    scratch: &mut Scratch,
    expr_builder: &mut wasmgen::ExprBuilder,
) {
    if target_type == source_type {
        // The widening operation is a no-op, because the source type is the same as the target type
    } else if target_type == ir::VarType::Any {
        // We are widening from a specific type to Any
        match source_type {
            ir::VarType::Any => {
                panic!("ICE");
            }
            ir::VarType::Undefined => {
                expr_builder.i64_const(0); // unused data
                expr_builder.i32_const(source_type.tag());
            }
            ir::VarType::Unassigned => {
                panic!("ICE: IR->Wasm: Cannot push to stack from unassigned value");
            }
            ir::VarType::Number => {
                expr_builder.i64_reinterpret_f64(); // convert f64 to i64
                expr_builder.i32_const(source_type.tag());
            }
            ir::VarType::Boolean | ir::VarType::String => {
                expr_builder.i64_extend_i32_u(); // convert i32 to i64
                expr_builder.i32_const(source_type.tag());
            }
            ir::VarType::StructT { typeidx: _ } => {
                expr_builder.i64_extend_i32_u(); // convert i32 to i64
                expr_builder.i32_const(source_type.tag());
            }
            ir::VarType::Func => {
                let localidx_tableidx: wasmgen::LocalIdx = scratch.push_i32();
                expr_builder.local_set(localidx_tableidx);
                expr_builder.i64_extend_i32_u(); // convert i32 to i64 (ptr to closure)
                expr_builder.i64_const(32);
                expr_builder.i64_shl();
                expr_builder.local_get(localidx_tableidx);
                expr_builder.i64_extend_i32_u(); // convert i32 to i64 (index in table)
                expr_builder.i64_or();
                expr_builder.i32_const(source_type.tag());
                scratch.pop_i32();
            }
        }
    } else {
        panic!("Widening operation target not a supertype of source");
    }
}

// net wasm stack: [<source_type>] -> [<target_type>]
fn encode_narrowing_operation(
    target_type: ir::VarType,
    source_type: ir::VarType,
    scratch: &mut Scratch,
    expr_builder: &mut wasmgen::ExprBuilder,
) {
    if target_type == source_type {
        // The narrowing operation is a no-op, because the source type is the same as the target type
    } else if source_type == ir::VarType::Any {
        // We are narrowing from Any to a specific type
        // emit type check (trap if not correct)

        // net wasm stack: [i64(data), i32(tag)] -> [i64(data)]
        expr_builder.i32_const(target_type.tag());
        expr_builder.i32_ne();
        expr_builder.if_(&[]);
        expr_builder.unreachable();
        expr_builder.end();

        // now the i64(data) is guaranteed to actually contain the target source_type
        // net wasm stack: [i64(data)] -> [<target_type>]
        match target_type {
            ir::VarType::Any => {
                panic!("ICE");
            }
            ir::VarType::Undefined => {
                expr_builder.drop(); // i64(data) unused
            }
            ir::VarType::Unassigned => {
                panic!("ICE: IR->Wasm: Cannot push unassigned value to stack");
            }
            ir::VarType::Number => {
                expr_builder.f64_reinterpret_i64(); // convert i64 to f64
            }
            ir::VarType::Boolean | ir::VarType::String | ir::VarType::StructT { typeidx: _ } => {
                expr_builder.i32_wrap_i64(); // convert i64 to i32
            }
            ir::VarType::Func => {
                let localidx_data: wasmgen::LocalIdx = scratch.push_i64();
                expr_builder.local_tee(localidx_data);
                expr_builder.i64_const(32);
                expr_builder.i64_shr_u();
                expr_builder.i32_wrap_i64();
                expr_builder.local_get(localidx_data);
                expr_builder.i32_wrap_i64();
                scratch.pop_i64();
            }
        }
    } else {
        panic!("Narrowing operation source not a supertype of target");
    }
}
