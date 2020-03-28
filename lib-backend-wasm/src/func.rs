use super::gc::HeapManager;
/**
 * Contains stuff related to encoding of function bodies.
 */
use wasmgen::Scratch;

use crate::pre_traverse::ShiftedStringPool;
use crate::string_prim_inst;
use crate::Options;

use super::var_conv::*;

struct EncodeContext<'a, 'b, 'c, 'd, 'e, 'f, Heap: HeapManager> {
    // Local to this function
    return_type: Option<ir::VarType>,
    // Global for whole program
    struct_types: &'a [Box<[ir::VarType]>],
    struct_field_byte_offsets: &'b [Box<[u32]>], // has same sizes as `struct_types`, but instead stores the byte offset of each field from the beginning of the struct
    funcs: &'c [ir::Func], // ir functions, so that callers can check the param type of return type
    wasm_funcidxs: &'d [wasmgen::FuncIdx], // mapping from ir::FuncIdx to wasmgen::FuncIdx (used when we need to invoke a DirectAppl)
    stackptr: wasmgen::GlobalIdx,
    memidx: wasmgen::MemIdx,
    heap: &'e Heap,
    string_pool: &'f ShiftedStringPool,
    error_func: wasmgen::FuncIdx, // imported function to call to error out (e.g. runtime type errors)
    options: Options,             // Compilation options (it implements Copy)
}

// Have to implement Copy and Clone manually, because #[derive(Copy, Clone)] doesn't work for generic types like Heap
impl<'a, 'b, 'c, 'd, 'e, 'f, Heap: HeapManager> Copy
    for EncodeContext<'a, 'b, 'c, 'd, 'e, 'f, Heap>
{
}
impl<'a, 'b, 'c, 'd, 'e, 'f, Heap: HeapManager> Clone
    for EncodeContext<'a, 'b, 'c, 'd, 'e, 'f, Heap>
{
    fn clone(&self) -> Self {
        *self
    }
}

struct MutContext<'a, 'b> {
    // Local to this function
    scratch: Scratch<'a>,
    wasm_local_map: Vec<wasmgen::LocalIdx>,
    local_map: Vec<usize>, // map from ir param/local index to wasm_local_map index
    local_types: Vec<ir::VarType>, // map from ir param/local index to ir param type
    // Global for whole program
    module_wrapper: ModuleEncodeWrapper<'b>,
    // will also include function indices
}
impl<'a, 'b> MutContext<'a, 'b> {
    /**
     * Adds a local to the context, storing it in scratch, and updating wasm_local_map, local_map, local_types appropriately.
     */
    fn push_local(&mut self, ir_vartype: ir::VarType) {
        assert!(self.local_types.len() == self.local_map.len());
        self.local_types.push(ir_vartype);
        self.local_map.push(self.wasm_local_map.len());
        let wasm_valtypes = encode_vartype(ir_vartype);
        for wasm_valtype in wasm_valtypes {
            let localidx = self.scratch.push(*wasm_valtype);
            self.wasm_local_map.push(localidx);
        }
    }
    /**
     * Undoes the corresponding pop_local().  Locals are pushed and popped like a stack.
     */
    fn pop_local(&mut self, ir_vartype: ir::VarType) {
        assert!(self.local_types.len() == self.local_map.len());
        let wasm_valtypes = encode_vartype(ir_vartype);
        for wasm_valtype in wasm_valtypes {
            self.wasm_local_map.pop();
            self.scratch.pop(*wasm_valtype);
        }
        assert!(self.local_map.last().copied() == Some(self.wasm_local_map.len()));
        self.local_map.pop();
        assert!(self.local_types.last().copied() == Some(ir_vartype));
        self.local_types.pop();
    }

    fn wasm_local_slice(&self, ir_localidx: usize) -> &[wasmgen::LocalIdx] {
        &self.wasm_local_map[self.local_map[ir_localidx]
            ..(if ir_localidx + 1 < self.local_map.len() {
                self.local_map[ir_localidx + 1]
            } else {
                self.wasm_local_map.len()
            })]
    }
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
        use_wasm_multi_value_feature: bool,
    ) -> wasmgen::TypeIdx {
        let (wasm_params, _, _) = encode_param_list(ir_params);
        let wasm_functype = wasmgen::FuncType::new(
            wasm_params,
            encode_result(ir_result, use_wasm_multi_value_feature),
        );
        self.add_wasm_type(wasm_functype)
    }
}

pub fn encode_funcs<Heap: HeapManager>(
    ir_funcs: &[ir::Func],
    ir_struct_types: &[Box<[ir::VarType]>],
    ir_struct_field_byte_offsets: &[Box<[u32]>],
    imported_funcs: Box<[wasmgen::FuncIdx]>,
    ir_entry_point_funcidx: ir::FuncIdx,
    globalidx_stackptr: wasmgen::GlobalIdx,
    memidx: wasmgen::MemIdx,
    heap: &Heap,
    string_pool: &ShiftedStringPool,
    error_func: wasmgen::FuncIdx,
    options: Options,
    wasm_module: &mut wasmgen::WasmModule,
) {
    struct WasmRegistry {
        typeidx: wasmgen::TypeIdx,
        funcidx: wasmgen::FuncIdx,
        wasm_param_map: Box<[wasmgen::LocalIdx]>, // map converts wasm_param_map index to actual wasm param index
        param_map: Box<[usize]>, // map converts ir param index to wasm_param_map index
        param_types: Box<[ir::VarType]>, // map converts ir param index to ir param type
    }

    // register all the funcs first, in order of ir::funcidx
    // also encode the params and the locals and the return type
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
                let (wasm_typeidx, wasm_funcidx) = wasm_module.register_func(&wasm_functype);
                let code_builder = wasmgen::CodeBuilder::new(wasm_functype);
                (
                    WasmRegistry {
                        typeidx: wasm_typeidx,
                        funcidx: wasm_funcidx,
                        wasm_param_map: wasm_param_map,
                        param_map: param_map,
                        param_types: ir_func.params.clone(),
                    },
                    (code_builder),
                )
            })
            .unzip();

    let wasm_funcidxs: Box<[wasmgen::FuncIdx]> = imported_funcs
        .into_iter()
        .copied()
        .chain(registry_list.iter().map(|x| x.funcidx))
        .collect();

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
                    funcs: ir_funcs,
                    wasm_funcidxs: &wasm_funcidxs,
                    stackptr: globalidx_stackptr,
                    memidx: memidx,
                    heap: heap,
                    string_pool: string_pool,
                    error_func: error_func,
                    options: options,
                };
                let mut mutctx = MutContext {
                    scratch: scratch,
                    wasm_local_map: registry.wasm_param_map.to_vec(),
                    local_map: registry.param_map.to_vec(),
                    local_types: registry.param_types.to_vec(),
                    module_wrapper: ModuleEncodeWrapper { wasm_module },
                };
                encode_block(&ir_func.block, ctx, &mut mutctx, expr_builder);
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
        for wasm_valtype in encoded {
            wasm_param_map.push(wasmgen::LocalIdx {
                idx: wasm_param_valtypes.len() as u32,
            });
            wasm_param_valtypes.push(*wasm_valtype);
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
fn encode_block<H: HeapManager>(
    block: &ir::Block,
    ctx: EncodeContext<H>,
    mutctx: &mut MutContext,
    expr_builder: &mut wasmgen::ExprBuilder,
) {
    // push all the locals
    for ir_vartype in &block.locals {
        mutctx.push_local(*ir_vartype);
    }
    // encode the statements
    encode_statements(&block.statements, ctx, mutctx, expr_builder);
    // pop all the locals
    for ir_vartype in &block.locals {
        mutctx.pop_local(*ir_vartype);
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

                    // net wasm stack: [<expr.vartype>] -> [<return_calling_conv(ctx.return_type.unwrap())>]
                    encode_return_calling_conv(
                        ret,
                        expr.vartype,
                        ctx.options.wasm_multi_value,
                        ctx.stackptr,
                        &mut mutctx.scratch,
                        expr_builder,
                    );
                    // return the value on the stack (or in the unprotected stack) (which now has the correct type)
                    expr_builder.return_();
                }
            }
        }
        ir::Statement::If {
            cond,
            true_block,
            false_block,
        } => {
            // encode the condition
            // net wasm stack: [] -> [<cond.vartype>]
            encode_expr(cond, ctx, mutctx, expr_builder);
            // convert the condition
            // net wasm stack: [<cond.vartype>] -> [i32 (condition)]
            encode_narrowing_operation(
                ir::VarType::Boolean,
                cond.vartype,
                |expr_builder| {
                    expr_builder
                        .i32_const(ir::error::ERROR_CODE_IF_STATEMENT_CONDITION_TYPE as i32);
                    expr_builder.i32_const(0);
                    expr_builder.i32_const(0);
                    expr_builder.i32_const(0);
                    expr_builder.i32_const(0);
                    expr_builder.call(ctx.error_func);
                    expr_builder.unreachable();
                },
                &mut mutctx.scratch,
                expr_builder,
            );
            // emit the if statement
            expr_builder.if_(&[]);
            encode_block(true_block, ctx, mutctx, expr_builder);
            expr_builder.else_();
            encode_block(false_block, ctx, mutctx, expr_builder);
            expr_builder.end();
        }
        ir::Statement::Block { block } => {
            encode_block(block, ctx, mutctx, expr_builder);
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
                        mutctx.local_types[*localidx]
                            == ir::VarType::StructT {
                                typeidx: struct_field.typeidx
                            },
                        "ICE: IR->Wasm: Struct type incorrect or not proved at compilation time"
                    );
                    // net wasm stack: [] -> [struct_ptr]
                    expr_builder.local_get(mutctx.wasm_local_slice(*localidx)[0]);

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
                        mutctx.wasm_local_slice(*localidx),
                        mutctx.local_types[*localidx],
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
/*fn get_local_roots_for_allocation<H: HeapManager>(
    mutctx: &MutContext<H>,
) -> Box<[(ir::VarType, wasmgen::LocalIdx)]> {
    ctx.params_locals_types
        .iter()
        .cloned()
        .zip(ctx.var_map.iter().cloned())
        .collect()
}*/

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
            expr_builder.i32_const(ctx.string_pool.lookup(val) as i32);
        }
        ir::ExprKind::PrimStructT { typeidx } => {
            assert!(
                expr.vartype == ir::VarType::StructT { typeidx: *typeidx },
                "ICE: IR->Wasm: PrimStructT does not have correct type, or typeidx is incorrect"
            );
            // todo!(Only store locals that are in scope at this point, instead of all of them.)
            ctx.heap.encode_fixed_allocation(
                expr.vartype,
                &mutctx.local_types,
                &mutctx.local_map,
                &mutctx.wasm_local_map,
                &mut mutctx.scratch,
                expr_builder,
            );
        }
        ir::ExprKind::PrimFunc { funcidx, closure } => {
            assert!(
                expr.vartype == ir::VarType::Func,
                "ICE: IR->Wasm: PrimFunc does not have type func"
            );
            assert!(ctx.funcs[*funcidx].params.first().copied() == Some(closure.vartype)); // make sure the closure type given to us is indeed what the function will expect

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
        ir::ExprKind::Trap {
            code: _,
            location: _,
        } => {
            panic!("trap cannot be an expression");
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
        ir::ExprKind::Trap { code, location } => {
            // Calls the predefined imported function, which must never return.
            expr_builder.i32_const(*code as i32);
            expr_builder.i32_const(0);
            expr_builder.i32_const(location.file as i32);
            expr_builder.i32_const(location.begin as i32);
            expr_builder.i32_const(location.end as i32);
            expr_builder.call(ctx.error_func);
            expr_builder.unreachable();
            // in the future, PrimInst::Trap should take a error code parameter, and maybe source location
            // and call a noreturn function to the embedder (JavaScript).
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
                        mutctx.wasm_local_slice(*localidx),
                        mutctx.local_types[*localidx],
                        outgoing_vartype,
                        expr_builder,
                    );
                }
                Some(struct_field) => {
                    // For Source1, structs can only arise from closures and local variables, so their types must already be known at compilation time.
                    assert!(
                        mutctx.local_types[*localidx]
                            == ir::VarType::StructT {
                                typeidx: struct_field.typeidx
                            },
                        "ICE: IR->Wasm: Struct type incorrect or not proved at compilation time"
                    );
                    // net wasm stack: [] -> [struct_ptr]
                    expr_builder.local_get(mutctx.wasm_local_slice(*localidx)[0]);

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
                    // webassembly has no floating point remainder operation...
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
                        &mutctx.local_types,
                        &mutctx.local_map,
                        &mutctx.wasm_local_map,
                        &mut mutctx.scratch,
                        expr_builder,
                    );
                }
                ir::PrimInst::StringEq => {
                    string_prim_inst::encode_string_eq(&mut mutctx.scratch, expr_builder);
                }
                ir::PrimInst::StringNeq => {
                    string_prim_inst::encode_string_ne(&mut mutctx.scratch, expr_builder);
                }
                ir::PrimInst::StringGt => {
                    string_prim_inst::encode_string_gt(&mut mutctx.scratch, expr_builder);
                }
                ir::PrimInst::StringLt => {
                    string_prim_inst::encode_string_lt(&mut mutctx.scratch, expr_builder);
                }
                ir::PrimInst::StringGe => {
                    string_prim_inst::encode_string_ge(&mut mutctx.scratch, expr_builder);
                }
                ir::PrimInst::StringLe => {
                    string_prim_inst::encode_string_le(&mut mutctx.scratch, expr_builder);
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
                {
                    // not a function type... so we will raise a runtime error
                    expr_builder.i32_const(
                        ir::error::ERROR_CODE_FUNCTION_APPLICATION_NOT_CALLABLE_TYPE as i32,
                    );
                    expr_builder.i32_const(0);
                    expr_builder.i32_const(0); // todo!(add actual source location)
                    expr_builder.i32_const(0);
                    expr_builder.i32_const(0);
                    expr_builder.call(ctx.error_func);
                    expr_builder.unreachable();
                }
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
            &mutctx.local_types,
            &mutctx.local_map,
            &mutctx.wasm_local_map,
            &mut mutctx.scratch,
            expr_builder,
        );

        // call the function (indirectly)
        expr_builder.call_indirect(
            mutctx.module_wrapper.add_ir_type(
                &anys,
                Some(ir::VarType::Any),
                ctx.options.wasm_multi_value,
            ),
            wasmgen::TableIdx { idx: 0 },
        );

        // encode local roots prologue
        ctx.heap.encode_local_roots_epilogue(
            &mutctx.local_types,
            &mutctx.local_map,
            &mutctx.wasm_local_map,
            &mut mutctx.scratch,
            expr_builder,
        );
    } else {
        // This function is guaranteed not to allocate memory, so we don't need to put the locals on the gc_roots stack.

        // call the function (indirectly)
        expr_builder.call_indirect(
            mutctx.module_wrapper.add_ir_type(
                &anys,
                Some(ir::VarType::Any),
                ctx.options.wasm_multi_value,
            ),
            wasmgen::TableIdx { idx: 0 },
        );
    }

    // fetch return values from the location prescribed by the calling convention back to the stack
    encode_post_appl_calling_conv(
        ir::VarType::Any,
        ctx.options.wasm_multi_value,
        ctx.stackptr,
        &mut mutctx.scratch,
        expr_builder,
    );

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
            &mutctx.local_types,
            &mutctx.local_map,
            &mutctx.wasm_local_map,
            &mut mutctx.scratch,
            expr_builder,
        );

        // call the function
        expr_builder.call(ctx.wasm_funcidxs[funcidx]);

        // encode local roots prologue
        ctx.heap.encode_local_roots_epilogue(
            &mutctx.local_types,
            &mutctx.local_map,
            &mutctx.wasm_local_map,
            &mut mutctx.scratch,
            expr_builder,
        );
    } else {
        // This function is guaranteed not to allocate memory, so we don't need to put the locals on the gc_roots stack.

        // call the function
        expr_builder.call(ctx.wasm_funcidxs[funcidx]);
    }

    // fetch return values from the location prescribed by the calling convention back to the stack
    encode_post_appl_calling_conv(
        return_type,
        ctx.options.wasm_multi_value,
        ctx.stackptr,
        &mut mutctx.scratch,
        expr_builder,
    );
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
            &mutctx.local_types,
            &mutctx.local_map,
            &mutctx.wasm_local_map,
            &mut mutctx.scratch,
            expr_builder,
        );

        // call the function
        expr_builder.call(ctx.wasm_funcidxs[funcidx]);

        // encode local roots prologue
        ctx.heap.encode_local_roots_epilogue(
            &mutctx.local_types,
            &mutctx.local_map,
            &mutctx.wasm_local_map,
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
    vartype: ir::VarType,
    use_wasm_multi_value_feature: bool,
    stackptr: wasmgen::GlobalIdx,
    scratch: &mut Scratch,
    expr_builder: &mut wasmgen::ExprBuilder,
) {
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

        // write the source value to memory
        // net wasm stack: [return_ptr, source_type] -> []
        encode_store_memory(0, vartype, vartype, scratch, expr_builder);
    }
}
