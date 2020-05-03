use crate::var_conv::*;

pub fn encode_opt_vartype(ir_opt_vartype: Option<ir::VarType>) -> &'static [wasmgen::ValType] {
    match ir_opt_vartype {
        Some(ir_vartype) => encode_vartype(ir_vartype),
        None => &[],
    }
}

// Like encode_widening_operation(), but handles unreachable cases.
// Used when widening the result of an expression to fit the argument of the next one.
// `wasm_reachable`: whether WebAssembly thinks that the variable is reachable (this is not necessarily the same as whether the IR Expr has unreachable vartype).
// Note: If source_type is None (i.e. unreachable), then the resultant expression will be unreachable to WebAssembly.  No widening operation will be encoded (except perhaps a single `unreachable` instruction).
// After this function is run, WebAssembly's notion of unreachability will be equivalent to whether source_type is None.
pub fn encode_opt_result_widening_operation(
    target_type: Option<ir::VarType>,
    source_type: Option<ir::VarType>,
    wasm_reachable: bool,
    scratch: &mut wasmgen::Scratch,
    expr_builder: &mut wasmgen::ExprBuilder,
) {
    if let Some(actual_source_type) = source_type {
        // We are not unreachable... so we should encode the widening operation
        assert!(
            wasm_reachable,
            "WebAssembly must think that we are reachable if we are actaully reachable"
        );
        if let Some(actual_target_type) = target_type {
            encode_widening_operation(
                actual_target_type,
                actual_source_type,
                scratch,
                expr_builder,
            );
        } else {
            panic!("Cannot widen non-Void type to Void");
        }
    } else if wasm_reachable {
        // IR knows that it is unreachable... but WebAssembly doesn't.  So we have to tell WebAssembly about it.
        expr_builder.unreachable();
    }
}
