use crate::mutcontext::MutContext;
use wasmgen::ExprBuilder;
use wasmgen::Scratch;
use wasmgen::ValType;

// Encodes an if-stmt (with or without 'else' part), abstracting over the issues relating to lack of multi-value support by spawning new locals if necessary
// net wasm stack [i32 cond] -> [valtypes...]
pub fn if_<
    F1: FnOnce(&mut MutContext, &mut ExprBuilder),
    F2: FnOnce(&mut MutContext, &mut ExprBuilder),
>(
    valtypes: &[ValType],
    use_multi_value: bool,
    mutctx: &mut MutContext,
    expr_builder: &mut ExprBuilder,
    true_encoder: F1,
    false_encoder: F2,
) {
    if use_multi_value || valtypes.len() <= 1 {
        // net wasm stack [i32 cond] -> [valtypes...]
        expr_builder.if_(valtypes);
        {
            // net wasm stack [] -> [valtypes...]
            true_encoder(mutctx, expr_builder);
        }
        expr_builder.else_();
        {
            // net wasm stack [] -> [valtypes...]
            false_encoder(mutctx, expr_builder);
        }
        expr_builder.end();
    } else {
        // we don't have multi-value enabled, but we have more than one value.  The last value (deepest in the stack) is left onto the stack, but everything else goes into locals.
        let (rest, last) = valtypes.split_at(valtypes.len() - 1);

        // make temporary variables for them
        mutctx.with_scratches(rest, |mutctx, tmp_locals| {
            expr_builder.if_(last);
            {
                // net wasm stack [] -> [valtypes...]
                true_encoder(mutctx, expr_builder);
                // net wasm stack [valtypes...] -> [last]
                tmp_locals.iter().copied().for_each(|localidx| {
                    expr_builder.local_set(localidx);
                });
            }
            expr_builder.else_();
            {
                // net wasm stack [] -> [valtypes...]
                false_encoder(mutctx, expr_builder);
                // net wasm stack [valtypes...] -> [last]
                tmp_locals.iter().copied().for_each(|localidx| {
                    expr_builder.local_set(localidx);
                });
            }
            expr_builder.end();
            // net wasm stack [last] -> [valtypes...]
            tmp_locals.iter().copied().rev().for_each(|localidx| {
                expr_builder.local_get(localidx);
            });
        });
    }
}

pub fn if_without_else<F: FnOnce(&mut MutContext, &mut ExprBuilder)>(
    valtypes: &[ValType],
    use_multi_value: bool,
    mutctx: &mut MutContext,
    expr_builder: &mut ExprBuilder,
    true_encoder: F,
) {
    if use_multi_value || valtypes.len() <= 1 {
        // net wasm stack [i32 cond] -> [valtypes...]
        expr_builder.if_(valtypes);
        {
            // net wasm stack [] -> [valtypes...]
            true_encoder(mutctx, expr_builder);
        }
        expr_builder.end();
    } else {
        // we don't have multi-value enabled, but we have more than one value.  The last value (deepest in the stack) is left onto the stack, but everything else goes into locals.
        let (rest, last) = valtypes.split_at(valtypes.len() - 1);

        // make temporary variables for them
        mutctx.with_scratches(rest, |mutctx, tmp_locals| {
            expr_builder.if_(last);
            {
                // net wasm stack [] -> [valtypes...]
                true_encoder(mutctx, expr_builder);
                // net wasm stack [valtypes...] -> [last]
                tmp_locals.iter().copied().for_each(|localidx| {
                    expr_builder.local_set(localidx);
                });
            }
            expr_builder.end();
            // net wasm stack [last] -> [valtypes...]
            tmp_locals.iter().copied().rev().for_each(|localidx| {
                expr_builder.local_get(localidx);
            });
        });
    }
}

pub fn if_with_opt_else<
    F1: FnOnce(&mut MutContext, &mut ExprBuilder),
    F2: for<'a, 'b> FnOnce(&'a mut MutContext, &'b mut ExprBuilder),
>(
    valtypes: &[ValType],
    use_multi_value: bool,
    mutctx: &mut MutContext,
    expr_builder: &mut ExprBuilder,
    true_encoder: F1,
    opt_false_encoder: Option<F2>,
) {
    match opt_false_encoder {
        None => if_without_else(
            valtypes,
            use_multi_value,
            mutctx,
            expr_builder,
            true_encoder,
        ),
        Some(false_encoder) => if_(
            valtypes,
            use_multi_value,
            mutctx,
            expr_builder,
            true_encoder,
            false_encoder,
        ),
    }
}
