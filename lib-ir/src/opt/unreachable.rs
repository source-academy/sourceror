use super::*;

/**
 * Removes all unreachable statements from the program.
 * It only does so _within_ functions - it does not do cross-function optimizations.
 * It only cares about whether the expr.vartype is None, it does not look at struct_types or anything else.
 * It also propagates unreachability out of sequences etc.
 * The second return value is true if the program got changed, or false otherwise.
 */
pub fn optimize(mut program: Program, start_funcidx: usize) -> (Program, bool) {
    let mut changed = false;
    for func in program.funcs.iter_mut().skip(start_funcidx) {
        changed |= optimize_func(func);
    }
    (program, changed)
}

/**
 * Removes all unreachable statements from the function.
 * The return value is true if the function got changed, or false otherwise.
 */
fn optimize_func(func: &mut Func) -> bool {
    optimize_expr(&mut func.expr)
}

fn optimize_expr(expr: &mut Expr) -> bool {
    // Note: we explicitly list out all possibilities so we will get a compile error if a new exprkind is added.
    match &mut expr.kind {
        ExprKind::PrimUndefined
        | ExprKind::PrimNumber { val: _ }
        | ExprKind::PrimBoolean { val: _ }
        | ExprKind::PrimStructT { typeidx: _ }
        | ExprKind::PrimString { val: _ } => false,
        ExprKind::PrimFunc {
            funcidxs: _,
            closure,
        } => optimize_expr(&mut **closure),
        ExprKind::TypeCast {
            test,
            expected: _,
            create_narrow_local: _,
            true_expr,
            false_expr,
        } => {
            optimize_expr(&mut **test)
                | optimize_expr(&mut **true_expr)
                | optimize_expr(&mut **false_expr)
        }
        ExprKind::VarName { source: _ } => false,
        ExprKind::PrimAppl { prim_inst: _, args } => args
            .iter_mut()
            .fold(false, |prev, arg| prev | optimize_expr(arg)),
        ExprKind::Appl {
            func,
            args,
            location: _,
        } => {
            optimize_expr(func)
                | args
                    .iter_mut()
                    .fold(false, |prev, arg| prev | optimize_expr(arg))
        }
        ExprKind::DirectAppl { funcidx: _, args } => args
            .iter_mut()
            .fold(false, |prev, arg| prev | optimize_expr(arg)),
        ExprKind::Conditional {
            cond,
            true_expr,
            false_expr,
        } => {
            optimize_expr(&mut **cond)
                | optimize_expr(&mut **true_expr)
                | optimize_expr(&mut **false_expr)
        }
        ExprKind::Declaration {
            local: _,
            init,
            contained_expr,
        } => {
            let (init_res, init_is_none) = if let Some(init_expr) = init {
                let res = optimize_expr(&mut **init_expr);
                (res, init_expr.vartype.is_none())
            } else {
                (false, false)
            };
            if init_is_none {
                let expr_tmp = std::mem::replace(&mut **(init.as_mut().unwrap()), dummy_expr());
                *expr = expr_tmp;
                true
            } else {
                let real_res = init_res | optimize_expr(&mut **contained_expr);
                expr.vartype = contained_expr.vartype;
                real_res
            }
        }
        ExprKind::Assign {
            target: _,
            expr: expr2,
        } => {
            let ret = optimize_expr(&mut **expr2);
            // If the RHS of assignment is none, then the assignment can't actually happen,
            // so we are just executing the RHS for its side-effects.
            if expr2.vartype.is_none() {
                let expr_tmp = std::mem::replace(&mut **expr2, dummy_expr());
                *expr = expr_tmp;
                true
            } else {
                ret
            }
        }
        ExprKind::Return { expr: expr2 } => {
            let ret = optimize_expr(&mut **expr2);
            // If the arg is none, then we will never return
            if expr2.vartype.is_none() {
                let expr_tmp = std::mem::replace(&mut **expr2, dummy_expr());
                *expr = expr_tmp;
                true
            } else {
                ret
            }
        }
        ExprKind::Break {
            num_frames: _,
            expr: expr2,
        } => {
            let ret = optimize_expr(&mut **expr2);
            // If the arg is none, then we will never break
            if expr2.vartype.is_none() {
                let expr_tmp = std::mem::replace(&mut **expr2, dummy_expr());
                *expr = expr_tmp;
                true
            } else {
                ret
            }
        }
        ExprKind::Block { expr } => optimize_expr(&mut **expr),
        ExprKind::Sequence { content } => {
            let tmp_content = std::mem::take(content);
            let mut changed = false;
            let mut new_content = Vec::new();
            let tmp_content_len = tmp_content.len();
            for mut expr2 in tmp_content {
                changed |= optimize_expr(&mut expr2);
                let is_none = expr2.vartype.is_none();
                new_content.push(expr2);
                if is_none {
                    break;
                }
            }
            changed |= new_content.len() != tmp_content_len;
            expr.vartype = new_content
                .last()
                .map_or_else(|| Some(VarType::Undefined), |expr2| expr2.vartype);
            *content = new_content;
            changed
        }
        ExprKind::Trap {
            code: _,
            location: _,
        } => false,
    }
}

fn dummy_expr() -> Expr {
    Expr {
        vartype: Some(VarType::Undefined),
        kind: ExprKind::PrimUndefined,
    }
}
