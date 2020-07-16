use super::*;

/**
 * Removes unnecessary typecasts from the program.
 * It removes the typecast (and chooses one of the branches) if the test expr has static type that isn't Any.
 * The second return value is true if the program got changed, or false otherwise.
 */
pub fn optimize(mut program: Program) -> (Program, bool) {
    let mut changed = false;
    for func in &mut program.funcs {
        changed |= optimize_func(func);
    }
    (program, changed)
}

/**
 * Removes all unreachable statements from the function.
 * The return value is true if the function got changed, or false otherwise.
 */
fn optimize_func(func: &mut Func) -> bool {
    optimize_expr(&mut func.expr, func.params.len())
}

fn write_new_expr(
    out: &mut Expr,
    test: Expr,
    test_vartype: VarType,
    num_decls: usize,
    create_narrow_local: bool,
    then: Expr,
) {
    if create_narrow_local {
        *out = Expr {
            vartype: then.vartype,
            kind: ExprKind::Declaration {
                local: test_vartype,
                expr: Box::new(Expr {
                    vartype: then.vartype,
                    kind: ExprKind::Sequence {
                        content: vec![
                            Expr {
                                vartype: Some(VarType::Undefined),
                                kind: ExprKind::Assign {
                                    target: TargetExpr::Local {
                                        localidx: num_decls,
                                        next: None,
                                    },
                                    expr: Box::new(test),
                                },
                            },
                            then,
                        ],
                    },
                }),
            },
        }
    } else {
        *out = Expr {
            vartype: then.vartype,
            kind: ExprKind::Sequence {
                content: vec![test, then],
            },
        }
    }
}

fn dummy_expr() -> Expr {
    Expr {
        vartype: Some(VarType::Undefined),
        kind: ExprKind::PrimUndefined,
    }
}

fn optimize_expr(expr: &mut Expr, num_decls: usize) -> bool {
    match &mut expr.kind {
        ExprKind::PrimFunc {
            funcidxs: _,
            closure,
        } => optimize_expr(&mut **closure, num_decls),
        ExprKind::TypeCast {
            test,
            expected,
            create_narrow_local,
            true_expr,
            false_expr,
        } => {
            let cnl = *create_narrow_local;
            let res = optimize_expr(&mut **test, num_decls);
            if let Some(vartype) = test.vartype {
                if vartype == VarType::Any {
                    // we still need the typecast
                    res | optimize_expr(
                        &mut **true_expr,
                        if cnl { num_decls + 1 } else { num_decls },
                    ) | optimize_expr(
                        &mut **false_expr,
                        if cnl { num_decls + 1 } else { num_decls },
                    )
                } else {
                    // replace with a dummy expr
                    let test2: Expr = std::mem::replace(test, dummy_expr());
                    if vartype == *expected {
                        // we should always take true_expr
                        let ret = res
                            | optimize_expr(
                                &mut **true_expr,
                                if cnl { num_decls + 1 } else { num_decls },
                            );
                        let true_expr2: Expr = std::mem::replace(&mut **true_expr, dummy_expr());
                        write_new_expr(expr, test2, vartype, num_decls, cnl, true_expr2);
                        ret
                    } else {
                        // we should always take false_expr
                        let ret = res
                            | optimize_expr(
                                &mut **false_expr,
                                if cnl { num_decls + 1 } else { num_decls },
                            );
                        let false_expr2: Expr = std::mem::replace(&mut **false_expr, dummy_expr());
                        write_new_expr(expr, test2, vartype, num_decls, cnl, false_expr2);
                        ret
                    }
                }
            } else {
                panic!("Vartype cannot be none for TypeCast test expr");
            }
        }
        ExprKind::PrimAppl { prim_inst: _, args } => args
            .iter_mut()
            .fold(false, |prev, arg| prev | optimize_expr(arg, num_decls)),
        ExprKind::Appl { func, args } => {
            optimize_expr(func, num_decls)
                | args
                    .iter_mut()
                    .fold(false, |prev, arg| prev | optimize_expr(arg, num_decls))
        }
        ExprKind::DirectAppl { funcidx: _, args } => args
            .iter_mut()
            .fold(false, |prev, arg| prev | optimize_expr(arg, num_decls)),
        ExprKind::Conditional {
            cond,
            true_expr,
            false_expr,
        } => {
            optimize_expr(&mut **cond, num_decls)
                | optimize_expr(&mut **true_expr, num_decls)
                | optimize_expr(&mut **false_expr, num_decls)
        }
        ExprKind::Declaration {
            local: _,
            expr: expr2,
        } => optimize_expr(&mut **expr2, num_decls + 1),
        ExprKind::Assign { target: _, expr } => optimize_expr(&mut **expr, num_decls),
        ExprKind::Return { expr } => optimize_expr(&mut **expr, num_decls),
        ExprKind::Sequence { content } => content
            .iter_mut()
            .fold(false, |prev, expr| prev | optimize_expr(expr, num_decls)),
        _ => false,
    }
}
