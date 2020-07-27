use super::*;

use super::relabeller::Relabeller;

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
    optimize_expr(
        &mut func.expr,
        &mut Relabeller::new_with_identities(0..func.params.len()),
    )
}

fn write_new_expr(
    out: &mut Expr,
    test: Expr, // if create_narrow_local == true then test should be already adjusted by one local, since we need to insert it in a declaration
    test_vartype: VarType,
    num_decls_for_narrow_local: Option<usize>,
    then: Expr,
) {
    if let Some(num_decls) = num_decls_for_narrow_local {
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

fn relabel_target(target: &mut TargetExpr, local_map: &mut Relabeller) -> bool {
    if let TargetExpr::Local { localidx, next: _ } = target {
        let new_localidx: usize = local_map.map_old_to_new(*localidx).unwrap();
        if new_localidx != *localidx {
            *localidx = new_localidx;
            true
        } else {
            false
        }
    } else {
        false
    }
}

/**
 * Mapping from current idx to new idx.
 */
fn optimize_expr(expr: &mut Expr, local_map: &mut Relabeller) -> bool {
    match &mut expr.kind {
        ExprKind::PrimFunc {
            funcidxs: _,
            closure,
        } => optimize_expr(&mut **closure, local_map),
        ExprKind::TypeCast {
            test,
            expected,
            create_narrow_local,
            true_expr,
            false_expr,
        } => {
            assert!(*expected != VarType::Any); // expected should never be any, otherwise we shouldn't have emitted this cast
            let cnl = *create_narrow_local;
            if let Some(vartype) = test.vartype {
                if vartype == VarType::Any {
                    // we still need the typecast
                    let ret = optimize_expr(&mut **test, local_map);
                    if cnl {
                        local_map.with_entry(|local_map, _, _| {
                            ret | optimize_expr(&mut **true_expr, local_map)
                                | optimize_expr(&mut **false_expr, local_map)
                        })
                    } else {
                        ret | optimize_expr(&mut **true_expr, local_map)
                            | optimize_expr(&mut **false_expr, local_map)
                    }
                } else {
                    // todo! this transformation may introduce a pessimization because the backend will have to initialize the declaration to a default value.  Perhaps we should let an ir declaration to have an optional initializer expr?
                    // if cnl is enabled, we need to skip one local because write_new_expr() will place the test expr inside a declaration.
                    if cnl {
                        local_map
                            .with_skipped_new(|local_map| optimize_expr(&mut **test, local_map));
                    } else {
                        optimize_expr(&mut **test, local_map);
                    }
                    if vartype == *expected {
                        // we should always take true_expr
                        let opt_num_locals = if cnl {
                            local_map.with_entry(|local_map, _, new_local| {
                                optimize_expr(&mut **true_expr, local_map);
                                Some(new_local)
                            })
                        } else {
                            optimize_expr(&mut **true_expr, local_map);
                            None
                        };
                        // replace with a dummy expr
                        let test2: Expr = std::mem::replace(test, dummy_expr());
                        let true_expr2: Expr = std::mem::replace(&mut **true_expr, dummy_expr());
                        write_new_expr(expr, test2, vartype, opt_num_locals, true_expr2);
                        true // always return true because we already modified the program
                    } else {
                        // we should always take false_expr
                        let opt_num_locals = if cnl {
                            local_map.with_entry(|local_map, _, new_local| {
                                optimize_expr(&mut **false_expr, local_map);
                                Some(new_local)
                            })
                        } else {
                            optimize_expr(&mut **false_expr, local_map);
                            None
                        };
                        // replace with a dummy expr
                        let test2: Expr = std::mem::replace(test, dummy_expr());
                        let false_expr2: Expr = std::mem::replace(&mut **false_expr, dummy_expr());
                        write_new_expr(expr, test2, vartype, opt_num_locals, false_expr2);
                        true // always return true because we already modified the program
                    }
                }
            } else {
                panic!("Vartype cannot be none for TypeCast test expr");
            }
        }
        ExprKind::PrimAppl { prim_inst: _, args } => args
            .iter_mut()
            .fold(false, |prev, arg| prev | optimize_expr(arg, local_map)),
        ExprKind::Appl { func, args } => {
            optimize_expr(func, local_map)
                | args
                    .iter_mut()
                    .fold(false, |prev, arg| prev | optimize_expr(arg, local_map))
        }
        ExprKind::DirectAppl { funcidx: _, args } => args
            .iter_mut()
            .fold(false, |prev, arg| prev | optimize_expr(arg, local_map)),
        ExprKind::Conditional {
            cond,
            true_expr,
            false_expr,
        } => {
            optimize_expr(&mut **cond, local_map)
                | optimize_expr(&mut **true_expr, local_map)
                | optimize_expr(&mut **false_expr, local_map)
        }
        ExprKind::Declaration {
            local: _,
            expr: expr2,
        } => local_map.with_entry(|local_map, _, _| optimize_expr(&mut **expr2, local_map)),
        ExprKind::Assign { target, expr } => {
            relabel_target(target, local_map) | optimize_expr(&mut **expr, local_map)
        }
        ExprKind::Return { expr } => optimize_expr(&mut **expr, local_map),
        ExprKind::Sequence { content } => content
            .iter_mut()
            .fold(false, |prev, expr| prev | optimize_expr(expr, local_map)),
        ExprKind::VarName { source } => relabel_target(source, local_map),
        _ => false,
    }
}
