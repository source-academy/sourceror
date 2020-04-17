use ir::*;
use projstd::log::Logger;
use projstd::log::Severity;
use projstd::log::SourceLocation;
use std::boxed::Box;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::option::Option;

/**
 * Constructs a wrapper over the given ir::Expr that adds the correct ExprKind::TypeOf checks if necessary.
 * If the given ir::Expr has the correct type already, then this function immediately returns it.
 * Returns None if the given ir::Expr is not Any and also not the target type.
 */
pub fn add_import_typecheck(
    target_val_type: ImportValType,
    unchecked_ir_expr: Expr,
    next_localidx: usize,
) -> Option<Expr> {
    match unchecked_ir_expr.vartype {
        x if x == target_val_type.into() => Some(unchecked_ir_expr),
        VarType::Any => Some(ir::Expr {
            vartype: target_val_type.into(),
            kind: ExprKind::Sequence {
                local: VarType::Any,
                statements: vec![
                    Statement::Assign {
                        target: TargetExpr::Local {
                            localidx: next_localidx,
                            next: None,
                        },
                        expr: unchecked_ir_expr,
                    },
                    Statement::If {
                        cond: Expr {
                            vartype: VarType::Boolean,
                            kind: ExprKind::TypeOf {
                                expr: Box::new(Expr {
                                    vartype: VarType::Any,
                                    kind: ExprKind::VarName {
                                        source: TargetExpr::Local {
                                            localidx: next_localidx,
                                            next: None,
                                        },
                                    },
                                }),
                                expected: target_val_type.into(),
                            },
                        },
                        true_block: Block {
                            locals: Vec::new(),
                            statements: Vec::new(),
                        },
                        false_block: Block {
                            locals: Vec::new(),
                            statements: vec![Statement::Void {
                                expr_kind: ExprKind::Trap {
                                    code: 0x14,
                                    location: Default::default(),
                                },
                            }],
                        },
                    },
                ],
                last: Box::new(Expr {
                    vartype: target_val_type.into(),
                    kind: ExprKind::VarName {
                        source: TargetExpr::Local {
                            localidx: next_localidx,
                            next: None,
                        },
                    },
                }),
            },
        }),
        _ => None,
    }
}
