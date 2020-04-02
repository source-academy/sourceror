use super::*;
/**
 * This module contains code to generate the primitive functions (e.g. `+(string, string) -> string`) and pre-declared operators (e.g. `+(any, any) -> any`).
 * The pre-declared operators internally call the primitive functions
 */
use std::vec::Vec;

use crate::error::ERROR_CODE_BINARY_OPERATOR_PARAM_TYPE;
use crate::error::ERROR_CODE_UNARY_OPERATOR_PARAM_TYPE;

trait IndexedPushRef<T> {
    fn indexed_push(&mut self, value: T) -> usize;
}

impl<T> IndexedPushRef<T> for (Vec<T>, usize) {
    fn indexed_push(&mut self, value: T) -> usize {
        let ret = self.0.len() + self.1;
        self.0.push(value);
        ret
    }
}

/**
 * Generates the primitive function (e.g. `+(param1_type, param2_type) -> result_type`) with the given primitive instruction.
 */
fn generate_binary_operator_primitive(
    prim_inst: PrimInst,
    param1_type: VarType,
    param2_type: VarType,
    result_type: VarType,
) -> Func {
    // create new Func with params and result
    let mut func = Func::new_with_params_and_result(&[param1_type, param2_type], result_type);

    // set the statements to just the PrimAppl with the given PrimInst
    // Equivalent code:
    // return local#0 + local#1;
    func.block.statements.push(Statement::Return {
        expr: Expr {
            vartype: result_type,
            kind: ExprKind::PrimAppl {
                prim_inst: prim_inst,
                args: Box::new([
                    Expr {
                        vartype: param1_type,
                        kind: ExprKind::VarName {
                            source: TargetExpr::Local {
                                localidx: 0,
                                next: None,
                            },
                        },
                    },
                    Expr {
                        vartype: param2_type,
                        kind: ExprKind::VarName {
                            source: TargetExpr::Local {
                                localidx: 1,
                                next: None,
                            },
                        },
                    },
                ]),
            },
        },
    });

    // note: since this is a primitive, it has no locals and no signature_filter (because nothing is more specialized than this function)

    // return the function
    func
}

/**
 * Generates the primitive function (e.g. `+(param1_type, param2_type) -> result_type`) with the given primitive instruction.
 */
fn generate_unary_operator_primitive(
    prim_inst: PrimInst,
    param_type: VarType,
    result_type: VarType,
) -> Func {
    // create new Func with params and result
    let mut func = Func::new_with_params_and_result(&[param_type], result_type);

    // set the statements to just the PrimAppl with the given PrimInst
    // Equivalent code:
    // return !local#0;
    func.block.statements.push(Statement::Return {
        expr: Expr {
            vartype: result_type,
            kind: ExprKind::PrimAppl {
                prim_inst: prim_inst,
                args: Box::new([Expr {
                    vartype: param_type,
                    kind: ExprKind::VarName {
                        source: TargetExpr::Local {
                            localidx: 0,
                            next: None,
                        },
                    },
                }]),
            },
        },
    });

    // note: since this is a primitive, it has no locals and no signature_filter (because nothing is more specialized than this function)

    // return the function
    func
}

/**
 * Generates the builtin (e.g. `+(any, any) -> any`) that asserts the argument types then calls the given primitive function.
 * `prim_funcidx` should be called if the types are acceptable.
 * Otherwise, it will execute Builtin::Trap.
 */
fn generate_number_binary_operator_wrapper(prim_funcidx: FuncIdx) -> Func {
    // create new Func with params and result
    // Note: we set the 'number' return type (because if this function ever returns, it must return a number)
    let mut func = Func::new_with_params_and_result(&[VarType::Any, VarType::Any], VarType::Number);

    // set the statements to check the type
    // Equivalent code:
    // if (typeof(local#0, NUMBER) && typeof(local#1, NUMBER)) {
    //     return `prim_funcidx`(local#0, local#1);
    // } else {
    //      Trap
    // }
    func.block.statements.push(Statement::If {
        cond: Expr {
            vartype: VarType::Boolean,
            kind: ExprKind::PrimAppl {
                prim_inst: PrimInst::BooleanAnd,
                args: Box::new([
                    Expr {
                        vartype: VarType::Boolean,
                        kind: ExprKind::TypeOf {
                            expr: Box::new(Expr {
                                vartype: VarType::Any,
                                kind: ExprKind::VarName {
                                    source: TargetExpr::Local {
                                        localidx: 0,
                                        next: None,
                                    },
                                },
                            }),
                            expected: VarType::Number,
                        },
                    },
                    Expr {
                        vartype: VarType::Boolean,
                        kind: ExprKind::TypeOf {
                            expr: Box::new(Expr {
                                vartype: VarType::Any,
                                kind: ExprKind::VarName {
                                    source: TargetExpr::Local {
                                        localidx: 1,
                                        next: None,
                                    },
                                },
                            }),
                            expected: VarType::Number,
                        },
                    },
                ]),
            },
        },
        true_block: Block::from_statements(vec![Statement::Return {
            expr: Expr {
                vartype: VarType::Number,
                kind: ExprKind::DirectAppl {
                    funcidx: prim_funcidx,
                    args: Box::new([
                        Expr {
                            vartype: VarType::Number,
                            kind: ExprKind::VarName {
                                source: TargetExpr::Local {
                                    localidx: 0,
                                    next: None,
                                },
                            },
                        },
                        Expr {
                            vartype: VarType::Number,
                            kind: ExprKind::VarName {
                                source: TargetExpr::Local {
                                    localidx: 1,
                                    next: None,
                                },
                            },
                        },
                    ]),
                },
            },
        }]),
        false_block: Block::from_statements(vec![Statement::Void {
            expr_kind: ExprKind::Trap {
                code: ERROR_CODE_BINARY_OPERATOR_PARAM_TYPE,
                location: SourceLocation::default(),
            },
        }]),
    });

    // note: no locals

    // signature_filter for the primitive version:
    func.signature_filter.push((
        Box::new([VarType::Number, VarType::Number]),
        VarType::Number,
        prim_funcidx,
    ));

    // return the function
    func
}

/**
 * Generates the builtin (e.g. `+(any, any) -> any`) that asserts the argument types then calls the given primitive function.
 * `prim_funcidx` should be called if the types are acceptable.
 * Otherwise, it will execute Builtin::Trap.
 */
fn generate_boolean_binary_operator_wrapper(prim_funcidx: FuncIdx) -> Func {
    // create new Func with params and result
    // Note: we set the 'boolean' return type (because if this function ever returns, it must return a boolean)
    let mut func =
        Func::new_with_params_and_result(&[VarType::Any, VarType::Any], VarType::Boolean);

    // set the statements to check the type
    // Equivalent code:
    // if (typeof(local#0, BOOLEAN) && typeof(local#1, BOOLEAN)) {
    //     return `prim_funcidx`(local#0, local#1);
    // } else {
    //      Trap
    // }
    func.block.statements.push(Statement::If {
        cond: Expr {
            vartype: VarType::Boolean,
            kind: ExprKind::PrimAppl {
                prim_inst: PrimInst::BooleanAnd,
                args: Box::new([
                    Expr {
                        vartype: VarType::Boolean,
                        kind: ExprKind::TypeOf {
                            expr: Box::new(Expr {
                                vartype: VarType::Any,
                                kind: ExprKind::VarName {
                                    source: TargetExpr::Local {
                                        localidx: 0,
                                        next: None,
                                    },
                                },
                            }),
                            expected: VarType::Boolean,
                        },
                    },
                    Expr {
                        vartype: VarType::Boolean,
                        kind: ExprKind::TypeOf {
                            expr: Box::new(Expr {
                                vartype: VarType::Any,
                                kind: ExprKind::VarName {
                                    source: TargetExpr::Local {
                                        localidx: 1,
                                        next: None,
                                    },
                                },
                            }),
                            expected: VarType::Boolean,
                        },
                    },
                ]),
            },
        },
        true_block: Block::from_statements(vec![Statement::Return {
            expr: Expr {
                vartype: VarType::Boolean,
                kind: ExprKind::DirectAppl {
                    funcidx: prim_funcidx,
                    args: Box::new([
                        Expr {
                            vartype: VarType::Boolean,
                            kind: ExprKind::VarName {
                                source: TargetExpr::Local {
                                    localidx: 0,
                                    next: None,
                                },
                            },
                        },
                        Expr {
                            vartype: VarType::Boolean,
                            kind: ExprKind::VarName {
                                source: TargetExpr::Local {
                                    localidx: 1,
                                    next: None,
                                },
                            },
                        },
                    ]),
                },
            },
        }]),
        false_block: Block::from_statements(vec![Statement::Void {
            expr_kind: ExprKind::Trap {
                code: ERROR_CODE_UNARY_OPERATOR_PARAM_TYPE,
                location: SourceLocation::default(),
            },
        }]),
    });

    // note: no locals

    // signature_filter for the primitive version:
    func.signature_filter.push((
        Box::new([VarType::Boolean, VarType::Boolean]),
        VarType::Boolean,
        prim_funcidx,
    ));

    // return the function
    func
}

/**
 * Generates the builtin (e.g. `+(any, any) -> any`) that asserts the argument types then calls the given primitive function.
 * `prim_number_funcidx` should be called if the types are Number, `prim_string_funcidx` should be called if the types are String.
 * Otherwise, it will execute Builtin::Trap.
 */
fn generate_number_string_binary_operator_wrapper(
    prim_number_funcidx: FuncIdx,
    prim_string_funcidx: FuncIdx,
    return_type_number: VarType,
    return_type_string: VarType,
    result_type: VarType,
) -> Func {
    // create new Func with params and result
    let mut func = Func::new_with_params_and_result(&[VarType::Any, VarType::Any], result_type);

    // set the statements to check the type
    // Equivalent code:
    // if (typeof(local#0, NUMBER)) {
    //     if (typeof(local#1, NUMBER)) {
    //         return `prim_number_funcidx`(local#0, local#1);
    //     } else {
    //         Trap
    //     }
    // } else if (typeof(local#0, STRING)) {
    //     if (typeof(local#1, STRING)) {
    //         return `prim_string_funcidx`(local#0, local#1);
    //     } else {
    //         Trap
    //     }
    // } else {
    //     Trap
    // }
    func.block.statements.push(Statement::If {
        cond: Expr {
            vartype: VarType::Boolean,
            kind: ExprKind::TypeOf {
                expr: Box::new(Expr {
                    vartype: VarType::Any,
                    kind: ExprKind::VarName {
                        source: TargetExpr::Local {
                            localidx: 0,
                            next: None,
                        },
                    },
                }),
                expected: VarType::Number,
            },
        },
        true_block: Block::from_statements(vec![Statement::If {
            cond: Expr {
                vartype: VarType::Boolean,
                kind: ExprKind::TypeOf {
                    expr: Box::new(Expr {
                        vartype: VarType::Any,
                        kind: ExprKind::VarName {
                            source: TargetExpr::Local {
                                localidx: 1,
                                next: None,
                            },
                        },
                    }),
                    expected: VarType::Number,
                },
            },
            true_block: Block::from_statements(vec![Statement::Return {
                expr: Expr {
                    vartype: return_type_number,
                    kind: ExprKind::DirectAppl {
                        funcidx: prim_number_funcidx,
                        args: Box::new([
                            Expr {
                                vartype: VarType::Number,
                                kind: ExprKind::VarName {
                                    source: TargetExpr::Local {
                                        localidx: 0,
                                        next: None,
                                    },
                                },
                            },
                            Expr {
                                vartype: VarType::Number,
                                kind: ExprKind::VarName {
                                    source: TargetExpr::Local {
                                        localidx: 1,
                                        next: None,
                                    },
                                },
                            },
                        ]),
                    },
                },
            }]),
            false_block: Block::from_statements(vec![Statement::Void {
                expr_kind: ExprKind::Trap {
                    code: ERROR_CODE_BINARY_OPERATOR_PARAM_TYPE,
                    location: SourceLocation::default(),
                },
            }]),
        }]),
        false_block: Block::from_statements(vec![Statement::If {
            cond: Expr {
                vartype: VarType::Boolean,
                kind: ExprKind::TypeOf {
                    expr: Box::new(Expr {
                        vartype: VarType::Any,
                        kind: ExprKind::VarName {
                            source: TargetExpr::Local {
                                localidx: 0,
                                next: None,
                            },
                        },
                    }),
                    expected: VarType::String,
                },
            },
            true_block: Block::from_statements(vec![Statement::If {
                cond: Expr {
                    vartype: VarType::Boolean,
                    kind: ExprKind::TypeOf {
                        expr: Box::new(Expr {
                            vartype: VarType::Any,
                            kind: ExprKind::VarName {
                                source: TargetExpr::Local {
                                    localidx: 1,
                                    next: None,
                                },
                            },
                        }),
                        expected: VarType::String,
                    },
                },
                true_block: Block::from_statements(vec![Statement::Return {
                    expr: Expr {
                        vartype: return_type_string,
                        kind: ExprKind::DirectAppl {
                            funcidx: prim_string_funcidx,
                            args: Box::new([
                                Expr {
                                    vartype: VarType::String,
                                    kind: ExprKind::VarName {
                                        source: TargetExpr::Local {
                                            localidx: 0,
                                            next: None,
                                        },
                                    },
                                },
                                Expr {
                                    vartype: VarType::String,
                                    kind: ExprKind::VarName {
                                        source: TargetExpr::Local {
                                            localidx: 1,
                                            next: None,
                                        },
                                    },
                                },
                            ]),
                        },
                    },
                }]),
                false_block: Block::from_statements(vec![Statement::Void {
                    expr_kind: ExprKind::Trap {
                        code: ERROR_CODE_BINARY_OPERATOR_PARAM_TYPE,
                        location: SourceLocation::default(),
                    },
                }]),
            }]),
            false_block: Block::from_statements(vec![Statement::Void {
                expr_kind: ExprKind::Trap {
                    code: ERROR_CODE_BINARY_OPERATOR_PARAM_TYPE,
                    location: SourceLocation::default(),
                },
            }]),
        }]),
    });

    // note: no locals

    // signature_filter for the primitive version:
    func.signature_filter.push((
        Box::new([VarType::Number, VarType::Number]),
        return_type_number,
        prim_number_funcidx,
    ));
    func.signature_filter.push((
        Box::new([VarType::String, VarType::String]),
        return_type_string,
        prim_string_funcidx,
    ));

    // return the function
    func
}

/**
 * Generates the builtin (e.g. `+(any, any) -> any`) that asserts the argument types then calls the given primitive function.
 * `prim_boolean_funcidx` should be called if the types are Boolean, `prim_number_funcidx` should be called if the types are Number, `prim_string_funcidx` should be called if the types are String.
 * Otherwise, it will execute Builtin::Trap.
 */
fn generate_boolean_number_string_binary_operator_wrapper(
    prim_boolean_funcidx: FuncIdx,
    prim_number_funcidx: FuncIdx,
    prim_string_funcidx: FuncIdx,
    return_type_boolean: VarType,
    return_type_number: VarType,
    return_type_string: VarType,
    result_type: VarType,
) -> Func {
    // create new Func with params and result
    let mut func = Func::new_with_params_and_result(&[VarType::Any, VarType::Any], result_type);

    // set the statements to check the type
    // Equivalent code:
    // if (typeof(local#0, BOOLEAN)) {
    //     if (typeof(local#1, BOOLEAN)) {
    //         return `prim_boolean_funcidx`(local#0, local#1);
    //     } else {
    //         Trap
    //     }
    // } else if (typeof(local#0, NUMBER)) {
    //     if (typeof(local#1, NUMBER)) {
    //         return `prim_number_funcidx`(local#0, local#1);
    //     } else {
    //         Trap
    //     }
    // } else if (typeof(local#0, STRING)) {
    //     if (typeof(local#1, STRING)) {
    //         return `prim_string_funcidx`(local#0, local#1);
    //     } else {
    //         Trap
    //     }
    // } else {
    //     Trap
    // }
    func.block.statements.push(Statement::If {
        cond: Expr {
            vartype: VarType::Boolean,
            kind: ExprKind::TypeOf {
                expr: Box::new(Expr {
                    vartype: VarType::Any,
                    kind: ExprKind::VarName {
                        source: TargetExpr::Local {
                            localidx: 0,
                            next: None,
                        },
                    },
                }),
                expected: VarType::Boolean,
            },
        },
        true_block: Block::from_statements(vec![Statement::If {
            cond: Expr {
                vartype: VarType::Boolean,
                kind: ExprKind::TypeOf {
                    expr: Box::new(Expr {
                        vartype: VarType::Any,
                        kind: ExprKind::VarName {
                            source: TargetExpr::Local {
                                localidx: 1,
                                next: None,
                            },
                        },
                    }),
                    expected: VarType::Boolean,
                },
            },
            true_block: Block::from_statements(vec![Statement::Return {
                expr: Expr {
                    vartype: return_type_boolean,
                    kind: ExprKind::DirectAppl {
                        funcidx: prim_boolean_funcidx,
                        args: Box::new([
                            Expr {
                                vartype: VarType::Boolean,
                                kind: ExprKind::VarName {
                                    source: TargetExpr::Local {
                                        localidx: 0,
                                        next: None,
                                    },
                                },
                            },
                            Expr {
                                vartype: VarType::Boolean,
                                kind: ExprKind::VarName {
                                    source: TargetExpr::Local {
                                        localidx: 1,
                                        next: None,
                                    },
                                },
                            },
                        ]),
                    },
                },
            }]),
            false_block: Block::from_statements(vec![Statement::Void {
                expr_kind: ExprKind::Trap {
                    code: ERROR_CODE_BINARY_OPERATOR_PARAM_TYPE,
                    location: SourceLocation::default(),
                },
            }]),
        }]),
        false_block: Block::from_statements(vec![Statement::If {
            cond: Expr {
                vartype: VarType::Boolean,
                kind: ExprKind::TypeOf {
                    expr: Box::new(Expr {
                        vartype: VarType::Any,
                        kind: ExprKind::VarName {
                            source: TargetExpr::Local {
                                localidx: 0,
                                next: None,
                            },
                        },
                    }),
                    expected: VarType::Number,
                },
            },
            true_block: Block::from_statements(vec![Statement::If {
                cond: Expr {
                    vartype: VarType::Boolean,
                    kind: ExprKind::TypeOf {
                        expr: Box::new(Expr {
                            vartype: VarType::Any,
                            kind: ExprKind::VarName {
                                source: TargetExpr::Local {
                                    localidx: 1,
                                    next: None,
                                },
                            },
                        }),
                        expected: VarType::Number,
                    },
                },
                true_block: Block::from_statements(vec![Statement::Return {
                    expr: Expr {
                        vartype: return_type_number,
                        kind: ExprKind::DirectAppl {
                            funcidx: prim_number_funcidx,
                            args: Box::new([
                                Expr {
                                    vartype: VarType::Number,
                                    kind: ExprKind::VarName {
                                        source: TargetExpr::Local {
                                            localidx: 0,
                                            next: None,
                                        },
                                    },
                                },
                                Expr {
                                    vartype: VarType::Number,
                                    kind: ExprKind::VarName {
                                        source: TargetExpr::Local {
                                            localidx: 1,
                                            next: None,
                                        },
                                    },
                                },
                            ]),
                        },
                    },
                }]),
                false_block: Block::from_statements(vec![Statement::Void {
                    expr_kind: ExprKind::Trap {
                        code: ERROR_CODE_BINARY_OPERATOR_PARAM_TYPE,
                        location: SourceLocation::default(),
                    },
                }]),
            }]),
            false_block: Block::from_statements(vec![Statement::If {
                cond: Expr {
                    vartype: VarType::Boolean,
                    kind: ExprKind::TypeOf {
                        expr: Box::new(Expr {
                            vartype: VarType::Any,
                            kind: ExprKind::VarName {
                                source: TargetExpr::Local {
                                    localidx: 0,
                                    next: None,
                                },
                            },
                        }),
                        expected: VarType::String,
                    },
                },
                true_block: Block::from_statements(vec![Statement::If {
                    cond: Expr {
                        vartype: VarType::Boolean,
                        kind: ExprKind::TypeOf {
                            expr: Box::new(Expr {
                                vartype: VarType::Any,
                                kind: ExprKind::VarName {
                                    source: TargetExpr::Local {
                                        localidx: 1,
                                        next: None,
                                    },
                                },
                            }),
                            expected: VarType::String,
                        },
                    },
                    true_block: Block::from_statements(vec![Statement::Return {
                        expr: Expr {
                            vartype: return_type_string,
                            kind: ExprKind::DirectAppl {
                                funcidx: prim_string_funcidx,
                                args: Box::new([
                                    Expr {
                                        vartype: VarType::String,
                                        kind: ExprKind::VarName {
                                            source: TargetExpr::Local {
                                                localidx: 0,
                                                next: None,
                                            },
                                        },
                                    },
                                    Expr {
                                        vartype: VarType::String,
                                        kind: ExprKind::VarName {
                                            source: TargetExpr::Local {
                                                localidx: 1,
                                                next: None,
                                            },
                                        },
                                    },
                                ]),
                            },
                        },
                    }]),
                    false_block: Block::from_statements(vec![Statement::Void {
                        expr_kind: ExprKind::Trap {
                            code: ERROR_CODE_BINARY_OPERATOR_PARAM_TYPE,
                            location: SourceLocation::default(),
                        },
                    }]),
                }]),
                false_block: Block::from_statements(vec![Statement::Void {
                    expr_kind: ExprKind::Trap {
                        code: ERROR_CODE_BINARY_OPERATOR_PARAM_TYPE,
                        location: SourceLocation::default(),
                    },
                }]),
            }]),
        }]),
    });

    // note: no locals

    // signature_filter for the primitive version:
    func.signature_filter.push((
        Box::new([VarType::Boolean, VarType::Boolean]),
        return_type_boolean,
        prim_boolean_funcidx,
    ));
    func.signature_filter.push((
        Box::new([VarType::Number, VarType::Number]),
        return_type_number,
        prim_number_funcidx,
    ));
    func.signature_filter.push((
        Box::new([VarType::String, VarType::String]),
        return_type_string,
        prim_string_funcidx,
    ));

    // return the function
    func
}

fn generate_unary_operator_wrapper(prim_funcidx: FuncIdx, vartype: VarType) -> Func {
    // create new Func with params and result
    let mut func = Func::new_with_params_and_result(&[VarType::Any], vartype);

    // set the statements to check the type
    // Equivalent code:
    // if (typeof(local#0, vartype)) {
    //     return `prim_funcidx`(local#0);
    // } else {
    //      Trap
    // }
    func.block.statements.push(Statement::If {
        cond: Expr {
            vartype: VarType::Boolean,
            kind: ExprKind::TypeOf {
                expr: Box::new(Expr {
                    vartype: VarType::Any,
                    kind: ExprKind::VarName {
                        source: TargetExpr::Local {
                            localidx: 0,
                            next: None,
                        },
                    },
                }),
                expected: vartype,
            },
        },
        true_block: Block::from_statements(vec![Statement::Return {
            expr: Expr {
                vartype: vartype,
                kind: ExprKind::DirectAppl {
                    funcidx: prim_funcidx,
                    args: Box::new([Expr {
                        vartype: vartype,
                        kind: ExprKind::VarName {
                            source: TargetExpr::Local {
                                localidx: 0,
                                next: None,
                            },
                        },
                    }]),
                },
            },
        }]),
        false_block: Block::from_statements(vec![Statement::Void {
            expr_kind: ExprKind::Trap {
                code: ERROR_CODE_UNARY_OPERATOR_PARAM_TYPE,
                location: SourceLocation::default(),
            },
        }]),
    });

    // note: no locals

    // signature_filter for the primitive version:
    func.signature_filter
        .push((Box::new([vartype]), vartype, prim_funcidx));

    // return the function
    func
}

/**
 * Generates the primitive function (e.g. `+(number, number) -> number`) and the builtin (e.g. `+(any, any) -> any`).
 */
fn generate_number_binary_operator(
    funcs: &mut (Vec<Func>, usize),
    builtin_funcidxs: &mut [FuncIdx; NUM_BUILTINS as usize],
    prim_inst: PrimInst,
    builtin: Builtin,
) {
    let primitive_funcidx = funcs.indexed_push(generate_binary_operator_primitive(
        prim_inst,
        VarType::Number,
        VarType::Number,
        VarType::Number,
    ));
    let builtin_funcidx =
        funcs.indexed_push(generate_number_binary_operator_wrapper(primitive_funcidx));
    builtin_funcidxs[builtin as usize] = builtin_funcidx;
}

fn generate_boolean_binary_operator(
    funcs: &mut (Vec<Func>, usize),
    builtin_funcidxs: &mut [FuncIdx; NUM_BUILTINS as usize],
    prim_inst: PrimInst,
    builtin: Builtin,
) {
    let primitive_funcidx = funcs.indexed_push(generate_binary_operator_primitive(
        prim_inst,
        VarType::Boolean,
        VarType::Boolean,
        VarType::Boolean,
    ));
    let builtin_funcidx =
        funcs.indexed_push(generate_boolean_binary_operator_wrapper(primitive_funcidx));
    builtin_funcidxs[builtin as usize] = builtin_funcidx;
}

fn generate_number_string_binary_operator(
    funcs: &mut (Vec<Func>, usize),
    builtin_funcidxs: &mut [FuncIdx; NUM_BUILTINS as usize],
    prim_inst_number: PrimInst,
    prim_inst_string: PrimInst,
    builtin: Builtin,
    return_type_number: VarType,
    return_type_string: VarType,
    wrapper_return_type: VarType,
) {
    let primitive_number_funcidx = funcs.indexed_push(generate_binary_operator_primitive(
        prim_inst_number,
        VarType::Number,
        VarType::Number,
        return_type_number,
    ));
    let primitive_string_funcidx = funcs.indexed_push(generate_binary_operator_primitive(
        prim_inst_string,
        VarType::String,
        VarType::String,
        return_type_string,
    ));
    let builtin_funcidx = funcs.indexed_push(generate_number_string_binary_operator_wrapper(
        primitive_number_funcidx,
        primitive_string_funcidx,
        return_type_number,
        return_type_string,
        wrapper_return_type,
    ));
    builtin_funcidxs[builtin as usize] = builtin_funcidx;
}

fn generate_boolean_number_string_binary_operator(
    funcs: &mut (Vec<Func>, usize),
    builtin_funcidxs: &mut [FuncIdx; NUM_BUILTINS as usize],
    prim_inst_boolean: PrimInst,
    prim_inst_number: PrimInst,
    prim_inst_string: PrimInst,
    builtin: Builtin,
    return_type_boolean: VarType,
    return_type_number: VarType,
    return_type_string: VarType,
    wrapper_return_type: VarType,
) {
    let primitive_boolean_funcidx = funcs.indexed_push(generate_binary_operator_primitive(
        prim_inst_boolean,
        VarType::Boolean,
        VarType::Boolean,
        return_type_boolean,
    ));
    let primitive_number_funcidx = funcs.indexed_push(generate_binary_operator_primitive(
        prim_inst_number,
        VarType::Number,
        VarType::Number,
        return_type_number,
    ));
    let primitive_string_funcidx = funcs.indexed_push(generate_binary_operator_primitive(
        prim_inst_string,
        VarType::String,
        VarType::String,
        return_type_string,
    ));
    let builtin_funcidx =
        funcs.indexed_push(generate_boolean_number_string_binary_operator_wrapper(
            primitive_boolean_funcidx,
            primitive_number_funcidx,
            primitive_string_funcidx,
            return_type_boolean,
            return_type_number,
            return_type_string,
            wrapper_return_type,
        ));
    builtin_funcidxs[builtin as usize] = builtin_funcidx;
}

fn generate_unary_operator(
    funcs: &mut (Vec<Func>, usize),
    builtin_funcidxs: &mut [FuncIdx; NUM_BUILTINS as usize],
    vartype: VarType,
    prim_inst: PrimInst,
    builtin: Builtin,
) {
    let primitive_funcidx = funcs.indexed_push(generate_unary_operator_primitive(
        prim_inst, vartype, vartype,
    ));
    let builtin_funcidx =
        funcs.indexed_push(generate_unary_operator_wrapper(primitive_funcidx, vartype));
    builtin_funcidxs[builtin as usize] = builtin_funcidx;
}

#[rustfmt::skip]
pub fn make_pregenerated_funcs(funcidx_offset: usize) -> (Vec<Func>, [FuncIdx; NUM_BUILTINS as usize]) {
    let mut funcs = (Vec::<Func>::new(), funcidx_offset);
    let mut builtin_funcidxs: [FuncIdx; NUM_BUILTINS as usize] = Default::default();

    {
        generate_number_string_binary_operator(&mut funcs, &mut builtin_funcidxs, PrimInst::NumberAdd, PrimInst::StringAdd, Builtin::Plus, VarType::Number, VarType::String, VarType::Any);
        generate_number_binary_operator(&mut funcs, &mut builtin_funcidxs, PrimInst::NumberSub, Builtin::Minus);
        generate_number_binary_operator(&mut funcs, &mut builtin_funcidxs, PrimInst::NumberMul, Builtin::Times);
        generate_number_binary_operator(&mut funcs, &mut builtin_funcidxs, PrimInst::NumberDiv, Builtin::Divide);
        generate_number_binary_operator(&mut funcs, &mut builtin_funcidxs, PrimInst::NumberRem, Builtin::Modulo);
        generate_boolean_number_string_binary_operator(&mut funcs, &mut builtin_funcidxs, PrimInst::BooleanEq, PrimInst::NumberEq, PrimInst::StringEq, Builtin::Eq, VarType::Boolean, VarType::Boolean, VarType::Boolean, VarType::Boolean);
        generate_boolean_number_string_binary_operator(&mut funcs, &mut builtin_funcidxs, PrimInst::BooleanNeq, PrimInst::NumberNeq, PrimInst::StringNeq, Builtin::Neq, VarType::Boolean, VarType::Boolean, VarType::Boolean, VarType::Boolean);
        generate_number_string_binary_operator(&mut funcs, &mut builtin_funcidxs, PrimInst::NumberGt, PrimInst::StringGt, Builtin::Gt, VarType::Boolean, VarType::Boolean, VarType::Boolean);
        generate_number_string_binary_operator(&mut funcs, &mut builtin_funcidxs, PrimInst::NumberLt, PrimInst::StringLt, Builtin::Lt, VarType::Boolean, VarType::Boolean, VarType::Boolean);
        generate_number_string_binary_operator(&mut funcs, &mut builtin_funcidxs, PrimInst::NumberGe, PrimInst::StringGe, Builtin::Ge, VarType::Boolean, VarType::Boolean, VarType::Boolean);
        generate_number_string_binary_operator(&mut funcs, &mut builtin_funcidxs, PrimInst::NumberLe, PrimInst::StringLe, Builtin::Le, VarType::Boolean, VarType::Boolean, VarType::Boolean);
        generate_boolean_binary_operator(&mut funcs, &mut builtin_funcidxs, PrimInst::BooleanAnd, Builtin::And); // todo!: it currently doesn't support non-boolean second argument
        generate_boolean_binary_operator(&mut funcs, &mut builtin_funcidxs, PrimInst::BooleanOr, Builtin::Or); // todo!: it currently doesn't support non-boolean second argument
        generate_unary_operator(&mut funcs, &mut builtin_funcidxs, VarType::Boolean, PrimInst::BooleanNot, Builtin::Not);
        generate_unary_operator(&mut funcs, &mut builtin_funcidxs, VarType::Number, PrimInst::NumberNegate, Builtin::UnaryMinus);
    }

    (funcs.0, builtin_funcidxs)
}
