use super::*;
/**
 * This module contains code to generate the primitive functions (e.g. `+(string, string) -> string`) and pre-declared operators (e.g. `+(any, any) -> any`).
 * The pre-declared operators internally call the primitive functions
 */
use std::vec::Vec;

fn indexed_push<T>(v: &mut Vec<T>, value: T) -> usize {
    let ret = v.len();
    v.push(value);
    ret
}

/**
 * Generates the primitive function (e.g. `+(number, number) -> number`) with the given primitive instruction.
 */
fn generate_number_binary_operator_primitive(prim_inst: PrimInst) -> Func {
    // create new Func with params and result
    let mut func =
        Func::new_with_params_and_result(&[VarType::Number, VarType::Number], VarType::Number);

    // set the statements to just the PrimAppl with the given PrimInst
    // Equivalent code:
    // return local#0 + local#1;
    func.statements.push(Statement::Return {
        expr: Expr {
            vartype: VarType::Number,
            kind: ExprKind::PrimAppl {
                prim_inst: prim_inst,
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
    func.statements.push(Statement::If {
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
        true_stmts: vec![Statement::Return {
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
        }],
        false_stmts: vec![Statement::Void {
            expr_kind: ExprKind::PrimAppl {
                prim_inst: PrimInst::Trap,
                args: Box::new([]),
            },
        }],
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
 * Generates the primitive function (e.g. `+(number, number) -> number`) and the builtin (e.g. `+(any, any) -> any`).
 */
fn generate_number_binary_operator(
    funcs: &mut Vec<Func>,
    builtin_funcidxs: &mut [FuncIdx; NUM_BUILTINS as usize],
    prim_inst: PrimInst,
    builtin: Builtin,
) {
    let primitive_funcidx =
        indexed_push(funcs, generate_number_binary_operator_primitive(prim_inst));
    let builtin_funcidx = indexed_push(
        funcs,
        generate_number_binary_operator_wrapper(primitive_funcidx),
    );
    builtin_funcidxs[builtin as usize] = builtin_funcidx;
}

#[rustfmt::skip]
pub fn make_pregenerated_funcs() -> (Vec<Func>, [FuncIdx; NUM_BUILTINS as usize]) {
    let mut funcs = Vec::<Func>::new();
    let mut builtin_funcidxs: [FuncIdx; NUM_BUILTINS as usize] = Default::default();

    {
        generate_number_binary_operator(&mut funcs, &mut builtin_funcidxs, PrimInst::NumberAdd, Builtin::Plus); // TODO: we need to change this to allow StringAdd for Plus
        generate_number_binary_operator(&mut funcs, &mut builtin_funcidxs, PrimInst::NumberSub, Builtin::Minus);
        generate_number_binary_operator(&mut funcs, &mut builtin_funcidxs, PrimInst::NumberMul, Builtin::Times);
        generate_number_binary_operator(&mut funcs, &mut builtin_funcidxs, PrimInst::NumberDiv, Builtin::Divide);
        generate_number_binary_operator(&mut funcs, &mut builtin_funcidxs, PrimInst::NumberRem, Builtin::Modulo);
    }

    (funcs, builtin_funcidxs)
}
