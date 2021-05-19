use crate::estree::PreVar;
use crate::frontendvar::Append;
use crate::frontendvar::OverloadSet;
use crate::ParseState;
use std::collections::HashMap;

// Operators
const UNARY_MINUS: &str = "-u";
const NOT: &str = "!";
const AND: &str = "&&";
const OR: &str = "||";
const EQ: &str = "===";
const NE: &str = "!==";
const LT: &str = "<";
const LE: &str = "<=";
const GT: &str = ">";
const GE: &str = ">=";
const ADD: &str = "+";
const SUB: &str = "-";
const MUL: &str = "*";
const DIV: &str = "/";
const MOD: &str = "%";

pub fn resolve_unary_operator(es_op: &str) -> Option<&'static str> {
    match es_op {
        "-" => Some(UNARY_MINUS),
        "!" => Some(NOT),
        _ => None,
    }
}

pub fn resolve_binary_operator(es_op: &str) -> Option<&'static str> {
    match es_op {
        "===" => Some(EQ),
        "!==" => Some(NE),
        "<" => Some(LT),
        "<=" => Some(LE),
        ">" => Some(GT),
        ">=" => Some(GE),
        "+" => Some(ADD),
        "-" => Some(SUB),
        "*" => Some(MUL),
        "/" => Some(DIV),
        "%" => Some(MOD),
        _ => None,
    }
}

pub fn resolve_logical_operator(es_op: &str) -> Option<&'static str> {
    match es_op {
        "&&" => Some(AND),
        "||" => Some(OR),
        _ => None,
    }
}

// Skip rust fmt so we can have one operator per line
#[rustfmt::skip]
pub fn state_with_builtins(
    _start_idx: &mut usize, // we currently don't modify it because all builtins are direct
    ir_program: &mut ir::Program,
) -> (HashMap<String, PreVar>, ParseState) {
    let mut name_ctx: HashMap<String, PreVar> = HashMap::default();
    let mut parse_ctx: ParseState = ParseState::default();

    register_unary_op(UNARY_MINUS, ir::PrimInst::NumberNegate, ir::VarType::Number, &mut name_ctx, &mut parse_ctx, ir_program);
    register_unary_op(NOT, ir::PrimInst::BooleanNot, ir::VarType::Boolean, &mut name_ctx, &mut parse_ctx, ir_program);
    register_binary_op(AND, ir::PrimInst::BooleanAnd, ir::VarType::Boolean, &mut name_ctx, &mut parse_ctx, ir_program);
    register_binary_op(OR, ir::PrimInst::BooleanOr, ir::VarType::Boolean, &mut name_ctx, &mut parse_ctx, ir_program);
    register_binary_op(SUB, ir::PrimInst::NumberSub, ir::VarType::Number, &mut name_ctx, &mut parse_ctx, ir_program);
    register_binary_op(MUL, ir::PrimInst::NumberMul, ir::VarType::Number, &mut name_ctx, &mut parse_ctx, ir_program);
    register_binary_op(DIV, ir::PrimInst::NumberDiv, ir::VarType::Number, &mut name_ctx, &mut parse_ctx, ir_program);
    register_binary_op(MOD, ir::PrimInst::NumberRem, ir::VarType::Number, &mut name_ctx, &mut parse_ctx, ir_program);
    register_addition_op(ADD, ir::PrimInst::NumberAdd, ir::PrimInst::StringAdd, &mut name_ctx, &mut parse_ctx, ir_program);
    register_comparison_op(LT, ir::PrimInst::NumberLt, ir::PrimInst::StringLt, &mut name_ctx, &mut parse_ctx, ir_program);
    register_comparison_op(LE, ir::PrimInst::NumberLe, ir::PrimInst::StringLe, &mut name_ctx, &mut parse_ctx, ir_program);
    register_comparison_op(GT, ir::PrimInst::NumberGt, ir::PrimInst::StringGt, &mut name_ctx, &mut parse_ctx, ir_program);
    register_comparison_op(GE, ir::PrimInst::NumberGe, ir::PrimInst::StringGe, &mut name_ctx, &mut parse_ctx, ir_program);
    register_equality_op(EQ, true, ir::PrimInst::NumberEq, ir::PrimInst::BooleanEq, ir::PrimInst::StringEq, &mut name_ctx, &mut parse_ctx, ir_program);
    register_equality_op(NE, false, ir::PrimInst::NumberNeq, ir::PrimInst::BooleanNeq, ir::PrimInst::StringNeq, &mut name_ctx, &mut parse_ctx, ir_program);

    (name_ctx, parse_ctx)
}

fn register_unary_op(
    name: &str,
    ir_priminst: ir::PrimInst,
    ir_vartype: ir::VarType,
    name_ctx: &mut HashMap<String, PreVar>,
    parse_ctx: &mut ParseState,
    ir_program: &mut ir::Program,
) {
    // write the actual function (we hope it gets inlined by the ir optimizer later)
    let ir_expr = ir::Expr {
        vartype: Some(ir_vartype),
        kind: ir::ExprKind::PrimAppl {
            prim_inst: ir_priminst,
            args: Box::new([ir::Expr {
                vartype: Some(ir_vartype),
                kind: ir::ExprKind::VarName {
                    source: ir::TargetExpr::Local {
                        localidx: 0,
                        next: None,
                    },
                },
            }]),
        },
    };

    let funcidx = ir_program.add_func(ir::Func {
        params: Box::new([ir_vartype]),
        result: Some(ir_vartype),
        expr: ir_expr,
        signature_filter: Default::default(),
        is_tail_callable: false
    });

    // insert the necessary things into name_ctx and parse_ctx
    name_ctx.insert(name.to_owned(), PreVar::Direct);
    parse_ctx.add_direct(
        name.to_owned(),
        OverloadSet::from_single((Box::new([ir_vartype]), funcidx)),
    );
}

// write the actual function (we hope it gets inlined by the ir optimizer later)
fn make_binary_op_impl(
    ir_priminst: ir::PrimInst,
    ir_param_vartype: ir::VarType,
    ir_result_vartype: ir::VarType,
    ir_program: &mut ir::Program,
) -> ir::FuncIdx {
    let ir_expr = ir::Expr {
        vartype: Some(ir_result_vartype),
        kind: ir::ExprKind::PrimAppl {
            prim_inst: ir_priminst,
            args: Box::new([
                ir::Expr {
                    vartype: Some(ir_param_vartype),
                    kind: ir::ExprKind::VarName {
                        source: ir::TargetExpr::Local {
                            localidx: 0,
                            next: None,
                        },
                    },
                },
                ir::Expr {
                    vartype: Some(ir_param_vartype),
                    kind: ir::ExprKind::VarName {
                        source: ir::TargetExpr::Local {
                            localidx: 1,
                            next: None,
                        },
                    },
                },
            ]),
        },
    };

    let funcidx = ir_program.add_func(ir::Func {
        params: Box::new([ir_param_vartype, ir_param_vartype]),
        result: Some(ir_result_vartype),
        expr: ir_expr,
        signature_filter: Default::default(),
        is_tail_callable: false
    });

    funcidx
}

// write the actual function (we hope it gets inlined by the ir optimizer later)
fn make_trivial_func_undefined_impl(ret: bool, ir_program: &mut ir::Program) -> ir::FuncIdx {
    let ir_expr = ir::Expr {
        vartype: Some(ir::VarType::Boolean),
        kind: ir::ExprKind::PrimBoolean { val: ret },
    };

    let funcidx = ir_program.add_func(ir::Func {
        params: Box::new([ir::VarType::Undefined, ir::VarType::Undefined]),
        result: Some(ir::VarType::Boolean),
        expr: ir_expr,
        signature_filter: Default::default(),
        is_tail_callable: false
    });

    funcidx
}

fn register_binary_op(
    name: &str,
    ir_priminst: ir::PrimInst,
    ir_vartype: ir::VarType,
    name_ctx: &mut HashMap<String, PreVar>,
    parse_ctx: &mut ParseState,
    ir_program: &mut ir::Program,
) {
    let funcidx = make_binary_op_impl(ir_priminst, ir_vartype, ir_vartype, ir_program);

    // insert the necessary things into name_ctx and parse_ctx
    name_ctx.insert(name.to_owned(), PreVar::Direct);
    parse_ctx.add_direct(
        name.to_owned(),
        OverloadSet::from_single((Box::new([ir_vartype, ir_vartype]), funcidx)),
    );
}

// overloaded on number and string
fn register_addition_op(
    name: &str,
    ir_priminst_number: ir::PrimInst,
    ir_priminst_string: ir::PrimInst,
    name_ctx: &mut HashMap<String, PreVar>,
    parse_ctx: &mut ParseState,
    ir_program: &mut ir::Program,
) {
    let funcidx_number = make_binary_op_impl(
        ir_priminst_number,
        ir::VarType::Number,
        ir::VarType::Number,
        ir_program,
    );
    let funcidx_string = make_binary_op_impl(
        ir_priminst_string,
        ir::VarType::String,
        ir::VarType::String,
        ir_program,
    );

    // insert the necessary things into name_ctx and parse_ctx
    name_ctx.insert(name.to_owned(), PreVar::Direct);
    let mut overload_set = OverloadSet::new();
    overload_set.append((
        Box::new([ir::VarType::Number, ir::VarType::Number]) as Box<[ir::VarType]>,
        funcidx_number,
    ));
    overload_set.append((
        Box::new([ir::VarType::String, ir::VarType::String]) as Box<[ir::VarType]>,
        funcidx_string,
    ));
    parse_ctx.add_direct(name.to_owned(), overload_set);
}

// overloaded on number and string
fn register_comparison_op(
    name: &str,
    ir_priminst_number: ir::PrimInst,
    ir_priminst_string: ir::PrimInst,
    name_ctx: &mut HashMap<String, PreVar>,
    parse_ctx: &mut ParseState,
    ir_program: &mut ir::Program,
) {
    let funcidx_number = make_binary_op_impl(
        ir_priminst_number,
        ir::VarType::Number,
        ir::VarType::Boolean,
        ir_program,
    );
    let funcidx_string = make_binary_op_impl(
        ir_priminst_string,
        ir::VarType::String,
        ir::VarType::Boolean,
        ir_program,
    );

    // insert the necessary things into name_ctx and parse_ctx
    name_ctx.insert(name.to_owned(), PreVar::Direct);
    let mut overload_set = OverloadSet::new();
    overload_set.append((
        Box::new([ir::VarType::Number, ir::VarType::Number]) as Box<[ir::VarType]>,
        funcidx_number,
    ));
    overload_set.append((
        Box::new([ir::VarType::String, ir::VarType::String]) as Box<[ir::VarType]>,
        funcidx_string,
    ));
    parse_ctx.add_direct(name.to_owned(), overload_set);
}

// overloaded all primitive types
// struct type might need to be supported later
fn register_equality_op(
    name: &str,
    undefined_ret_val: bool,
    ir_priminst_number: ir::PrimInst,
    ir_priminst_boolean: ir::PrimInst,
    ir_priminst_string: ir::PrimInst,
    /*ir_priminst_func: ir::PrimInst,*/ // for now, ir has not implemented it yet
    name_ctx: &mut HashMap<String, PreVar>,
    parse_ctx: &mut ParseState,
    ir_program: &mut ir::Program,
) {
    let funcidx_undefined = make_trivial_func_undefined_impl(undefined_ret_val, ir_program);
    let funcidx_number = make_binary_op_impl(
        ir_priminst_number,
        ir::VarType::Number,
        ir::VarType::Boolean,
        ir_program,
    );
    let funcidx_boolean = make_binary_op_impl(
        ir_priminst_boolean,
        ir::VarType::Boolean,
        ir::VarType::Boolean,
        ir_program,
    );
    let funcidx_string = make_binary_op_impl(
        ir_priminst_string,
        ir::VarType::String,
        ir::VarType::Boolean,
        ir_program,
    );
    //let funcidx_func = make_binary_op_impl(ir_priminst_func, ir::VarType::Func, ir::VarType::Boolean, ir_program);

    // insert the necessary things into name_ctx and parse_ctx
    name_ctx.insert(name.to_owned(), PreVar::Direct);
    let mut overload_set = OverloadSet::new();
    overload_set.append((
        Box::new([ir::VarType::Undefined, ir::VarType::Undefined]) as Box<[ir::VarType]>,
        funcidx_undefined,
    ));
    overload_set.append((
        Box::new([ir::VarType::Number, ir::VarType::Number]) as Box<[ir::VarType]>,
        funcidx_number,
    ));
    overload_set.append((
        Box::new([ir::VarType::Boolean, ir::VarType::Boolean]) as Box<[ir::VarType]>,
        funcidx_boolean,
    ));
    overload_set.append((
        Box::new([ir::VarType::String, ir::VarType::String]) as Box<[ir::VarType]>,
        funcidx_string,
    ));
    //overload_set.append((Box::new([ir::VarType::Func, ir::VarType::Func]), funcidx_func));
    parse_ctx.add_direct(name.to_owned(), overload_set);
}
