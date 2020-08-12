use super::relabeller::Relabeller;
use super::superset::*;
use super::*;
use itertools::Itertools;
use projstd::iter::*;

/**
 * Discretionary optimisation to propagate all types and values as much as possible.
 * This does a superset of unreachable.rs and typecast.rs, so you don't need to use those if you use this optimization.
 * The second return value is true if the program got changed, or false otherwise.
 */
pub fn optimize(mut program: Program) -> (Program, bool) {
    let mut changed = false;
    let param_types: Box<[Box<[VarType]>]> = program
        .imports
        .iter()
        .map(|import| import.params.iter().map(|ivt| (*ivt).into()).collect())
        .chain(program.funcs.iter().map(|func| func.params.clone()))
        .collect();
    let result_types: Box<[Option<VarType>]> = program
        .imports
        .iter()
        .map(|import| Some(import.result.into()))
        .chain(program.funcs.iter().map(|func| func.result))
        .collect();
    for func in &mut program.funcs {
        changed |= optimize_func(
            func,
            Context {
                param_types: &param_types,
                result_types: &result_types,
            },
        );
    }
    (program, changed)
}

#[derive(Copy, Clone)]
pub struct Context<'a, 'b> {
    param_types: &'a [Box<[VarType]>], // param type of each FuncIdx (including imports)
    result_types: &'b [Option<VarType>], // result type of each FuncIdx (including imports)
}

/**
 * Optimises the function the function.
 * The return value is true if the function got changed, or false otherwise.
 */
fn optimize_func(func: &mut Func, ctx: Context) -> bool {
    optimize_expr(
        &mut func.expr,
        &mut Relabeller::new_with_identities(0..func.params.len()),
        ctx,
    )
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

fn optimize_expr(expr: &mut Expr, local_map: &mut Relabeller, ctx: Context) -> bool {
    // Note: we explicitly list out all possibilities so we will get a compile error if a new exprkind is added.
    match &mut expr.kind {
        ExprKind::PrimUndefined => {
            assert!(expr.vartype == Some(VarType::Undefined));
            false
        }
        ExprKind::PrimNumber { val: _ } => {
            assert!(expr.vartype == Some(VarType::Number));
            false
        }
        ExprKind::PrimBoolean { val: _ } => {
            assert!(expr.vartype == Some(VarType::Boolean));
            false
        }
        ExprKind::PrimStructT { typeidx } => {
            assert!(expr.vartype == Some(VarType::StructT { typeidx: *typeidx }));
            false
        }
        ExprKind::PrimString { val: _ } => {
            assert!(expr.vartype == Some(VarType::String));
            false
        }
        ExprKind::PrimFunc {
            funcidxs: _,
            closure,
        } => {
            let ret = optimize_expr(&mut **closure, local_map, ctx);
            if closure.vartype.is_none() {
                let expr_tmp = std::mem::replace(&mut **closure, dummy_expr());
                *expr = expr_tmp;
                true
            } else {
                ret
            }
        }
        ExprKind::TypeCast {
            test,
            expected,
            create_narrow_local,
            true_expr,
            false_expr,
        } => {
            assert!(*expected != VarType::Any); // expected should never be any, otherwise we shouldn't have emitted this cast
            let cnl = *create_narrow_local;
            let test_res = optimize_expr(&mut **test, local_map, ctx);
            match test.vartype {
                None => {
                    // test expr is noreturn
                    let expr_tmp = std::mem::replace(&mut **test, dummy_expr());
                    *expr = expr_tmp;
                    true
                }
                Some(vartype) => {
                    if vartype == VarType::Any {
                        // we still need the typecast
                        let ret = test_res
                            | if cnl {
                                local_map.with_entry(|local_map, _, _| {
                                    optimize_expr(&mut **true_expr, local_map, ctx)
                                })
                            } else {
                                optimize_expr(&mut **true_expr, local_map, ctx)
                            }
                            | optimize_expr(&mut **false_expr, local_map, ctx);
                        expr.vartype = union_type(true_expr.vartype, false_expr.vartype);
                        ret
                    } else {
                        fn write_expr(
                            out: &mut Expr,
                            test_vartype: VarType,
                            test: Expr,
                            then: Expr,
                            cnl: bool,
                        ) {
                            if cnl {
                                *out = Expr {
                                    vartype: then.vartype,
                                    kind: ExprKind::Declaration {
                                        local: test_vartype,
                                        init: Some(Box::new(test)),
                                        contained_expr: Box::new(then),
                                    },
                                };
                            } else {
                                *out = Expr {
                                    vartype: then.vartype,
                                    kind: ExprKind::Sequence {
                                        content: vec![test, then],
                                    },
                                };
                            }
                        }
                        // we don't need the typecast
                        let test_tmp = std::mem::replace(&mut **test, dummy_expr());
                        if vartype == *expected {
                            // only need the true branch
                            if cnl {
                                local_map.with_entry(|local_map, _, _| {
                                    optimize_expr(&mut **true_expr, local_map, ctx);
                                })
                            } else {
                                optimize_expr(&mut **true_expr, local_map, ctx);
                            }
                            let true_tmp = std::mem::replace(&mut **true_expr, dummy_expr());
                            write_expr(expr, vartype, test_tmp, true_tmp, cnl); // also sets expr.vartype appropriately
                        } else {
                            // only need the false branch
                            optimize_expr(&mut **false_expr, local_map, ctx);
                            let false_tmp = std::mem::replace(&mut **false_expr, dummy_expr());
                            write_expr(expr, vartype, test_tmp, false_tmp, false);
                            // also sets expr.vartype appropriately
                        }
                        // we modified the expr, so definitely return true
                        true
                    }
                }
            }
        }
        ExprKind::VarName { source } => relabel_target(source, local_map),
        ExprKind::PrimAppl { prim_inst: _, args } => {
            let mut ret = false;
            for (i, arg) in args.iter_mut().enumerate() {
                ret |= optimize_expr(arg, local_map, ctx);
                if arg.vartype.is_none() {
                    let mut tmp_args = std::mem::take(args).into_vec();
                    tmp_args.truncate(i + 1);
                    *expr = make_sequence_from_exprs(tmp_args);
                    return true;
                }
            }
            ret | try_const_eval(expr)
        }
        ExprKind::Appl { func, args } => {
            let mut ret = optimize_expr(func, local_map, ctx);
            if func.vartype.is_none() {
                let tmp_func = std::mem::replace(&mut **func, dummy_expr());
                *expr = tmp_func;
                true
            } else {
                for (i, arg) in args.iter_mut().enumerate() {
                    ret |= optimize_expr(arg, local_map, ctx);
                    if arg.vartype.is_none() {
                        let tmp_func = std::mem::replace(&mut **func, dummy_expr());
                        let mut tmp_args = std::mem::take(args).into_vec();
                        tmp_args.truncate(i + 1);
                        tmp_args.insert(0, tmp_func); // preprend the func expr to the args
                        *expr = make_sequence_from_exprs(tmp_args);
                        return true;
                    }
                }
                ret | try_devirtualize_appl(expr, local_map, ctx) // note: inlining is not done in this optimization, because those heuristics are complicated
            }
        }
        ExprKind::DirectAppl { funcidx, args } => {
            let mut ret = false;
            for (i, arg) in args.iter_mut().enumerate() {
                ret |= optimize_expr(arg, local_map, ctx);
                if arg.vartype.is_none() {
                    let mut tmp_args = std::mem::take(args).into_vec();
                    tmp_args.truncate(i + 1);
                    *expr = make_sequence_from_exprs(tmp_args);
                    return true;
                }
            }
            expr.vartype = ctx.result_types[*funcidx];
            ret
        }
        ExprKind::Conditional {
            cond,
            true_expr,
            false_expr,
        } => {
            let cond_res = optimize_expr(&mut **cond, local_map, ctx);
            if cond.vartype.is_none() {
                let expr_tmp = std::mem::replace(&mut **cond, dummy_expr());
                *expr = expr_tmp;
                true
            } else {
                if let ExprKind::PrimBoolean { val: cond_val } = cond.kind {
                    // it is a constant expr
                    // just keep and optimize the reachable branch
                    if cond_val {
                        // true_expr is reachable
                        optimize_expr(&mut **true_expr, local_map, ctx);
                        // just keep the true_expr (since the cond has no side-effects)
                        let expr_tmp = std::mem::replace(&mut **true_expr, dummy_expr());
                        *expr = expr_tmp;
                    } else {
                        // false_expr is reachable
                        optimize_expr(&mut **false_expr, local_map, ctx);
                        // just keep the false_expr (since the cond has no side-effects)
                        let expr_tmp = std::mem::replace(&mut **false_expr, dummy_expr());
                        *expr = expr_tmp;
                    }
                    true // we've definitely modified something
                } else {
                    // not a constant expr
                    // so we have to optimize both branches
                    let ret = cond_res
                        | optimize_expr(&mut **true_expr, local_map, ctx)
                        | optimize_expr(&mut **false_expr, local_map, ctx);
                    expr.vartype = union_type(true_expr.vartype, false_expr.vartype);
                    ret
                }
            }
        }
        ExprKind::Declaration {
            local: _,
            init,
            contained_expr,
        } => {
            let (init_res, init_is_none) = if let Some(init_expr) = init {
                let res = optimize_expr(&mut **init_expr, local_map, ctx);
                (res, init_expr.vartype.is_none())
            } else {
                (false, false)
            };
            if init_is_none {
                let expr_tmp = std::mem::replace(&mut **(init.as_mut().unwrap()), dummy_expr());
                *expr = expr_tmp;
                true
            } else {
                let real_res = init_res
                    | local_map.with_entry(|local_map, _, _| {
                        optimize_expr(&mut **contained_expr, local_map, ctx)
                    });
                expr.vartype = contained_expr.vartype;
                real_res
            }
        }
        ExprKind::Assign {
            target,
            expr: expr2,
        } => {
            assert!(expr.vartype == Some(VarType::Undefined));
            let ret =
                relabel_target(target, local_map) | optimize_expr(&mut **expr2, local_map, ctx);
            // If the RHS of assignment is none, then the assignment can't actually happen,
            // so we are just executing the RHS for its side-effects.
            if expr2.vartype.is_none() {
                let expr_tmp = std::mem::replace(&mut **expr2, dummy_expr());
                *expr = expr_tmp;
                true
            } else {
                // the return type of Assign is Undefined, so we don't need to change it
                ret
            }
        }
        ExprKind::Return { expr: expr2 } => {
            assert!(expr.vartype == None);
            let ret = optimize_expr(&mut **expr2, local_map, ctx);
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
            let ret = optimize_expr(&mut **expr2, local_map, ctx);
            if expr2.vartype.is_none() {
                let expr_tmp = std::mem::replace(&mut **expr2, dummy_expr());
                *expr = expr_tmp;
                true
            } else {
                ret
            }
        }
        ExprKind::Block { expr: expr2 } => {
            let ret = optimize_expr(&mut **expr2, local_map, ctx);
            // todo! actually figure out the union of all the types that can jump to the end of this block
            // for now we don't optimize the Block
            ret
        }
        ExprKind::Sequence { content } => {
            let tmp_content = std::mem::take(content);
            let mut changed = false;
            let mut new_content = Vec::new();
            let tmp_content_len = tmp_content.len();
            for mut expr2 in tmp_content {
                changed |= optimize_expr(&mut expr2, local_map, ctx);
                let is_none = expr2.vartype.is_none();
                new_content.push(expr2);
                if is_none {
                    break;
                }
            }
            changed |= new_content.len() != tmp_content_len;
            match new_content.len() {
                0 => {
                    *expr = make_prim_undefined();
                    true
                }
                1 => {
                    *expr = new_content.into_iter().next().unwrap();
                    true
                }
                _ => {
                    expr.vartype = new_content.last().unwrap().vartype;
                    *content = new_content;
                    changed
                }
            }
        }
        ExprKind::Trap {
            code: _,
            location: _,
        } => {
            assert!(expr.vartype == None);
            false
        }
    }
}

/**
 * Try to evaluate a PrimAppl at compile time.
 * Requires that expr is actually a PrimAppl, and that func and all args are non-noreturn.
 * This function will check if arguments are actually constants before attempting constant evaluation.
 * This function will also set the expr's vartype to the most specific result type.
 * The return value is true if the expr got changed, or false otherwise.
 */
fn try_const_eval(expr: &mut Expr) -> bool {
    fn set_vartype(out: &mut Option<VarType>, new: VarType) -> bool {
        let ret = *out != Some(new);
        *out = Some(new);
        ret
    }
    fn try_as_prim_number(expr: &Expr) -> Result<f64, ()> {
        assert!(expr.vartype == Some(VarType::Number));
        if let ExprKind::PrimNumber { val } = &expr.kind {
            Ok(*val)
        } else {
            Err(())
        }
    }
    fn try_as_prim_boolean(expr: &Expr) -> Result<bool, ()> {
        assert!(expr.vartype == Some(VarType::Boolean));
        if let ExprKind::PrimBoolean { val } = &expr.kind {
            Ok(*val)
        } else {
            Err(())
        }
    }
    fn try_as_prim_string(expr: &Expr) -> Result<&str, ()> {
        assert!(expr.vartype == Some(VarType::String));
        if let ExprKind::PrimString { val } = &expr.kind {
            Ok(val.as_str())
        } else {
            Err(())
        }
    }
    if let ExprKind::PrimAppl { prim_inst, args } = &mut expr.kind {
        match prim_inst {
            PrimInst::NumberAdd => {
                assert!(args.len() == 2);
                if let Ok((a, b)) = try_as_prim_number(&args[0])
                    .and_then(|a| Ok((a, try_as_prim_number(&args[1])?)))
                {
                    *expr = make_prim_number(a + b);
                    true
                } else {
                    set_vartype(&mut expr.vartype, VarType::Number)
                }
            }
            PrimInst::NumberSub => {
                assert!(args.len() == 2);
                if let Ok((a, b)) = try_as_prim_number(&args[0])
                    .and_then(|a| Ok((a, try_as_prim_number(&args[1])?)))
                {
                    *expr = make_prim_number(a - b);
                    true
                } else {
                    set_vartype(&mut expr.vartype, VarType::Number)
                }
            }
            PrimInst::NumberMul => {
                assert!(args.len() == 2);
                if let Ok((a, b)) = try_as_prim_number(&args[0])
                    .and_then(|a| Ok((a, try_as_prim_number(&args[1])?)))
                {
                    *expr = make_prim_number(a * b);
                    true
                } else {
                    set_vartype(&mut expr.vartype, VarType::Number)
                }
            }
            PrimInst::NumberDiv => {
                assert!(args.len() == 2);
                if let Ok((a, b)) = try_as_prim_number(&args[0])
                    .and_then(|a| Ok((a, try_as_prim_number(&args[1])?)))
                {
                    *expr = make_prim_number(a / b);
                    true
                } else {
                    set_vartype(&mut expr.vartype, VarType::Number)
                }
            }
            PrimInst::NumberRem => {
                assert!(args.len() == 2);
                if let Ok((a, b)) = try_as_prim_number(&args[0])
                    .and_then(|a| Ok((a, try_as_prim_number(&args[1])?)))
                {
                    *expr = make_prim_number(a % b);
                    true
                } else {
                    set_vartype(&mut expr.vartype, VarType::Number)
                }
            }
            PrimInst::NumberEq => {
                assert!(args.len() == 2);
                if let Ok((a, b)) = try_as_prim_number(&args[0])
                    .and_then(|a| Ok((a, try_as_prim_number(&args[1])?)))
                {
                    *expr = make_prim_boolean(a == b);
                    true
                } else {
                    set_vartype(&mut expr.vartype, VarType::Boolean)
                }
            }
            PrimInst::NumberNeq => {
                assert!(args.len() == 2);
                if let Ok((a, b)) = try_as_prim_number(&args[0])
                    .and_then(|a| Ok((a, try_as_prim_number(&args[1])?)))
                {
                    *expr = make_prim_boolean(a != b);
                    true
                } else {
                    set_vartype(&mut expr.vartype, VarType::Boolean)
                }
            }
            PrimInst::NumberGt => {
                assert!(args.len() == 2);
                if let Ok((a, b)) = try_as_prim_number(&args[0])
                    .and_then(|a| Ok((a, try_as_prim_number(&args[1])?)))
                {
                    *expr = make_prim_boolean(a > b);
                    true
                } else {
                    set_vartype(&mut expr.vartype, VarType::Boolean)
                }
            }
            PrimInst::NumberLt => {
                assert!(args.len() == 2);
                if let Ok((a, b)) = try_as_prim_number(&args[0])
                    .and_then(|a| Ok((a, try_as_prim_number(&args[1])?)))
                {
                    *expr = make_prim_boolean(a < b);
                    true
                } else {
                    set_vartype(&mut expr.vartype, VarType::Boolean)
                }
            }
            PrimInst::NumberGe => {
                assert!(args.len() == 2);
                if let Ok((a, b)) = try_as_prim_number(&args[0])
                    .and_then(|a| Ok((a, try_as_prim_number(&args[1])?)))
                {
                    *expr = make_prim_boolean(a >= b);
                    true
                } else {
                    set_vartype(&mut expr.vartype, VarType::Boolean)
                }
            }
            PrimInst::NumberLe => {
                assert!(args.len() == 2);
                if let Ok((a, b)) = try_as_prim_number(&args[0])
                    .and_then(|a| Ok((a, try_as_prim_number(&args[1])?)))
                {
                    *expr = make_prim_boolean(a <= b);
                    true
                } else {
                    set_vartype(&mut expr.vartype, VarType::Boolean)
                }
            }
            PrimInst::BooleanEq => {
                assert!(args.len() == 2);
                if let Ok((a, b)) = try_as_prim_boolean(&args[0])
                    .and_then(|a| Ok((a, try_as_prim_boolean(&args[1])?)))
                {
                    *expr = make_prim_boolean(a == b);
                    true
                } else {
                    set_vartype(&mut expr.vartype, VarType::Boolean)
                }
            }
            PrimInst::BooleanNeq => {
                assert!(args.len() == 2);
                if let Ok((a, b)) = try_as_prim_boolean(&args[0])
                    .and_then(|a| Ok((a, try_as_prim_boolean(&args[1])?)))
                {
                    *expr = make_prim_boolean(a != b);
                    true
                } else {
                    set_vartype(&mut expr.vartype, VarType::Boolean)
                }
            }
            PrimInst::BooleanAnd => {
                assert!(args.len() == 2);
                if let Ok((a, b)) = try_as_prim_boolean(&args[0])
                    .and_then(|a| Ok((a, try_as_prim_boolean(&args[1])?)))
                {
                    *expr = make_prim_boolean(a & b);
                    true
                } else {
                    set_vartype(&mut expr.vartype, VarType::Boolean)
                }
            }
            PrimInst::BooleanOr => {
                assert!(args.len() == 2);
                if let Ok((a, b)) = try_as_prim_boolean(&args[0])
                    .and_then(|a| Ok((a, try_as_prim_boolean(&args[1])?)))
                {
                    *expr = make_prim_boolean(a | b);
                    true
                } else {
                    set_vartype(&mut expr.vartype, VarType::Boolean)
                }
            }
            PrimInst::BooleanNot => {
                assert!(args.len() == 1);
                if let Ok(a) = try_as_prim_boolean(&args[0]) {
                    *expr = make_prim_boolean(!a);
                    true
                } else {
                    set_vartype(&mut expr.vartype, VarType::Boolean)
                }
            }
            PrimInst::NumberNegate => {
                assert!(args.len() == 1);
                if let Ok(a) = try_as_prim_number(&args[0]) {
                    *expr = make_prim_number(-a);
                    true
                } else {
                    set_vartype(&mut expr.vartype, VarType::Number)
                }
            }
            PrimInst::StringAdd => {
                assert!(args.len() == 2);
                if let Ok((a, b)) = try_as_prim_string(&args[0])
                    .and_then(|a| Ok((a, try_as_prim_string(&args[1])?)))
                {
                    *expr = make_prim_string(String::from(a) + b);
                    true
                } else {
                    set_vartype(&mut expr.vartype, VarType::String)
                }
            }
            PrimInst::StringEq => {
                assert!(args.len() == 2);
                if let Ok((a, b)) = try_as_prim_string(&args[0])
                    .and_then(|a| Ok((a, try_as_prim_string(&args[1])?)))
                {
                    *expr = make_prim_boolean(a == b);
                    true
                } else {
                    set_vartype(&mut expr.vartype, VarType::Boolean)
                }
            }
            PrimInst::StringNeq => {
                assert!(args.len() == 2);
                if let Ok((a, b)) = try_as_prim_string(&args[0])
                    .and_then(|a| Ok((a, try_as_prim_string(&args[1])?)))
                {
                    *expr = make_prim_boolean(a != b);
                    true
                } else {
                    set_vartype(&mut expr.vartype, VarType::Boolean)
                }
            }
            PrimInst::StringGt => {
                assert!(args.len() == 2);
                if let Ok((a, b)) = try_as_prim_string(&args[0])
                    .and_then(|a| Ok((a, try_as_prim_string(&args[1])?)))
                {
                    *expr = make_prim_boolean(a > b);
                    true
                } else {
                    set_vartype(&mut expr.vartype, VarType::Boolean)
                }
            }
            PrimInst::StringLt => {
                assert!(args.len() == 2);
                if let Ok((a, b)) = try_as_prim_string(&args[0])
                    .and_then(|a| Ok((a, try_as_prim_string(&args[1])?)))
                {
                    *expr = make_prim_boolean(a < b);
                    true
                } else {
                    set_vartype(&mut expr.vartype, VarType::Boolean)
                }
            }
            PrimInst::StringGe => {
                assert!(args.len() == 2);
                if let Ok((a, b)) = try_as_prim_string(&args[0])
                    .and_then(|a| Ok((a, try_as_prim_string(&args[1])?)))
                {
                    *expr = make_prim_boolean(a >= b);
                    true
                } else {
                    set_vartype(&mut expr.vartype, VarType::Boolean)
                }
            }
            PrimInst::StringLe => {
                assert!(args.len() == 2);
                if let Ok((a, b)) = try_as_prim_string(&args[0])
                    .and_then(|a| Ok((a, try_as_prim_string(&args[1])?)))
                {
                    *expr = make_prim_boolean(a <= b);
                    true
                } else {
                    set_vartype(&mut expr.vartype, VarType::Boolean)
                }
            }
        }
    } else {
        panic!("Expected PrimAppl");
    }
}

/**
 * Try to devirtualize an Appl at compile time.
 * Requires that expr is actually a Appl, and that func and all args are non-noreturn.
 * This function will also set the expr's vartype to the most specific result type.
 * The return value is true if the expr got changed, or false otherwise.
 */
fn try_devirtualize_appl(expr: &mut Expr, local_map: &mut Relabeller, ctx: Context) -> bool {
    assert!(expr.vartype == Some(VarType::Any));
    if let ExprKind::Appl { func, args } = &mut expr.kind {
        if let ExprKind::PrimFunc { funcidxs, closure } = &mut func.kind {
            // args will be temporarily saved into locals
            // we don't create Declarations now; we just wrap them later

            // calculate the allowable overloads
            let overloads = std::mem::take(funcidxs);
            let mut allowable_overloads: Vec<OverloadEntry> = Vec::new();
            // iterate in the reverse direction, since we match them from back to front
            'outer: for overload in Vec::from(overloads).into_iter() {
                let sig: &[VarType] = &ctx.param_types[overload.funcidx];
                if sig.len() != args.len() {
                    // wrong number of params, will never be matched
                    continue;
                }
                // to figure out if this overload is 'useful'
                // we first restrict this overload to the given set of arg types
                // and then see if it is fully contained in any existing selected overload
                let opt_restricted_sig: Option<Box<[VarType]>> = intersect_signature(
                    args.iter().map(|expr| expr.vartype.unwrap()),
                    sig.iter().copied(),
                );
                if let Some(restricted_sig) = opt_restricted_sig {
                    for allowable_overload in &allowable_overloads {
                        if ctx.param_types[allowable_overload.funcidx].superset(&restricted_sig) {
                            // this is not a useful overload
                            continue 'outer;
                        }
                    }
                    // this is a useful overload, save it
                    allowable_overloads.push(overload);
                } else {
                    // no intersection, so this overload will never be called
                    continue;
                }
            }

            // todo! make a special case for a single overload
            // (this is non-trivial, because we need to preserve order of evaluation; maybe a variable optimizer will make this unnecessary)
            match allowable_overloads.len() {
                0 => {
                    // too bad, we immediately emit closure and all args, then trap
                    let mut content = Vec::new();
                    let tmp_closure = std::mem::replace(&mut **closure, dummy_expr());
                    let tmp_args = std::mem::take(args);
                    if !tmp_closure.is_prim_undefined() {
                        content.push(tmp_closure);
                    }
                    content.append(&mut Vec::from(tmp_args));
                    content.push(Expr {
                        vartype: None,
                        kind: ExprKind::Trap {
                            code: error::ERROR_CODE_FUNCTION_PARAM_TYPE,
                            location: SourceLocation {
                                file: 0,
                                start: Position { line: 0, column: 0 },
                                end: Position { line: 0, column: 0 },
                            },
                        },
                    });

                    *expr = make_sequence_from_exprs(content);
                }
                _ => {
                    // now, all the overloads are useful
                    // emit them (remember that they should be matched from front to back now)
                    // and if the last one isn't an exact match, then we should emit the trap too
                    // for emit_level: idx is the param idx that we are processing now, is_last is whether this block is responsible for emitting the trap if necessary
                    fn emit_level(
                        idx: usize,
                        is_last: bool,
                        local_start_idx: usize,
                        closure_localidx: usize,
                        arg_localidxs: &mut [usize],
                        closure_vartype: VarType,
                        args: &[Expr],
                        mut it: impl Iterator<Item = OverloadEntry>,
                        ctx: Context,
                        out: &mut Vec<Expr>,
                    ) -> Option<VarType> {
                        if idx == args.len() {
                            // done typechecking... call the direct function
                            let oe = it.next().unwrap();
                            assert!(it.next().is_none());

                            // emit the call, and after returning we immediately jump out of the thunk
                            let direct_appl = Expr {
                                vartype: ctx.result_types[oe.funcidx],
                                kind: ExprKind::DirectAppl {
                                    funcidx: oe.funcidx,
                                    args: (if oe.has_closure_param {
                                        Some((closure_vartype, closure_localidx))
                                    } else {
                                        None
                                    })
                                    .into_iter()
                                    .map(|(vartype, localidx)| Expr {
                                        vartype: Some(vartype),
                                        kind: ExprKind::VarName {
                                            source: TargetExpr::Local {
                                                localidx: localidx,
                                                next: None,
                                            },
                                        },
                                    })
                                    .chain(arg_localidxs.iter().copied().enumerate().map(
                                        |(i, localidx)| {
                                            Expr {
                                                vartype: Some(
                                                    intersect_type(
                                                        args[i].vartype.unwrap(),
                                                        ctx.param_types[oe.funcidx][i],
                                                    )
                                                    .unwrap(),
                                                ),
                                                kind: ExprKind::VarName {
                                                    source: TargetExpr::Local {
                                                        localidx: localidx,
                                                        next: None,
                                                    },
                                                },
                                            }
                                        },
                                    ))
                                    .collect(),
                                },
                            };
                            out.push(if direct_appl.vartype.is_some() {
                                Expr {
                                    vartype: None,
                                    kind: ExprKind::Break {
                                        num_frames: 0, // jump out to the closest containing Block
                                        expr: Box::new(direct_appl),
                                    },
                                }
                            } else {
                                direct_appl
                            });
                            return ctx.result_types[oe.funcidx];
                        }

                        let mut unioned_type = None;

                        for (new_is_last, (vartype, it2)) in it
                            .group_by(|oe| {
                                intersect_type(
                                    args[idx].vartype.unwrap(),
                                    ctx.param_types[oe.funcidx][idx],
                                )
                                .unwrap()
                            })
                            .into_iter()
                            .special_last()
                        {
                            // we have to box up it2, otherwise we will get a recursive template instantiation (infinite type)
                            let boxed_it2: Box<dyn Iterator<Item = OverloadEntry>> = Box::new(it2);
                            if vartype == args[idx].vartype.unwrap() {
                                // no need to emit a check for this param
                                unioned_type = union_type(
                                    unioned_type,
                                    emit_level(
                                        idx + 1,
                                        is_last && new_is_last,
                                        local_start_idx,
                                        closure_localidx,
                                        arg_localidxs,
                                        closure_vartype,
                                        args,
                                        boxed_it2,
                                        ctx,
                                        out,
                                    ),
                                );
                            } else {
                                // need a type check
                                let mut new_out = Vec::new();
                                let orig_arg_localidx =
                                    std::mem::replace(&mut arg_localidxs[idx], local_start_idx); // the new variable created by TypeCast
                                unioned_type = union_type(
                                    unioned_type,
                                    emit_level(
                                        idx + 1,
                                        false,
                                        local_start_idx + 1,
                                        closure_localidx,
                                        arg_localidxs,
                                        closure_vartype,
                                        args,
                                        boxed_it2,
                                        ctx,
                                        &mut new_out,
                                    ),
                                );
                                arg_localidxs[idx] = orig_arg_localidx;
                                let tmp_seq = make_sequence_from_exprs(new_out);
                                out.push(Expr {
                                    vartype: tmp_seq.vartype,
                                    kind: ExprKind::TypeCast {
                                        test: Box::new(Expr {
                                            vartype: args[idx].vartype,
                                            kind: ExprKind::VarName {
                                                source: TargetExpr::Local {
                                                    localidx: orig_arg_localidx,
                                                    next: None,
                                                },
                                            },
                                        }),
                                        expected: vartype,
                                        create_narrow_local: true,
                                        true_expr: Box::new(tmp_seq),
                                        false_expr: Box::new(make_prim_undefined()),
                                    },
                                });

                                if is_last && new_is_last {
                                    // we have to emit a trap here
                                    out.push(Expr {
                                        vartype: None,
                                        kind: ExprKind::Trap {
                                            code: error::ERROR_CODE_FUNCTION_PARAM_TYPE,
                                            location: SourceLocation {
                                                file: 0,
                                                start: Position { line: 0, column: 0 },
                                                end: Position { line: 0, column: 0 },
                                            },
                                        },
                                    });
                                }
                            }
                        }

                        unioned_type
                    }

                    // but we need to shift local idxs because of the new closure and locals we create here
                    let closure_localidx = local_map.num_new();
                    let (tmp_expr, unioned_type) = local_map.with_skipped_new(|local_map| {
                        let local_start_idx = local_map.num_new();
                        local_map.with_skipped_news(args.len(), |local_map| {
                            let mut tmp_exprs = Vec::new();
                            let mut arg_localidxs: Box<[usize]> =
                                (local_start_idx..(local_start_idx + args.len())).collect();
                            let unioned_type = emit_level(
                                0,
                                true,
                                local_map.num_new(),
                                closure_localidx,
                                &mut arg_localidxs,
                                closure.vartype.unwrap(),
                                args,
                                allowable_overloads.into_iter(),
                                ctx,
                                &mut tmp_exprs,
                            );
                            (make_sequence_from_exprs(tmp_exprs), unioned_type)
                        })
                    });

                    // wrap all the declarations
                    let tmp_args = std::mem::take(args);
                    fn wrap_declarations(
                        wrapped_expr: Expr,
                        mut args_it: impl Iterator<Item = Expr>,
                        local_map: &mut Relabeller,
                        ctx: Context,
                    ) -> Expr {
                        if let Some(mut arg) = args_it.next() {
                            let inner_expr = local_map.with_skipped_new(|local_map| {
                                wrap_declarations(wrapped_expr, args_it, local_map, ctx)
                            });

                            // reoptimize arg, to get the locals re-numbered
                            // hopefully this is not too slow (since each call can only be converted to direct once)
                            optimize_expr(&mut arg, local_map, ctx);

                            Expr {
                                vartype: inner_expr.vartype,
                                kind: ExprKind::Declaration {
                                    local: arg.vartype.unwrap(),
                                    init: Some(Box::new(arg)),
                                    contained_expr: Box::new(inner_expr),
                                },
                            }
                        } else {
                            wrapped_expr
                        }
                    }
                    // remember to wrap the closure too
                    // but don't need to renumber the locals for the closure, because it will be the same
                    let wrapped_declarations_expr = local_map.with_skipped_new(|local_map| {
                        wrap_declarations(tmp_expr, Vec::from(tmp_args).into_iter(), local_map, ctx)
                    });
                    let tmp_closure = std::mem::replace(&mut **closure, dummy_expr());
                    let wrapped_declarations_with_closure = Expr {
                        vartype: wrapped_declarations_expr.vartype,
                        kind: ExprKind::Declaration {
                            local: closure.vartype.unwrap(),
                            init: Some(Box::new(tmp_closure)),
                            contained_expr: Box::new(wrapped_declarations_expr),
                        },
                    };
                    // wrap it in a block so that we can jump here after calling the function
                    *expr = Expr {
                        vartype: unioned_type,
                        kind: ExprKind::Block {
                            expr: Box::new(wrapped_declarations_with_closure),
                        },
                    };
                }
            }
            true
        } else {
            // can't devirtualize
            false
        }
    } else {
        panic!("Expected Appl");
    }
}

fn make_sequence_from_exprs(exprs: Vec<Expr>) -> Expr {
    match exprs.len() {
        0 => make_prim_undefined(),
        1 => exprs.into_iter().next().unwrap(), /* get the zeroth element */
        _ => Expr {
            vartype: exprs.last().unwrap().vartype,
            kind: ExprKind::Sequence { content: exprs },
        },
    }
}

/**
 * Returns the intersection of two signatures (if any).
 * The iterators should have the same length.
 */
fn intersect_signature(
    iter1: impl Iterator<Item = VarType>,
    iter2: impl Iterator<Item = VarType>,
) -> Option<Box<[VarType]>> {
    iter1
        .zip(iter2)
        .map(|(item1, item2)| intersect_type(item1, item2).ok_or(()))
        .collect::<Result<Box<[VarType]>, ()>>()
        .ok()
}

fn intersect_type(first: VarType, second: VarType) -> Option<VarType> {
    if first == VarType::Any {
        return Some(second);
    }
    if second == VarType::Any {
        return Some(first);
    }
    // now both first and second are not Any
    if first == second {
        Some(first)
    } else {
        None
    }
}

/**
 * Returns the type wide enough to contain both the given two types.
 */
fn union_type(first: Option<VarType>, second: Option<VarType>) -> Option<VarType> {
    if first.is_none() {
        return second;
    }
    if second.is_none() {
        return first;
    }
    // now both first and second are not None
    let u_first = first.unwrap();
    let u_second = second.unwrap();
    if u_first == u_second {
        return Some(u_first);
    }
    Some(VarType::Any)
}

fn dummy_expr() -> Expr {
    Expr {
        vartype: Some(VarType::Undefined),
        kind: ExprKind::PrimUndefined,
    }
}

fn make_prim_undefined() -> Expr {
    Expr {
        vartype: Some(VarType::Undefined),
        kind: ExprKind::PrimUndefined,
    }
}

fn make_prim_number(val: f64) -> Expr {
    Expr {
        vartype: Some(VarType::Number),
        kind: ExprKind::PrimNumber { val: val },
    }
}

fn make_prim_boolean(val: bool) -> Expr {
    Expr {
        vartype: Some(VarType::Boolean),
        kind: ExprKind::PrimBoolean { val: val },
    }
}

fn make_prim_string(val: String) -> Expr {
    Expr {
        vartype: Some(VarType::String),
        kind: ExprKind::PrimString { val: val },
    }
}
