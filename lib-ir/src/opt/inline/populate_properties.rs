use super::*;

/**
 * Populates the function properties.
 * Also sets functions with outgoing indirect calls to null.
 */
pub(super) fn populate_properties_func(
    funcidx: usize,
    func: &mut Func,
    func_props: &mut [Option<FunctionProperties>],
) {
    populate_properties(
        funcidx,
        &mut func.expr,
        func_props,
        SiteProperties::new_with_amount(func.params.len()),
    );
}

fn populate_properties(
    funcidx: usize,
    expr: &mut Expr,
    func_props: &mut [Option<FunctionProperties>],
    site: SiteProperties,
) {
    // Note: we explicitly list out all possibilities so we will get a compile error if a new exprkind is added.
    match &mut expr.kind {
        ExprKind::PrimUndefined => {}
        ExprKind::PrimNumber { val: _ }
        | ExprKind::PrimBoolean { val: _ }
        | ExprKind::PrimStructT { typeidx: _ }
        | ExprKind::PrimString { val: _ } => {
            inc_cost(&mut func_props[funcidx]);
        }
        ExprKind::PrimFunc { funcidxs, closure } => {
            populate_properties(funcidx, closure, func_props, site);
            inc_cost(&mut func_props[funcidx]);
            for oe in &**funcidxs {
                set_has_indirect_calls(&mut func_props[oe.funcidx]);
            }
        }
        ExprKind::TypeCast {
            test,
            expected: _,
            create_narrow_local,
            true_expr,
            false_expr,
        } => {
            populate_properties(funcidx, test, func_props, site);
            populate_properties(
                funcidx,
                true_expr,
                func_props,
                if *create_narrow_local {
                    site.with_local()
                } else {
                    site
                },
            );
            populate_properties(funcidx, false_expr, func_props, site);
        }
        ExprKind::VarName { source: _ } => {
            inc_cost(&mut func_props[funcidx]);
        }
        ExprKind::PrimAppl { prim_inst: _, args } => {
            for arg in &mut **args {
                populate_properties(funcidx, arg, func_props, site);
            }
        }
        ExprKind::Appl {
            func,
            args,
            location: _,
        } => {
            purge_func(funcidx, func_props);
            populate_properties(funcidx, func, func_props, site);
            for arg in &mut **args {
                populate_properties(funcidx, arg, func_props, site);
            }
        }
        ExprKind::DirectAppl {
            funcidx: target_funcidx,
            args,
        } => {
            for arg in &mut **args {
                populate_properties(funcidx, arg, func_props, site);
            }
            add_direct_call(funcidx, *target_funcidx, func_props, expr, site);
        }
        ExprKind::Conditional {
            cond,
            true_expr,
            false_expr,
        } => {
            populate_properties(funcidx, cond, func_props, site);
            populate_properties(funcidx, true_expr, func_props, site);
            populate_properties(funcidx, false_expr, func_props, site);
        }
        ExprKind::Declaration {
            local: _,
            init,
            contained_expr,
        } => {
            if let Some(init_expr) = init {
                populate_properties(funcidx, init_expr, func_props, site);
            }
            populate_properties(funcidx, contained_expr, func_props, site.with_local());
        }
        ExprKind::Assign { target: _, expr } => {
            populate_properties(funcidx, expr, func_props, site);
        }
        ExprKind::Return { expr } => {
            populate_properties(funcidx, expr, func_props, site);
        }
        ExprKind::Break {
            num_frames: _,
            expr,
        } => {
            populate_properties(funcidx, expr, func_props, site);
        }
        ExprKind::Block { expr } => {
            populate_properties(funcidx, expr, func_props, site);
        }
        ExprKind::Sequence { content } => {
            for expr in content {
                populate_properties(funcidx, expr, func_props, site);
            }
        }
        ExprKind::Trap {
            code: _,
            location: _,
        } => {
            inc_cost(&mut func_props[funcidx]);
        }
    }
}

fn inc_cost(opt_fp: &mut Option<FunctionProperties>) {
    if let Some(fp) = opt_fp {
        fp.cost += 1;
    }
}

fn set_has_indirect_calls(opt_fp: &mut Option<FunctionProperties>) {
    if let Some(fp) = opt_fp {
        fp.has_indirect_calls = true;
    }
}

fn add_direct_call(
    source_funcidx: FuncIdx,
    target_funcidx: FuncIdx,
    func_props: &mut [Option<FunctionProperties>],
    direct_call_expr: &mut Expr,
    site: SiteProperties,
) {
    if let Some(target_fp) = &mut func_props[target_funcidx] {
        target_fp
            .parents
            .push((source_funcidx, direct_call_expr.into(), site));
        if let Some(source_fp) = &mut func_props[source_funcidx] {
            source_fp.num_calls += 1;
        }
    }
}

fn purge_func(funcidx: usize, func_props: &mut [Option<FunctionProperties>]) {
    if let Some(fp) = std::mem::replace(&mut func_props[funcidx], None) {
        for (funcidx, _, _) in fp.parents {
            if let Some(caller_fp) = &mut func_props[funcidx] {
                caller_fp.num_calls -= 1;
            }
        }
    }
}
