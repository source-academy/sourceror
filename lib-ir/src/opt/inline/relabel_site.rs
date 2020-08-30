use super::SiteProperties;
use crate::*;

/**
 * Relabels all the locals and landings by the relative offset given by the site properties.
 * Note that we only need to rewrite Returns but not Breaks, because Breaks are relative.
 * site.num_landings()
 * Also wraps everything in a block so that returns can jump here.
 */
pub(super) fn relabel_inline_func(mut expr: Expr, site: SiteProperties) -> (Expr, bool) {
    let ret = relabel_site(&mut expr, site, 0);
    (
        Expr {
            vartype: expr.vartype,
            kind: ExprKind::Block {
                expr: Box::new(expr),
            },
        },
        ret,
    )
}

fn relabel_site(expr: &mut Expr, site: SiteProperties, num_landings: usize) -> bool {
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
        } => relabel_site(&mut **closure, site, num_landings),
        ExprKind::TypeCast {
            test,
            expected: _,
            create_narrow_local: _,
            true_expr,
            false_expr,
        } => {
            relabel_site(&mut **test, site, num_landings)
                | relabel_site(&mut **true_expr, site, num_landings)
                | relabel_site(&mut **false_expr, site, num_landings)
        }
        ExprKind::VarName { source } => relabel_site_target(source, site),
        ExprKind::PrimAppl { prim_inst: _, args } => args.iter_mut().fold(false, |prev, arg| {
            prev | relabel_site(arg, site, num_landings)
        }),
        ExprKind::Appl {
            func,
            args,
            location: _,
        } => {
            relabel_site(func, site, num_landings)
                | args.iter_mut().fold(false, |prev, arg| {
                    prev | relabel_site(arg, site, num_landings)
                })
        }
        ExprKind::DirectAppl { funcidx: _, args } => args.iter_mut().fold(false, |prev, arg| {
            prev | relabel_site(arg, site, num_landings)
        }),
        ExprKind::Conditional {
            cond,
            true_expr,
            false_expr,
        } => {
            relabel_site(&mut **cond, site, num_landings)
                | relabel_site(&mut **true_expr, site, num_landings)
                | relabel_site(&mut **false_expr, site, num_landings)
        }
        ExprKind::Declaration {
            local: _,
            init,
            contained_expr,
        } => {
            (if let Some(init_expr) = init {
                relabel_site(&mut **init_expr, site, num_landings)
            } else {
                false
            }) | relabel_site(&mut **contained_expr, site, num_landings)
        }
        ExprKind::Assign { target, expr } => {
            relabel_site_target(target, site) | relabel_site(&mut **expr, site, num_landings)
        }
        ExprKind::Return { expr: inner_expr } => {
            // Return is converted to Break when inlining the function
            relabel_site(&mut **inner_expr, site, num_landings);
            *expr = Expr {
                vartype: None,
                kind: ExprKind::Break {
                    num_frames: num_landings,
                    expr: Box::new(std::mem::replace(&mut **inner_expr, dummy_expr())),
                },
            };
            true
        }
        ExprKind::Break {
            num_frames: _,
            expr,
        } => relabel_site(&mut **expr, site, num_landings),
        ExprKind::Block { expr } => relabel_site(&mut **expr, site, num_landings + 1),
        ExprKind::Sequence { content } => content.iter_mut().fold(false, |prev, expr| {
            prev | relabel_site(expr, site, num_landings)
        }),
        ExprKind::Trap {
            code: _,
            location: _,
        } => false,
    }
}

fn relabel_site_target(target: &mut TargetExpr, site: SiteProperties) -> bool {
    if let TargetExpr::Local { localidx, next: _ } = target {
        let new_localidx: usize = *localidx + site.num_locals();
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

fn dummy_expr() -> Expr {
    Expr {
        vartype: Some(VarType::Undefined),
        kind: ExprKind::PrimUndefined,
    }
}
