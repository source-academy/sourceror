use super::Relabeller;
use crate::*;

/**
 * Relabels all the TargetExpr that target locals, according to the relabeller.
 * While the relabeller is mutable, this function will return the relabeller to its original state before returning.
 */
pub fn relabel(expr: &mut Expr, relabeller: &mut Relabeller) -> bool {
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
        } => relabel(&mut **closure, relabeller),
        ExprKind::TypeCast {
            test,
            expected: _,
            create_narrow_local,
            true_expr,
            false_expr,
        } => {
            relabel(&mut **test, relabeller)
                | if *create_narrow_local {
                    relabeller.with_entry(|relabeller, _, _| relabel(&mut **true_expr, relabeller))
                } else {
                    relabel(&mut **true_expr, relabeller)
                }
                | relabel(&mut **false_expr, relabeller)
        }
        ExprKind::VarName { source } => relabel_target(source, relabeller),
        ExprKind::PrimAppl { prim_inst: _, args } => args
            .iter_mut()
            .fold(false, |prev, arg| prev | relabel(arg, relabeller)),
        ExprKind::Appl {
            is_tail,
            func,
            args,
            location: _,
        } => {
            relabel(func, relabeller)
                | args
                    .iter_mut()
                    .fold(false, |prev, arg| prev | relabel(arg, relabeller))
        }
        ExprKind::DirectAppl { is_tail, funcidx: _, args } => args
            .iter_mut()
            .fold(false, |prev, arg| prev | relabel(arg, relabeller)),
        ExprKind::Conditional {
            cond,
            true_expr,
            false_expr,
        } => {
            relabel(&mut **cond, relabeller)
                | relabel(&mut **true_expr, relabeller)
                | relabel(&mut **false_expr, relabeller)
        }
        ExprKind::Declaration {
            local: _,
            init,
            contained_expr,
        } => {
            (if let Some(init_expr) = init {
                relabel(&mut **init_expr, relabeller)
            } else {
                false
            }) | relabeller
                .with_entry(|relabeller, _, _| relabel(&mut **contained_expr, relabeller))
        }
        ExprKind::Assign { target, expr } => {
            relabel_target(target, relabeller) | relabel(&mut **expr, relabeller)
        }
        ExprKind::Return { expr } => relabel(&mut **expr, relabeller),
        ExprKind::Break {
            num_frames: _,
            expr,
        } => relabel(&mut **expr, relabeller),
        ExprKind::Block { expr } => relabel(&mut **expr, relabeller),
        ExprKind::Sequence { content } => content
            .iter_mut()
            .fold(false, |prev, expr| prev | relabel(expr, relabeller)),
        ExprKind::Trap {
            code: _,
            location: _,
        } => false,
    }
}

pub fn relabel_target(target: &mut TargetExpr, relabeller: &mut Relabeller) -> bool {
    if let TargetExpr::Local { localidx, next: _ } = target {
        let new_localidx: usize = relabeller.map_old_to_new(*localidx).unwrap();
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
