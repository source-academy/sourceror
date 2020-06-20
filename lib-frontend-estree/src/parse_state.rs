use super::frontendvar::*;
use crate::estree::*;
use crate::vartype_superset::*;
use std::collections::HashMap;

#[derive(Default)]
pub struct ParseState {
    targets: HashMap<VarLocId, ir::TargetExpr>, // for the Targets
    directs: VarCtx<String, OverloadSet<(Box<[ir::VarType]>, ir::FuncIdx)>>, // for the Directs
}

// Undoable multiple targets
type AddTargetsUndoCtx = Vec<(VarLocId, Option<ir::TargetExpr>)>;
impl ParseState {
    pub fn add_targets(
        &mut self,
        new_targets: Box<[(VarLocId, ir::TargetExpr)]>,
    ) -> AddTargetsUndoCtx {
        Vec::from(new_targets)
            .into_iter()
            .map(|(varlocid, target_expr)| (varlocid, self.targets.insert(varlocid, target_expr)))
            .collect()
    }
    pub fn remove_targets(&mut self, undo_ctx: AddTargetsUndoCtx) {
        undo_ctx
            .into_iter()
            .rev()
            .for_each(|(varlocid, opt_target_expr)| match opt_target_expr {
                Some(target_expr) => {
                    self.targets.insert(varlocid, target_expr);
                }
                None => {
                    self.targets.remove(&varlocid);
                }
            });
    }
}

// Undoable target
type AddTargetUndoCtx = (VarLocId, Option<ir::TargetExpr>);
impl ParseState {
    pub fn add_target(
        &mut self,
        varlocid: VarLocId,
        target_expr: ir::TargetExpr,
    ) -> AddTargetUndoCtx {
        (varlocid, self.targets.insert(varlocid, target_expr))
    }
    pub fn remove_target(&mut self, undo_ctx: AddTargetUndoCtx) {
        let (varlocid, opt_target_expr) = undo_ctx;
        match opt_target_expr {
            Some(target_expr) => {
                self.targets.insert(varlocid, target_expr);
            }
            None => {
                self.targets.remove(&varlocid);
            }
        }
    }
}

// Get target
impl ParseState {
    pub fn get_target(&mut self, varlocid: &VarLocId) -> Option<&ir::TargetExpr> {
        self.targets.get(varlocid)
    }
}

impl Superset for (Box<[ir::VarType]>, ir::FuncIdx) {
    /**
     * Returns true if self is a superset of other.
     */
    fn superset(&self, other: &Self) -> bool {
        self.0.superset(&other.0)
    }
}

// Undoable multiple directs
type AddDirectsUndoCtx = Vec<(
    String,
    Option<OverloadSet<(Box<[ir::VarType]>, ir::FuncIdx)>>,
)>;
impl ParseState {
    pub fn add_directs(
        &mut self,
        new_directs: Box<[(String, (Box<[ir::VarType]>, ir::FuncIdx))]>,
    ) -> AddDirectsUndoCtx {
        Vec::from(new_directs)
            .into_iter()
            .map(|(name, overload_entry)| {
                let tmp = self.directs.save(name.as_str());
                self.directs
                    .coalesce(name.clone(), OverloadSet::from_single(overload_entry));
                (name, tmp)
            })
            .collect()
    }
    pub fn remove_directs(&mut self, undo_ctx: AddDirectsUndoCtx) {
        undo_ctx
            .into_iter()
            .rev()
            .for_each(|(name, restore_state)| {
                self.directs.restore(name.as_str(), restore_state);
            });
    }

    pub fn add_direct_overloadsets(
        &mut self,
        new_directs: Box<[(String, OverloadSet<(Box<[ir::VarType]>, ir::FuncIdx)>)]>,
    ) -> AddDirectsUndoCtx {
        Vec::from(new_directs)
            .into_iter()
            .map(|(name, overload_set)| {
                let tmp = self.directs.save(name.as_str());
                self.directs.coalesce(name.clone(), overload_set);
                (name, tmp)
            })
            .collect()
    }
    pub fn remove_direct_overloadsets(&mut self, undo_ctx: AddDirectsUndoCtx) {
        self.remove_directs(undo_ctx);
    }
}

// Single direct (not undoable)
impl ParseState {
    pub fn add_direct(
        &mut self,
        name: String,
        overload_set: OverloadSet<(Box<[ir::VarType]>, ir::FuncIdx)>,
    ) {
        self.directs.coalesce(name, overload_set);
    }
}

// Get direct
impl ParseState {
    pub fn get_direct(
        &self,
        name: &str,
    ) -> Option<&OverloadSet<(Box<[ir::VarType]>, ir::FuncIdx)>> {
        self.directs.get(name)
    }
}

type ClosureUndoCtx = HashMap<VarLocId, ir::TargetExpr>;
impl ParseState {
    pub fn enter_closure(
        &mut self,
        closed_targets: Box<[(VarLocId, ir::TargetExpr)]>,
    ) -> ClosureUndoCtx {
        let mut new_targets: HashMap<VarLocId, ir::TargetExpr> = HashMap::new();
        for (varlocid, target_expr) in &self.targets {
            if varlocid.depth == 0 {
                new_targets.insert(*varlocid, target_expr.clone());
            }
        }
        for (varlocid, target_expr) in Vec::from(closed_targets) {
            new_targets.insert(varlocid, target_expr);
        }
        std::mem::replace(&mut self.targets, new_targets)
    }
    pub fn leave_closure(&mut self, undo_ctx: ClosureUndoCtx) {
        self.targets = undo_ctx;
    }
}
