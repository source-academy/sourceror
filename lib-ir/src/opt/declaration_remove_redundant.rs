use super::*;

/**
 * Discretionary optimisation to remove redundant writes to variables, and also eventually remove variables that are never written to.
 * This does a single backward pass over the IR.
 * TODO: Single-declare-use variables should be optimized regardless of pureness or identity-safeness.
 *
 * How it works:
 * We do a single pass backwards, and then a single pass forwards (to remove decls slated for removal)
 * Each variable can be in one of these states:
 * * Unused (variable is never read (i.e. we can delete it if it remains unused throughout); this is the initial state of locals)
 * * UsedUnread (variable is read after some future write only (i.e. the current content of the variable is unused, but the variable will be used later))
 * * Read (the content of this variable may be read later)
 * Initially (at the end of the function), there are no locals, and all globals are Read (i.e. some other function might use them later)
 * Where we reach a read:
 * * Set the state to Read.
 * When we reach a write:
 * * If the variable is currently Read, then set it to UsedUnread.
 * * Otherwise remove this write (but remember to keep the expr in case there are side effects).
 * When we get to a declaration:
 * * If the variable is currently Unused, mark it for deletion.
 * * (we delete all marked variables later for efficiency, since we will need to relabel all reads and writes)
 *
 * Note: We will have to figure out how to process loops.
 * Probably we figure out which variables are written to, and just say that they have unknown value.
 */
pub fn optimize(mut program: Program, start_funcidx: usize) -> (Program, bool) {
    let mut changed = false;
    for func in program.funcs.iter_mut().skip(start_funcidx) {
        changed |= optimize_func(func, program.globals.len());
    }
    (program, changed)
}

// Enum values so that Ord/PartialOrd work
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum VarState {
    Unused = 0,
    UsedUnread = 1,
    Read = 2,
}

impl VarState {
    fn merge_with(&mut self, other: VarState) {
        *self = (*self).max(other);
    }
}

// An owned local/global states
#[derive(Clone)]
struct States {
    globals: Box<[VarState]>,
    locals: Vec<VarState>,
}

impl States {
    fn merge_with(&mut self, other: States) {
        fn merge_slice_impl<I: IntoIterator<Item = VarState>>(this: &mut [VarState], that: I) {
            for (t1, t2) in this.into_iter().zip(that.into_iter()) {
                t1.merge_with(t2);
            }
        }

        assert_eq!(self.globals.len(), other.globals.len());
        assert_eq!(self.locals.len(), other.locals.len());
        merge_slice_impl(&mut self.globals, Vec::from(other.globals));
        merge_slice_impl(&mut self.locals, other.locals);
    }

    fn new_with_read_globals_and_unused_locals(num_globals: usize, num_locals: usize) -> Self {
        Self {
            globals: (0..num_globals)
                .into_iter()
                .map(|_| VarState::Read)
                .collect(),
            locals: (0..num_locals)
                .into_iter()
                .map(|_| VarState::Unused)
                .collect(),
        }
    }

    // make all the globals Read (used when we called a function)
    fn make_globals_read(&mut self) {
        for varstate in self.globals.iter_mut() {
            *varstate = VarState::Read;
        }
    }

    fn reset_and_grow_unused_from(&mut self, other: &States) {
        assert!(other.globals.len() == self.globals.len());
        assert!(other.locals.len() <= self.locals.len());
        for (self_global, other_global) in
            self.globals.iter_mut().zip(other.globals.iter().copied())
        {
            *self_global = other_global;
        }
        for (self_local, other_local) in self.locals.iter_mut().zip(
            other
                .locals
                .iter()
                .copied()
                .chain(std::iter::repeat(VarState::Unused)),
        ) {
            *self_local = other_local;
        }
    }

    fn push_local(&mut self, state: VarState) {
        self.locals.push(state);
    }

    fn pop_local(&mut self) -> VarState {
        self.locals.pop().unwrap()
    }
    fn global_mut(&mut self, globalidx: usize) -> &mut VarState {
        &mut self.globals[globalidx]
    }
    fn local_mut(&mut self, localidx: usize) -> &mut VarState {
        &mut self.locals[localidx]
    }
}

// stores the globals and locals states that jump out of a break or return statement
// unlike States (which is cloned when we reach an if-else), we do not ever want to clone this.
struct Context {
    landings: Vec<States>, // the states of variables at the *end* of blocks/loops
    decls_for_removal: Vec<bool>, // this is a list of Declarations/TypeCast[with create_narrow_local==true] in post-order reversed traversal.  True means we should delete it, false to retain it.  Params are not stored here.
}

impl Context {
    // create landing context for a new function
    // there will be a single state, and
    // all globals will be Read (since they may be used outside the function), and there will be no locals.
    fn new(num_globals: usize) -> Self {
        Self {
            landings: vec![States::new_with_read_globals_and_unused_locals(
                num_globals,
                0,
            )],
            decls_for_removal: Vec::new(),
        }
    }

    fn add_decl_for_removal(&mut self, to_remove: bool) {
        self.decls_for_removal.push(to_remove);
    }

    // add a new landing (used at the end of a Block)
    fn push_landing(&mut self, states: States) {
        self.landings.push(states);
    }

    // remove the last landing (used at the start of a Block)
    fn pop_landing(&mut self) -> States {
        self.landings.pop().unwrap()
    }

    fn get_return_states(&self) -> &States {
        &self.landings[0]
    }

    fn get_break_states(&self, num_frames: usize) -> &States {
        let index = self.landings.len() - 1 - num_frames;
        &self.landings[index]
    }

    fn into_removal_context(self, num_params: usize) -> RemovalContext {
        assert_eq!(self.landings.len(), 1);
        RemovalContext::new(num_params, self.decls_for_removal)
    }
}

struct RemovalContext {
    mapping: Vec<Option<usize>>, // This is the new index of the local.  None = local is deleted.
    num_locals: usize,           // The number of new locals now ( == the next localidx to use)
    decls_for_removal: Vec<bool>,
}

impl RemovalContext {
    fn new(num_params: usize, decls_for_removal: Vec<bool>) -> Self {
        Self {
            mapping: (0..num_params).into_iter().map(|x| Some(x)).collect(),
            num_locals: num_params,
            decls_for_removal,
        }
    }
    // Called when we encounter the start of a declaration.  Returns true if this declaration should be removed.
    fn push_decl(&mut self) -> bool {
        let should_remove = self.decls_for_removal.pop().unwrap();
        self.mapping.push(if should_remove {
            None
        } else {
            let tmp = self.num_locals;
            self.num_locals += 1;
            Some(tmp)
        });
        should_remove
    }
    // Called when we encounter the end of a declaration.
    fn pop_decl(&mut self) {
        if let Some(new_localidx) = self.mapping.pop().unwrap() {
            self.num_locals -= 1;
            assert_eq!(new_localidx, self.num_locals);
        }
    }
    // Get the new localidx from the old localidx.  Panics if this local is should be removed.
    fn query(&self, old_localidx: usize) -> usize {
        self.mapping[old_localidx].unwrap()
    }
    fn assert_finished(self) {
        assert!(self.decls_for_removal.is_empty());
    }
}

/**
 * Optimises the function.
 * The return value is true if the function got changed, or false otherwise.
 */
#[must_use]
fn optimize_func(func: &mut Func, num_globals: usize) -> bool {
    let mut context = Context::new(num_globals);
    let mut changed = optimize_expr(
        &mut func.expr,
        &mut States::new_with_read_globals_and_unused_locals(num_globals, func.params.len()),
        &mut context,
    );
    let mut removal_context = context.into_removal_context(func.params.len());
    changed |= remove_decls(&mut func.expr, &mut removal_context);
    removal_context.assert_finished();
    changed
}

/**
 * Optimises the expr.
 * The return value is true if the expr got changed, or false otherwise.
 * @param expr: The expr to optimize
 * @param states: The variable states at the end of this expr (since we're going backwards)
 * @param context: Context
 */
#[must_use]
fn optimize_expr(expr: &mut Expr, states: &mut States, context: &mut Context) -> bool {
    match &mut expr.kind {
        ExprKind::PrimUndefined
        | ExprKind::PrimNumber { val: _ }
        | ExprKind::PrimBoolean { val: _ }
        | ExprKind::PrimString { val: _ }
        | ExprKind::PrimStructT { typeidx: _ } => false,
        ExprKind::PrimFunc {
            funcidxs: _,
            closure,
        } => optimize_expr(closure, states, context),
        ExprKind::TypeCast {
            test,
            expected: _,
            create_narrow_local,
            true_expr,
            false_expr,
        } => {
            let mut cloned_states = states.clone();
            let mut changed = optimize_expr(false_expr, &mut cloned_states, context);
            if *create_narrow_local {
                // this is a decl
                states.push_local(VarState::Unused);
                changed |= optimize_expr(true_expr, states, context);
                context.add_decl_for_removal(states.pop_local() == VarState::Unused);
            } else {
                changed |= optimize_expr(true_expr, states, context);
            }
            states.merge_with(cloned_states);
            changed |= optimize_expr(test, states, context);
            changed
        }
        ExprKind::VarName { source } => {
            // this is a read
            match source {
                TargetExpr::Global { globalidx, next: _ } => {
                    *states.global_mut(*globalidx) = VarState::Read;
                }
                TargetExpr::Local { localidx, next: _ } => {
                    *states.local_mut(*localidx) = VarState::Read;
                }
            }
            false
        }
        ExprKind::PrimAppl { prim_inst: _, args } => {
            args.iter_mut().rev().fold(false, |prev, curr| {
                prev | optimize_expr(curr, states, context)
            })
        }
        ExprKind::Appl {
            func,
            args,
            location: _,
        } => {
            // after calling a function, all globals are reset to Read (the callee might read them)
            states.make_globals_read();
            args.iter_mut().rev().fold(false, |prev, curr| {
                prev | optimize_expr(curr, states, context)
            }) | optimize_expr(func, states, context)
        }
        ExprKind::DirectAppl { funcidx: _, args } => {
            // after calling a function, all globals are reset to Read (the callee might read them)
            states.make_globals_read();
            args.iter_mut().rev().fold(false, |prev, curr| {
                prev | optimize_expr(curr, states, context)
            })
        }
        ExprKind::Conditional {
            cond,
            true_expr,
            false_expr,
        } => {
            let mut cloned_states = states.clone();
            let mut changed = optimize_expr(false_expr, &mut cloned_states, context);
            changed |= optimize_expr(true_expr, states, context);
            states.merge_with(cloned_states);
            changed |= optimize_expr(cond, states, context);
            changed
        }
        ExprKind::Declaration {
            local: _,
            init,
            contained_expr,
        } => {
            // this is a decl
            states.push_local(VarState::Unused);
            let mut changed = optimize_expr(contained_expr, states, context);
            let varstate = states.pop_local();
            context.add_decl_for_removal(varstate == VarState::Unused);
            changed |= if let Some(init_expr) = init {
                optimize_expr(init_expr, states, context)
            } else {
                false
            };
            changed |= if varstate != VarState::Read && init.is_some() {
                // remove the initialization (but convert it to a sequence in case the init has side effects)
                let mut tmp_expr = std::mem::replace(expr, make_prim_undefined());
                let init_expr: Expr = if let ExprKind::Declaration {
                    local: _,
                    init,
                    contained_expr: _,
                } = &mut tmp_expr.kind
                {
                    *init.take().unwrap()
                } else {
                    panic!()
                };
                // the is_pure_primitive() below is really just to make it a bit faster, but propagate.rs should already coalesce nested sequences
                *expr = if is_pure_primitive(&init_expr) {
                    tmp_expr
                } else {
                    Expr {
                        vartype: tmp_expr.vartype,
                        kind: ExprKind::Sequence {
                            content: vec![init_expr, tmp_expr],
                        },
                    }
                };
                true
            } else {
                false
            };
            // the above code guarantees that every decl for removal must have init == None
            changed
        }
        ExprKind::Assign {
            target,
            expr: expr2,
        } => {
            // this is a write
            let delete_write = match target {
                TargetExpr::Global { globalidx, next } => {
                    let state = states.global_mut(*globalidx);
                    if next.is_none() {
                        if *state == VarState::Read {
                            *state = VarState::UsedUnread;
                            false
                        } else {
                            true
                        }
                    } else {
                        // this is actually sort of a read
                        *state = VarState::Read;
                        false
                    }
                }
                TargetExpr::Local { localidx, next } => {
                    let state = states.local_mut(*localidx);
                    if next.is_none() {
                        if *state == VarState::Read {
                            *state = VarState::UsedUnread;
                            false
                        } else {
                            true
                        }
                    } else {
                        // this is actually sort of a read
                        *state = VarState::Read;
                        false
                    }
                }
            };
            let mut changed = optimize_expr(expr2, states, context);
            changed |= if delete_write {
                // remove this assignment by converting it to the RHS expr
                let tmp_expr = std::mem::replace(&mut **expr2, make_prim_undefined());
                *expr = tmp_expr;
                true
            } else {
                false
            };
            changed
        }
        ExprKind::Return { expr } => {
            states.reset_and_grow_unused_from(context.get_return_states());
            optimize_expr(expr, states, context)
        }
        ExprKind::Break { num_frames, expr } => {
            states.reset_and_grow_unused_from(context.get_break_states(*num_frames));
            optimize_expr(expr, states, context)
        }
        ExprKind::Block { expr } => {
            context.push_landing(states.clone());
            let changed = optimize_expr(expr, states, context);
            context.pop_landing();
            changed
        }
        ExprKind::Sequence { content } => content.iter_mut().rev().fold(false, |prev, curr| {
            prev | optimize_expr(curr, states, context)
        }),
        ExprKind::Trap {
            code: _,
            location: _,
        } => false,
    }
}

/**
 * Removes the decls that have been slated for removal, and re-indexes the other decls.
 * The return value is true if the expr got changed, or false otherwise.
 * @param expr: The expr to carry out relabelling
 * @param context: Context
 */
#[must_use]
fn remove_decls(expr: &mut Expr, context: &mut RemovalContext) -> bool {
    match &mut expr.kind {
        ExprKind::PrimUndefined
        | ExprKind::PrimNumber { val: _ }
        | ExprKind::PrimBoolean { val: _ }
        | ExprKind::PrimString { val: _ }
        | ExprKind::PrimStructT { typeidx: _ } => false,
        ExprKind::PrimFunc {
            funcidxs: _,
            closure,
        } => remove_decls(closure, context),
        ExprKind::TypeCast {
            test,
            expected: _,
            create_narrow_local,
            true_expr,
            false_expr,
        } => {
            let mut changed = remove_decls(test, context);
            if *create_narrow_local {
                // this is a decl
                let should_delete = context.push_decl();
                changed |= remove_decls(true_expr, context);
                context.pop_decl();
                if should_delete {
                    *create_narrow_local = false;
                    changed = true;
                }
            } else {
                changed |= remove_decls(true_expr, context);
            }
            changed |= remove_decls(false_expr, context);
            changed
        }
        ExprKind::VarName { source } => {
            // this is a read
            if let TargetExpr::Local { localidx, next: _ } = source {
                *localidx = context.query(*localidx);
            }
            false
        }
        ExprKind::PrimAppl { prim_inst: _, args } => args
            .iter_mut()
            .fold(false, |prev, curr| prev | remove_decls(curr, context)),
        ExprKind::Appl {
            func,
            args,
            location: _,
        } => {
            remove_decls(func, context)
                | args
                    .iter_mut()
                    .fold(false, |prev, curr| prev | remove_decls(curr, context))
        }
        ExprKind::DirectAppl { funcidx: _, args } => args
            .iter_mut()
            .fold(false, |prev, curr| prev | remove_decls(curr, context)),
        ExprKind::Conditional {
            cond,
            true_expr,
            false_expr,
        } => {
            remove_decls(cond, context)
                | remove_decls(true_expr, context)
                | remove_decls(false_expr, context)
        }
        ExprKind::Declaration {
            local: _,
            init,
            contained_expr,
        } => {
            // this is a decl
            let mut changed = if let Some(init_expr) = init {
                remove_decls(init_expr, context)
            } else {
                false
            };
            let should_remove = context.push_decl();
            changed |= remove_decls(contained_expr, context);
            context.pop_decl();
            if should_remove {
                assert!(init.is_none());
                let tmp_expr = std::mem::replace(&mut **contained_expr, make_prim_undefined());
                *expr = tmp_expr;
                changed = true
            }
            changed
        }
        ExprKind::Assign {
            target,
            expr: expr2,
        } => {
            // this is a write
            if let TargetExpr::Local { localidx, next: _ } = target {
                *localidx = context.query(*localidx);
            }
            remove_decls(expr2, context)
        }
        ExprKind::Return { expr } => remove_decls(expr, context),
        ExprKind::Break {
            num_frames: _,
            expr,
        } => remove_decls(expr, context),
        ExprKind::Block { expr } => remove_decls(expr, context),
        ExprKind::Sequence { content } => content
            .iter_mut()
            .fold(false, |prev, curr| prev | remove_decls(curr, context)),
        ExprKind::Trap {
            code: _,
            location: _,
        } => false,
    }
}

#[must_use]
fn make_prim_undefined() -> Expr {
    Expr {
        vartype: Some(VarType::Undefined),
        kind: ExprKind::PrimUndefined,
    }
}
