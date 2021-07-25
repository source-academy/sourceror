use super::*;

/**
 * Discretionary optimisation to determine where we are storing pure primitives into variables,
 * and hence can know the type and possibly values of the variables.  This does a single forward pass over the IR.
 * TODO: Single-declare-use variables should be optimized regardless of pureness or identity-safeness.
 *
 * How it works:
 * We make a single pass where we take note of every write and read to variables.
 * Each variable can be in one of these states:
 * * Unknown value (type could be either Any or some more specific type, this is stored)
 * * Known value (this is an identity-safe known value) (this also has a specific type)
 * * Uninitialized (this value is garbage (and we can assume it is never read))
 * Where we reach a read:
 * * If the variable is of known value, we replace the read with the known value.
 * When we reach a write:
 * * Set whether the variable has known or unknown value, based on the thing being currently written.
 *
 * Note: when combining alteriatives (e.g. if-else, loops, break, return), we have to take the union of those states.
 * Note: We will have to figure out how to process loops.
 * Probably we figure out which variables are written to, and just say that they have unknown value.
 */
pub fn optimize(mut program: Program, start_funcidx: usize) -> (Program, bool) {
    let mut changed = false;
    for func in program.funcs.iter_mut().skip(start_funcidx) {
        changed |= optimize_func(func, &program.globals);
    }
    (program, changed)
}

#[derive(Clone)]
enum VarState {
    Unknown(VarType), // could be Any
    Known(Expr),      // type cannot be Any
    Uninitialized,
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

    fn merge_with_truncated_locals(&mut self, mut other: States) {
        other.locals.truncate(self.locals.len());
        self.merge_with(other);
    }

    fn new_with_unknown_globals_and_locals(
        globals_vartypes: &[VarType],
        locals_vartypes: &[VarType],
    ) -> Self {
        Self {
            globals: globals_vartypes
                .iter()
                .copied()
                .map(|vartype| VarState::Unknown(vartype))
                .collect(),
            locals: locals_vartypes
                .iter()
                .copied()
                .map(|vartype| VarState::Unknown(vartype))
                .collect(),
        }
    }

    fn new_with_uninitialized_globals_and_locals(num_globals: usize, num_locals: usize) -> Self {
        Self {
            globals: (0..num_globals)
                .into_iter()
                .map(|_| VarState::Uninitialized)
                .collect(),
            locals: (0..num_locals)
                .into_iter()
                .map(|_| VarState::Uninitialized)
                .collect(),
        }
    }

    fn new_with_uninitialized_scaffolding(scaffolding: &Self) -> Self {
        Self::new_with_uninitialized_globals_and_locals(
            scaffolding.globals.len(),
            scaffolding.locals.len(),
        )
    }

    // make all the globals Unknown (used when we called a function)
    fn make_globals_unknown(&mut self, globals_vartypes: &[VarType]) {
        assert_eq!(self.globals.len(), globals_vartypes.len());
        for (varstate, vartype) in self.globals.iter_mut().zip(globals_vartypes.iter()) {
            *varstate = VarState::Unknown(*vartype);
        }
    }

    // make all globals and locals uninitialized (used when creating a new landing)
    fn make_all_uninitialized(&mut self) {
        for global in (&mut self.globals).iter_mut() {
            *global = VarState::Uninitialized;
        }
        for local in &mut self.locals {
            *local = VarState::Uninitialized;
        }
    }

    // optimized function to clone self and set all states in self to uninitialized
    // equivalent to:
    // let tmp = self.clone();
    // self.make_all_uninitialized();
    // return tmp;
    // but avoids allocation of new VarStates
    fn clone_and_make_self_all_uninitialized(&mut self) -> Self {
        Self {
            globals: self
                .globals
                .iter_mut()
                .map(|varstate| std::mem::replace(varstate, VarState::Uninitialized))
                .collect(),
            locals: self
                .locals
                .iter_mut()
                .map(|varstate| std::mem::replace(varstate, VarState::Uninitialized))
                .collect(),
        }
    }

    fn push_local(&mut self, state: VarState) {
        self.locals.push(state);
    }

    fn pop_local(&mut self) -> VarState {
        self.locals.pop().unwrap()
    }
    fn global(&self, globalidx: usize) -> &VarState {
        &self.globals[globalidx]
    }
    fn global_mut(&mut self, globalidx: usize) -> &mut VarState {
        &mut self.globals[globalidx]
    }
    fn local(&self, localidx: usize) -> &VarState {
        &self.locals[localidx]
    }
    fn local_mut(&mut self, localidx: usize) -> &mut VarState {
        &mut self.locals[localidx]
    }
}

// stores the globals and locals states that jump out of a break or return statement
// unlike States (which is cloned when we reach an if-else), we do not ever want to clone this.
struct Context<'a> {
    landings: Vec<States>,
    globals_vartypes: &'a [VarType], // the vartypes of all accessible globals (for reinitializing states to Unknown)
}

impl<'a> Context<'a> {
    // create landing context for a new function
    // there will be a single state, and
    // all globals will be Uninitialized (since we haven't encountered a return statement), and there will be no locals.
    fn new(globals_vartypes: &'a [VarType]) -> Self {
        Self {
            landings: vec![States::new_with_uninitialized_globals_and_locals(
                globals_vartypes.len(),
                0,
            )],
            globals_vartypes,
        }
    }

    // add a new landing (used at the start of a Block)
    fn push_uninitialized_landing(&mut self, scaffolding: &States) {
        self.landings
            .push(States::new_with_uninitialized_scaffolding(scaffolding));
    }

    // remove the last landing (used at the end of a Block)
    fn pop_landing(&mut self) -> States {
        self.landings.pop().unwrap()
    }

    // add a route to landings[0]
    fn update_states_for_return(&mut self, states: States) {
        // extra locals are out of scope after the return, so we remove them
        self.landings[0].merge_with_truncated_locals(states);
    }

    // add a route to the correct relative landing
    fn update_states_for_break(&mut self, states: States, num_frames: usize) {
        let index = self.landings.len() - 1 - num_frames;
        self.landings[index].merge_with_truncated_locals(states);
    }
}

/**
 * Optimises the function.
 * The return value is true if the function got changed, or false otherwise.
 */
#[must_use]
fn optimize_func(func: &mut Func, globals: &[VarType]) -> bool {
    optimize_expr(
        &mut func.expr,
        &mut States::new_with_unknown_globals_and_locals(globals, &func.params),
        &mut Context::new(globals),
    )
}

/**
 * Optimises the expr.
 * The return value is true if the expr got changed, or false otherwise.
 * @param globals_states: The states of all globals
 * @param globals_states: The states of all in-scope locals
 */
#[must_use]
fn optimize_expr(expr: &mut Expr, states: &mut States, context: &mut Context<'_>) -> bool {
    let changed = match &mut expr.kind {
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
            expected,
            create_narrow_local,
            true_expr,
            false_expr,
        } => {
            let mut changed = optimize_expr(test, states, context);
            let mut cloned_states = states.clone();
            if *create_narrow_local {
                // this is a write
                if test.vartype == Some(*expected) {
                    states.push_local(VarState::from_expr(test));
                } else {
                    states.push_local(VarState::Unknown(*expected));
                }
                changed |= optimize_expr(true_expr, states, context);
                states.pop_local();
            } else {
                changed |= optimize_expr(true_expr, states, context);
            }
            changed |= optimize_expr(false_expr, &mut cloned_states, context);
            states.merge_with(cloned_states);
            changed
        }
        ExprKind::VarName { source } => {
            // this is a read
            match source {
                TargetExpr::Global { globalidx, next } => {
                    if next.is_none() {
                        let state = states.global(*globalidx);
                        try_replace_read(expr, state)
                    } else {
                        false
                    }
                }
                TargetExpr::Local { localidx, next } => {
                    if next.is_none() {
                        let state = states.local(*localidx);
                        try_replace_read(expr, state)
                    } else {
                        false
                    }
                }
            }
        }
        ExprKind::PrimAppl { prim_inst: _, args } => args.iter_mut().fold(false, |prev, curr| {
            prev | optimize_expr(curr, states, context)
        }),
        ExprKind::Appl {
            func,
            args,
            location: _,
        } => {
            // after calling a function, all globals are unknown
            let changed = optimize_expr(func, states, context)
                | args.iter_mut().fold(false, |prev, curr| {
                    prev | optimize_expr(curr, states, context)
                });
            states.make_globals_unknown(context.globals_vartypes);
            changed
        }
        ExprKind::DirectAppl { funcidx: _, args } => {
            // after calling a function, all globals are unknown
            let changed = args.iter_mut().fold(false, |prev, curr| {
                prev | optimize_expr(curr, states, context)
            });
            states.make_globals_unknown(context.globals_vartypes);
            changed
        }
        ExprKind::Conditional {
            cond,
            true_expr,
            false_expr,
        } => {
            let mut changed = optimize_expr(cond, states, context);
            let mut cloned_states = states.clone();
            changed |= optimize_expr(true_expr, states, context);
            changed |= optimize_expr(false_expr, &mut cloned_states, context);
            states.merge_with(cloned_states);
            changed
        }
        ExprKind::Declaration {
            local: _,
            init,
            contained_expr,
        } => {
            // this is a write
            let mut changed = if let Some(init_expr) = init {
                let changed = optimize_expr(init_expr, states, context);
                states.push_local(VarState::from_expr(init_expr));
                changed
            } else {
                states.push_local(VarState::Uninitialized);
                false
            };
            changed |= optimize_expr(contained_expr, states, context);
            states.pop_local();
            changed
        }
        ExprKind::Assign { target, expr } => {
            // this is a write
            let changed = optimize_expr(expr, states, context);
            match target {
                TargetExpr::Global { globalidx, next } => {
                    *states.global_mut(*globalidx) = if next.is_none() {
                        VarState::from_expr(expr)
                    } else {
                        // structs are not identity safe, so it is always unknown (but we know the struct type)
                        VarState::Unknown(VarType::StructT {
                            typeidx: next.as_ref().unwrap().typeidx,
                        })
                    }
                }
                TargetExpr::Local { localidx, next } => {
                    *states.local_mut(*localidx) = if next.is_none() {
                        VarState::from_expr(expr)
                    } else {
                        // structs are not identity safe, so it is always unknown (but we know the struct type)
                        VarState::Unknown(VarType::StructT {
                            typeidx: next.as_ref().unwrap().typeidx,
                        })
                    };
                }
            }
            changed
        }
        ExprKind::Return { expr } => {
            let changed = optimize_expr(expr, states, context);
            context.update_states_for_return(states.clone_and_make_self_all_uninitialized());
            changed
        }
        ExprKind::Break { num_frames, expr } => {
            let changed = optimize_expr(expr, states, context);
            context.update_states_for_break(
                states.clone_and_make_self_all_uninitialized(),
                *num_frames,
            );
            changed
        }
        ExprKind::Block { expr } => {
            context.push_uninitialized_landing(states);
            let changed = optimize_expr(expr, states, context);
            states.merge_with(context.pop_landing());
            changed
        }
        ExprKind::Sequence { content } => content.iter_mut().fold(false, |prev, curr| {
            prev | optimize_expr(curr, states, context)
        }),
        ExprKind::Trap {
            code: _,
            location: _,
        } => false,
    };
    if expr.vartype.is_none() {
        // this expr is noreturn, so we set everything to uninitialized
        // this is useful e.g. when we have a noreturn expr at the end of a Block, since merging with uninitialized will keep the landing state
        states.make_all_uninitialized();
    }
    changed
}

// The return value is true if the expr got changed, or false otherwise.
fn try_replace_read(expr: &mut Expr, state: &VarState) -> bool {
    assert!(matches!(expr.kind, ExprKind::VarName { source: _ }));
    assert!(expr.vartype.is_some());
    match state {
        VarState::Unknown(vartype) => {
            let changed = expr.vartype != Some(*vartype);
            expr.vartype = Some(*vartype);
            changed
        }
        VarState::Known(expr2) => {
            *expr = expr2.clone();
            true
        }
        VarState::Uninitialized => {
            *expr = make_prim_undefined();
            true
        }
    }
}

impl VarState {
    // Determines the VarState of the given expr.  The type of the returned VarState should be no wider than expr.vartype (which must not be None).
    // If this is a Trap, we can bail out and return VarState::Unknown(VarType::Any).
    // Should use is_pure_primitive_and_identity_safe()
    fn from_expr(expr: &Expr) -> VarState {
        if is_pure_primitive_and_identity_safe(expr) {
            VarState::Known(expr.clone())
        } else if let Some(vartype) = expr.vartype {
            VarState::Unknown(vartype)
        } else {
            VarState::Unknown(VarType::Any)
        }
    }

    fn merge_with(&mut self, other: Self) {
        // merge this state with the other state, and store the result in this state
        // if the two states are different, then the state downgrades to Unknown and the widest type required.
        if let VarState::Uninitialized = self {
            // primary is none, result will be secondary
            *self = other;
        } else if let VarState::Uninitialized = other {
            // secondary is none, result will be primary
            // so do nothing
        } else {
            if let VarState::Known(pri_expr) = self {
                if let VarState::Known(sec_expr) = &other {
                    if is_same_identity_safe_pure_primitive(pri_expr, sec_expr) {
                        // two same Known states, result will be either of them

                        // for safety let's take the intersection type
                        pri_expr.vartype = intersection_type(pri_expr.vartype, sec_expr.vartype);
                        return;
                    }
                }
            }

            // two different states, we ought to take the union (which is definitely Unknown)
            let vartype = union_type_nonvoid(self.unwrap_vartype(), other.unwrap_vartype());
            *self = VarState::Unknown(vartype);
        }
    }

    // Gets the VarType of the given state, if it is Known or Unknown.  Traps if uninitialized.
    fn unwrap_vartype(&self) -> VarType {
        match self {
            VarState::Unknown(vartype) => *vartype,
            VarState::Known(expr) => expr.vartype.unwrap(),
            VarState::Uninitialized => {
                panic!("Trying to unwrap vartype of VarState::Uninitialized")
            }
        }
    }
}

// Returns true if the two varstates are both identity safe pure primitives, and they are identical
fn is_same_identity_safe_pure_primitive(first: &Expr, second: &Expr) -> bool {
    match &first.kind {
        ExprKind::PrimUndefined => {
            if let ExprKind::PrimUndefined = &second.kind {
                true
            } else {
                false
            }
        }
        ExprKind::PrimNumber { val } => {
            if let ExprKind::PrimNumber { val: val2 } = &second.kind {
                val == val2
            } else {
                false
            }
        }
        ExprKind::PrimBoolean { val } => {
            if let ExprKind::PrimBoolean { val: val2 } = &second.kind {
                val == val2
            } else {
                false
            }
        }
        ExprKind::PrimString { val } => {
            if let ExprKind::PrimString { val: val2 } = &second.kind {
                val == val2
            } else {
                false
            }
        }
        ExprKind::PrimFunc { funcidxs, closure } => {
            if let ExprKind::PrimFunc {
                funcidxs: funcidxs2,
                closure: closure2,
            } = &second.kind
            {
                funcidxs == funcidxs2 && is_same_identity_safe_pure_primitive(&closure, &closure2)
            } else {
                false
            }
        }
        _ => false,
    }
}

#[must_use]
fn make_prim_undefined() -> Expr {
    Expr {
        vartype: Some(VarType::Undefined),
        kind: ExprKind::PrimUndefined,
    }
}
