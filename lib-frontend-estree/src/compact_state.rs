use ir;
use std::collections::hash_map::Entry;
use std::collections::HashMap;

/**
 * Updates the current var type with the other one.
 * This may or may not retain any information from the old state.
 * the return value is 'true' if the current var type got completely replaced (so it would be a duplicate variable declaration)
 */
pub trait CompactStateReceiver {
    fn receive(&mut self, other: Self) -> bool;
}

pub enum StateName {
    Operator(ir::Builtin),
    Variable(String),
}

/**
 * Non-undoable state
 */
#[derive(Clone)]
pub struct CompactState<T: CompactStateReceiver> {
    map: HashMap<String, T>,
}
impl<T: CompactStateReceiver> CompactState<T> {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }
    pub fn lookup(&self, name: &str) -> Option<&T> {
        self.map.get(name)
    }
    pub fn insert(&mut self, name: String, val: T) {
        match self.map.entry(name) {
            Entry::Occupied(o) => {
                o.get_mut().receive(val);
            }
            Entry::Vacant(v) => {
                v.insert(val);
            }
        }
    }
    // returns true if the current var type got completely replaced (so it is a duplicate var)
    pub fn unique_insert(&mut self, name: String, val: T) -> bool {
        match self.map.entry(name) {
            Entry::Occupied(o) => o.get_mut().receive(val),
            Entry::Vacant(v) => {
                v.insert(val);
                false
            }
        }
    }
}

#[derive(Clone)]
pub enum FrontendVarBase<T, O: Overload> {
    Target(T),                  // normal variables (you can get its type from the ir_program)
    Overloaded(OverloadSet<O>), // magic overloaded functions
}

pub type FrontendVar = FrontendVarBase<ir::TargetExpr, MaterializedOverload>;
pub type UnmaterializedFrontendVar = FrontendVarBase<ir::VarType, UnmaterializedOverload>;

/**
 * Each signature must have a unique set of params, and they should be all 'useful'.\
 * It is resolved in priority from back to front.
 */
#[derive(Clone)]
pub struct OverloadSet<O: Overload> {
    pub signatures: Vec<O>,
}

/**
 * Each overload must be bound to a direct application without closure!
 * So it can only access globals, not locals.
 */
#[derive(Clone)]
pub struct MaterializedOverload {
    pub params: Box<[ir::VarType]>,
    pub result: ir::VarType,
    pub funcidx: ir::FuncIdx,
    pub order: usize,
}

/**
 * Each overload must be bound to a direct application without closure!
 * So it can only access globals, not locals.
 */
#[derive(Clone)]
pub struct UnmaterializedOverload {
    pub params: Box<[ir::VarType]>,
    pub result: ir::VarType,
    pub order: usize,
}

impl<T, O: Overload> CompactStateReceiver for FrontendVarBase<T, O> {
    fn receive(&mut self, other: Self) -> bool {
        match other {
            FrontendVarBase::Any => {
                *self = other;
                true
            }
            FrontendVarBase::Overloaded(oset) => match self {
                FrontendVarBase::Any => {
                    *self = other;
                    true
                }
                FrontendVarBase::Overloaded(sset) => sset.merge_with(oset),
            },
        }
    }
}

impl<T, O: Overload> FrontendVarBase<T, O> {
    pub fn new_target(target: ir::TargetExpr) -> Self {
        FrontendVar::Target(target)
    }
    pub fn new_overload(overload: O) -> Self {
        FrontendVar::Overloaded(OverloadSet {
            signatures: vec![overload],
        })
    }
}

impl<O: Overload> OverloadSet<O> {
    fn merge_with(&mut self, other: OverloadSet<O>) -> bool {
        fn insert_val(out: &mut Vec<O>, new_o: O) -> bool {
            for o in out {
                if o.contains(&new_o) {
                    return false;
                }
            }
            out.push(new_o);
            true
        }
        let mut new_sig: Vec<O> = Vec::new();
        let self_iter = self.signatures.into_iter().rev().peekable();
        let other_iter = other.signatures.into_iter().rev().peekable();
        let mut self_preserved = false;
        while let (Some(self_val), Some(other_val)) = (self_iter.peek(), other_iter.peek()) {
            if self_val.order() > other_val.order() {
                if insert_val(&mut new_sig, self_iter.next().unwrap()) {
                    self_preserved = true;
                }
            } else {
                insert_val(&mut new_sig, other_iter.next().unwrap());
            }
        }
        while let Some(self_val) = self_iter.next() {
            if insert_val(&mut new_sig, self_val) {
                self_preserved = true;
            }
        }
        while let Some(other_val) = other_iter.next() {
            insert_val(&mut new_sig, other_val);
        }
        new_sig.reverse();
        self.signatures = new_sig;
        self_preserved
    }
}

trait Overload {
    fn params(&self) -> &[ir::VarType];
    fn order() -> usize;
    fn contains(&self, other: &Self) -> bool {
        fn is_superset(big: ir::VarType, small: ir::VarType) -> bool {
            big == small || big == ir::VarType::Any
        }
        self.params.len() == other.params.len()
            && self
                .params
                .iter()
                .zip(other.params.iter())
                .all(|(big, small)| is_superset(*big, *small))
    }
}

impl Overload for MaterializedOverload {
    fn params(&self) -> &[ir::VarType] {
        &*self.params
    }
    fn order(&self) -> usize {
        self.order
    }
}
impl Overload for UnmaterializedOverload {
    fn params(&self) -> &[ir::VarType] {
        &*self.params
    }
    fn order(&self) -> usize {
        self.order
    }
}

#[derive(Clone)]
pub enum CurrentScopeItem {
    Unmaterialized(UnmaterializedFrontendVar),
    Materialized(FrontendVar),
}

impl CompactState<CurrentScopeItem> {
    pub fn from_unmaterialized(other: CompactState<UnmaterializedFrontendVar>) -> Self {
        other
            .map
            .into_iter()
            .map(|(key, val)| (key, CurrentScopeItem::Unmaterialized(val)))
            .collect()
    }
}

impl CompactStateReceiver for CurrentScopeItem {
    fn receive(&mut self, other: Self) -> bool {
        /*match other {
            FrontendVarBase::Any => {
                *self = other;
                true
            }
            FrontendVarBase::Overloaded(oset) => match self {
                FrontendVarBase::Any => {
                    *self = other;
                    true
                }
                FrontendVarBase::Overloaded(sset) => sset.merge_with(oset),
            },
        }*/
        unimplemented!();
    }
}
