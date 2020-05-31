use std::borrow::Borrow;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::hash::Hash;

#[derive(Clone)]
pub struct VarCtx<K, V> {
    map: HashMap<K, V>,
}

impl<K, V> VarCtx<K, V> {
    pub fn new() -> Self {
        VarCtx {
            map: HashMap::new(),
        }
    }
    pub fn len(&self) -> usize {
        self.map.len()
    }
}

impl<K: Hash + Eq, V: Append<V>> VarCtx<K, V> {
    /**
     * Returns false if some old thing would have been eliminated.
     */
    pub fn try_coalesce(&mut self, key: K, val: V) -> bool {
        match self.map.entry(key) {
            Entry::Vacant(v) => {
                v.insert(val);
                true
            }
            Entry::Occupied(o) => o.get_mut().try_append(val),
        }
    }
    /**
     * Returns false if some old thing got eliminated.
     */
    pub fn coalesce(&mut self, key: K, val: V) -> bool {
        match self.map.entry(key) {
            Entry::Vacant(v) => {
                v.insert(val);
                true
            }
            Entry::Occupied(o) => o.get_mut().append(val),
        }
    }
}

impl<K: Hash + Eq, V: Append<V>> VarCtx<K, V> {
    /**
     * Gets the value with this key.
     */
    pub fn get<Q: Hash + Eq + ?Sized>(&mut self, k: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
    {
        self.get(k)
    }
}

pub trait Append<T> {
    /**
     * Returns false if some old thing would have been eliminated.
     */
    fn try_append(&mut self, t: T) -> bool;
    /**
     * Returns false if some old thing got eliminated.
     */
    fn append(&mut self, t: T) -> bool;
}

#[derive(Clone)]
pub enum VarValue<T, O> {
    Target(T),              // normal variables (you can get its type from the ir_program)
    Direct(OverloadSet<O>), // magic overloaded functions
}

impl<T, O> VarValue<T, O> {
    pub fn new_target(target: T) -> Self {
        VarValue::Target(target)
    }
    pub fn new_direct(overload: O) -> Self {
        VarValue::Direct(OverloadSet {
            signatures: vec![overload],
        })
    }
}

impl<T, O: Superset> Append<VarValue<T, O>> for VarValue<T, O> {
    fn try_append(&mut self, other: VarValue<T, O>) -> bool {
        match self {
            VarValue::Target(_) => false,
            VarValue::Direct(o) => match other {
                VarValue::Target(_) => false,
                VarValue::Direct(o2) => o.try_append(o2),
            },
        }
    }
    fn append(&mut self, other: VarValue<T, O>) -> bool {
        match self {
            VarValue::Target(_) => {
                *self = other;
                false
            }
            VarValue::Direct(o) => match other {
                VarValue::Target(_) => {
                    *self = other;
                    false
                }
                VarValue::Direct(o2) => o.append(o2),
            },
        }
    }
}

/**
 * Each signature must have a unique set of params, and they should be all 'useful'.
 * It is resolved in priority from back to front.
 */
#[derive(Clone)]
pub struct OverloadSet<O> {
    pub signatures: Vec<O>,
}

impl<O> OverloadSet<O> {
    pub fn from_single(o: O) -> Self {
        let ret: Vec<O> = vec![o];
        Self { signatures: ret }
    }
}

impl<O: Superset> Append<OverloadSet<O>> for OverloadSet<O> {
    /**
     * Adds this name to the current overload set.
     * Returns false if some old thing would have been eliminated.
     */
    fn try_append(&mut self, other: OverloadSet<O>) -> bool {
        unimplemented!();
    }
    /**
     * Adds this name to the current overload set.
     * Returns false if some old thing got eliminated.
     */
    fn append(&mut self, other: OverloadSet<O>) -> bool {
        unimplemented!();
    }
}

impl<O: Superset> Append<O> for OverloadSet<O> {
    /**
     * Adds this name to the current overload set.
     * Returns false if some old thing would have been eliminated.
     * The ordering a > b means that a is a superset of b.
     */
    fn try_append(&mut self, other: O) -> bool {
        for o in &self.signatures {
            if other.superset(o) {
                return false;
            }
        }
        self.signatures.push(other);
        true
    }
    /**
     * Adds this name to the current overload set.
     * Returns false if some old thing got eliminated.
     */
    fn append(&mut self, other: O) -> bool {
        unimplemented!();
    }
}

pub trait Superset {
    /**
     * Returns true if self is a superset of other.
     */
    fn superset(&self, other: &Self) -> bool;
}
