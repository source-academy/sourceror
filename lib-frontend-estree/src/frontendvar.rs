use ir::superset::Superset;
use std::borrow::Borrow;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::hash::Hash;

#[derive(Clone)]
pub struct VarCtx<K, V> {
    map: HashMap<K, V>,
}

impl<K, V> Default for VarCtx<K, V> {
    fn default() -> Self {
        VarCtx {
            map: Default::default(),
        }
    }
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
            Entry::Occupied(mut o) => o.get_mut().try_append(val),
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
            Entry::Occupied(mut o) => o.get_mut().append(val),
        }
    }
}

impl<K: Hash + Eq, V> VarCtx<K, V> {
    /**
     * Gets the value with this key.
     */
    pub fn get<Q: Hash + Eq + ?Sized>(&self, k: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
    {
        self.map.get(k)
    }
}

impl<K: Hash + Eq, V: Clone> VarCtx<K, V> {
    /**
     * Returns a copy of the value that can be restored later.
     */
    pub fn save<Q: Hash + Eq + ?Sized>(&self, k: &Q) -> Option<V>
    where
        K: Borrow<Q>,
    {
        self.map.get(k).cloned()
    }
    /**
     * Restore the value from the copy.
     */
    pub fn restore<Q: Hash + Eq + ?Sized>(&mut self, k: &Q, opt_v: Option<V>)
    where
        K: Borrow<Q>,
    {
        match opt_v {
            Some(v) => {
                *self.map.get_mut(k).unwrap() = v;
            }
            None => {
                self.map.remove(k);
            }
        }
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
#[derive(Clone, Default)]
pub struct OverloadSet<O> {
    pub signatures: Vec<O>,
}

impl<O> OverloadSet<O> {
    pub fn new() -> Self {
        Self {
            signatures: Vec::new(),
        }
    }
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
        let orig_len = self.signatures.len();
        for other_o in other.signatures {
            // we just check the first `orig_len` elements because we don't need to compare elements that were from `other`
            for o in &self.signatures[0..orig_len] {
                if other_o.superset(o) {
                    self.signatures.truncate(orig_len);
                    return false;
                }
            }
            self.signatures.push(other_o);
        }
        true
    }
    /**
     * Adds this name to the current overload set.
     * Returns false if some old thing got eliminated.
     */
    fn append(&mut self, other: OverloadSet<O>) -> bool {
        other
            .signatures
            .into_iter()
            .fold(true, |prev, curr| prev & self.append(curr))
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
        let orig_len = self.signatures.len();
        self.signatures.retain(|o| !other.superset(o));
        let ret = orig_len == self.signatures.len();
        self.signatures.push(other);
        ret
    }
}
