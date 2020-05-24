use std::collections::HashMap;
use std::hash::BuildHasher;
use std::hash::Hash;

/**
 * Extends HashMap to have undoable functionality - allows adding map entries and undoing them later, restoring the old entries (if any).
 */
pub trait UndoableHashMap<K, V> {
    type Ctx;
    type UndoCtx;

    /**
     * Adds the list of entries into the hash map.
     */
    fn add_scope(&mut self, scope: Vec<(K, V)>) -> Self::UndoCtx;

    /**
     * Remove the entries from the hash map represented by the given UndoCtx that was previously returned from add_scope.
     */
    fn remove_scope(&mut self, undo_ctx: Self::UndoCtx);
}

impl<K: Hash + Eq + Clone, V, S: BuildHasher> UndoableHashMap<K, V> for HashMap<K, V, S> {
    type Ctx = HashMap<K, V>;
    type UndoCtx = Vec<(K, Option<V>)>;

    fn add_scope(&mut self, scope: Vec<(K, V)>) -> Self::UndoCtx {
        scope
            .into_iter()
            .map(|(key, val)| {
                let key_clone = key.clone();
                (key_clone, self.insert(key, val))
            })
            .collect()
    }

    fn remove_scope(&mut self, undo_ctx: Self::UndoCtx) {
        undo_ctx.into_iter().rev().for_each(|(key, opt_val)| {
            match opt_val {
                Some(val) => {
                    // note: we forcefully unwrap because we are guaranteed that the name must exist
                    *self.get_mut(&key).unwrap() = val;
                }
                None => {
                    self.remove(&key);
                }
            }
        });
    }
}
