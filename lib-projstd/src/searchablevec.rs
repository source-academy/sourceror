use std::cmp::Eq;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::hash::Hash;
/**
 * This class is like a vector, but can also be searched quickly using an additional BTreeMap.
 */
use std::vec::Vec;

#[derive(Default)]
pub struct SearchableVec<T: Eq + Hash + Clone> {
    vec: Vec<T>,
    index: HashMap<T, usize>,
}

impl<T: Eq + Hash + Clone> SearchableVec<T> {
    pub fn new() -> SearchableVec<T> {
        SearchableVec::<T> {
            vec: Vec::<T>::new(),
            index: HashMap::<T, usize>::new(),
        }
    }
    /**
     * If no such element already exists, then inserts the given element and returns its new index.
     * Otherwise returns the index of the existing element
     */
    pub fn insert(&mut self, value: T) -> usize {
        match self.index.entry(value) {
            Entry::Occupied(occupied_entry) => *(occupied_entry.get()),
            Entry::Vacant(vacant_entry) => {
                let curr_len = self.vec.len();
                let value_cloned = vacant_entry.key().clone();
                vacant_entry.insert(curr_len);
                self.vec.push(value_cloned);
                curr_len
            }
        }
    }
    pub fn insert_copy(&mut self, value: &T) -> usize {
        self.insert(value.clone())
    }
    pub fn vec(&self) -> &Vec<T> {
        &self.vec
    }
}
