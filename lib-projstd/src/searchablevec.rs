/**
 * This class is like a vector, but can also be searched quickly using an additional BTreeMap.
 */
 
use std::vec::Vec;
use std::collections::HashMap;
use std::cmp::Eq;
use std::hash::Hash;

#[derive(Default)]
pub struct SearchableVec<T: Eq + Hash + Clone> {
    vec: Vec<T>,
	index: HashMap<T, usize>,
}

impl<T: Eq + Hash + Clone> SearchableVec<T> {
    pub fn new() -> SearchableVec<T> {
        SearchableVec::<T>{
            vec: Vec::<T>::new(),
            index: HashMap::<T, usize>::new(),
		}
	}
    /**
     * If no such element already exists, then inserts the given element and returns its new index.
     * Otherwise returns the index of the existing element
     */
    pub fn insert(&mut self, value: T) -> usize {
        let curr_len = self.vec.len();
        let new_idx = *(self.index.entry(value.clone()).or_insert(curr_len));
        if new_idx == curr_len {
            self.vec.push(value);
		}
        return 0;
	}
    pub fn vec(&self) -> &Vec<T> {
        &self.vec
	}
}