use crate::estree::VarLocId;
use std::collections::btree_map::Entry;
use std::collections::BTreeMap;

/**
 * Create a new map that uses a single variable.
 */
pub fn from_used(varlocid: VarLocId) -> BTreeMap<VarLocId, Usage> {
    let mut ret = BTreeMap::new();
    ret.insert(varlocid, Usage::UsedButNotModified);
    ret
}

/**
 * Create a new map that modifies a single variable.
 */
pub fn from_modified(varlocid: VarLocId) -> BTreeMap<VarLocId, Usage> {
    let mut ret = BTreeMap::new();
    ret.insert(varlocid, Usage::UsedAndModified);
    ret
}

/**
 * Merge two use/modify maps, where `first` is executed before `second`.
 * In order to ensure linearithmic time complexity, the smaller map is merged into the bigger map.
 */
pub fn merge_series(
    first: BTreeMap<VarLocId, Usage>,
    second: BTreeMap<VarLocId, Usage>,
) -> BTreeMap<VarLocId, Usage> {
    sized_merge_btreemap(first, second, Usage::series)
}

/**
 * Merge two use/modify maps, where either `first` or `second` will be executed (but not both).
 * In order to ensure linearithmic time complexity, the smaller map is merged into the bigger map.
 */
pub fn merge_parallel(
    first: BTreeMap<VarLocId, Usage>,
    second: BTreeMap<VarLocId, Usage>,
) -> BTreeMap<VarLocId, Usage> {
    sized_merge_btreemap(first, second, Usage::parallel)
}

/**
 * Wrap the given map in a closure, doing the necessary usage transformations to represent a closure.
 */
pub fn wrap_closure(m: BTreeMap<VarLocId, Usage>) -> BTreeMap<VarLocId, Usage> {
    transform_btreemap_values(m, Usage::closure)
}

/**
 * Wrap the given map in a loop, doing the necessary usage transformations to represent a loop.
 */
pub fn wrap_loop(m: BTreeMap<VarLocId, Usage>) -> BTreeMap<VarLocId, Usage> {
    transform_btreemap_values(m, Usage::repeat)
}

fn sized_merge_btreemap<K: Ord, V: Copy, F: Fn(V, V) -> V>(
    first: BTreeMap<K, V>,
    second: BTreeMap<K, V>,
    f: F,
) -> BTreeMap<K, V> {
    if first.len() >= second.len() {
        sized_merge_btreemap_impl(first, second, f)
    } else {
        sized_merge_btreemap_impl(second, first, |x, y| f(y, x))
    }
}

fn sized_merge_btreemap_impl<K: Ord, V: Copy, F: Fn(V, V) -> V>(
    big: BTreeMap<K, V>,
    small: BTreeMap<K, V>,
    f: F,
) -> BTreeMap<K, V> {
    let mut ret = big;
    small
        .into_iter()
        .for_each(|(key, val)| match ret.entry(key) {
            Entry::Vacant(v) => {
                v.insert(val);
            }
            Entry::Occupied(mut o) => {
                let prev: V = *o.get_mut();
                *o.get_mut() = f(prev, val);
            }
        });
    ret
}

fn transform_btreemap_values<K: Ord, V: Copy, F: Fn(V) -> V>(
    mut m: BTreeMap<K, V>,
    f: F,
) -> BTreeMap<K, V> {
    m.values_mut().for_each(|v| {
        let prev: V = *v;
        *v = f(prev);
    });
    m
}

#[derive(PartialEq, Eq, Copy, Clone, Debug)]
pub enum Usage {
    UsedButNotModified,
    UsedAndModified,
    UsedButNotModifiedWithClosedButNotModified,
    UsedAndModifiedWithClosedButNotModified,
    ClosedAndModified,
    AddressTaken,
}

impl Usage {
    /**
     * Construct the resultant of two events in series.
     * This function is associative but not commutative.
     * See the documentation for more information on variable usage detection.
     */
    #[rustfmt::skip]
    fn series(first: Usage, second: Usage) -> Usage {
        match (first, second) {
            (Usage::UsedButNotModified, Usage::UsedButNotModified) => Usage::UsedButNotModified,
            (Usage::UsedButNotModified, Usage::UsedAndModified) => Usage::UsedAndModified,
            (Usage::UsedButNotModified, Usage::UsedButNotModifiedWithClosedButNotModified) => Usage::UsedButNotModifiedWithClosedButNotModified,
            (Usage::UsedButNotModified, Usage::UsedAndModifiedWithClosedButNotModified) => Usage::UsedAndModifiedWithClosedButNotModified,
            (Usage::UsedButNotModified, Usage::ClosedAndModified) => Usage::ClosedAndModified,
            (Usage::UsedAndModified, Usage::UsedButNotModified) => Usage::UsedAndModified,
            (Usage::UsedAndModified, Usage::UsedAndModified) => Usage::UsedAndModified,
            (Usage::UsedAndModified, Usage::UsedButNotModifiedWithClosedButNotModified) => Usage::UsedAndModifiedWithClosedButNotModified,
            (Usage::UsedAndModified, Usage::UsedAndModifiedWithClosedButNotModified) => Usage::UsedAndModifiedWithClosedButNotModified,
            (Usage::UsedAndModified, Usage::ClosedAndModified) => Usage::ClosedAndModified,
            (Usage::UsedButNotModifiedWithClosedButNotModified, Usage::UsedButNotModified) => Usage::UsedButNotModifiedWithClosedButNotModified,
            (Usage::UsedButNotModifiedWithClosedButNotModified, Usage::UsedAndModified) => Usage::AddressTaken,
            (Usage::UsedButNotModifiedWithClosedButNotModified, Usage::UsedButNotModifiedWithClosedButNotModified) => Usage::UsedButNotModifiedWithClosedButNotModified,
            (Usage::UsedButNotModifiedWithClosedButNotModified, Usage::UsedAndModifiedWithClosedButNotModified) => Usage::AddressTaken,
            (Usage::UsedButNotModifiedWithClosedButNotModified, Usage::ClosedAndModified) => Usage::AddressTaken,
            (Usage::UsedAndModifiedWithClosedButNotModified, Usage::UsedButNotModified) => Usage::UsedAndModifiedWithClosedButNotModified,
            (Usage::UsedAndModifiedWithClosedButNotModified, Usage::UsedAndModified) => Usage::AddressTaken,
            (Usage::UsedAndModifiedWithClosedButNotModified, Usage::UsedButNotModifiedWithClosedButNotModified) => Usage::UsedAndModifiedWithClosedButNotModified,
            (Usage::UsedAndModifiedWithClosedButNotModified, Usage::UsedAndModifiedWithClosedButNotModified) => Usage::AddressTaken,
            (Usage::UsedAndModifiedWithClosedButNotModified, Usage::ClosedAndModified) => Usage::AddressTaken,
            (Usage::ClosedAndModified, Usage::UsedButNotModified) => Usage::AddressTaken,
            (Usage::ClosedAndModified, Usage::UsedAndModified) => Usage::AddressTaken,
            (Usage::ClosedAndModified, Usage::UsedButNotModifiedWithClosedButNotModified) => Usage::AddressTaken,
            (Usage::ClosedAndModified, Usage::UsedAndModifiedWithClosedButNotModified) => Usage::AddressTaken,
            (Usage::ClosedAndModified, Usage::ClosedAndModified) => Usage::AddressTaken,
            (_, Usage::AddressTaken) => Usage::AddressTaken,
            (Usage::AddressTaken, _) => Usage::AddressTaken,
		}
    }

    /**
     * Construct the resultant of two events in parallel (i.e. if-statement).
     * This function is commutative.
     * See the documentation for more information on variable usage detection.
     */
    #[rustfmt::skip]
    fn parallel(first: Usage, second: Usage) -> Usage {
        match (first, second) {
            (Usage::UsedButNotModified, Usage::UsedButNotModified) => Usage::UsedButNotModified,
            (Usage::UsedButNotModified, Usage::UsedAndModified) => Usage::UsedAndModified,
            (Usage::UsedButNotModified, Usage::UsedButNotModifiedWithClosedButNotModified) => Usage::UsedButNotModifiedWithClosedButNotModified,
            (Usage::UsedButNotModified, Usage::UsedAndModifiedWithClosedButNotModified) => Usage::UsedAndModifiedWithClosedButNotModified,
            (Usage::UsedButNotModified, Usage::ClosedAndModified) => Usage::ClosedAndModified,
            (Usage::UsedAndModified, Usage::UsedButNotModified) => Usage::UsedAndModified,
            (Usage::UsedAndModified, Usage::UsedAndModified) => Usage::UsedAndModified,
            (Usage::UsedAndModified, Usage::UsedButNotModifiedWithClosedButNotModified) => Usage::UsedAndModifiedWithClosedButNotModified,
            (Usage::UsedAndModified, Usage::UsedAndModifiedWithClosedButNotModified) => Usage::UsedAndModifiedWithClosedButNotModified,
            (Usage::UsedAndModified, Usage::ClosedAndModified) => Usage::ClosedAndModified,
            (Usage::UsedButNotModifiedWithClosedButNotModified, Usage::UsedButNotModified) => Usage::UsedButNotModifiedWithClosedButNotModified,
            (Usage::UsedButNotModifiedWithClosedButNotModified, Usage::UsedAndModified) => Usage::UsedAndModifiedWithClosedButNotModified,
            (Usage::UsedButNotModifiedWithClosedButNotModified, Usage::UsedButNotModifiedWithClosedButNotModified) => Usage::UsedButNotModifiedWithClosedButNotModified,
            (Usage::UsedButNotModifiedWithClosedButNotModified, Usage::UsedAndModifiedWithClosedButNotModified) => Usage::UsedAndModifiedWithClosedButNotModified,
            (Usage::UsedButNotModifiedWithClosedButNotModified, Usage::ClosedAndModified) => Usage::ClosedAndModified,
            (Usage::UsedAndModifiedWithClosedButNotModified, Usage::UsedButNotModified) => Usage::UsedAndModifiedWithClosedButNotModified,
            (Usage::UsedAndModifiedWithClosedButNotModified, Usage::UsedAndModified) => Usage::UsedAndModifiedWithClosedButNotModified,
            (Usage::UsedAndModifiedWithClosedButNotModified, Usage::UsedButNotModifiedWithClosedButNotModified) => Usage::UsedAndModifiedWithClosedButNotModified,
            (Usage::UsedAndModifiedWithClosedButNotModified, Usage::UsedAndModifiedWithClosedButNotModified) => Usage::UsedAndModifiedWithClosedButNotModified,
            (Usage::UsedAndModifiedWithClosedButNotModified, Usage::ClosedAndModified) => Usage::ClosedAndModified,
            (Usage::ClosedAndModified, Usage::UsedButNotModified) => Usage::ClosedAndModified,
            (Usage::ClosedAndModified, Usage::UsedAndModified) => Usage::ClosedAndModified,
            (Usage::ClosedAndModified, Usage::UsedButNotModifiedWithClosedButNotModified) => Usage::ClosedAndModified,
            (Usage::ClosedAndModified, Usage::UsedAndModifiedWithClosedButNotModified) => Usage::ClosedAndModified,
            (Usage::ClosedAndModified, Usage::ClosedAndModified) => Usage::ClosedAndModified,
            (_, Usage::AddressTaken) => Usage::AddressTaken,
            (Usage::AddressTaken, _) => Usage::AddressTaken,
		}
    }

    /**
     * Construct the resultant of an event from a closure.
     */
    #[rustfmt::skip]
    fn closure(orig: Usage) -> Usage {
        match orig {
            Usage::UsedButNotModified => Usage::UsedButNotModifiedWithClosedButNotModified,
            Usage::UsedAndModified => Usage::ClosedAndModified,
            Usage::UsedButNotModifiedWithClosedButNotModified => Usage::UsedButNotModifiedWithClosedButNotModified,
            Usage::UsedAndModifiedWithClosedButNotModified => Usage::ClosedAndModified,
            Usage::ClosedAndModified => Usage::ClosedAndModified,
            Usage::AddressTaken => Usage::AddressTaken,
		}
    }

    /**
     * Construct the resultant of an event from a closure.
     */
    #[rustfmt::skip]
    fn repeat(orig: Usage) -> Usage {
        match orig {
            Usage::UsedButNotModified => Usage::UsedButNotModified,
            Usage::UsedAndModified => Usage::UsedAndModified,
            Usage::UsedButNotModifiedWithClosedButNotModified => Usage::UsedButNotModifiedWithClosedButNotModified,
            Usage::UsedAndModifiedWithClosedButNotModified => Usage::AddressTaken,
            Usage::ClosedAndModified => Usage::AddressTaken,
            Usage::AddressTaken => Usage::AddressTaken,
		}
    }
}
