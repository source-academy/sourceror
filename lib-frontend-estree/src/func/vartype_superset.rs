use crate::frontendvar::Superset;
use std::ops::Deref;

impl Superset for ir::VarType {
    /**
     * Returns true if self is a superset of other.
     */
    fn superset(&self, other: &Self) -> bool {
        *self == *other || *self == ir::VarType::Any
    }
}

impl<C: Superset> Superset for Vec<C> {
    /**
     * Returns true if self is a superset of other.
     */
    fn superset(&self, other: &Self) -> bool {
        self.deref().superset(other.deref())
    }
}

impl<C: Superset> Superset for Box<[C]> {
    /**
     * Returns true if self is a superset of other.
     */
    fn superset(&self, other: &Self) -> bool {
        self.deref().superset(other.deref())
    }
}

impl<C: Superset> Superset for [C] {
    /**
     * Returns true if self is a superset of other.
     */
    fn superset(&self, other: &Self) -> bool {
        self.len() == other.len() && self.iter().zip(other.iter()).all(|(s, o)| s.superset(o))
    }
}
