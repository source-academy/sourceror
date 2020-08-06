use crate::FuncIdx;
use crate::VarType;
use std::ops::Deref;

pub trait Superset {
    /**
     * Returns true if self is a superset of other.
     */
    fn superset(&self, other: &Self) -> bool;
}

impl Superset for (Box<[VarType]>, FuncIdx) {
    /**
     * Returns true if self is a superset of other.
     */
    fn superset(&self, other: &Self) -> bool {
        self.0.superset(&other.0)
    }
}

impl Superset for VarType {
    /**
     * Returns true if self is a superset of other.
     */
    fn superset(&self, other: &Self) -> bool {
        *self == *other || *self == VarType::Any
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
