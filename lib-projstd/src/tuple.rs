// Tuple tools

use std::boxed::Box;
use std::vec::Vec;

pub trait IntoBoxedSlices<T1, T2> {
    fn into_boxed_slices(self) -> (Box<[T1]>, Box<[T2]>);
}

impl<T1, T2> IntoBoxedSlices<T1, T2> for (Vec<T1>, Vec<T2>) {
    fn into_boxed_slices(self) -> (Box<[T1]>, Box<[T2]>) {
        (self.0.into_boxed_slice(), self.1.into_boxed_slice())
    }
}
