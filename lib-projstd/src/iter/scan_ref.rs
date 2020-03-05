/**
 * This is an iterator that is like scan(), but takes in a mutable reference of the state instead of a value.
 * This allows the caller to inspect the final value of the state.
 */

pub struct ScanRef<'a, I, St, F>
where
    I: Iterator,
{
    iter: I,
    state: &'a mut St,
    f: F,
}

impl<'a, I, St, F> ScanRef<'a, I, St, F>
where
    I: Iterator,
{
    fn new(iter: I, state: &'a mut St, f: F) -> Self {
        ScanRef {
            iter: iter,
            state: state,
            f: f,
        }
    }
}

impl<'a, B, I, St, F> Iterator for ScanRef<'a, I, St, F>
where
    I: Iterator,
    F: FnMut(&mut St, I::Item) -> Option<B>,
{
    type Item = B;

    fn next(&mut self) -> Option<Self::Item> {
        // Check the next value in the inner iterator
        match self.iter.next() {
            // There is a value
            Some(head) => (self.f)(self.state, head),
            // The inner iterator is complete, so we are also complete
            None => None,
        }
    }
}

pub trait ScanRefAdapter: Iterator {
    fn scan_ref<St, B, F>(self, state: &mut St, f: F) -> ScanRef<Self, St, F>
    where
        Self: Sized,
        F: FnMut(&mut St, Self::Item) -> Option<B>,
    {
        ScanRef::new(self, state, f)
    }
}

impl<I> ScanRefAdapter for I where I: Iterator {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scan_ref() {
        {
            let mut ct = 0;
            assert_eq!(
                Vec::<char>::new()
                    .into_iter()
                    .scan_ref(&mut ct, |st, x| Some(x))
                    .collect::<Vec<char>>(),
                Vec::<char>::new()
            );
            assert!(ct == 0);
        }
        {
            let mut ct = 0;
            assert_eq!(
                vec!(4, 5, 7, 8)
                    .into_iter()
                    .scan_ref(&mut ct, |st, x| {
                        *st += x;
                        Some(x)
                    })
                    .collect::<Vec<i32>>(),
                vec!(4, 5, 7, 8)
            );
            assert!(ct == 24);
        }
        {
            let mut ct = 0;
            assert_eq!(
                vec!(4, 5, 7, 8)
                    .into_iter()
                    .scan_ref(&mut ct, |st, x| {
                        *st += x;
                        Some(*st)
                    })
                    .collect::<Vec<i32>>(),
                vec!(4, 9, 16, 24)
            );
            assert!(ct == 24);
        }
    }
}
