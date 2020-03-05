use std::iter::Peekable;

/**
 * This is an iterator that coalesces adjacent elements that are equal, and returns a pair (element, count).
 * E.g. [3,3,5,7,7,7,6,6,7,7] into [(3,2),(5,1),(7,3),(6,2),(7,2)].
 */

// https://stackoverflow.com/questions/32702386/iterator-adapter-that-counts-repeated-characters

pub struct SequentialCount<I>
where
    I: Iterator,
{
    iter: Peekable<I>,
}

impl<I> SequentialCount<I>
where
    I: Iterator,
{
    fn new(iter: I) -> Self {
        SequentialCount {
            iter: iter.peekable(),
        }
    }
}

impl<I> Iterator for SequentialCount<I>
where
    I: Iterator,
    I::Item: Eq,
{
    type Item = (I::Item, usize);

    fn next(&mut self) -> Option<Self::Item> {
        // Check the next value in the inner iterator
        match self.iter.next() {
            // There is a value, so keep it
            Some(head) => {
                // We've seen one value so far
                let mut count: usize = 1;
                // Check to see what the next value is without
                // actually advancing the inner iterator
                while self.iter.peek() == Some(&head) {
                    // It's the same value, so go ahead and consume it
                    self.iter.next();
                    count += 1;
                }
                // The next element doesn't match the current value
                // complete this iteration
                Some((head, count))
            }
            // The inner iterator is complete, so we are also complete
            None => None,
        }
    }
}

pub trait SequentialCountAdapter: Iterator {
    fn sequential_count(self) -> SequentialCount<Self>
    where
        Self: Sized,
    {
        SequentialCount::new(self)
    }
}

impl<I> SequentialCountAdapter for I where I: Iterator {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sequential_count() {
        assert_eq!(
            Vec::<char>::new()
                .into_iter()
                .sequential_count()
                .collect::<Vec::<(char, usize)>>(),
            Vec::<(char, usize)>::new()
        );
        assert_eq!(
            [8].iter()
                .cloned()
                .sequential_count()
                .collect::<Vec::<(i32, usize)>>(),
            [(8, 1)]
        );
        assert_eq!(
            [1, 2]
                .iter()
                .cloned()
                .sequential_count()
                .collect::<Vec::<(i32, usize)>>(),
            [(1, 1), (2, 1)]
        );
        assert_eq!(
            [1, 1, 1]
                .iter()
                .cloned()
                .sequential_count()
                .collect::<Vec::<(i32, usize)>>(),
            [(1, 3)]
        );
        assert_eq!(
            [1, 1, 1, 9]
                .iter()
                .cloned()
                .sequential_count()
                .collect::<Vec::<(i32, usize)>>(),
            [(1, 3), (9, 1)]
        );
        assert_eq!(
            [9, 1, 1, 1]
                .iter()
                .cloned()
                .sequential_count()
                .collect::<Vec::<(i32, usize)>>(),
            [(9, 1), (1, 3)]
        );
        assert_eq!(
            [9, 9, 1, 1, 1]
                .iter()
                .cloned()
                .sequential_count()
                .collect::<Vec::<(i32, usize)>>(),
            [(9, 2), (1, 3)]
        );
        assert_eq!(
            [3, 3, 5, 7, 7, 7, 6, 6, 7, 7]
                .iter()
                .cloned()
                .sequential_count()
                .collect::<Vec::<(i32, usize)>>(),
            [(3, 2), (5, 1), (7, 3), (6, 2), (7, 2)]
        );
    }
}
