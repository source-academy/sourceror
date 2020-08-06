use std::iter::Peekable;

/**
 * This is an iterator that identifies the last element.
 * E.g. [3,3,5,7,7] into [(false,3),(false,3),(false,5),(false,7),(true,7)]
 */

pub struct SpecialLast<I>
where
    I: Iterator,
{
    iter: Peekable<I>,
}

impl<I> SpecialLast<I>
where
    I: Iterator,
{
    fn new(iter: I) -> Self {
        SpecialLast {
            iter: iter.peekable(),
        }
    }
}

impl<I> Iterator for SpecialLast<I>
where
    I: Iterator,
{
    type Item = (bool, I::Item);

    fn next(&mut self) -> Option<Self::Item> {
        match self.iter.next() {
            // There is a value, so keep it
            // and heck the next element to see if we are last
            Some(head) => Some((self.iter.peek().is_none(), head)),
            // The inner iterator is complete, so we are also complete
            None => None,
        }
    }
}

pub trait SpecialLastAdapter: Iterator {
    fn special_last(self) -> SpecialLast<Self>
    where
        Self: Sized,
    {
        SpecialLast::new(self)
    }
}

impl<I> SpecialLastAdapter for I where I: Iterator {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn special_last() {
        assert_eq!(
            Vec::<char>::new()
                .into_iter()
                .special_last()
                .collect::<Vec::<(bool, char)>>(),
            Vec::<(bool, char)>::new()
        );
        assert_eq!(
            [8].iter()
                .cloned()
                .special_last()
                .collect::<Vec::<(bool, i32)>>(),
            [(true, 8)]
        );
        assert_eq!(
            [1, 2]
                .iter()
                .cloned()
                .special_last()
                .collect::<Vec::<(bool, i32)>>(),
            [(false, 1), (true, 2)]
        );
        assert_eq!(
            [9, 9, 1, 1, 1]
                .iter()
                .cloned()
                .special_last()
                .collect::<Vec::<(bool, i32)>>(),
            [(false, 9), (false, 9), (false, 1), (false, 1), (true, 1)]
        );
    }
}
