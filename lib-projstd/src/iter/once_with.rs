/**
 * This is an iterator that does once_with().
 */

pub struct OnceWith<F> {
    f: Option<F>,
}

impl<F> OnceWith<F> {
    fn new(f: F) -> Self {
        OnceWith { f: Some(f) }
    }
}

impl<A, F> Iterator for OnceWith<F>
where
    F: FnOnce() -> A,
{
    type Item = A;

    fn next(&mut self) -> Option<A> {
        self.f.take().map(|f| f())
    }
}

impl<A, F> ExactSizeIterator for OnceWith<F>
where
    F: FnOnce() -> A,
{
    fn len(&self) -> usize {
        1
    }
}

pub fn once_with<A, F>(f: F) -> OnceWith<F>
where
    F: FnOnce() -> A,
{
    OnceWith::new(f)
}
