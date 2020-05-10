// From: https://stackoverflow.com/questions/50781561/how-to-find-the-starting-offset-of-a-string-slice-of-another-string

use std::ops::Range;

pub trait SubsliceOffset {
    /**
    Returns the byte offset of an inner slice relative to an enclosing outer slice.

    Examples

    ```ignore
    let string = "a\nb\nc";
    let lines: Vec<&str> = string.lines().collect();
    assert!(string.subslice_offset_start(lines[0]) == 0); // &"a"
    assert!(string.subslice_offset_start(lines[1]) == 2); // &"b"
    assert!(string.subslice_offset_start(lines[2]) == 4); // &"c"
    ```
    */
    fn subslice_offset_range(&self, inner: &Self) -> Range<usize>;
}

impl SubsliceOffset for str {
    fn subslice_offset_range(&self, inner: &Self) -> Range<usize> {
        let self_beg = self.as_ptr() as usize;
        let Range { start, end } = Range {
            start: inner.as_ptr(),
            end: unsafe { inner.as_ptr().add(inner.len()) },
        }; // todo!: use this when stable: inner.as_ptr_range();
        Range {
            start: (start as usize) - self_beg,
            end: (end as usize) - self_beg,
        }
    }
}
