/**
 * Struct used for relabelling local variables.
 * It maps between an "old" and "new" id.
 * Inserted entries must be larger than all existing entries, so that the ordering is maintained.
 */
pub struct Relabeller {
    content: Vec<(usize, usize)>, // old, new
    num_old: usize,               // this is the expected old index of the next added thing
    num_new: usize,               // this is the expected new index of the next added thing
}

impl Relabeller {
    /**
     * Initialize empty.
     */
    pub fn new() -> Self {
        Self {
            content: Vec::new(),
            num_old: 0,
            num_new: 0,
        }
    }

    /**
     * Initialize with pre-added content.
     */
    pub fn new_with_content<I: Iterator<Item = (usize, usize)>>(iter: I) -> Self {
        let tmp_content: Vec<(usize, usize)> = iter.collect();
        let len = tmp_content.len();
        Self {
            content: tmp_content,
            num_old: len,
            num_new: len,
        }
    }

    /**
     * Initialize with identity mappings.
     * Usually used for function parameters.
     */
    pub fn new_with_identities<I: Iterator<Item = usize>>(iter: I) -> Self {
        let tmp_content: Vec<(usize, usize)> = iter.map(|el| (el, el)).collect();
        let len = tmp_content.len();
        Self {
            content: tmp_content,
            num_old: len,
            num_new: len,
        }
    }

    pub fn skip_old(&mut self) {
        self.num_old += 1;
    }

    pub fn unskip_old(&mut self) {
        assert!(self.num_old > 0);
        self.num_old -= 1;
    }

    pub fn with_skipped_old<R, F: FnOnce(&mut Relabeller) -> R>(&mut self, f: F) -> R {
        self.num_old += 1;
        let ret = f(self);
        self.num_old -= 1;
        ret
    }

    pub fn skip_new(&mut self) {
        self.num_new += 1;
    }

    pub fn unskip_new(&mut self) {
        assert!(self.num_new > 0);
        self.num_new -= 1;
    }

    pub fn with_skipped_new<R, F: FnOnce(&mut Relabeller) -> R>(&mut self, f: F) -> R {
        self.num_new += 1;
        let ret = f(self);
        self.num_new -= 1;
        ret
    }

    pub fn with_skipped_news<R, F: FnOnce(&mut Relabeller) -> R>(
        &mut self,
        amount: usize,
        f: F,
    ) -> R {
        self.num_new += amount;
        let ret = f(self);
        self.num_new -= amount;
        ret
    }

    /**
     * Adds a new mapping entry.
     */
    pub fn push(&mut self) -> (usize, usize) {
        let ret = (self.num_old, self.num_new);
        self.content.push(ret);
        self.num_old += 1;
        self.num_new += 1;
        ret
    }
    pub fn pop(&mut self) {
        assert!(!self.content.is_empty());
        self.num_old -= 1;
        self.num_new -= 1;
        self.content.pop();
    }
    pub fn with_entry<R, F: FnOnce(&mut Relabeller, usize, usize) -> R>(&mut self, f: F) -> R {
        let tmp_old = self.num_old;
        let tmp_new = self.num_new;
        self.content.push((tmp_old, tmp_new));
        self.num_old += 1;
        self.num_new += 1;
        let ret = f(self, tmp_old, tmp_new);
        self.num_old -= 1;
        self.num_new -= 1;
        self.content.pop();
        ret
    }

    pub fn map_old_to_new(&self, old: usize) -> Option<usize> {
        self.content
            .binary_search_by(|el| el.0.cmp(&old))
            .ok()
            .map(|idx| self.content[idx].1)
    }
    pub fn map_new_to_old(&self, new: usize) -> Option<usize> {
        self.content
            .binary_search_by(|el| el.1.cmp(&new))
            .ok()
            .map(|idx| self.content[idx].0)
    }
    pub fn len(&self) -> usize {
        self.content.len()
    }
    pub fn num_old(&self) -> usize {
        self.num_old
    }
    pub fn num_new(&self) -> usize {
        self.num_new
    }
}
