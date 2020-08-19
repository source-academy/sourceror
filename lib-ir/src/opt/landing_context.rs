use super::union_type;
use crate::VarType;

// List of current unioned types of return and break
// the content[0] is for returns (and breaks that end the function)
pub struct LandingContext {
    content: Vec<(usize, Option<VarType>)>, // pair(new index, current union)
    num_new: usize,
}

impl LandingContext {
    pub fn for_new_func() -> Self {
        Self {
            content: vec![(0, None)],
            num_new: 1,
        }
    }
    pub fn with_landing<R, F: FnOnce(&mut Self) -> R>(&mut self, f: F) -> (R, Option<VarType>) {
        self.content.push((self.num_new, None));
        self.num_new += 1;
        let ret = f(self);
        self.num_new -= 1;
        (ret, self.content.pop().unwrap().1)
    }

    pub fn skip_new_landing<R, F: FnOnce(&mut Self) -> R>(&mut self, f: F) -> R {
        self.num_new += 1;
        let ret = f(self);
        self.num_new -= 1;
        ret
    }

    // will panic if num_frames is out of range
    // returns new num_frames to use
    pub fn add_break(&mut self, num_frames: usize, vartype: VarType) -> usize {
        let idx = self.content.len() - 1 - num_frames;
        let landing_ref = &mut self.content[idx];
        landing_ref.1 = union_type(landing_ref.1, Some(vartype));
        let new_idx = self.num_new - 1 - landing_ref.0;
        new_idx
    }
    pub fn add_return(&mut self, vartype: VarType) {
        let landing_ref = &mut self.content[0];
        landing_ref.1 = union_type(landing_ref.1, Some(vartype));
    }
}
