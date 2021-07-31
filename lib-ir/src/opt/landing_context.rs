use super::union_type;
use crate::VarType;

// List of current unioned types of return and break
// the content[0] is for returns (and breaks that end the function)
pub struct LandingContext {
    content: Vec<(Option<usize>, Option<VarType>)>, // pair(new index, current union) // if new index is None, it means this landing doesn't exist in the new expr
    num_new: usize,
}

impl LandingContext {
    pub fn with_new_func<R, F: FnOnce(&mut Self) -> R>(f: F) -> (R, Option<VarType>) {
        let mut ctx = Self {
            content: vec![(Some(0), None)],
            num_new: 1,
        };
        let ret = f(&mut ctx);
        (ret, ctx.content[0].1)
    }
    pub fn with_landing<R, F: FnOnce(&mut Self) -> R>(&mut self, f: F) -> (R, Option<VarType>) {
        self.content.push((Some(self.num_new), None));
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

    pub fn skip_old_landing<R, F: FnOnce(&mut Self) -> R>(&mut self, f: F) -> R {
        self.content.push((None, None));
        let ret = f(self);
        assert!(self.content.pop().unwrap().1.is_none());
        ret
    }

    // will panic if num_frames is out of range
    // returns new num_frames to use
    pub fn add_break(&mut self, num_frames: usize, vartype: VarType) -> usize {
        let idx = self.content.len() - 1 - num_frames;
        let landing_ref = &mut self.content[idx];
        let new_index = landing_ref.0.unwrap();
        landing_ref.1 = union_type(landing_ref.1, Some(vartype));
        let new_num_frames = self.num_new - 1 - new_index;
        new_num_frames
    }
    pub fn add_return(&mut self, vartype: VarType) {
        let landing_ref = &mut self.content[0];
        landing_ref.1 = union_type(landing_ref.1, Some(vartype));
    }
}
