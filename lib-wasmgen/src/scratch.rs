use super::*;

// scratch space (additional locals) that any encoder might want
// generally, push and pop should match up within every function
// (callers should not generally push stuff without popping them in the same function)
pub struct Scratch<'a> {
    locals_builder: &'a mut LocalsManager,
    i32_buffer: Vec<LocalIdx>,
    i64_buffer: Vec<LocalIdx>,
    f32_buffer: Vec<LocalIdx>,
    f64_buffer: Vec<LocalIdx>,
    i32_idx: usize,
    i64_idx: usize,
    f32_idx: usize,
    f64_idx: usize,
}

impl<'a> Scratch<'a> {
    pub fn new(locals_builder: &'a mut LocalsManager) -> Scratch<'a> {
        Scratch {
            locals_builder: locals_builder,
            i32_buffer: Default::default(),
            i64_buffer: Default::default(),
            f32_buffer: Default::default(),
            f64_buffer: Default::default(),
            i32_idx: 0,
            i64_idx: 0,
            f32_idx: 0,
            f64_idx: 0,
        }
    }
    pub fn push_i32(&mut self) -> LocalIdx {
        Self::push_impl(
            &mut self.locals_builder,
            ValType::I32,
            &mut self.i32_buffer,
            &mut self.i32_idx,
        )
    }
    pub fn push_i64(&mut self) -> LocalIdx {
        Self::push_impl(
            &mut self.locals_builder,
            ValType::I64,
            &mut self.i64_buffer,
            &mut self.i64_idx,
        )
    }
    pub fn push_f32(&mut self) -> LocalIdx {
        Self::push_impl(
            &mut self.locals_builder,
            ValType::F32,
            &mut self.f32_buffer,
            &mut self.f32_idx,
        )
    }
    pub fn push_f64(&mut self) -> LocalIdx {
        Self::push_impl(
            &mut self.locals_builder,
            ValType::F64,
            &mut self.f64_buffer,
            &mut self.f64_idx,
        )
    }
    pub fn pop_i32(&mut self) {
        Self::pop_impl(&mut self.i32_idx)
    }
    pub fn pop_i64(&mut self) {
        Self::pop_impl(&mut self.i64_idx)
    }
    pub fn pop_f32(&mut self) {
        Self::pop_impl(&mut self.f32_idx)
    }
    pub fn pop_f64(&mut self) {
        Self::pop_impl(&mut self.f64_idx)
    }
    pub fn push(&mut self, valtype: ValType) -> LocalIdx {
        match valtype {
            ValType::I32 => self.push_i32(),
            ValType::I64 => self.push_i64(),
            ValType::F32 => self.push_f32(),
            ValType::F64 => self.push_f64(),
        }
    }
    pub fn pop(&mut self, valtype: ValType) {
        match valtype {
            ValType::I32 => self.pop_i32(),
            ValType::I64 => self.pop_i64(),
            ValType::F32 => self.pop_f32(),
            ValType::F64 => self.pop_f64(),
        }
    }
    fn push_impl(
        locals_builder: &mut LocalsManager,
        valtype: ValType,
        buffer: &mut Vec<LocalIdx>,
        idx: &mut usize,
    ) -> LocalIdx {
        if *idx == buffer.len() {
            buffer.push(locals_builder.add(valtype));
        }
        let ret: LocalIdx = buffer[*idx];
        *idx += 1;
        ret
    }
    fn pop_impl(idx: &mut usize) {
        assert!(*idx > 0);
        *idx -= 1;
    }
    pub fn with_i32<R, F: FnOnce(&mut Self, LocalIdx) -> R>(&mut self, f: F) -> R {
        let idx = self.push_i32();
        let result = f(self, idx);
        self.pop_i32();
        result
    }
    pub fn with_i64<R, F: FnOnce(&mut Self, LocalIdx) -> R>(&mut self, f: F) -> R {
        let idx = self.push_i64();
        let result = f(self, idx);
        self.pop_i64();
        result
    }
    pub fn with_f32<R, F: FnOnce(&mut Self, LocalIdx) -> R>(&mut self, f: F) -> R {
        let idx = self.push_f32();
        let result = f(self, idx);
        self.pop_f32();
        result
    }
    pub fn with_f64<R, F: FnOnce(&mut Self, LocalIdx) -> R>(&mut self, f: F) -> R {
        let idx = self.push_f64();
        let result = f(self, idx);
        self.pop_f64();
        result
    }
    pub fn with<R, F: FnOnce(&mut Self, LocalIdx) -> R>(&mut self, valtype: ValType, f: F) -> R {
        let idx = self.push(valtype);
        let result = f(self, idx);
        self.pop(valtype);
        result
    }
}
