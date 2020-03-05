// scratch space (additional locals) that any encoder might want
// generally, push and pop should match up within every function
// (callers should not generally push stuff without popping them in the same function)
pub struct Scratch<'a> {
    locals_builder: &'a mut wasmgen::LocalsManager,
    i32_buffer: Vec<wasmgen::LocalIdx>,
    i64_buffer: Vec<wasmgen::LocalIdx>,
    f32_buffer: Vec<wasmgen::LocalIdx>,
    f64_buffer: Vec<wasmgen::LocalIdx>,
    i32_idx: usize,
    i64_idx: usize,
    f32_idx: usize,
    f64_idx: usize,
}

impl<'a> Scratch<'a> {
    pub fn new(locals_builder: &'a mut wasmgen::LocalsManager) -> Scratch<'a> {
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
    pub fn push_i32(&mut self) -> wasmgen::LocalIdx {
        Self::push_impl(
            &mut self.locals_builder,
            wasmgen::ValType::I32,
            &mut self.i32_buffer,
            &mut self.i32_idx,
        )
    }
    pub fn push_i64(&mut self) -> wasmgen::LocalIdx {
        Self::push_impl(
            &mut self.locals_builder,
            wasmgen::ValType::I64,
            &mut self.i64_buffer,
            &mut self.i64_idx,
        )
    }
    pub fn push_f32(&mut self) -> wasmgen::LocalIdx {
        Self::push_impl(
            &mut self.locals_builder,
            wasmgen::ValType::F32,
            &mut self.f32_buffer,
            &mut self.f32_idx,
        )
    }
    pub fn push_f64(&mut self) -> wasmgen::LocalIdx {
        Self::push_impl(
            &mut self.locals_builder,
            wasmgen::ValType::F64,
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
    fn push_impl(
        locals_builder: &mut wasmgen::LocalsManager,
        valtype: wasmgen::ValType,
        buffer: &mut Vec<wasmgen::LocalIdx>,
        idx: &mut usize,
    ) -> wasmgen::LocalIdx {
        if *idx == buffer.len() {
            buffer.push(locals_builder.add(valtype));
        }
        let ret: wasmgen::LocalIdx = buffer[*idx];
        *idx += 1;
        ret
    }
    fn pop_impl(idx: &mut usize) {
        assert!(*idx > 0);
        *idx -= 1;
    }
}
