use crate::var_conv::encode_vartype;
use std::ops::Deref;

// Manages mapping between ir global variables and wasm global variables.
// Does not include non-ir globals (e.g. stackptr)
#[derive(Default)]
pub struct GlobalVarManager {
    wasm_global_map: Vec<wasmgen::GlobalIdx>,
    global_map: Vec<usize>, // map from ir global index to wasm_global_map index
    global_types: Vec<ir::VarType>, // map from ir global index to ir param type
}

#[derive(Copy, Clone)]
pub struct GlobalVarManagerRef<'a> {
    pub wasm_global_map: &'a [wasmgen::GlobalIdx],
    pub global_map: &'a [usize],
    pub global_types: &'a [ir::VarType],
}

impl GlobalVarManager {
    pub fn deref<'a>(&'a self) -> GlobalVarManagerRef<'a> {
        GlobalVarManagerRef {
            wasm_global_map: &self.wasm_global_map,
            global_map: &self.global_map,
            global_types: &self.global_types,
        }
    }

    pub fn make_from_ir_globals(
        ir_globals: &[ir::VarType],
        wasm_module: &mut wasmgen::WasmModule,
    ) -> Self {
        let mut ret = Self::default();
        for &ir_vartype in ir_globals {
            ret.global_types.push(ir_vartype);
            ret.global_map.push(ret.wasm_global_map.len());
            let wasm_valtypes = encode_vartype(ir_vartype);
            for &wasm_valtype in wasm_valtypes {
                // todo! Support constexpr globals
                let globalidx = wasm_module.add_zeroed_global(wasm_valtype, wasmgen::Mut::Var);
                ret.wasm_global_map.push(globalidx);
            }
        }
        ret
    }
}

impl<'a> GlobalVarManagerRef<'a> {
    pub fn wasm_global_slice(&self, ir_globalidx: usize) -> &'a [wasmgen::GlobalIdx] {
        &self.wasm_global_map[self.global_map[ir_globalidx]
            ..(if ir_globalidx + 1 < self.global_map.len() {
                self.global_map[ir_globalidx + 1]
            } else {
                self.wasm_global_map.len()
            })]
    }
}

pub struct GlobalVarManagerIter<'a> {
    s: GlobalVarManagerRef<'a>,
    i: usize,
}
impl<'a> std::iter::Iterator for GlobalVarManagerIter<'a> {
    type Item = (ir::VarType, &'a [wasmgen::GlobalIdx]);
    fn next(&mut self) -> Option<Self::Item> {
        if self.i != self.s.global_types.len() {
            let ret = (
                self.s.global_types[self.i],
                self.s.wasm_global_slice(self.i),
            );
            self.i += 1;
            Some(ret)
        } else {
            None
        }
    }
}
impl<'a> std::iter::FusedIterator for GlobalVarManagerIter<'a> {}
impl<'a> std::iter::ExactSizeIterator for GlobalVarManagerIter<'a> {
    fn len(&self) -> usize {
        self.s.global_types.len() - self.i
    }
}
impl<'a> IntoIterator for GlobalVarManagerRef<'a> {
    type Item = (ir::VarType, &'a [wasmgen::GlobalIdx]);
    type IntoIter = GlobalVarManagerIter<'a>;
    fn into_iter(self) -> Self::IntoIter {
        GlobalVarManagerIter { s: self, i: 0 }
    }
}
