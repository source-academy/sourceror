/**
 * Traits and implementations for adding stuff to the WebAssembly module.
 */

use super::*;

pub struct WasmImportBuilderModule {
	type_section: TypeSection,
	import_section: ImportSection,
}

impl WasmModule {
    pub fn add_func(&mut self, code_builder: CodeBuilder) -> (TypeIdx, FuncIdx) {
        let (functype, code) = code_builder.build();
        let typeidx = self.type_section.insert(functype);
        let funcidx = self.func_section.push(typeidx);
        self.code_section.push(code);
        (typeidx, funcidx)
	}
    pub fn export_func(&mut self, funcidx: FuncIdx, exported_name: String) {
        self.export_section.push_func(exported_name, funcidx);
	}
}

impl TypeSection {
    fn insert(&mut self, functype: FuncType) -> TypeIdx {
        TypeIdx{ idx: self.content.insert(functype) as u32 }
	}
}

impl FuncSection {
    fn push(&mut self, typeidx: TypeIdx) -> FuncIdx {
        let funcidx = FuncIdx{ idx: self.content.len() as u32 };
        self.content.push(typeidx);
        funcidx
	}
}

impl CodeSection {
    fn push(&mut self, code: Code) {
        self.content.push(code);
	}
}

impl ExportSection {
    fn push_func(&mut self, exported_name: String, funcidx: FuncIdx) {
        self.content.push(Export{
             entity_name: exported_name,
             desc: ExportDesc::Func(funcidx),
		});
	}
}
