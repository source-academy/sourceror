/**
 * Traits and implementations for adding stuff to the WebAssembly module.
 */
use super::*;

#[derive(Default)]
pub struct WasmImportBuilderModule {
    type_section: TypeSection,
    import_section: ImportSection,
    num_funcs: u32,
    num_tables: u32,
    num_mems: u32,
    num_globals: u32,
}

/**
 * To make a new function:
 * 1. Call register_func(functype) to get a typeidx and funcidx for the new function
 * This typeidx/funcidx may be used in other code (e.g. call instruction).
 * 2. Create your function using `CodeBuilder::new(functype)`.
 * 3. Call commit_func(funcidx, codebuilder) when you are done appending instructions.
 * This will 'commit' your function body so it cannot be changed any more.
 * Note: You can export your function using export_func() at any time after completing step 1.
 */
impl WasmModule {
    pub fn new_builder() -> write::WasmImportBuilderModule {
        Default::default()
    }
    pub fn register_func(&mut self, functype: &FuncType) -> (TypeIdx, FuncIdx) {
        let typeidx = self.type_section.insert_copy(functype);
        let funcidx = self.func_section.push(typeidx);
        self.code_section.push(Code{func: None});
        (typeidx, funcidx)
    }
    pub fn commit_func(&mut self, funcidx: FuncIdx, code_builder: CodeBuilder) {
        let (_functype, bytes) = code_builder.build();
        self.code_section.content[funcidx.idx as usize].func = Some(bytes);
    }
    pub fn export_func(&mut self, funcidx: FuncIdx, exported_name: String) {
        self.export_section.push_func(exported_name, funcidx);
    }
}

impl WasmImportBuilderModule {
    pub fn build(self) -> WasmModule {
        WasmModule {
            type_section: self.type_section,
            import_section: self.import_section,
            func_section: FuncSection::new_with_offset(self.num_funcs),
            table_section: TableSection::new_with_offset(self.num_tables),
            mem_section: MemSection::new_with_offset(self.num_mems),
            global_section: GlobalSection::new_with_offset(self.num_globals),
            ..Default::default()
        }
    }
}

impl TypeSection {
    /*fn insert(&mut self, functype: FuncType) -> TypeIdx {
        TypeIdx {
            idx: self.content.insert(functype) as u32,
        }
    }*/
    fn insert_copy(&mut self, functype: &FuncType) -> TypeIdx {
        TypeIdx {
            idx: self.content.insert_copy(functype) as u32,
        }
    }
}

impl FuncSection {
    fn push(&mut self, typeidx: TypeIdx) -> FuncIdx {
        let funcidx = FuncIdx {
            idx: self.content.len() as u32 + self.idx_offset,
        }; // the offset is the number of imported functions, which precedes the functions defined in this module
        self.content.push(typeidx);
        funcidx
    }
    fn new_with_offset(idx_offset: u32) -> FuncSection {
        FuncSection {
            idx_offset: idx_offset,
            ..Default::default()
        }
    }
}

impl TableSection {
    fn new_with_offset(idx_offset: u32) -> TableSection {
        TableSection {
            idx_offset: idx_offset,
            ..Default::default()
        }
    }
}

impl MemSection {
    fn new_with_offset(idx_offset: u32) -> MemSection {
        MemSection {
            idx_offset: idx_offset,
            ..Default::default()
        }
    }
}

impl GlobalSection {
    fn new_with_offset(idx_offset: u32) -> GlobalSection {
        GlobalSection {
            idx_offset: idx_offset,
            ..Default::default()
        }
    }
}

impl CodeSection {
    fn push(&mut self, code: Code) {
        self.content.push(code);
    }
}

impl ExportSection {
    fn push_func(&mut self, exported_name: String, funcidx: FuncIdx) {
        self.content.push(Export {
            entity_name: exported_name,
            desc: ExportDesc::Func(funcidx),
        });
    }
}
