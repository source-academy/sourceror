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
    // Gets the typeidx of a given type, adding it to the type section if necessary
    pub fn insert_type_into(&mut self, functype: FuncType) -> TypeIdx {
        self.type_section.insert(functype)
    }
    // Register a new function with the given type
    pub fn register_func(&mut self, functype: &FuncType) -> (TypeIdx, FuncIdx) {
        let typeidx = self.type_section.insert_copy(functype);
        let funcidx = self.func_section.push(typeidx);
        self.code_section.push(Code { func: None });
        (typeidx, funcidx)
    }
    // Commit a function that has been previously registered
    pub fn commit_func(&mut self, funcidx: FuncIdx, code_builder: CodeBuilder) {
        let (_functype, bytes) = code_builder.build();
        self.code_section.content[self.func_section.plain_index_without_offset(funcidx) as usize]
            .func = Some(bytes);
    }
    // Export a function so that the environment (i.e. JavaScript) can call it
    pub fn export_func(&mut self, funcidx: FuncIdx, exported_name: String) {
        self.export_section.push_func(exported_name, funcidx);
    }
    // Export a memory so that the environment (i.e. JavaScript) can read it
    pub fn export_mem(&mut self, memidx: MemIdx, exported_name: String) {
        self.export_section.push_mem(exported_name, memidx);
    }
    // Export a global so that the environment (i.e. JavaScript) can read from or write to it
    pub fn export_global(&mut self, globalidx: GlobalIdx, exported_name: String) {
        self.export_section.push_global(exported_name, globalidx);
    }
    pub fn add_bounded_memory(&mut self, initial_num_pages: u32, max_num_pages: u32) -> MemIdx {
        self.mem_section
            .add_bounded(initial_num_pages, max_num_pages)
    }
    pub fn add_unbounded_memory(&mut self, initial_num_pages: u32) -> MemIdx {
        self.mem_section.add_unbounded(initial_num_pages)
    }
    pub fn add_zeroed_global(&mut self, valtype: ValType, mutability: Mut) -> GlobalIdx {
        self.global_section.add(
            valtype,
            mutability,
            match valtype {
                ValType::I32 => make_init_expr_from_i32(0),
                ValType::I64 => make_init_expr_from_i64(0),
                ValType::F32 => make_init_expr_from_f32(0.0),
                ValType::F64 => make_init_expr_from_f64(0.0),
            },
        )
    }
    pub fn add_global(&mut self, valtype: ValType, mutability: Mut, init_expr: Expr) -> GlobalIdx {
        self.global_section.add(valtype, mutability, init_expr)
    }
    pub fn add_i32_global(&mut self, mutability: Mut, init_val: i32) -> GlobalIdx {
        self.add_global(ValType::I32, mutability, make_init_expr_from_i32(init_val))
    }
    pub fn add_i64_global(&mut self, mutability: Mut, init_val: i64) -> GlobalIdx {
        self.add_global(ValType::I64, mutability, make_init_expr_from_i64(init_val))
    }
    pub fn add_f32_global(&mut self, mutability: Mut, init_val: f32) -> GlobalIdx {
        self.add_global(ValType::F32, mutability, make_init_expr_from_f32(init_val))
    }
    pub fn add_f64_global(&mut self, mutability: Mut, init_val: f64) -> GlobalIdx {
        self.add_global(ValType::F64, mutability, make_init_expr_from_f64(init_val))
    }
    pub fn get_or_add_table(&mut self) -> TableIdx {
        self.table_section.get_or_add_table()
    }
    // returns the offset of the 0-th element
    pub fn reserve_table_elements(&mut self, tableidx: TableIdx, length: u32) -> u32 {
        self.table_section.increase_table_limit(tableidx, length)
    }
    // use the offset returned by `reserve_table_elements`, and must have the correct length specified earlier.
    // it is allowed to have multiple reserved but uncommitted elements
    // it is also allowed to reserve without committing, or commiting separate chunks (so we can leave some entries empty, which will automatically trap if accessed at runtime)
    pub fn commit_table_elements(
        &mut self,
        tableidx: TableIdx,
        offset: u32,
        content: Box<[FuncIdx]>,
    ) {
        self.elem_section.add(tableidx, offset, content);
    }
    pub fn add_data(&mut self, memidx: MemIdx, offset: u32, content: &[u8]) {
        self.data_section.add(memidx, offset, content);
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
    pub fn import_func(
        &mut self,
        module_name: String,
        entity_name: String,
        functype: &FuncType,
    ) -> FuncIdx {
        let typeidx = self.type_section.insert_copy(functype);
        self.import_section
            .add_func(module_name, entity_name, typeidx);
        let ret = FuncIdx {
            idx: self.num_funcs,
        };
        self.num_funcs += 1;
        ret
    }
    pub fn import_unbounded_memory(
        &mut self,
        module_name: String,
        entity_name: String,
        initial_num_pages: u32,
    ) -> MemIdx {
        self.import_section
            .add_unbounded_memory(module_name, entity_name, initial_num_pages);
        let ret = MemIdx { idx: self.num_mems };
        self.num_mems += 1;
        ret
    }
}

impl TypeSection {
    fn insert(&mut self, functype: FuncType) -> TypeIdx {
        TypeIdx {
            idx: self.content.insert(functype) as u32,
        }
    }
    fn insert_copy(&mut self, functype: &FuncType) -> TypeIdx {
        TypeIdx {
            idx: self.content.insert_copy(functype) as u32,
        }
    }
}

impl ImportSection {
    fn add_func(&mut self, module_name: String, entity_name: String, typeidx: TypeIdx) {
        self.content.push(Import {
            module_name: module_name,
            entity_name: entity_name,
            desc: ImportDesc::Func(typeidx),
        })
    }
    fn add_unbounded_memory(
        &mut self,
        module_name: String,
        entity_name: String,
        initial_num_pages: u32,
    ) {
        self.content.push(Import {
            module_name: module_name,
            entity_name: entity_name,
            desc: ImportDesc::Mem(MemType {
                limits: Limits::Unbounded {
                    min: initial_num_pages,
                },
            }),
        })
    }
}

impl FuncSection {
    fn push(&mut self, typeidx: TypeIdx) -> FuncIdx {
        let funcidx = FuncIdx {
            idx: self.idx_offset + self.content.len() as u32,
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
    fn plain_index_without_offset(&self, funcidx: FuncIdx) -> u32 {
        assert!(funcidx.idx >= self.idx_offset);
        funcidx.idx - self.idx_offset
    }
}

impl TableSection {
    fn new_with_offset(idx_offset: u32) -> TableSection {
        TableSection {
            idx_offset: idx_offset,
            ..Default::default()
        }
    }
    pub fn get_or_add_table(&mut self) -> TableIdx {
        if self.content.is_empty() {
            self.content.push(Table {
                table_type: TableType {
                    elem_type: ElemType::FuncRef,
                    limits: Limits::Bounded { min: 0, max: 0 },
                },
            });
        }
        assert!(self.content.len() == 1);
        TableIdx { idx: 0 }
    }
    // returns the old size (which is the index of the 0th element being added)
    pub fn increase_table_limit(&mut self, tableidx: TableIdx, increment: u32) -> u32 {
        let limits: &mut Limits = &mut self.content.last_mut().unwrap().table_type.limits;
        match limits {
            Limits::Unbounded { min: _ } => {
                panic!("incorrect limit for table, needs to be bounded");
            }
            Limits::Bounded { min, max } => {
                let ret = *max;
                *max += increment;
                *min = *max; // in tables, min==max.
                return ret;
            }
        }
    }
}

impl ElemSection {
    fn add(&mut self, tableidx: TableIdx, offset: u32, content: Box<[FuncIdx]>) {
        self.content.push(Elem {
            table_idx: tableidx,
            offset: make_init_expr_from_i32(offset as i32),
            content: content,
        })
    }
}

impl MemSection {
    fn new_with_offset(idx_offset: u32) -> MemSection {
        MemSection {
            idx_offset: idx_offset,
            ..Default::default()
        }
    }
    fn add_bounded(&mut self, initial_num_pages: u32, max_num_pages: u32) -> MemIdx {
        assert!(initial_num_pages <= max_num_pages);
        let memidx = MemIdx {
            idx: self.idx_offset + self.content.len() as u32,
        };
        assert!(memidx.idx == 0); // for wasm 1.0, only can have one memory.
        self.content.push(Mem {
            mem_type: MemType {
                limits: Limits::Bounded {
                    min: initial_num_pages,
                    max: max_num_pages,
                },
            },
        });
        memidx
    }
    fn add_unbounded(&mut self, initial_num_pages: u32) -> MemIdx {
        let memidx = MemIdx {
            idx: self.idx_offset + self.content.len() as u32,
        };
        assert!(memidx.idx == 0); // for wasm 1.0, only can have one memory.
        self.content.push(Mem {
            mem_type: MemType {
                limits: Limits::Unbounded {
                    min: initial_num_pages,
                },
            },
        });
        memidx
    }
}

impl GlobalSection {
    fn new_with_offset(idx_offset: u32) -> GlobalSection {
        GlobalSection {
            idx_offset: idx_offset,
            ..Default::default()
        }
    }
    fn add(&mut self, valtype: ValType, mutability: Mut, expr: Expr) -> GlobalIdx {
        let globalidx = GlobalIdx {
            idx: self.idx_offset + self.content.len() as u32,
        };
        self.content.push(Global {
            global_type: GlobalType {
                val_type: valtype,
                mutability: mutability,
            },
            init_expr: expr,
        });
        globalidx
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
    fn push_mem(&mut self, exported_name: String, memidx: MemIdx) {
        self.content.push(Export {
            entity_name: exported_name,
            desc: ExportDesc::Mem(memidx),
        });
    }
    fn push_global(&mut self, exported_name: String, globalidx: GlobalIdx) {
        self.content.push(Export {
            entity_name: exported_name,
            desc: ExportDesc::Global(globalidx),
        });
    }
}

impl DataSection {
    fn add(&mut self, memidx: MemIdx, offset: u32, content: &[u8]) {
        self.content.push(Data {
            mem_idx: memidx,
            offset: make_init_expr_from_i32(offset as i32),
            content: content.into(),
        })
    }
}

fn make_init_expr_from_i32(init_val: i32) -> Expr {
    let mut expr_builder: ExprBuilder = Default::default();
    expr_builder.i32_const(init_val);
    expr_builder.end();
    expr_builder.build()
}
fn make_init_expr_from_i64(init_val: i64) -> Expr {
    let mut expr_builder: ExprBuilder = Default::default();
    expr_builder.i64_const(init_val);
    expr_builder.end();
    expr_builder.build()
}
fn make_init_expr_from_f32(init_val: f32) -> Expr {
    let mut expr_builder: ExprBuilder = Default::default();
    expr_builder.f32_const(init_val);
    expr_builder.end();
    expr_builder.build()
}
fn make_init_expr_from_f64(init_val: f64) -> Expr {
    let mut expr_builder: ExprBuilder = Default::default();
    expr_builder.f64_const(init_val);
    expr_builder.end();
    expr_builder.build()
}
