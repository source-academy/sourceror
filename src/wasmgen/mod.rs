/**
 * The structs here are equivalent to those in the WebAssembly spec here:
 * https://webassembly.github.io/spec/core/index.html
 */

use std::vec::Vec;
use std::option::Option;
use crate::searchablevec::SearchableVec;

pub mod serialize;
pub mod write;
pub mod codewriter;
pub use serialize::*;
pub use write::*;
pub use codewriter::*;

#[derive(Default)]
pub struct WasmModule {
	type_section: TypeSection,
	import_section: ImportSection, // cannot be modified (use WasmImportBuilderModule instead), otherwise the indices will be wrong
	func_section: FuncSection,
	table_section: TableSection,
	mem_section: MemSection,
	global_section: GlobalSection,
	export_section: ExportSection,
	start_section: StartSection,
	elem_section: ElemSection,
	code_section: CodeSection,
	data_section: DataSection,
}

pub trait Insert<T> {
	fn insert(value: T);
}

pub struct TypeSection {
	content: SearchableVec<FuncType>,
}

#[derive(Eq, PartialEq, Clone, Hash)]
pub struct FuncType {
	param_types: Box<[ValType]>,
	result_types: Box<[ValType]>,
}

#[derive(Eq, PartialEq, Copy, Clone, Hash)]
pub enum ValType {
	I32,
	I64,
	F32,
	F64,
}

#[derive(Default)]
pub struct ImportSection {
	content: /*Searchable*/Vec<Import>, // TODO: module+entity name should be searchable?
}

#[derive(Eq, PartialEq, Clone, Hash)]
pub struct Import {
	module_name: String,
	entity_name: String,
	desc: ImportDesc,
}

#[derive(Eq, PartialEq, Copy, Clone, Hash)]
pub enum ImportDesc {
	Func(TypeIdx),
	Table(TableType),
	Mem(MemType),
	Global(GlobalType),
}

#[derive(Eq, PartialEq, Copy, Clone, Hash)]
pub struct TableType {
	elem_type: ElemType,
	limits: Limits,
}

#[derive(Eq, PartialEq, Copy, Clone, Hash)]
pub enum ElemType {
	FuncRef,
}

#[derive(Eq, PartialEq, Copy, Clone, Hash)]
pub enum Limits {
	Unbounded{min: u32},
	Bounded{min: u32, max: u32},
}

#[derive(Eq, PartialEq, Copy, Clone, Hash)]
pub struct MemType {
	limits: Limits,
}

#[derive(Eq, PartialEq, Copy, Clone, Hash)]
pub struct GlobalType {
	val_type: ValType,
	mutability: Mut,
}

#[derive(Eq, PartialEq, Copy, Clone, Hash)]
pub enum Mut {
	Const,
	Var,
}

#[derive(Default)]
pub struct FuncSection {
	content: Vec<TypeIdx>,
}

#[derive(Default)]
pub struct TableSection {
	content: Vec<Table>,
}

pub struct Table {
	table_type: TableType,
}

#[derive(Default)]
pub struct MemSection {
	content: Vec<Mem>,
}

pub struct Mem {
	mem_type: MemType,
}

#[derive(Default)]
pub struct GlobalSection {
	content: Vec<Global>,
}

pub struct Global {
	global_type: GlobalType,
	init_expr: Expr,
}

pub struct Expr {
	bytecode: Box<[u8]>,
}

#[derive(Default)]
pub struct ExportSection {
	content: Vec<Export>,
}

pub struct Export {
	entity_name: String,
	desc: ExportDesc,
}

pub enum ExportDesc {
	Func(FuncIdx),
	Table(TableIdx),
	Mem(MemIdx),
	Global(GlobalIdx),
}

#[derive(Default)]
pub struct StartSection {
	start: Option<FuncIdx>,
}

#[derive(Default)]
pub struct ElemSection {
	content: Vec<Elem>,
}

pub struct Elem {
	table_idx: TableIdx,
	offset: Expr,
	content: Vec<FuncIdx>,
}

#[derive(Default)]
pub struct CodeSection {
	content: Vec<Code>,
}

pub struct Code {
	func: Box<[u8]>, // `func` is pre-serialized by the CodeWriter.
}

#[derive(Default)]
pub struct DataSection {
	content: Vec<Data>,
}

pub struct Data {
	mem_idx: MemIdx,
	offset: Expr,
	content: Vec<u8>,
}






#[derive(Eq, PartialEq, Copy, Clone, Hash)]
pub struct TypeIdx {
	idx: u32,
}

#[derive(Eq, PartialEq, Copy, Clone, Hash)]
pub struct FuncIdx {
	idx: u32,
}

#[derive(Eq, PartialEq, Copy, Clone, Hash)]
pub struct TableIdx {
	idx: u32,
}

#[derive(Eq, PartialEq, Copy, Clone, Hash)]
pub struct MemIdx {
	idx: u32,
}

#[derive(Eq, PartialEq, Copy, Clone, Hash)]
pub struct GlobalIdx {
	idx: u32,
}

#[derive(Eq, PartialEq, Copy, Clone, Hash)]
pub struct LocalIdx {
	idx: u32,
}



impl ValType {
	fn value(&self) -> u8 {
		match *self {
			ValType::I32 => 0x7F,
			ValType::I64 => 0x7E,
			ValType::F32 => 0x7D,
			ValType::F64 => 0x7C,
		}
	}
}

impl FuncType {
	pub fn new(param_types: Box<[ValType]>, result_types: Box<[ValType]>) -> FuncType {
		FuncType{ param_types: param_types, result_types: result_types }
	}
}

impl Default for TypeSection {
    fn default() -> TypeSection { TypeSection{content: SearchableVec::new()} }
}
