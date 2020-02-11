/**
 * The structs here are equivalent to those in the WebAssembly spec here:
 * https://webassembly.github.io/spec/core/index.html
 */

use std::vec::Vec;
use std::option::Option;

pub mod serialize;

pub struct WasmModule {
	type_section: TypeSection,
	import_section: ImportSection,
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

pub struct SearchableVec<T> {
	vec: Vec<T>,
	index: std::collections::BTreeMap<T, usize>,
}

pub struct TypeSection {
	content: SearchableVec<FuncType>,
}

pub struct FuncType {
	param_types: Vec<ValType>,
	result_types: Vec<ValType>,
}

pub enum ValType {
	I32,
	I64,
	F32,
	F64,
}

pub struct ImportSection {
	content: SearchableVec<Import>,
}

pub struct Import {
	module_name: String,
	entity_name: String,
	desc: ImportDesc,
}

pub enum ImportDesc {
	Func(TypeIdx),
	Table(TableType),
	Mem(MemType),
	Global(GlobalType),
}

pub struct TableType {
	elem_type: ElemType,
	limits: Limits,
}

pub enum ElemType {
	FuncRef,
}

pub enum Limits {
	Unbounded{min: u32},
	Bounded{min: u32, max: u32},
}

pub struct MemType {
	limits: Limits,
}

pub struct GlobalType {
	val_type: ValType,
	mutability: Mut,
}

pub enum Mut {
	Const,
	Var,
}

pub struct FuncSection {
	content: Vec<TypeIdx>,
}

pub struct TableSection {
	content: Vec<Table>,
}

pub struct Table {
	table_type: TableType,
}

pub struct MemSection {
	content: Vec<Mem>,
}

pub struct Mem {
	mem_type: MemType,
}

pub struct GlobalSection {
	content: Vec<Global>,
}

pub struct Global {
	global_type: GlobalType,
	init_expr: ConstExpr,
}

pub struct ConstExpr {
	bytecode: Box<[u8]>,
}

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

pub struct StartSection {
	start: Option<FuncIdx>,
}

pub struct ElemSection {
	content: Vec<Elem>,
}

pub struct Elem {
	table_idx: TableIdx,
	offset: ConstExpr,
	content: Vec<FuncIdx>,
}

pub struct CodeSection {
	content: Vec<Code>,
}

pub struct Code {
	func: Box<[u8]>, // `func` is pre-serialized by the CodeWriter.
}

pub struct DataSection {
	content: Vec<Data>,
}

pub struct Data {
	mem_idx: MemIdx,
	offset: ConstExpr,
	content: Vec<u8>,
}






pub struct TypeIdx {
	idx: u32,
}

pub struct FuncIdx {
	idx: u32,
}

pub struct TableIdx {
	idx: u32,
}

pub struct MemIdx {
	idx: u32,
}

pub struct GlobalIdx {
	idx: u32,
}

