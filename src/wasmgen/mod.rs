/**
 * The structs here are equivalent to those in the WebAssembly spec here:
 * https://webassembly.github.io/spec/core/index.html
 */

use std::vec::Vec;
use std::option::Option;

pub struct WasmModule {
	type_section: TypeSection,
	import_section: ImportSection,
	function_section: FuncSection,
	table_section: TableSection,
	mem_section: MemSection,
	global_section: GlobalSection,
	export_section: ExportSection,
	start_section: StartSection,
	elem_section: ElemSection,
	data_section: DataSection,
	// TODO: more sections
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
	init_expr: Expr,
}

pub struct Expr {
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
	offset: Expr,
	content: Vec<FuncIdx>,
}

pub struct DataSection {
	content: Vec<Data>,
}

pub struct Data {
	mem_idx: MemIdx,
	offset: Expr,
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



/**
 * Traits and implementations for serializing the WebAssembly module into bytecode.
 */

pub trait WasmSerialize {
	fn wasm_serialize<Rec>(&self, receiver: &mut Rec)
		where
			for<'a> Rec: std::iter::Extend<&'a u8>;
}

pub trait LebSerialize { // LEB serialization
	fn leb_serialize<Rec>(&self, receiver: &mut Rec)
		where
			for<'a> Rec: std::iter::Extend<&'a u8>;
}

pub trait LebSerialize5Byte { // special LEB serialization for i32 and u32 that will use exactly 5 bytes (for relocation purposes)
	fn leb_serialize_5_byte<'a, Rec: std::iter::Extend<u8> + std::iter::Extend<&'a u8>>(&self, receiver: &mut Rec);
}

impl WasmSerialize for WasmModule {
	fn wasm_serialize<Rec>(&self, receiver: &mut Rec)
		where
			for<'a> Rec: std::iter::Extend<&'a u8> {
		receiver.extend(&[0x00, 0x61, 0x73, 0x6D]); // magic value "\0asm"
		receiver.extend(&[0x01, 0x00, 0x00, 0x00]); // WebAssembly version 1
		self.type_section.wasm_serialize(receiver);
		self.import_section.wasm_serialize(receiver);
		self.function_section.wasm_serialize(receiver);
		self.table_section.wasm_serialize(receiver);
		self.mem_section.wasm_serialize(receiver);
		self.global_section.wasm_serialize(receiver);
		self.export_section.wasm_serialize(receiver);
		self.start_section.wasm_serialize(receiver);
		self.elem_section.wasm_serialize(receiver);
		self.data_section.wasm_serialize(receiver);
	}
}

impl WasmSerialize for TypeSection {
	fn wasm_serialize<Rec>(&self, receiver: &mut Rec)
		where
			for<'a> Rec: std::iter::Extend<&'a u8> {
		receiver.extend(&[1u8]); // the magic value for Type Section
		serialize_section(&self.content.vec, receiver);
	}
}

impl WasmSerialize for FuncType {
	fn wasm_serialize<Rec>(&self, receiver: &mut Rec)
		where
			for<'a> Rec: std::iter::Extend<&'a u8> {
		receiver.extend(&[0x60]); // magic value for FuncType
		self.param_types.wasm_serialize(receiver);
		self.result_types.wasm_serialize(receiver);
	}
}

impl WasmSerialize for ValType {
	fn wasm_serialize<Rec>(&self, receiver: &mut Rec)
		where
			for<'a> Rec: std::iter::Extend<&'a u8> {
		match *self {
			ValType::I32 => receiver.extend(&[0x7F]),
			ValType::I64 => receiver.extend(&[0x7E]),
			ValType::F32 => receiver.extend(&[0x7D]),
			ValType::F64 => receiver.extend(&[0x7C]),
		}
	}
}

fn serialize_section<T: WasmSerialize, Rec>(content: &T, receiver: &mut Rec)
	where
		for<'a> Rec: std::iter::Extend<&'a u8> {
	let mut buf = Vec::<u8>::new();
	content.wasm_serialize(&mut buf);
	(buf.len() as u32).leb_serialize(receiver);
	receiver.extend(&buf);
}





impl WasmSerialize for TypeIdx {
	fn wasm_serialize<Rec>(&self, receiver: &mut Rec)
		where
			for<'a> Rec: std::iter::Extend<&'a u8> {
		self.idx.leb_serialize(receiver);
	}
}

impl WasmSerialize for FuncIdx {
	fn wasm_serialize<Rec>(&self, receiver: &mut Rec)
		where
			for<'a> Rec: std::iter::Extend<&'a u8> {
		self.idx.leb_serialize(receiver);
	}
}

impl WasmSerialize for TableIdx {
	fn wasm_serialize<Rec>(&self, receiver: &mut Rec)
		where
			for<'a> Rec: std::iter::Extend<&'a u8> {
		self.idx.leb_serialize(receiver);
	}
}

impl WasmSerialize for MemIdx {
	fn wasm_serialize<Rec>(&self, receiver: &mut Rec)
		where
			for<'a> Rec: std::iter::Extend<&'a u8> {
		self.idx.leb_serialize(receiver);
	}
}

impl WasmSerialize for GlobalIdx {
	fn wasm_serialize<Rec>(&self, receiver: &mut Rec)
		where
			for<'a> Rec: std::iter::Extend<&'a u8> {
		self.idx.leb_serialize(receiver);
	}
}

impl WasmSerialize for ImportSection {
	fn wasm_serialize<Rec>(&self, receiver: &mut Rec)
		where
			for<'a> Rec: std::iter::Extend<&'a u8> {
		receiver.extend(&[2u8]); // the magic value for Import Section
		serialize_section(&self.content.vec, receiver);
	}
}

impl WasmSerialize for Import {
	fn wasm_serialize<Rec>(&self, receiver: &mut Rec)
		where
			for<'a> Rec: std::iter::Extend<&'a u8> {
		self.module_name.wasm_serialize(receiver);
		self.entity_name.wasm_serialize(receiver);
		self.desc.wasm_serialize(receiver);
	}
}

impl WasmSerialize for ImportDesc {
	fn wasm_serialize<Rec>(&self, receiver: &mut Rec)
		where
			for<'a> Rec: std::iter::Extend<&'a u8> {
		match self {
			ImportDesc::Func(type_idx) => {
				receiver.extend(&[0x00]);
				type_idx.wasm_serialize(receiver);
			}
			ImportDesc::Table(table_type) => {
				receiver.extend(&[0x01]);
				table_type.wasm_serialize(receiver);
			}
			ImportDesc::Mem(mem_type) => {
				receiver.extend(&[0x02]);
				mem_type.wasm_serialize(receiver);
			}
			ImportDesc::Global(global_type) => {
				receiver.extend(&[0x03]);
				global_type.wasm_serialize(receiver);
			}
		}
	}
}

impl WasmSerialize for TableType {
	fn wasm_serialize<Rec>(&self, receiver: &mut Rec)
		where
			for<'a> Rec: std::iter::Extend<&'a u8> {
		self.elem_type.wasm_serialize(receiver);
		self.limits.wasm_serialize(receiver);
	}
}


impl WasmSerialize for ElemType {
	fn wasm_serialize<Rec>(&self, receiver: &mut Rec)
		where
			for<'a> Rec: std::iter::Extend<&'a u8> {
		match *self {
			ElemType::FuncRef => receiver.extend(&[0x70]),
		}
	}
}

impl WasmSerialize for Limits {
	fn wasm_serialize<Rec>(&self, receiver: &mut Rec)
		where
			for<'a> Rec: std::iter::Extend<&'a u8> {
		match self {
			Limits::Unbounded{min} => {
				receiver.extend(&[0x00]);
				min.leb_serialize(receiver);
			}
			Limits::Bounded{min, max} => {
				receiver.extend(&[0x01]);
				min.leb_serialize(receiver);
				max.leb_serialize(receiver);
			}
		}
	}
}

impl WasmSerialize for MemType {
	fn wasm_serialize<Rec>(&self, receiver: &mut Rec)
		where
			for<'a> Rec: std::iter::Extend<&'a u8> {
		self.limits.wasm_serialize(receiver);
	}
}

impl WasmSerialize for GlobalType {
	fn wasm_serialize<Rec>(&self, receiver: &mut Rec)
		where
			for<'a> Rec: std::iter::Extend<&'a u8> {
		self.val_type.wasm_serialize(receiver);
		self.mutability.wasm_serialize(receiver);
	}
}

impl WasmSerialize for Mut {
	fn wasm_serialize<Rec>(&self, receiver: &mut Rec)
		where
			for<'a> Rec: std::iter::Extend<&'a u8> {
		match *self {
			Mut::Const => receiver.extend(&[0x00]),
			Mut::Var => receiver.extend(&[0x01]),
		}
	}
}

impl WasmSerialize for FuncSection {
	fn wasm_serialize<Rec>(&self, receiver: &mut Rec)
		where
			for<'a> Rec: std::iter::Extend<&'a u8> {
		receiver.extend(&[3u8]); // the magic value for Function Section
		serialize_section(&self.content, receiver);
	}
}

impl WasmSerialize for TableSection {
	fn wasm_serialize<Rec>(&self, receiver: &mut Rec)
		where
			for<'a> Rec: std::iter::Extend<&'a u8> {
		receiver.extend(&[4u8]); // the magic value for Table Section
		serialize_section(&self.content, receiver);
	}
}

impl WasmSerialize for Table {
	fn wasm_serialize<Rec>(&self, receiver: &mut Rec)
		where
			for<'a> Rec: std::iter::Extend<&'a u8> {
		self.table_type.wasm_serialize(receiver);
	}
}

impl WasmSerialize for MemSection {
	fn wasm_serialize<Rec>(&self, receiver: &mut Rec)
		where
			for<'a> Rec: std::iter::Extend<&'a u8> {
		receiver.extend(&[5u8]); // the magic value for Memory Section
		serialize_section(&self.content, receiver);
	}
}

impl WasmSerialize for Mem {
	fn wasm_serialize<Rec>(&self, receiver: &mut Rec)
		where
			for<'a> Rec: std::iter::Extend<&'a u8> {
		self.mem_type.wasm_serialize(receiver);
	}
}

impl WasmSerialize for GlobalSection {
	fn wasm_serialize<Rec>(&self, receiver: &mut Rec)
		where
			for<'a> Rec: std::iter::Extend<&'a u8> {
		receiver.extend(&[6u8]); // the magic value for Global Section
		serialize_section(&self.content, receiver);
	}
}

impl WasmSerialize for Global {
	fn wasm_serialize<Rec>(&self, receiver: &mut Rec)
		where
			for<'a> Rec: std::iter::Extend<&'a u8> {
		self.global_type.wasm_serialize(receiver);
		self.init_expr.wasm_serialize(receiver);
	}
}

impl WasmSerialize for Expr {
	fn wasm_serialize<Rec>(&self, receiver: &mut Rec)
		where
			for<'a> Rec: std::iter::Extend<&'a u8> {
		receiver.extend(self.bytecode.as_ref());
	}
}

impl WasmSerialize for ExportSection {
	fn wasm_serialize<Rec>(&self, receiver: &mut Rec)
		where
			for<'a> Rec: std::iter::Extend<&'a u8> {
		receiver.extend(&[7u8]); // the magic value for Export Section
		serialize_section(&self.content, receiver);
	}
}

impl WasmSerialize for Export {
	fn wasm_serialize<Rec>(&self, receiver: &mut Rec)
		where
			for<'a> Rec: std::iter::Extend<&'a u8> {
		self.entity_name.wasm_serialize(receiver);
		self.desc.wasm_serialize(receiver);
	}
}

impl WasmSerialize for ExportDesc {
	fn wasm_serialize<Rec>(&self, receiver: &mut Rec)
		where
			for<'a> Rec: std::iter::Extend<&'a u8> {
		match self {
			ExportDesc::Func(func_idx) => {
				receiver.extend(&[0x00]);
				func_idx.wasm_serialize(receiver);
			}
			ExportDesc::Table(table_idx) => {
				receiver.extend(&[0x01]);
				table_idx.wasm_serialize(receiver);
			}
			ExportDesc::Mem(mem_idx) => {
				receiver.extend(&[0x02]);
				mem_idx.wasm_serialize(receiver);
			}
			ExportDesc::Global(global_idx) => {
				receiver.extend(&[0x03]);
				global_idx.wasm_serialize(receiver);
			}
		}
	}
}

impl WasmSerialize for StartSection {
	fn wasm_serialize<Rec>(&self, receiver: &mut Rec)
		where
			for<'a> Rec: std::iter::Extend<&'a u8> {
		match &self.start {
			Some(start_idx) => {
				receiver.extend(&[8u8]); // the magic value for Start Section
				serialize_section(start_idx, receiver);
			}
			None => {} // don't generate the section at all if there is no start function
		}
	}
}

impl WasmSerialize for ElemSection {
	fn wasm_serialize<Rec>(&self, receiver: &mut Rec)
		where
			for<'a> Rec: std::iter::Extend<&'a u8> {
		receiver.extend(&[9u8]); // the magic value for Element Section
		serialize_section(&self.content, receiver);
	}
}

impl WasmSerialize for Elem {
	fn wasm_serialize<Rec>(&self, receiver: &mut Rec)
		where
			for<'a> Rec: std::iter::Extend<&'a u8> {
		self.table_idx.wasm_serialize(receiver);
		self.offset.wasm_serialize(receiver);
		self.content.wasm_serialize(receiver);
	}
}

impl WasmSerialize for DataSection {
	fn wasm_serialize<Rec>(&self, receiver: &mut Rec)
		where
			for<'a> Rec: std::iter::Extend<&'a u8> {
		receiver.extend(&[11u8]); // the magic value for Data Section
		serialize_section(&self.content, receiver);
	}
}

impl WasmSerialize for Data {
	fn wasm_serialize<Rec>(&self, receiver: &mut Rec)
		where
			for<'a> Rec: std::iter::Extend<&'a u8> {
		self.mem_idx.wasm_serialize(receiver);
		self.offset.wasm_serialize(receiver);
		receiver.extend(self.content.as_ref() as &[u8]); // note: we actually want type ascription instead of `as`, but type ascription is still experimental.
	}
}







impl<T: WasmSerialize> WasmSerialize for Vec::<T> {
	fn wasm_serialize<Rec>(&self, receiver: &mut Rec)
		where
			for<'a> Rec: std::iter::Extend<&'a u8> {
		(self.len() as u32).leb_serialize(receiver);
		for elem in self {
			elem.wasm_serialize(receiver);
		}
	}
}

impl WasmSerialize for str {
	fn wasm_serialize<Rec>(&self, receiver: &mut Rec)
		where
			for<'a> Rec: std::iter::Extend<&'a u8> {
		let u8bytes = self.as_bytes();
		(u8bytes.len() as u32).leb_serialize(receiver);
		receiver.extend(u8bytes);
	}
}






impl LebSerialize for u32 {
	fn leb_serialize<Rec>(&self, receiver: &mut Rec)
		where
			for<'a> Rec: std::iter::Extend<&'a u8> {
		// LEB128 conversion
		let mut x: u32 = *self;
		loop {
			let b: u8 = (x & 127u32) as u8;
			x >>= 7;
			if x != 0 { // still have more bytes
				receiver.extend(&[b | 128u8]); // set the 'more bytes' flag
			}
			else { // no more bytes
				receiver.extend(&[b]);
				break;
			}
		}
	}
}

impl LebSerialize for i32 {
	fn leb_serialize<Rec>(&self, receiver: &mut Rec)
		where
			for<'a> Rec: std::iter::Extend<&'a u8> {
		// LEB128 conversion
		let mut x: i32 = *self;
		loop {
			let b: u8 = (x & 127i32) as u8;
			x >>= 7; // this does sign extension (i.e. arithmetic shift right) when the left argument is signed
			if (x != 0 || b & 64u8 != 0) && (x != -1 || b & 64u8 == 0) { // still have more bytes
				// note about condition above: `b & 64u8` will become the sign bit if there are no more bytes, so we need to check if the sign bit is actually what we want
				receiver.extend(&[b | 128u8]); // set the 'more bytes' flag
			}
			else { // no more bytes
				receiver.extend(&[b]);
				break;
			}
		}
	}
}

#[cfg(test)]
mod tests {
	use crate::wasmgen::*;
	fn wasm_serializer_wrapper<T: WasmSerialize + ?Sized>(val: &T) -> Vec::<u8> {
		let mut tmp = Vec::<u8>::new();
        val.wasm_serialize(&mut tmp);
		tmp
    }
	fn leb_serializer_wrapper<T: LebSerialize>(val: T) -> Vec::<u8> {
		let mut tmp = Vec::<u8>::new();
        val.leb_serialize(&mut tmp);
		tmp
    }

    #[test]
    fn wasm_serialize_vec() {
        assert_eq!(wasm_serializer_wrapper(&Vec::<ValType>::new()), [0]);
        assert_eq!(wasm_serializer_wrapper(&vec![ValType::I32]), [1, wasm_serializer_wrapper(&ValType::I32)[0]]);
        assert_eq!(wasm_serializer_wrapper(&vec![ValType::I32, ValType::F32]), [2, wasm_serializer_wrapper(&ValType::I32)[0], wasm_serializer_wrapper(&ValType::F32)[0]]);
    }

    #[test]
    fn wasm_serialize_string() {
        assert_eq!(wasm_serializer_wrapper(""), [0]);
        assert_eq!(wasm_serializer_wrapper("a"), [1, 'a' as u8]);
        assert_eq!(wasm_serializer_wrapper("test"), [4, 't' as u8, 'e' as u8, 's' as u8, 't' as u8]);
    }

    #[test]
    fn leb_serialize_unsigned() {
        assert_eq!(leb_serializer_wrapper(0u32), [0]);
        assert_eq!(leb_serializer_wrapper(1u32), [1]);
        assert_eq!(leb_serializer_wrapper(4u32), [4]);
        assert_eq!(leb_serializer_wrapper(127u32), [127]);
        assert_eq!(leb_serializer_wrapper(128u32), [128, 1]);
        assert_eq!(leb_serializer_wrapper(255u32), [255, 1]);
        assert_eq!(leb_serializer_wrapper(256u32), [128, 2]);
    }

    #[test]
    fn leb_serialize_signed() {
        assert_eq!(leb_serializer_wrapper(0i32), [0]);
        assert_eq!(leb_serializer_wrapper(1i32), [1]);
        assert_eq!(leb_serializer_wrapper(4i32), [4]);
        assert_eq!(leb_serializer_wrapper(63i32), [63]);
        assert_eq!(leb_serializer_wrapper(64i32), [192, 0]);
        assert_eq!(leb_serializer_wrapper(127i32), [255, 0]);
        assert_eq!(leb_serializer_wrapper(128i32), [128, 1]);
        assert_eq!(leb_serializer_wrapper(255i32), [255, 1]);
        assert_eq!(leb_serializer_wrapper(256i32), [128, 2]);

        assert_eq!(leb_serializer_wrapper(-1i32), [127]);
        assert_eq!(leb_serializer_wrapper(-4i32), [124]);
        assert_eq!(leb_serializer_wrapper(-63i32), [65]);
        assert_eq!(leb_serializer_wrapper(-64i32), [64]);
        assert_eq!(leb_serializer_wrapper(-65i32), [191, 127]);
        assert_eq!(leb_serializer_wrapper(-127i32), [129, 127]);
        assert_eq!(leb_serializer_wrapper(-128i32), [128, 127]);
        assert_eq!(leb_serializer_wrapper(-129i32), [255, 126]);
        assert_eq!(leb_serializer_wrapper(-255i32), [129, 126]);
        assert_eq!(leb_serializer_wrapper(-256i32), [128, 126]);
        assert_eq!(leb_serializer_wrapper(-257i32), [255, 125]);
    }
}

