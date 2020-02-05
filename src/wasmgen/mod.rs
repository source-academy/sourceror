/**
 * The structs here are equivalent to those in the WebAssembly spec here:
 * https://webassembly.github.io/spec/core/index.html
 */

pub struct WasmModule {
	pub type_section: TypeSection,
	// TODO: more sections
}

pub trait Insert<T> {
	fn insert(value: T);
}

pub struct SearchableVec<T> {
	vec: std::vec::Vec<T>,
	index: std::collections::BTreeMap<T, usize>,
}

pub struct TypeSection {
	data: SearchableVec<FuncType>,
}

pub struct FuncType {
	// TODO
}



/**
 * Traits and implementations for serializing the WebAssembly module into bytecode.
 */

pub trait Serialize {
	fn serialize<'a, Rec: std::iter::Extend<u8> + std::iter::Extend<&'a u8>>(&self, receiver: &mut Rec);
}

impl Serialize for WasmModule {
	fn serialize<'a, Rec: std::iter::Extend<u8> + std::iter::Extend<&'a u8>>(&self, receiver: &mut Rec) {
		self.type_section.serialize(receiver);
	}
}

impl Serialize for TypeSection {
	fn serialize<'a, Rec: std::iter::Extend<u8> + std::iter::Extend<&'a u8>>(&self, receiver: &mut Rec) {
		receiver.extend(&[1u8]); // the magic value for Type Section
		serialize_section(&self.data.vec, receiver);
	}
}

impl Serialize for FuncType {
	fn serialize<'a, Rec: std::iter::Extend<u8> + std::iter::Extend<&'a u8>>(&self, receiver: &mut Rec) {
		// TODO
	}
}

fn serialize_section<'a, T: Serialize, Rec: std::iter::Extend<u8> + std::iter::Extend<&'a u8>>(vec: &std::vec::Vec::<T>, receiver: &mut Rec) {
	let mut buf = std::vec::Vec::<u8>::new();
	vec.serialize(&mut buf);
	(vec.len() as u32).serialize(receiver);
	receiver.extend(buf);
}

impl<T: Serialize> Serialize for std::vec::Vec::<T> {
	fn serialize<'a, Rec: std::iter::Extend<u8> + std::iter::Extend<&'a u8>>(&self, receiver: &mut Rec) {
		(self.len() as u32).serialize(receiver);
		for elem in self {
			elem.serialize(receiver);
		}
	}
}



impl Serialize for u32 {
	fn serialize<'a, Rec: std::iter::Extend<u8> + std::iter::Extend<&'a u8>>(&self, receiver: &mut Rec) {
		// TODO, need some LEB128 conversion
	}
}
