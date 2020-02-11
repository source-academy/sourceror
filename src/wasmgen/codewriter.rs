/**
 * Structs and implementations for encoding instructions in an expression.
 */

use super::*;
use crate::iter::SequentialCountAdapter;

pub struct ExprBuilder {
    bytecode: Vec<u8>,
}

pub struct CodeBuilder {
    locals: Vec<ValType>,
    expr: ExprBuilder,
}

impl ExprBuilder {
    pub fn build(self) -> Expr {
        Expr{ bytecode: self.bytecode.into_boxed_slice() }
	}
    fn len(&self) -> usize {
        self.bytecode.len()
	}
    fn write_to_slice(self, out: &mut [u8]) {
        out.copy_from_slice(self.bytecode.as_slice());
	}
}

impl CodeBuilder {
    pub fn build(self) -> Code {
        let mut receiver = Vec::<u8>::new();
        serialize_locals(self.locals, &mut receiver);
        let locals_len = receiver.len();
        receiver.resize_with(locals_len + self.expr.len(), Default::default);
        self.expr.write_to_slice(&mut receiver[locals_len..]);
        Code{ func: receiver.into_boxed_slice() }
	}
}

fn serialize_locals(locals: Vec<ValType>, receiver: &mut Vec<u8>) {
    // special serialization by merging adjacent same-typed locals as specified by wasm binary format
    let counted_vec = locals.into_iter().sequential_count().collect::<Vec<(ValType, usize)>>();
    counted_vec.wasm_container_serialize(receiver, |(val_type, len), receiver| {
        (*len as u32).leb_serialize(receiver);
        val_type.wasm_serialize(receiver);
    });
}


