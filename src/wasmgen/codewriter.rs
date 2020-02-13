/**
 * Structs and implementations for encoding instructions in an expression.
 */

use super::*;
use crate::iter::SequentialCountAdapter;

#[derive(Default)]
pub struct ExprBuilder {
    bytecode: Vec<u8>,
}

pub struct LocalsManager {
    num_params: u32,
    locals: Vec<ValType>,
}

pub struct CodeBuilder {
    functype: FuncType,
    locals_builder: LocalsManager,
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

impl LocalsManager {
    pub fn add(&mut self, valtype: ValType) -> LocalIdx {
        let len = self.locals.len();
        self.locals.push(valtype);
        LocalIdx{ idx: self.num_params + (len as u32) }
	}
    pub fn param(&self, idx: u32) -> LocalIdx {
        assert!(idx < self.num_params as u32);
        LocalIdx{ idx: idx }
	}
}

impl CodeBuilder {
    pub fn new(functype: FuncType) -> CodeBuilder {
        let num_params = functype.param_types.len() as u32;
        CodeBuilder{
            functype: functype,
            locals_builder: LocalsManager{
                num_params: num_params,
                locals: Default::default(),
            },
            expr: Default::default(),
		}
	}
    pub fn build(self) -> (FuncType, Code) {
        let mut receiver = Vec::<u8>::new();
        serialize_locals(self.locals_builder.locals, &mut receiver);
        let locals_len = receiver.len();
        receiver.resize_with(locals_len + self.expr.len(), Default::default);
        self.expr.write_to_slice(&mut receiver[locals_len..]);
        (self.functype, Code{ func: receiver.into_boxed_slice() })
	}
    pub fn split(&mut self) -> (&mut LocalsManager, &mut ExprBuilder) {
        (&mut self.locals_builder, &mut self.expr)
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



enum OpCode {
    Unreachable,
    Nop,
    Block,
    Loop,
    If,
    Else,
    End,
    Br,
    BrIf,
    BrTable,
    Return,
    Call,
    CallIndirect,

    Drop,
    Select,

    LocalGet,
    LocalSet,
    LocalTee,
    GlobalGet,
    GlobalSet,

    I32Load,
    I64Load,
    F32Load,
    F64Load,
    I32Load8S,
    I32Load8U,
    I32Load16S,
    I32Load16U,
    I64Load8S,
    I64Load8U,
    I64Load16S,
    I64Load16U,
    I64Load32S,
    I64Load32U,
    I32Store,
    I64Store,
    F32Store,
    F64Store,
    I32Store8,
    I32Store16,
    I64Store8,
    I64Store16,
    I64Store32,
    MemorySize,
    MemoryGrow,

    I32Const,
    I64Const,
    F32Const,
    F64Const,
}

impl OpCode {
    pub fn value(&self) -> &'static [u8] {
        match self {
            OpCode::Unreachable => &[0x00],
            OpCode::Nop => &[0x01],
            OpCode::Block => &[0x02],
            OpCode::Loop => &[0x03],
            OpCode::If => &[0x04],
            OpCode::Else => &[0x05],
            OpCode::End => &[0x0B],
            OpCode::Br => &[0x0C],
            OpCode::BrIf => &[0x0D],
            OpCode::BrTable => &[0x0E],
            OpCode::Return => &[0x0F],
            OpCode::Call => &[0x10],
            OpCode::CallIndirect => &[0x11],
              
            OpCode::Drop => &[0x1A],
            OpCode::Select => &[0x1B],
              
            OpCode::LocalGet => &[0x20],
            OpCode::LocalSet => &[0x21],
            OpCode::LocalTee => &[0x22],
            OpCode::GlobalGet => &[0x23],
            OpCode::GlobalSet => &[0x24],
              
            OpCode::I32Load => &[0x28],
            OpCode::I64Load => &[0x29],
            OpCode::F32Load => &[0x2A],
            OpCode::F64Load => &[0x2B],
            OpCode::I32Load8S => &[0x2C],
            OpCode::I32Load8U => &[0x2D],
            OpCode::I32Load16S => &[0x2E],
            OpCode::I32Load16U => &[0x2F],
            OpCode::I64Load8S => &[0x30],
            OpCode::I64Load8U => &[0x31],
            OpCode::I64Load16S => &[0x32],
            OpCode::I64Load16U => &[0x33],
            OpCode::I64Load32S => &[0x34],
            OpCode::I64Load32U => &[0x35],
            OpCode::I32Store => &[0x36],
            OpCode::I64Store => &[0x37],
            OpCode::F32Store => &[0x38],
            OpCode::F64Store => &[0x39],
            OpCode::I32Store8 => &[0x3A],
            OpCode::I32Store16 => &[0x3B],
            OpCode::I64Store8 => &[0x3C],
            OpCode::I64Store16 => &[0x3D],
            OpCode::I64Store32 => &[0x3E],
            OpCode::MemorySize => &[0x3F],
            OpCode::MemoryGrow => &[0x40],

            OpCode::I32Const => &[0x41],
            OpCode::I64Const => &[0x42],
            OpCode::F32Const => &[0x43],
            OpCode::F64Const => &[0x44],
		}
	}
}


#[derive(Copy, Clone)]
pub struct MemArg {
    offset: u32,
    align: u32, // expressed as the logarithm of the actual alignment
}

impl MemArg {
    /**
     * Construct 1-byte aligned memarg
     */
    fn new1(offset: u32) -> MemArg {
        MemArg {offset: offset, align: 0}
	}
    /**
     * Construct 2-byte aligned memarg
     */
    fn new2(offset: u32) -> MemArg {
        MemArg {offset: offset, align: 1}
	}
    /**
     * Construct 4-byte aligned memarg
     */
    fn new4(offset: u32) -> MemArg {
        MemArg {offset: offset, align: 2}
	}
    /**
     * Construct 8-byte aligned memarg
     */
    fn new8(offset: u32) -> MemArg {
        MemArg {offset: offset, align: 3}
	}

    fn leb_serialize<Rec>(&self, receiver: &mut Rec)
		where
			for<'a> Rec: std::iter::Extend<&'a u8> {
        self.align.leb_serialize(receiver);
        self.offset.leb_serialize(receiver);
	}
}


impl ExprBuilder {
    fn append_bytes(&mut self, bytes: &[u8]) {
        self.bytecode.extend(bytes);
    }
    fn append_opcode(&mut self, op_code: OpCode) {
        self.append_bytes(op_code.value());
    }
    fn append_result_type(&mut self, result_type: &[ValType]) {
        assert!(result_type.len() <= 1, "Wasm 1.0 only allows at most one result type");
        if result_type.is_empty() {
            self.append_bytes(&[0x40]);
		} else {
            self.append_bytes(&[result_type[0].value()]);  
		}
	}
    pub fn unreachable(&mut self) {
        self.append_opcode(OpCode::Unreachable);
	}
    pub fn nop(&mut self) {
        self.append_opcode(OpCode::Nop);
	}
    pub fn block(&mut self, blocktype: &[ValType]) {
        self.append_opcode(OpCode::Block);
        self.append_result_type(blocktype);
	}
    pub fn loop_(&mut self, blocktype: &[ValType]) {
        self.append_opcode(OpCode::Loop);
        self.append_result_type(blocktype);
	}
    pub fn if_(&mut self, blocktype: &[ValType]) {
        self.append_opcode(OpCode::If);
        self.append_result_type(blocktype);
	}
    pub fn else_(&mut self) {
        self.append_opcode(OpCode::Else);
	}
    pub fn end(&mut self) {
        self.append_opcode(OpCode::End);
	}
    pub fn br(&mut self, labelidx: u32) {
        self.append_opcode(OpCode::Br);
        labelidx.leb_serialize(&mut self.bytecode);
	}
    pub fn br_if(&mut self, labelidx: u32) {
        self.append_opcode(OpCode::BrIf);
        labelidx.leb_serialize(&mut self.bytecode);
	}
    pub fn br_table(&mut self, labelidxs: &[u32], default_labelidx: u32) {
        self.append_opcode(OpCode::BrTable);
        (labelidxs.len() as u32).leb_serialize(&mut self.bytecode);
        for l in labelidxs {
              l.leb_serialize(&mut self.bytecode);
		}
        default_labelidx.leb_serialize(&mut self.bytecode);
	}
    pub fn return_(&mut self) {
        self.append_opcode(OpCode::Return);
	}
    pub fn call(&mut self, funcidx: FuncIdx) {
        self.append_opcode(OpCode::Call);
        funcidx.wasm_serialize(&mut self.bytecode);
	}
    pub fn call_indirect(&mut self, typeidx: TypeIdx, tableidx: TableIdx) {
        assert!(tableidx.idx == 0, "Wasm 1.0 only allows one table");
        self.append_opcode(OpCode::CallIndirect);
        typeidx.wasm_serialize(&mut self.bytecode);
        tableidx.wasm_serialize(&mut self.bytecode);
	}
    pub fn drop(&mut self) {
        self.append_opcode(OpCode::Drop);
	}
    pub fn select(&mut self) {
        self.append_opcode(OpCode::Select);
	}
    pub fn local_get(&mut self, localidx: LocalIdx) {
        self.append_opcode(OpCode::LocalGet);
        localidx.wasm_serialize(&mut self.bytecode);
	}
    pub fn local_set(&mut self, localidx: LocalIdx) {
        self.append_opcode(OpCode::LocalSet);
        localidx.wasm_serialize(&mut self.bytecode);
	}
    pub fn local_tee(&mut self, localidx: LocalIdx) {
        self.append_opcode(OpCode::LocalTee);
        localidx.wasm_serialize(&mut self.bytecode);
	}
    pub fn global_get(&mut self, globalidx: GlobalIdx) {
        self.append_opcode(OpCode::GlobalGet);
        globalidx.wasm_serialize(&mut self.bytecode);
	}
    pub fn global_set(&mut self, globalidx: GlobalIdx) {
        self.append_opcode(OpCode::GlobalSet);
        globalidx.wasm_serialize(&mut self.bytecode);
	}
    pub fn i32_load(&mut self, m: MemArg) {
        self.append_opcode(OpCode::I32Load);
        m.leb_serialize(&mut self.bytecode);
	}
    pub fn i64_load(&mut self, m: MemArg) {
        self.append_opcode(OpCode::I64Load);
        m.leb_serialize(&mut self.bytecode);
	}
    pub fn f32_load(&mut self, m: MemArg) {
        self.append_opcode(OpCode::F32Load);
        m.leb_serialize(&mut self.bytecode);
	}
    pub fn f64_load(&mut self, m: MemArg) {
        self.append_opcode(OpCode::F64Load);
        m.leb_serialize(&mut self.bytecode);
	}
    pub fn i32_load8_s(&mut self, m: MemArg) {
        self.append_opcode(OpCode::I32Load8S);
        m.leb_serialize(&mut self.bytecode);
	}
    pub fn i32_load8_u(&mut self, m: MemArg) {
        self.append_opcode(OpCode::I32Load8U);
        m.leb_serialize(&mut self.bytecode);
	}
    pub fn i32_load16_s(&mut self, m: MemArg) {
        self.append_opcode(OpCode::I32Load16S);
        m.leb_serialize(&mut self.bytecode);
	}
    pub fn i32_load16_u(&mut self, m: MemArg) {
        self.append_opcode(OpCode::I32Load16U);
        m.leb_serialize(&mut self.bytecode);
	}
    pub fn i64_load8_s(&mut self, m: MemArg) {
        self.append_opcode(OpCode::I64Load8S);
        m.leb_serialize(&mut self.bytecode);
	}
    pub fn i64_load8_u(&mut self, m: MemArg) {
        self.append_opcode(OpCode::I64Load8U);
        m.leb_serialize(&mut self.bytecode);
	}
    pub fn i64_load16_s(&mut self, m: MemArg) {
        self.append_opcode(OpCode::I64Load16S);
        m.leb_serialize(&mut self.bytecode);
	}
    pub fn i64_load16_u(&mut self, m: MemArg) {
        self.append_opcode(OpCode::I64Load16U);
        m.leb_serialize(&mut self.bytecode);
	}
    pub fn i64_load32_s(&mut self, m: MemArg) {
        self.append_opcode(OpCode::I64Load32S);
        m.leb_serialize(&mut self.bytecode);
	}
    pub fn i64_load32_u(&mut self, m: MemArg) {
        self.append_opcode(OpCode::I64Load32U);
        m.leb_serialize(&mut self.bytecode);
	}
    pub fn i32_store(&mut self, m: MemArg) {
        self.append_opcode(OpCode::I32Store);
        m.leb_serialize(&mut self.bytecode);
	}
    pub fn i64_store(&mut self, m: MemArg) {
        self.append_opcode(OpCode::I64Store);
        m.leb_serialize(&mut self.bytecode);
	}
    pub fn f32_store(&mut self, m: MemArg) {
        self.append_opcode(OpCode::F32Store);
        m.leb_serialize(&mut self.bytecode);
	}
    pub fn f64_store(&mut self, m: MemArg) {
        self.append_opcode(OpCode::F64Store);
        m.leb_serialize(&mut self.bytecode);
	}
    pub fn i32_store8(&mut self, m: MemArg) {
        self.append_opcode(OpCode::I32Store8);
        m.leb_serialize(&mut self.bytecode);
	}
    pub fn i32_store16(&mut self, m: MemArg) {
        self.append_opcode(OpCode::I32Store16);
        m.leb_serialize(&mut self.bytecode);
	}
    pub fn i64_store8(&mut self, m: MemArg) {
        self.append_opcode(OpCode::I64Store8);
        m.leb_serialize(&mut self.bytecode);
	}
    pub fn i64_store16(&mut self, m: MemArg) {
        self.append_opcode(OpCode::I64Store16);
        m.leb_serialize(&mut self.bytecode);
	}
    pub fn i64_store32(&mut self, m: MemArg) {
        self.append_opcode(OpCode::I64Store32);
        m.leb_serialize(&mut self.bytecode);
	}
    pub fn memory_size(&mut self, memidx: MemIdx) {
        assert!(memidx.idx == 0, "Wasm 1.0 only allows one memory");
        self.append_opcode(OpCode::MemorySize);
        memidx.wasm_serialize(&mut self.bytecode);
	}
    pub fn memory_grow(&mut self, memidx: MemIdx) {
        assert!(memidx.idx == 0, "Wasm 1.0 only allows one memory");
        self.append_opcode(OpCode::MemoryGrow);
        memidx.wasm_serialize(&mut self.bytecode);
	}
    pub fn i32_const(&mut self, val: i32) {
        self.append_opcode(OpCode::I32Const);
        val.leb_serialize(&mut self.bytecode);
	}
    pub fn i64_const(&mut self, val: i64) {
        self.append_opcode(OpCode::I64Const);
        val.leb_serialize(&mut self.bytecode);
	}
    pub fn f32_const(&mut self, val: f32) {
        self.append_opcode(OpCode::F32Const);
        val.bitwise_serialize(&mut self.bytecode);
	}
    pub fn f64_const(&mut self, val: f64) {
        self.append_opcode(OpCode::F64Const);
        val.bitwise_serialize(&mut self.bytecode);
	}
}
