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

    I32Eqz,
    I32Eq,
    I32Ne,
    I32LtS,
    I32LtU,
    I32GtS,
    I32GtU,
    I32LeS,
    I32LeU,
    I32GeS,
    I32GeU,

    I64Eqz,
    I64Eq,
    I64Ne,
    I64LtS,
    I64LtU,
    I64GtS,
    I64GtU,
    I64LeS,
    I64LeU,
    I64GeS,
    I64GeU,

    F32Eq,
    F32Ne,
    F32Lt,
    F32Gt,
    F32Le,
    F32Ge,

    F64Eq,
    F64Ne,
    F64Lt,
    F64Gt,
    F64Le,
    F64Ge,

    I32Clz,
    I32Ctz,
    I32Popcnt,
    I32Add,
    I32Sub,
    I32Mul,
    I32DivS,
    I32DivU,
    I32RemS,
    I32RemU,
    I32And,
    I32Or,
    I32Xor,
    I32Shl,
    I32ShrS,
    I32ShrU,
    I32Rotl,
    I32Rotr,

    I64Clz,
    I64Ctz,
    I64Popcnt,
    I64Add,
    I64Sub,
    I64Mul,
    I64DivS,
    I64DivU,
    I64RemS,
    I64RemU,
    I64And,
    I64Or,
    I64Xor,
    I64Shl,
    I64ShrS,
    I64ShrU,
    I64Rotl,
    I64Rotr,

    F32Abs,
    F32Neg,
    F32Ceil,
    F32Floor,
    F32Trunc,
    F32Nearest,
    F32Sqrt,
    F32Add,
    F32Sub,
    F32Mul,
    F32Div,
    F32Min,
    F32Max,
    F32Copysign,

    F64Abs,
    F64Neg,
    F64Ceil,
    F64Floor,
    F64Trunc,
    F64Nearest,
    F64Sqrt,
    F64Add,
    F64Sub,
    F64Mul,
    F64Div,
    F64Min,
    F64Max,
    F64Copysign,

    I32WrapI64,
    I32TruncF32S,
    I32TruncF32U,
    I32TruncF64S,
    I32TruncF64U,
    I64ExtendI32S,
    I64ExtendI32U,
    I64TruncF32S,
    I64TruncF32U,
    I64TruncF64S,
    I64TruncF64U,
    F32ConvertI32S,
    F32ConvertI32U,
    F32ConvertI64S,
    F32ConvertI64U,
    F32DemoteF64,
    F64ConvertI32S,
    F64ConvertI32U,
    F64ConvertI64S,
    F64ConvertI64U,
    F64PromoteF32,
    I32ReinterpretF32,
    I64ReinterpretF64,
    F32ReinterpretI32,
    F64ReinterpretI64,
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

            OpCode::I32Eqz => &[0x45],
            OpCode::I32Eq => &[0x46],
            OpCode::I32Ne => &[0x47],
            OpCode::I32LtS => &[0x48],
            OpCode::I32LtU => &[0x49],
            OpCode::I32GtS => &[0x4A],
            OpCode::I32GtU => &[0x4B],
            OpCode::I32LeS => &[0x4C],
            OpCode::I32LeU => &[0x4D],
            OpCode::I32GeS => &[0x4E],
            OpCode::I32GeU => &[0x4F],

            OpCode::I64Eqz => &[0x50],
            OpCode::I64Eq => &[0x51],
            OpCode::I64Ne => &[0x52],
            OpCode::I64LtS => &[0x53],
            OpCode::I64LtU => &[0x54],
            OpCode::I64GtS => &[0x55],
            OpCode::I64GtU => &[0x56],
            OpCode::I64LeS => &[0x57],
            OpCode::I64LeU => &[0x58],
            OpCode::I64GeS => &[0x59],
            OpCode::I64GeU => &[0x5A],

            OpCode::F32Eq => &[0x5B],
            OpCode::F32Ne => &[0x5C],
            OpCode::F32Lt => &[0x5D],
            OpCode::F32Gt => &[0x5E],
            OpCode::F32Le => &[0x5F],
            OpCode::F32Ge => &[0x60],

            OpCode::F64Eq => &[0x61],
            OpCode::F64Ne => &[0x62],
            OpCode::F64Lt => &[0x63],
            OpCode::F64Gt => &[0x64],
            OpCode::F64Le => &[0x65],
            OpCode::F64Ge => &[0x66],

            OpCode::I32Clz => &[0x67],
            OpCode::I32Ctz => &[0x68],
            OpCode::I32Popcnt => &[0x69],
            OpCode::I32Add => &[0x6A],
            OpCode::I32Sub => &[0x6B],
            OpCode::I32Mul => &[0x6C],
            OpCode::I32DivS => &[0x6D],
            OpCode::I32DivU => &[0x6E],
            OpCode::I32RemS => &[0x6F],
            OpCode::I32RemU => &[0x70],
            OpCode::I32And => &[0x71],
            OpCode::I32Or => &[0x72],
            OpCode::I32Xor => &[0x73],
            OpCode::I32Shl => &[0x74],
            OpCode::I32ShrS => &[0x75],
            OpCode::I32ShrU => &[0x76],
            OpCode::I32Rotl => &[0x77],
            OpCode::I32Rotr => &[0x78],

            OpCode::I64Clz => &[0x79],
            OpCode::I64Ctz => &[0x7A],
            OpCode::I64Popcnt => &[0x7B],
            OpCode::I64Add => &[0x7C],
            OpCode::I64Sub => &[0x7D],
            OpCode::I64Mul => &[0x7E],
            OpCode::I64DivS => &[0x7F],
            OpCode::I64DivU => &[0x80],
            OpCode::I64RemS => &[0x81],
            OpCode::I64RemU => &[0x82],
            OpCode::I64And => &[0x83],
            OpCode::I64Or => &[0x84],
            OpCode::I64Xor => &[0x85],
            OpCode::I64Shl => &[0x86],
            OpCode::I64ShrS => &[0x87],
            OpCode::I64ShrU => &[0x88],
            OpCode::I64Rotl => &[0x89],
            OpCode::I64Rotr => &[0x8A],

            OpCode::F32Abs => &[0x8B],
            OpCode::F32Neg => &[0x8C],
            OpCode::F32Ceil => &[0x8D],
            OpCode::F32Floor => &[0x8E],
            OpCode::F32Trunc => &[0x8F],
            OpCode::F32Nearest => &[0x90],
            OpCode::F32Sqrt => &[0x91],
            OpCode::F32Add => &[0x92],
            OpCode::F32Sub => &[0x93],
            OpCode::F32Mul => &[0x94],
            OpCode::F32Div => &[0x95],
            OpCode::F32Min => &[0x96],
            OpCode::F32Max => &[0x97],
            OpCode::F32Copysign => &[0x98],

            OpCode::F64Abs => &[0x99],
            OpCode::F64Neg => &[0x9A],
            OpCode::F64Ceil => &[0x9B],
            OpCode::F64Floor => &[0x9C],
            OpCode::F64Trunc => &[0x9D],
            OpCode::F64Nearest => &[0x9E],
            OpCode::F64Sqrt => &[0x9F],
            OpCode::F64Add => &[0xA0],
            OpCode::F64Sub => &[0xA1],
            OpCode::F64Mul => &[0xA2],
            OpCode::F64Div => &[0xA3],
            OpCode::F64Min => &[0xA4],
            OpCode::F64Max => &[0xA5],
            OpCode::F64Copysign => &[0xA6],

            OpCode::I32WrapI64 => &[0xA7],
            OpCode::I32TruncF32S => &[0xA8],
            OpCode::I32TruncF32U => &[0xA9],
            OpCode::I32TruncF64S => &[0xAA],
            OpCode::I32TruncF64U => &[0xAB],
            OpCode::I64ExtendI32S => &[0xAC],
            OpCode::I64ExtendI32U => &[0xAD],
            OpCode::I64TruncF32S => &[0xAE],
            OpCode::I64TruncF32U => &[0xAF],
            OpCode::I64TruncF64S => &[0xB0],
            OpCode::I64TruncF64U => &[0xB1],
            OpCode::F32ConvertI32S => &[0xB2],
            OpCode::F32ConvertI32U => &[0xB3],
            OpCode::F32ConvertI64S => &[0xB4],
            OpCode::F32ConvertI64U => &[0xB5],
            OpCode::F32DemoteF64 => &[0xB6],
            OpCode::F64ConvertI32S => &[0xB7],
            OpCode::F64ConvertI32U => &[0xB8],
            OpCode::F64ConvertI64S => &[0xB9],
            OpCode::F64ConvertI64U => &[0xBA],
            OpCode::F64PromoteF32 => &[0xBB],
            OpCode::I32ReinterpretF32 => &[0xBC],
            OpCode::I64ReinterpretF64 => &[0xBD],
            OpCode::F32ReinterpretI32 => &[0xBE],
            OpCode::F64ReinterpretI64 => &[0xBF],
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
    pub fn i32_eqz(&mut self) {
        self.append_opcode(OpCode::I32Eqz);
	}
    pub fn i32_eq(&mut self) {
        self.append_opcode(OpCode::I32Eq);
	}
    pub fn i32_ne(&mut self) {
        self.append_opcode(OpCode::I32Ne);
	}
    pub fn i32_lt_s(&mut self) {
        self.append_opcode(OpCode::I32LtS);
	}
    pub fn i32_lt_u(&mut self) {
        self.append_opcode(OpCode::I32LtU);
	}
    pub fn i32_gt_s(&mut self) {
        self.append_opcode(OpCode::I32GtS);
	}
    pub fn i32_gt_u(&mut self) {
        self.append_opcode(OpCode::I32GtU);
	}
    pub fn i32_le_s(&mut self) {
        self.append_opcode(OpCode::I32LeS);
	}
    pub fn i32_le_u(&mut self) {
        self.append_opcode(OpCode::I32LeU);
	}
    pub fn i32_ge_s(&mut self) {
        self.append_opcode(OpCode::I32GeS);
	}
    pub fn i32_ge_u(&mut self) {
        self.append_opcode(OpCode::I32GeU);
	}
    pub fn i64_eqz(&mut self) {
        self.append_opcode(OpCode::I64Eqz);
	}
    pub fn i64_eq(&mut self) {
        self.append_opcode(OpCode::I64Eq);
	}
    pub fn i64_ne(&mut self) {
        self.append_opcode(OpCode::I64Ne);
	}
    pub fn i64_lt_s(&mut self) {
        self.append_opcode(OpCode::I64LtS);
	}
    pub fn i64_lt_u(&mut self) {
        self.append_opcode(OpCode::I64LtU);
	}
    pub fn i64_gt_s(&mut self) {
        self.append_opcode(OpCode::I64GtS);
	}
    pub fn i64_gt_u(&mut self) {
        self.append_opcode(OpCode::I64GtU);
	}
    pub fn i64_le_s(&mut self) {
        self.append_opcode(OpCode::I64LeS);
	}
    pub fn i64_le_u(&mut self) {
        self.append_opcode(OpCode::I64LeU);
	}
    pub fn i64_ge_s(&mut self) {
        self.append_opcode(OpCode::I64GeS);
	}
    pub fn i64_ge_u(&mut self) {
        self.append_opcode(OpCode::I64GeU);
	}
    pub fn f32_eq(&mut self) {
        self.append_opcode(OpCode::F32Eq);
	}
    pub fn f32_ne(&mut self) {
        self.append_opcode(OpCode::F32Ne);
	}
    pub fn f32_lt(&mut self) {
        self.append_opcode(OpCode::F32Lt);
	}
    pub fn f32_gt(&mut self) {
        self.append_opcode(OpCode::F32Gt);
	}
    pub fn f32_le(&mut self) {
        self.append_opcode(OpCode::F32Le);
	}
    pub fn f32_ge(&mut self) {
        self.append_opcode(OpCode::F32Ge);
	}
    pub fn f64_eq(&mut self) {
        self.append_opcode(OpCode::F64Eq);
	}
    pub fn f64_ne(&mut self) {
        self.append_opcode(OpCode::F64Ne);
	}
    pub fn f64_lt(&mut self) {
        self.append_opcode(OpCode::F64Lt);
	}
    pub fn f64_gt(&mut self) {
        self.append_opcode(OpCode::F64Gt);
	}
    pub fn f64_le(&mut self) {
        self.append_opcode(OpCode::F64Le);
	}
    pub fn f64_ge(&mut self) {
        self.append_opcode(OpCode::F64Ge);
	}
    pub fn i32_clz(&mut self) {
        self.append_opcode(OpCode::I32Clz);
	}
    pub fn i32_ctz(&mut self) {
        self.append_opcode(OpCode::I32Ctz);
	}
    pub fn i32_popcnt(&mut self) {
        self.append_opcode(OpCode::I32Popcnt);
	}
    pub fn i32_add(&mut self) {
        self.append_opcode(OpCode::I32Add);
	}
    pub fn i32_sub(&mut self) {
        self.append_opcode(OpCode::I32Sub);
	}
    pub fn i32_mul(&mut self) {
        self.append_opcode(OpCode::I32Mul);
	}
    pub fn i32_div_s(&mut self) {
        self.append_opcode(OpCode::I32DivS);
	}
    pub fn i32_div_u(&mut self) {
        self.append_opcode(OpCode::I32DivU);
	}
    pub fn i32_rem_s(&mut self) {
        self.append_opcode(OpCode::I32RemS);
	}
    pub fn i32_rem_u(&mut self) {
        self.append_opcode(OpCode::I32RemU);
	}
    pub fn i32_and(&mut self) {
        self.append_opcode(OpCode::I32And);
	}
    pub fn i32_or(&mut self) {
        self.append_opcode(OpCode::I32Or);
	}
    pub fn i32_xor(&mut self) {
        self.append_opcode(OpCode::I32Xor);
	}
    pub fn i32_shl(&mut self) {
        self.append_opcode(OpCode::I32Shl);
	}
    pub fn i32_shr_s(&mut self) {
        self.append_opcode(OpCode::I32ShrS);
	}
    pub fn i32_shr_u(&mut self) {
        self.append_opcode(OpCode::I32ShrU);
	}
    pub fn i32_rotl(&mut self) {
        self.append_opcode(OpCode::I32Rotl);
	}
    pub fn i32_rotr(&mut self) {
        self.append_opcode(OpCode::I32Rotr);
	}
    pub fn i64_clz(&mut self) {
        self.append_opcode(OpCode::I64Clz);
	}
    pub fn i64_ctz(&mut self) {
        self.append_opcode(OpCode::I64Ctz);
	}
    pub fn i64_popcnt(&mut self) {
        self.append_opcode(OpCode::I64Popcnt);
	}
    pub fn i64_add(&mut self) {
        self.append_opcode(OpCode::I64Add);
	}
    pub fn i64_sub(&mut self) {
        self.append_opcode(OpCode::I64Sub);
	}
    pub fn i64_mul(&mut self) {
        self.append_opcode(OpCode::I64Mul);
	}
    pub fn i64_div_s(&mut self) {
        self.append_opcode(OpCode::I64DivS);
	}
    pub fn i64_div_u(&mut self) {
        self.append_opcode(OpCode::I64DivU);
	}
    pub fn i64_rem_s(&mut self) {
        self.append_opcode(OpCode::I64RemS);
	}
    pub fn i64_rem_u(&mut self) {
        self.append_opcode(OpCode::I64RemU);
	}
    pub fn i64_and(&mut self) {
        self.append_opcode(OpCode::I64And);
	}
    pub fn i64_or(&mut self) {
        self.append_opcode(OpCode::I64Or);
	}
    pub fn i64_xor(&mut self) {
        self.append_opcode(OpCode::I64Xor);
	}
    pub fn i64_shl(&mut self) {
        self.append_opcode(OpCode::I64Shl);
	}
    pub fn i64_shr_s(&mut self) {
        self.append_opcode(OpCode::I64ShrS);
	}
    pub fn i64_shr_u(&mut self) {
        self.append_opcode(OpCode::I64ShrU);
	}
    pub fn i64_rotl(&mut self) {
        self.append_opcode(OpCode::I64Rotl);
	}
    pub fn i64_rotr(&mut self) {
        self.append_opcode(OpCode::I64Rotr);
	}
    pub fn f32_abs(&mut self) {
        self.append_opcode(OpCode::F32Abs);
	}
    pub fn f32_neg(&mut self) {
        self.append_opcode(OpCode::F32Neg);
	}
    pub fn f32_ceil(&mut self) {
        self.append_opcode(OpCode::F32Ceil);
	}
    pub fn f32_floor(&mut self) {
        self.append_opcode(OpCode::F32Floor);
	}
    pub fn f32_trunc(&mut self) {
        self.append_opcode(OpCode::F32Trunc);
	}
    pub fn f32_nearest(&mut self) {
        self.append_opcode(OpCode::F32Nearest);
	}
    pub fn f32_sqrt(&mut self) {
        self.append_opcode(OpCode::F32Sqrt);
	}
    pub fn f32_add(&mut self) {
        self.append_opcode(OpCode::F32Add);
	}
    pub fn f32_sub(&mut self) {
        self.append_opcode(OpCode::F32Sub);
	}
    pub fn f32_mul(&mut self) {
        self.append_opcode(OpCode::F32Mul);
	}
    pub fn f32_div(&mut self) {
        self.append_opcode(OpCode::F32Div);
	}
    pub fn f32_min(&mut self) {
        self.append_opcode(OpCode::F32Min);
	}
    pub fn f32_max(&mut self) {
        self.append_opcode(OpCode::F32Max);
	}
    pub fn f32_copysign(&mut self) {
        self.append_opcode(OpCode::F32Copysign);
	}
    pub fn f64_abs(&mut self) {
        self.append_opcode(OpCode::F64Abs);
	}
    pub fn f64_neg(&mut self) {
        self.append_opcode(OpCode::F64Neg);
	}
    pub fn f64_ceil(&mut self) {
        self.append_opcode(OpCode::F64Ceil);
	}
    pub fn f64_floor(&mut self) {
        self.append_opcode(OpCode::F64Floor);
	}
    pub fn f64_trunc(&mut self) {
        self.append_opcode(OpCode::F64Trunc);
	}
    pub fn f64_nearest(&mut self) {
        self.append_opcode(OpCode::F64Nearest);
	}
    pub fn f64_sqrt(&mut self) {
        self.append_opcode(OpCode::F64Sqrt);
	}
    pub fn f64_add(&mut self) {
        self.append_opcode(OpCode::F64Add);
	}
    pub fn f64_sub(&mut self) {
        self.append_opcode(OpCode::F64Sub);
	}
    pub fn f64_mul(&mut self) {
        self.append_opcode(OpCode::F64Mul);
	}
    pub fn f64_div(&mut self) {
        self.append_opcode(OpCode::F64Div);
	}
    pub fn f64_min(&mut self) {
        self.append_opcode(OpCode::F64Min);
	}
    pub fn f64_max(&mut self) {
        self.append_opcode(OpCode::F64Max);
	}
    pub fn f64_copysign(&mut self) {
        self.append_opcode(OpCode::F64Copysign);
	}
    pub fn i32_wrap_i64(&mut self) {
        self.append_opcode(OpCode::I32WrapI64);
	}
    pub fn i32_trunc_f32_s(&mut self) {
        self.append_opcode(OpCode::I32TruncF32S);
	}
    pub fn i32_trunc_f32_u(&mut self) {
        self.append_opcode(OpCode::I32TruncF32U);
	}
    pub fn i32_trunc_f64_s(&mut self) {
        self.append_opcode(OpCode::I32TruncF64S);
	}
    pub fn i32_trunc_f64_u(&mut self) {
        self.append_opcode(OpCode::I32TruncF64U);
	}
    pub fn i64_extend_i32_s(&mut self) {
        self.append_opcode(OpCode::I64ExtendI32S);
	}
    pub fn i64_extend_i32_u(&mut self) {
        self.append_opcode(OpCode::I64ExtendI32U);
	}
    pub fn i64_trunc_f32_s(&mut self) {
        self.append_opcode(OpCode::I64TruncF32S);
	}
    pub fn i64_trunc_f32_u(&mut self) {
        self.append_opcode(OpCode::I64TruncF32U);
	}
    pub fn i64_trunc_f64_s(&mut self) {
        self.append_opcode(OpCode::I64TruncF64S);
	}
    pub fn i64_trunc_f64_u(&mut self) {
        self.append_opcode(OpCode::I64TruncF64U);
	}
    pub fn f32_convert_i32_s(&mut self) {
        self.append_opcode(OpCode::F32ConvertI32S);
	}
    pub fn f32_convert_i32_u(&mut self) {
        self.append_opcode(OpCode::F32ConvertI32U);
	}
    pub fn f32_convert_i64_s(&mut self) {
        self.append_opcode(OpCode::F32ConvertI64S);
	}
    pub fn f32_convert_i64_u(&mut self) {
        self.append_opcode(OpCode::F32ConvertI64U);
	}
    pub fn f32_demote_f64(&mut self) {
        self.append_opcode(OpCode::F32DemoteF64);
	}
    pub fn f64_convert_i32_s(&mut self) {
        self.append_opcode(OpCode::F64ConvertI32S);
	}
    pub fn f64_convert_i32_u(&mut self) {
        self.append_opcode(OpCode::F64ConvertI32U);
	}
    pub fn f64_convert_i64_s(&mut self) {
        self.append_opcode(OpCode::F64ConvertI64S);
	}
    pub fn f64_convert_i64_u(&mut self) {
        self.append_opcode(OpCode::F64ConvertI64U);
	}
    pub fn f64_promote_f32(&mut self) {
        self.append_opcode(OpCode::F64PromoteF32);
	}
    pub fn i32_reinterpret_f32(&mut self) {
        self.append_opcode(OpCode::I32ReinterpretF32);
	}
    pub fn i64_reinterpret_f64(&mut self) {
        self.append_opcode(OpCode::I64ReinterpretF64);
	}
    pub fn f32_reinterpret_i32(&mut self) {
        self.append_opcode(OpCode::F32ReinterpretI32);
	}
    pub fn f64_reinterpret_i64(&mut self) {
        self.append_opcode(OpCode::F64ReinterpretI64);
	}
    
}
