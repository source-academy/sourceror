/*
This module contains WebAssembly instruction sequences for the seven string primitives:
- StringAdd, StringEq, StringNeq, StringGt, StringLt, StringGe, StringLe

StringAdd will allocate new memory for the returned string.
The six other primitive instructions do not allocate any memory.
*/

use wasmgen::ExprBuilder;
use wasmgen::MemArg;
use wasmgen::Scratch;
use wasmgen::ValType;

// net wasm stack [string_1(i32), string_2(i32)] -> [ret(i32)]
pub fn encode_string_eq(scratch: &mut Scratch, expr_builder: &mut ExprBuilder) {
    // Algorithm:
    // Note that we don't actually encode 'return' instructions, because we are part of the caller function.
    // We simply encode the `break` instruction (with the correct depth parameter) instead.
    // Note: We compare 4 bytes at a time because our memory is 4-byte aligned, so it will be faster.
    /*
    let len = *string_1;
    let len_2 = *string_2;
    if len == len_2 {
        string_1 += 4; // skip the length
        string_2 += 4; // skip the length
        while len >= 4 {
            if *(i32*)(string_1) != *(i32*)(string_2) {
                return false;
            }
            string_1 += 4;
            string_2 += 4;
            len -= 4;
        }
        if len & 0x2 {
            if (*(i16*)(string_1) != *(i16*)(string_2)) {
                return false;
            }
            string_1 += 2;
            string_2 += 2;
        }
        if len & 0x1 {
            return (*(i8*)(string_1) == *(i8*)(string_2));
        } else {
            return true;
        }
    } else {
        return false;
    }
    */

    let string_1 = scratch.push_i32();
    let string_2 = scratch.push_i32();
    let len = scratch.push_i32();

    // let len = *string_1;
    // let len_2 = *string_2;
    // net wasm stack: [string_1(i32), string_2(i32)] -> [len(i32), len_2(i32)]
    expr_builder.local_set(string_2);
    expr_builder.local_tee(string_1);
    expr_builder.i32_load(MemArg::new4(0));
    expr_builder.local_tee(len);
    expr_builder.local_get(string_2);
    expr_builder.i32_load(MemArg::new4(0));

    // if len == len_2 {
    //     return <...>;
    // }
    // else {
    //     return false;
    // }
    // net wasm stack: [len(i32), len_2(i32)] -> [ret(i32)]
    expr_builder.i32_eq();
    expr_builder.if_(&[ValType::I32]);
    {
        // string_1 += 4;
        // string_2 += 4;
        // net wasm stack: [] -> []
        expr_builder.local_get(string_1);
        expr_builder.i32_const(4);
        expr_builder.i32_add();
        expr_builder.local_set(string_1);
        expr_builder.local_get(string_2);
        expr_builder.i32_const(4);
        expr_builder.i32_add();
        expr_builder.local_set(string_2);

        // if len >= 4 {
        //     loop {
        //         <...>
        //         len -= 4;
        //         br_if len >= 4;
        //     };
        // }
        // net wasm stack: [] -> []
        expr_builder.local_get(len);
        expr_builder.i32_const(4);
        expr_builder.i32_ge_u();
        expr_builder.if_(&[]);
        {
            expr_builder.loop_(&[]);
            {
                // if *(i32*)(string_1) != *(i32*)(string_2) {
                //     return false;
                // }
                // net wasm stack: [] -> []
                expr_builder.local_get(string_1);
                expr_builder.i32_load(MemArg::new4(0));
                expr_builder.local_get(string_2);
                expr_builder.i32_load(MemArg::new4(0));
                expr_builder.i32_ne();
                expr_builder.if_(&[]);
                {
                    expr_builder.i32_const(0);
                    expr_builder.br(3);
                }
                expr_builder.end();

                // string_1 += 4;
                // string_2 += 4;
                // net wasm stack: [] -> []
                expr_builder.local_get(string_1);
                expr_builder.i32_const(4);
                expr_builder.i32_add();
                expr_builder.local_set(string_1);
                expr_builder.local_get(string_2);
                expr_builder.i32_const(4);
                expr_builder.i32_add();
                expr_builder.local_set(string_2);

                // net wasm stack: [] -> []
                expr_builder.local_get(len);
                expr_builder.i32_const(4);
                expr_builder.i32_sub();
                expr_builder.local_tee(len);
                expr_builder.i32_const(4);
                expr_builder.i32_ge_u();
                expr_builder.br_if(0);
            }
            expr_builder.end();
        }
        expr_builder.end();

        // if len & 0x2 {
        //     <...>
        // }
        // net wasm stack: [] -> []
        expr_builder.local_get(len);
        expr_builder.i32_const(2);
        expr_builder.i32_and();
        expr_builder.if_(&[]);
        {
            // if (*(i16*)(string_1) != *(i16*)(string_2)) {
            //     return false;
            // }
            // net wasm stack: [] -> []
            expr_builder.local_get(string_1);
            expr_builder.i32_load16_u(MemArg::new2(0));
            expr_builder.local_get(string_2);
            expr_builder.i32_load16_u(MemArg::new2(0));
            expr_builder.i32_ne();
            expr_builder.if_(&[]);
            {
                expr_builder.i32_const(0);
                expr_builder.br(2);
            }
            expr_builder.end();

            // string_1 += 2;
            // string_2 += 2;
            // net wasm stack: [] -> []
            expr_builder.local_get(string_1);
            expr_builder.i32_const(2);
            expr_builder.i32_add();
            expr_builder.local_set(string_1);
            expr_builder.local_get(string_2);
            expr_builder.i32_const(2);
            expr_builder.i32_add();
            expr_builder.local_set(string_2);
        }
        expr_builder.end();

        // if len & 0x1 {
        //     return (*(i8*)(string_1) == *(i8*)(string_2));
        // } else {
        //     return true;
        // }
        // net wasm stack: [] -> [ret(i32)]
        expr_builder.local_get(len);
        expr_builder.i32_const(1);
        expr_builder.i32_and();
        expr_builder.if_(&[ValType::I32]);
        {
            expr_builder.local_get(string_1);
            expr_builder.i32_load8_u(MemArg::new1(0));
            expr_builder.local_get(string_2);
            expr_builder.i32_load8_u(MemArg::new1(0));
            expr_builder.i32_eq();
        }
        expr_builder.else_();
        {
            expr_builder.i32_const(1);
        }
        expr_builder.end();
    }
    expr_builder.else_();
    {
        expr_builder.i32_const(0);
    }
    expr_builder.end();

    scratch.pop_i32();
    scratch.pop_i32();
    scratch.pop_i32();
}

// net wasm stack [string_1(i32), string_2(i32)] -> [ret(i32)]
pub fn encode_string_ne(scratch: &mut Scratch, expr_builder: &mut ExprBuilder) {
    // Algorithm:
    // Note that we don't actually encode 'return' instructions, because we are part of the caller function.
    // We simply encode the `break` instruction (with the correct depth parameter) instead.
    // Note: We compare 4 bytes at a time because our memory is 4-byte aligned, so it will be faster.
    /*
    let len = *string_1;
    let len_2 = *string_2;
    if len == len_2 {
        string_1 += 4; // skip the length
        string_2 += 4; // skip the length
        while len >= 4 {
            if *(i32*)(string_1) != *(i32*)(string_2) {
                return true;
            }
            string_1 += 4;
            string_2 += 4;
            len -= 4;
        }
        if len & 0x2 {
            if (*(i16*)(string_1) != *(i16*)(string_2)) {
                return true;
            }
            string_1 += 2;
            string_2 += 2;
        }
        if len & 0x1 {
            return (*(i8*)(string_1) != *(i8*)(string_2));
        } else {
            return false;
        }
    } else {
        return true;
    }
    */

    let string_1 = scratch.push_i32();
    let string_2 = scratch.push_i32();
    let len = scratch.push_i32();

    // let len = *string_1;
    // let len_2 = *string_2;
    // net wasm stack: [string_1(i32), string_2(i32)] -> [len(i32), len_2(i32)]
    expr_builder.local_set(string_2);
    expr_builder.local_tee(string_1);
    expr_builder.i32_load(MemArg::new4(0));
    expr_builder.local_tee(len);
    expr_builder.local_get(string_2);
    expr_builder.i32_load(MemArg::new4(0));

    // if len == len_2 {
    //     return <...>;
    // }
    // else {
    //     return true;
    // }
    // net wasm stack: [len(i32), len_2(i32)] -> [ret(i32)]
    expr_builder.i32_eq();
    expr_builder.if_(&[ValType::I32]);
    {
        // string_1 += 4;
        // string_2 += 4;
        // net wasm stack: [] -> []
        expr_builder.local_get(string_1);
        expr_builder.i32_const(4);
        expr_builder.i32_add();
        expr_builder.local_set(string_1);
        expr_builder.local_get(string_2);
        expr_builder.i32_const(4);
        expr_builder.i32_add();
        expr_builder.local_set(string_2);

        // if len >= 4 {
        //     loop {
        //         <...>
        //         len -= 4;
        //         br_if len >= 4;
        //     };
        // }
        // net wasm stack: [] -> []
        expr_builder.local_get(len);
        expr_builder.i32_const(4);
        expr_builder.i32_ge_u();
        expr_builder.if_(&[]);
        {
            expr_builder.loop_(&[]);
            {
                // if *(i32*)(string_1) != *(i32*)(string_2) {
                //     return true;
                // }
                // net wasm stack: [] -> []
                expr_builder.local_get(string_1);
                expr_builder.i32_load(MemArg::new4(0));
                expr_builder.local_get(string_2);
                expr_builder.i32_load(MemArg::new4(0));
                expr_builder.i32_ne();
                expr_builder.if_(&[]);
                {
                    expr_builder.i32_const(1);
                    expr_builder.br(3);
                }
                expr_builder.end();

                // string_1 += 4;
                // string_2 += 4;
                // net wasm stack: [] -> []
                expr_builder.local_get(string_1);
                expr_builder.i32_const(4);
                expr_builder.i32_add();
                expr_builder.local_set(string_1);
                expr_builder.local_get(string_2);
                expr_builder.i32_const(4);
                expr_builder.i32_add();
                expr_builder.local_set(string_2);

                // net wasm stack: [] -> []
                expr_builder.local_get(len);
                expr_builder.i32_const(4);
                expr_builder.i32_sub();
                expr_builder.local_tee(len);
                expr_builder.i32_const(4);
                expr_builder.i32_ge_u();
                expr_builder.br_if(0);
            }
            expr_builder.end();
        }
        expr_builder.end();

        // if len & 0x2 {
        //     <...>
        // }
        // net wasm stack: [] -> []
        expr_builder.local_get(len);
        expr_builder.i32_const(2);
        expr_builder.i32_and();
        expr_builder.if_(&[]);
        {
            // if (*(i16*)(string_1) != *(i16*)(string_2)) {
            //     return true;
            // }
            // net wasm stack: [] -> []
            expr_builder.local_get(string_1);
            expr_builder.i32_load16_u(MemArg::new2(0));
            expr_builder.local_get(string_2);
            expr_builder.i32_load16_u(MemArg::new2(0));
            expr_builder.i32_ne();
            expr_builder.if_(&[]);
            {
                expr_builder.i32_const(1);
                expr_builder.br(2);
            }
            expr_builder.end();

            // string_1 += 2;
            // string_2 += 2;
            // net wasm stack: [] -> []
            expr_builder.local_get(string_1);
            expr_builder.i32_const(2);
            expr_builder.i32_add();
            expr_builder.local_set(string_1);
            expr_builder.local_get(string_2);
            expr_builder.i32_const(2);
            expr_builder.i32_add();
            expr_builder.local_set(string_2);
        }
        expr_builder.end();

        // if len & 0x1 {
        //     return (*(i8*)(string_1) != *(i8*)(string_2));
        // } else {
        //     return true;
        // }
        // net wasm stack: [] -> [ret(i32)]
        expr_builder.local_get(len);
        expr_builder.i32_const(1);
        expr_builder.i32_and();
        expr_builder.if_(&[ValType::I32]);
        {
            expr_builder.local_get(string_1);
            expr_builder.i32_load8_u(MemArg::new1(0));
            expr_builder.local_get(string_2);
            expr_builder.i32_load8_u(MemArg::new1(0));
            expr_builder.i32_ne();
        }
        expr_builder.else_();
        {
            expr_builder.i32_const(0);
        }
        expr_builder.end();
    }
    expr_builder.else_();
    {
        expr_builder.i32_const(1);
    }
    expr_builder.end();

    scratch.pop_i32();
    scratch.pop_i32();
    scratch.pop_i32();
}

// Encodes a comparison operation on strings.
// `compare` has stack [int_1(i32), int_2(i32)] -> [ret(i32)], which returns (int_1 <op> int_2).  It is used to compare characters and lengths.
// net wasm stack [string_1(i32), string_2(i32)] -> [ret(i32)]
fn encode_string_compare<F: Fn(&mut ExprBuilder)>(
    compare: F,
    scratch: &mut Scratch,
    expr_builder: &mut ExprBuilder,
) {
    // Algorithm:
    // Note that we don't actually encode 'return' instructions, because we are part of the caller function.
    // We simply encode the `break` instruction (with the correct depth parameter) instead.
    // Note: We can't compare 4 bytes at a time because WebAssembly is little-endian; comparing 4 bytes using i32 comparison might give the wrong order.
    /*
    let len_1 = *string_1;
    let len_2 = *string_2;
    string_1 += 4; // skip the length
    string_2 += 4; // skip the length
    let len_min = min(len_1, len_2);
    while len_min != 0 {
        let tmp_1 = *(i8*)(string_1);
        let tmp_2 = *(i8*)(string_2);
        if tmp_1 != tmp_2 {
            return compare(tmp_1, tmp_2);
        }
        len_min -= 1;
    }
    return compare(len_1, len_2);
    */

    let string_1 = scratch.push_i32();
    let string_2 = scratch.push_i32();
    let len_1 = scratch.push_i32();
    let len_2 = scratch.push_i32();
    let len_min = scratch.push_i32();

    // add block... so we can 'return' later using the branch instruction
    expr_builder.block(&[ValType::I32]);
    {
        // let len_1 = *string_1;
        // let len_2 = *string_2;
        // net wasm stack: [string_1(i32), string_2(i32)] -> [len_1(i32), len_2(i32)]
        expr_builder.local_set(string_2);
        expr_builder.local_tee(string_1);
        expr_builder.i32_load(MemArg::new4(0));
        expr_builder.local_tee(len_1);
        expr_builder.local_get(string_2);
        expr_builder.i32_load(MemArg::new4(0));
        expr_builder.local_tee(len_2);

        // string_1 += 4;
        // string_2 += 4;
        // net wasm stack: [] -> []
        expr_builder.local_get(string_1);
        expr_builder.i32_const(4);
        expr_builder.i32_add();
        expr_builder.local_set(string_1);
        expr_builder.local_get(string_2);
        expr_builder.i32_const(4);
        expr_builder.i32_add();
        expr_builder.local_set(string_2);

        // let len_min = min(len_1, len_2);
        // net wasm stack: [len_1(i32), len_2(i32)] -> [len_min(i32)]
        expr_builder.local_get(len_1);
        expr_builder.local_get(len_2);
        expr_builder.i32_lt_u();
        expr_builder.select();
        expr_builder.local_tee(len_min);

        // if len_min != 0 {
        //     loop {
        //         <...>
        //         len_min -= 1;
        //         br_if len_min != 0;
        //     }
        // }
        // net wasm stack: [] -> []
        expr_builder.if_(&[]);
        {
            expr_builder.loop_(&[]);
            {
                // let tmp_1 = *(i8*)(string_1);
                // let tmp_2 = *(i8*)(string_2);
                // if tmp_1 != tmp_2 {
                //     return compare(tmp_1, tmp_2);
                // }
                // net wasm stack: [] -> []
                let tmp_1 = scratch.push_i32();
                let tmp_2 = scratch.push_i32();
                expr_builder.local_get(string_1);
                expr_builder.i32_load8_u(MemArg::new1(0));
                expr_builder.local_tee(tmp_1);
                expr_builder.local_get(string_2);
                expr_builder.i32_load8_u(MemArg::new1(0));
                expr_builder.local_tee(tmp_2);
                expr_builder.i32_ne();
                expr_builder.if_(&[]);
                {
                    expr_builder.local_get(tmp_1);
                    expr_builder.local_get(tmp_2);
                    compare(expr_builder);
                    expr_builder.br(3);
                }
                expr_builder.end();
                scratch.pop_i32();
                scratch.pop_i32();

                // net wasm stack: [] -> []
                expr_builder.local_get(len_min);
                expr_builder.i32_const(1);
                expr_builder.i32_sub();
                expr_builder.local_tee(len_min);
                expr_builder.br_if(0);
            }
            expr_builder.end();
        }
        expr_builder.end();

        // return compare(len_1, len_2);
        // net wasm stack: [] -> []
        expr_builder.local_get(len_1);
        expr_builder.local_get(len_2);
        compare(expr_builder);
    }
    expr_builder.end();

    scratch.pop_i32();
    scratch.pop_i32();
    scratch.pop_i32();
    scratch.pop_i32();
    scratch.pop_i32();
}

// net wasm stack [string_1(i32), string_2(i32)] -> [ret(i32)]
pub fn encode_string_gt(scratch: &mut Scratch, expr_builder: &mut ExprBuilder) {
    encode_string_compare(
        |expr_builder| {
            expr_builder.i32_gt_u();
        },
        scratch,
        expr_builder,
    );
}

// net wasm stack [string_1(i32), string_2(i32)] -> [ret(i32)]
pub fn encode_string_lt(scratch: &mut Scratch, expr_builder: &mut ExprBuilder) {
    encode_string_compare(
        |expr_builder| {
            expr_builder.i32_lt_u();
        },
        scratch,
        expr_builder,
    );
}

// net wasm stack [string_1(i32), string_2(i32)] -> [ret(i32)]
pub fn encode_string_ge(scratch: &mut Scratch, expr_builder: &mut ExprBuilder) {
    encode_string_compare(
        |expr_builder| {
            expr_builder.i32_ge_u();
        },
        scratch,
        expr_builder,
    );
}

// net wasm stack [string_1(i32), string_2(i32)] -> [ret(i32)]
pub fn encode_string_le(scratch: &mut Scratch, expr_builder: &mut ExprBuilder) {
    encode_string_compare(
        |expr_builder| {
            expr_builder.i32_le_u();
        },
        scratch,
        expr_builder,
    );
}
