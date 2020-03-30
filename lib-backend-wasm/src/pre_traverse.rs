use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::vec::Vec;

/*
String pool for adding strings.
*/
#[derive(Default)]
pub struct StringPool {
    map: HashMap<String, u32>, // map from string to index in buf (which is a pointer the start of the string (i.e. the length field))
    buf: Vec<u8>,              // buffer of static data
}

/*
String pool for querying strings.  Cannot add more strings here.
The strings have been shifted by a constant offset (usually the MEM_STACK_SIZE).
*/
pub struct ShiftedStringPool {
    map: HashMap<String, u32>,
}

#[derive(Default)]
pub struct TraverseResult {
    pub string_pool: StringPool,
}

/*
Traverses the IR to put all string constants in a string pool, and encodes the static data buffer.
*/
pub fn pre_traverse_funcs(funcs: &[ir::Func]) -> TraverseResult {
    let mut res = TraverseResult::default();
    for func in funcs {
        pre_traverse_func(func, &mut res);
    }
    res
}

fn pre_traverse_func(func: &ir::Func, res: &mut TraverseResult) {
    pre_traverse_block(&func.block, res);
}

fn pre_traverse_block(block: &ir::Block, res: &mut TraverseResult) {
    pre_traverse_statements(&block.statements, res);
}

fn pre_traverse_statements(statements: &[ir::Statement], res: &mut TraverseResult) {
    for statement in statements {
        pre_traverse_statement(statement, res);
    }
}

fn pre_traverse_statement(statement: &ir::Statement, res: &mut TraverseResult) {
    match statement {
        ir::Statement::Assign { target: _, expr } => pre_traverse_expr(expr, res),
        ir::Statement::Return { expr } => pre_traverse_expr(expr, res),
        ir::Statement::If {
            cond,
            true_block,
            false_block,
        } => {
            pre_traverse_expr(cond, res);
            pre_traverse_block(true_block, res);
            pre_traverse_block(false_block, res);
        }
        ir::Statement::Block { block } => pre_traverse_block(block, res),
        ir::Statement::Expr { expr } => pre_traverse_expr(expr, res),
        ir::Statement::Void { expr_kind } => pre_traverse_expr_kind(expr_kind, res),
    }
}

fn pre_traverse_exprs(exprs: &[ir::Expr], res: &mut TraverseResult) {
    for expr in exprs {
        pre_traverse_expr(expr, res);
    }
}

fn pre_traverse_expr(expr: &ir::Expr, res: &mut TraverseResult) {
    pre_traverse_expr_kind(&expr.kind, res);
}

fn pre_traverse_expr_kind(expr_kind: &ir::ExprKind, res: &mut TraverseResult) {
    match expr_kind {
        ir::ExprKind::PrimUndefined
        | ir::ExprKind::PrimNumber { val: _ }
        | ir::ExprKind::PrimBoolean { val: _ }
        | ir::ExprKind::PrimStructT { typeidx: _ } => {}
        ir::ExprKind::PrimString { val } => res.string_pool.insert(val),
        ir::ExprKind::PrimFunc {
            funcidx: _,
            closure,
        } => pre_traverse_expr(closure, res),
        ir::ExprKind::TypeOf { expr, expected: _ } => pre_traverse_expr(expr, res),
        ir::ExprKind::VarName { source: _ } => {}
        ir::ExprKind::PrimAppl { prim_inst: _, args } => pre_traverse_exprs(args, res),
        ir::ExprKind::Appl { func, args } => {
            pre_traverse_expr(func, res);
            pre_traverse_exprs(args, res);
        }
        ir::ExprKind::DirectAppl { funcidx: _, args } => pre_traverse_exprs(args, res),
        ir::ExprKind::Conditional {
            cond,
            true_expr,
            false_expr,
        } => {
            pre_traverse_expr(cond, res);
            pre_traverse_expr(true_expr, res);
            pre_traverse_expr(false_expr, res);
        }
        ir::ExprKind::Trap {
            code: _,
            location: _,
        } => {}
    }
}

impl StringPool {
    /*
    Inserts this string into the string pool if it doesn't already exist.
    Otherwise, does nothing.
    */
    fn insert(&mut self, str: &str) {
        if let Entry::Vacant(vacant_entry) = self.map.entry(str.to_owned()) {
            let key: &str = vacant_entry.key();

            // index into the buffer
            let ptr = self.buf.len() as u32;

            // extend the buffer
            self.buf
                .extend_from_slice(&(key.len() as u32).to_le_bytes());
            self.buf.extend_from_slice(key.as_bytes());
            // add extra padding to end at a multiple of 4 bytes
            match key.len() % 4 {
                0 => {}
                1 => self.buf.extend_from_slice(&[0, 0, 0]),
                2 => self.buf.extend_from_slice(&[0, 0]),
                3 => self.buf.extend_from_slice(&[0]),
                _ => unreachable!(),
            }

            // insert the entry into the hashmap
            vacant_entry.insert(ptr);
        }
    }

    /*
    Shifts the string pool by a constant offset, then splits it into the (readonly) shifted pool and the buffer.
    */
    pub fn into_shifted_and_buffer(self, offset: u32) -> (ShiftedStringPool, Vec<u8>) {
        let mut map = self.map;
        for (_, ptr) in &mut map {
            *ptr += offset;
        }
        (ShiftedStringPool { map: map }, self.buf)
    }
}

impl ShiftedStringPool {
    /*
    Inserts this string into the string pool if it doesn't already exist.
    Otherwise, does nothing.
    */
    pub fn lookup(&self, str: &String) -> u32 {
        *self.map.get(str).unwrap()
    }
}
