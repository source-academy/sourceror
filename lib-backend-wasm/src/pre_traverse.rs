use projstd::searchablevec::SearchableVec;
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
    // map from [FuncIdx] to tableidx, since [FuncIdx] uniquely determines the thunk content
    // (note: we can know the signature from the funcidx)
    pub thunk_sv: SearchableVec<Box<[ir::OverloadEntry]>>,
    pub appl_location_sv: SearchableVec<ir::SourceLocation>,
}

/*
Traverses the IR to:
- put all string constants in a string pool, and encodes the static data buffer
- put all overload sets (thunks) in a SearchableVec
- extract all SourceLocations in Appls into a SearchableVec
*/
pub fn pre_traverse_funcs(funcs: &[ir::Func]) -> TraverseResult {
    let mut res = TraverseResult::default();
    for func in funcs {
        pre_traverse_func(func, &mut res);
    }
    res
}

fn pre_traverse_func(func: &ir::Func, res: &mut TraverseResult) {
    pre_traverse_expr(&func.expr, res);
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
        ir::ExprKind::PrimFunc { funcidxs, closure } => {
            res.thunk_sv.insert_copy(funcidxs);
            pre_traverse_expr(closure, res);
        }
        ir::ExprKind::TypeCast {
            test,
            expected: _,
            create_narrow_local: _,
            true_expr,
            false_expr,
        } => {
            pre_traverse_expr(test, res);
            pre_traverse_expr(true_expr, res);
            pre_traverse_expr(false_expr, res);
        }
        ir::ExprKind::VarName { source: _ } => {}
        ir::ExprKind::PrimAppl { prim_inst: _, args } => pre_traverse_exprs(args, res),
        ir::ExprKind::Appl {
            is_tail,
            func,
            args,
            location,
        } => {
            pre_traverse_expr(func, res);
            pre_traverse_exprs(args, res);
            res.appl_location_sv.insert_copy(location);
        }
        ir::ExprKind::DirectAppl { is_tail, funcidx: _, args } => pre_traverse_exprs(args, res),
        ir::ExprKind::Conditional {
            cond,
            true_expr,
            false_expr,
        } => {
            pre_traverse_expr(cond, res);
            pre_traverse_expr(true_expr, res);
            pre_traverse_expr(false_expr, res);
        }
        ir::ExprKind::Declaration {
            local: _,
            init,
            contained_expr,
        } => {
            if let Some(init_expr) = init {
                pre_traverse_expr(init_expr, res);
            }
            pre_traverse_expr(contained_expr, res);
        }
        ir::ExprKind::Assign { target: _, expr }
        | ir::ExprKind::Return { expr }
        | ir::ExprKind::Break {
            num_frames: _,
            expr,
        }
        | ir::ExprKind::Block { expr } => pre_traverse_expr(expr, res),
        ir::ExprKind::Sequence { content } => {
            pre_traverse_exprs(content, res);
        }
        ir::ExprKind::Trap {
            code: _,
            location: _,
        } => {}
    };
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

// encodes the appl_location_sv into static data
// returns the data and the map from the SourceLocation to the index in memory (using the given `offset` to adjust this index)
pub fn make_appl_location_static_data(
    appl_location_sv: SearchableVec<ir::SourceLocation>,
    offset: u32,
) -> (Box<[u8]>, HashMap<ir::SourceLocation, u32>) {
    let (v, hm) = appl_location_sv.into_parts();
    let mut ret_bytes = Vec::new();
    // 4 bytes per u32, and 5 u32 to specify the location, so 20 bytes per iterm
    let ret_len = v.len() * 20;
    ret_bytes.reserve_exact(ret_len);
    for sl in v {
        ret_bytes.extend_from_slice(&sl.file.to_le_bytes());
        ret_bytes.extend_from_slice(&sl.start.line.to_le_bytes());
        ret_bytes.extend_from_slice(&sl.start.column.to_le_bytes());
        ret_bytes.extend_from_slice(&sl.end.line.to_le_bytes());
        ret_bytes.extend_from_slice(&sl.end.column.to_le_bytes());
    }
    assert!(ret_bytes.len() == ret_len);
    let ret_hm: HashMap<ir::SourceLocation, u32> = hm
        .into_iter()
        .map(|(sl, idx)| (sl, offset + (idx as u32) * 20))
        .collect();
    (ret_bytes.into_boxed_slice(), ret_hm)
}
