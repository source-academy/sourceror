/**
 * The structs here represent the intermediate representation of the program.
 * In particular:
 * * All variables are represented with the location it should be read/written to (i.e. local#1.var#1, or local#1.var#1.var#1, or global#1).
 * * Note: A param is implicitly a local (with the same index space), and param indices preceed the local indices.
 * * Note: Params may also be modified (just like locals and globals).  They do not affect the value in the caller (because of pass-by-value).
 * * All expressions have optional type annotation (which will be `any` for now).
 * * All functions are top level and don't have closures (the closure is just treated as another parameter).
 * * Top level code are put in the entry point function.
 * *
 * * There are no constant/variable declarations - semantic analyser should hoist all declarations:
 * * Semantic analyser should generate a struct to put all 'local variables' into, and place this struct in Func::locals.  Same for global variables, put them in a single struct that is in Program::globals.
 * * If params need to be captured, semantic analyser should copy them into the struct too.
 * * IR optimisation passes might pull out fields and put them in separate local variables (can create a new struct type so we don't disturb the old one, then dead code elimination can remove the old struct).
 * * There is no difference between constant declarations and variable declarations in the IR.
 * *
 * * Pre-generated functions can be something like `+(any, any) -> any`, which will internally query the type of its arguments and then forward it to the `Add` primitive or the builtin concat(string, string) function.
 */

use std::vec::Vec;
use std::option::Option;
use std::default::Default;

mod primfunc;


pub type FuncIdx = usize;

pub struct Program {
    struct_types: Vec<Box<[VarType]>>, // stores the list of fields of all structs (i.e. objects) in the program
    funcs: Vec<Func>, // list of functions (some will be pre-generated for the pre-declared operators, e.g. + - * / % === and more)
    globals: Vec<VarType>, // list of global variables
    entry_point: FuncIdx, // index of function to run when the program is started
}

#[derive(Copy, Clone)]
pub enum VarType {
    Any, // used if we don't know the type contained in the variable.  Most of the time we will use this.  Generates a variant in the output program unless it gets optimised away.
    Undefined,
    Number,
    Boolean,
    String, // reference type
    Func, // holds a function ptr and a closure
    StructT{typeidx: usize}, // reference type; typeid starts from zero and should be in range [0, object_types.len()).
}
impl Default for VarType {
    fn default() -> Self {
        VarType::Any
	}
}

pub type Block = Vec<Statement>;

pub struct Func {
    params: Box<[VarType]>, // list of function parameters
    result: Option<VarType>, // if `None`, it means that this function never returns (e.g. it guarantees to trap, see the generated runtime error function)
    locals: Vec<VarType>, // list of local variables
    statements: Block, // body of the function
    signature_filter: Vec<(Box<[VarType]>, VarType, FuncIdx)>, // list of possibly acceptable signatures (param_types, return_type, constrained_func).
                                                        // If a signature is not in this list, then it will be guaranteed to error;
                                                        // but converse need not be true.  All entries must be a subtype of `params`.
                                                        // `constrained_func` (funcidx) is a version of this function that has the specified param_types and return_type of this entry.
                                                        // (i.e. if the caller can guarantee to have the correct types,
                                                        // then it can emit code to call the constrained_func instead of the current one)
                                                        // this list should not contain the entry where all the param types and return type are identical to the current one
                                                        // (because there is no use for a self-reference)
}
 
pub enum Statement {
    Assign{target: TargetExpr, expr: Expr}, // Assigment statement
    Return{expr: Expr}, // Return statmenent,
    // todo!: Should If be an Expr instead of a separate statement?  (Wasm if-statements can return a value.)
    If{cond: Expr, true_stmts: Block, false_stmts: Block}, // If statement
    Expr{expr: Expr}, // Expression statement
}

pub struct Expr {
    vartype: VarType, // the type set that this Expr is guaranteed to evaluate to (if unknown, just use ValType::Any).  Users of this expression will generate code that only works on this type.  It may also affect the memory layout of the expr.
    kind: ExprKind, // the variant kind of this expression
}

// Represents any lvalue (assignable value)
pub enum TargetExpr {
    // if the field is a struct, then `next` can (but not necessarily) refer to a field inside the struct
    Global{globalidx: usize, next: Option<Box<StructField>>}, // for targetting a global variable
    Local{localidx: usize, next: Option<Box<StructField>>}, // for targetting a local variable
}

pub struct StructField {
    typeidx: usize, // the struct type id (index into struct_types)
    fieldidx: usize, // the index of the referred field in the struct
    next: Option<Box<StructField>>, // if the field is a struct, then this can (but not necessarily) refer to a field inside the struct
}

pub enum ExprKind {
    PrimUndefined,
    PrimNumber{val: f64}, // e.g. `2`
    PrimBoolean{val: bool}, // e.g. `true`
    PrimString{val: String}, // e.g. `"hello world"`, may be placed in a region of memory immune to garbage collection
    PrimInst{funcidx: FuncIdx}, // e.g. `() => {}`
    TypeAssert{expr: Box<Expr>, vartype: VarType}, // returns undefined if the expr has correct type, otherwise traps.  Generated by compiler.
    VarName{source: TargetExpr}, // something that is located somewhere in memory (i.e. an lvalue), e.g. `x`.  Note: even if `x` has type `any`, the Expr::vartype of this expr can still be more specific, if the compiler can prove its correctness.  i.e. we can load from a variant without checking the tag, if we already proved that the data must be of a particular VarType.
    PrimAppl{prim_inst: PrimInst, args: Box<[Expr]>}, // primitive operations (e.g. number+number) hardcoded into the compiler.  Expr must have the correct VarType.  Should not be added directly by semantic analyser, because parser is not type-aware.
    Appl{func: Box<Expr>, args: Box<[Expr]>}, // function application (operators are functions too)
    DirectAppl{funcidx: FuncIdx, args: Box<[Expr]>}, // direct function application (operators are functions too)
}

// private enum of possible primitive functions, used by pre-declared operators, or added during type-checking optimisation
// these functions expect a particular type signature
// this is an implementation detail of the IR, and subject to change
enum PrimInst {
    NumberAdd,
    NumberSub,
    NumberMul,
    NumberDiv,
    NumberRem,
    NumberEq,
    NumberNeq,
    NumberGt,
    NumberLt,
    NumberGe,
    NumberLe,
    BooleanEq,
    BooleanNeq,
    BooleanAnd,
    BooleanOr,
    BooleanNot,
    NumberNegate,
    StringAdd,
    Trap, // used for runtime errors (e.g. type errors).  args should currently be empty, but subject to change.
}

// enum of pre-declared operators
#[derive(Eq, PartialEq, Copy, Clone)]
#[repr(u8)]
pub enum Builtin {
    Plus,
    Minus,
    Times,
    Divide,
    Modulo,
    Eq,
    Neq,
    Gt,
    Lt,
    Ge,
    Le,
    And,
    Or,
    Not,
    UnaryMinus,
}
pub const NUM_BUILTINS : u8 = Builtin::UnaryMinus as u8 + 1;

impl Program {
    // Creates an empty Program, and an array of pre-declared operators that can be used.
    // `funcs` will have pre-declared operators, but might also have other primitive functions (e.g. typed version of pre-declared operators).
    // The caller should only use the functions that match the funcidxs specified in the returned array of pre-declared operators.
    // Other things in the `funcs` array should not be used.
    pub fn new() -> (Program, [FuncIdx; NUM_BUILTINS as usize]) {
        let (funcs, builtin_funcidxs) = primfunc::make_pregenerated_funcs();
        (
            Program{
                struct_types: Default::default(),
                funcs: funcs,
                globals: Default::default(),
                entry_point: Default::default(),
		    },
            builtin_funcidxs
        )
	}
}

impl Func {
    pub fn new_with_params_and_result(params: &[VarType], result: VarType) -> Func
    {
        Func{
            params: params.into(),
            result: Some(result),
            locals: Default::default(),
            statements: Default::default(),
            signature_filter: Default::default(), 
		}
	}
}
