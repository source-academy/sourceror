/**
 * The structs here represent the intermediate representation of the program.
 * In particular:
 * * All variables are represented with the location it should be read/written to (i.e. local#1, or param#1.var#1, or param#1.var#1.var#1, or global#1).
 * * All expressions have optional type annotation (which will be `any` for now).
 * * All functions are top level and don't have closures (the closure is just treated as another parameter).
 * * Top level code are put in the entry point function.
 * *
 * * There are no constant/variable declarations - semantic analyser should hoist all declarations:
 * * Semantic analyser should generate a struct to put all 'local variables' into, and place this struct in Func::locals.  Same for global variables, put them in a single struct that is in Program::globals.
 * * IR optimisation passes might pull out fields and put them in separate local variables (can create a new struct type so we don't disturb the old one, then dead code elimination can remove the old struct).
 * * There is no difference between constant declarations and variable declarations in the IR.
 * *
 * * Pre-generated functions can be something like `+(any, any) -> any`, which will internally query the type of its arguments and then forward it to the `Add` primitive or the builtin concat(string, string) function.
 */

use std::vec::Vec;
use std::option::Option;
use std::default::Default;

mod primfunc;

pub struct Program {
    struct_types: Vec<Box<[VarType]>>, // stores the list of fields of all structs (i.e. objects) in the program
    funcs: Vec<Func>, // list of functions (some will be pre-generated for the pre-declared operators, e.g. + - * / % === and more)
    globals: Vec<VarType>, // list of global variables
    entry_point: u32, // index of function to run when the program is started
}


pub enum VarType {
    Any, // used if we don't know the type contained in the variable.  Most of the time we will use this.  Generates a variant in the output program unless it gets optimised away.
    Undefined,
    Number,
    Boolean,
    String, // reference type
    StructT{typeidx: u32}, // reference type; typeid starts from zero and should be in range [0, object_types.len()).
}
impl Default for VarType {
    fn default() -> Self {
        VarType::Any
	}
}

type Block = Vec<Statement>;

pub struct Func {
    params: Vec<VarType>, // list of function parameters
    locals: Vec<VarType>, // list of local variables
    statements: Block, // body of the function
}
 
pub enum Statement {
    Assign{target: TargetExpr, expr: Expr}, // Assigment statement
    Return{expr: Expr}, // Return statmenent,
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
    Global{globalidx: u32, next: Option<Box<StructField>>}, // for targetting a global variable
    Local{localidx: u32, next: Option<Box<StructField>>}, // for targetting a local variable
}

pub struct StructField {
    typeidx: u32, // the struct type id (index into struct_types)
    fieldidx: u32, // the index of the referred field in the struct
    next: Option<Box<StructField>>, // if the field is a struct, then this can (but not necessarily) refer to a field inside the struct
}


pub enum ExprKind {
    PrimitiveNumber{val: f64}, // e.g. `2`
    PrimitiveBoolean{val: bool}, // e.g. `true`
    PrimitiveString{val: String}, // e.g. `"hello world"`, may be placed in a region of memory immune to garbage collection
    VarName{source: TargetExpr}, // something that is located somewhere in memory (i.e. an lvalue), e.g. `x`
    PrimAppl{prim_func: PrimFunc, args: Box<[Expr]>}, // primitive operations (e.g. number+number) hardcoded into the compiler.  Expr must have the correct VarType.  Should not be added directly by semantic analyser, because parser is not type-aware.
    FuncAppl{funcidx: u32, args: Box<[Expr]>}, // function application (operators are functions too)
}

// private enum of possible primitive functions, used by pre-declared operators, or added during type-checking optimisation
// these functions expect a particular type signature
// this is an implementation detail of the IR, and subject to change
enum PrimFunc {
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
}

impl Program {
    pub fn new() -> Program {
        // populates `funcs` with pre-declared operators, then returns the Program
        todo!("populate 'funcs' with pre-declared operators");
        todo!("populate some table, or have some static table to map the operator to the funcidx");
        Program{
            struct_types: Default::default(),
            funcs: Default::default(),
            globals: Default::default(),
            entry_point: Default::default(),
		}
	}
}
