use std::default::Default;
use std::option::Option;
use std::vec::Vec;
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
 * *
 * * todo!: For optimisation of eliding storage locals in gc roots during a function call, we need to have information about the lifetime of each local.  This IR should be modified to accomodate it.
 * * * Or we could do some control flow analysis to figure out when the value in a variable is actually useful.
 * * * * E.g. if the current statement never leads to any reads (before being overwritten), then this variable doesn't contain useful information.
 * * * It should also figure out if a variable is read but never written between two function calls, then we can know if we need to pop then push it back to the gc roots, or just tee it from the gc roots into locals.
 * * todo!: Also, functions should be annotated with a flag whether they might do heap allocations.
 */
pub mod error;
// mod primfunc;

// If it stores value `func_idx`, then it refers to imports[func_idx] if (func_idx < imports.len())
// or funcs[func_idx - imports.len()] otherwise.
pub type FuncIdx = usize;

#[derive(Debug)]
pub struct Program {
    pub struct_types: Vec<Box<[VarType]>>, // stores the list of fields of all structs (i.e. objects) in the program (indexed with typeidx)
    pub imports: Box<[Import]>,            // list of imported functions
    pub funcs: Vec<Func>, // list of functions (some will be pre-generated for the pre-declared operators, e.g. + - * / % === and more)
    pub globals: Vec<VarType>, // list of global variables
    pub entry_point: FuncIdx, // index of function to run when the program is started
}

#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub enum VarType {
    Any, // used if we don't know the type contained in the variable.  Most of the time we will use this.  Generates a variant in the output program unless it gets optimised away.
    Unassigned, // unassigned (due to hoisting)
    Undefined,
    Number,
    Boolean,
    String,                     // reference type
    Func,                       // holds a function ptr and a closure
    StructT { typeidx: usize }, // reference type; typeid starts from zero and should be in range [0, object_types.len()).
}
impl Default for VarType {
    fn default() -> Self {
        VarType::Any
    }
}
impl VarType {
    pub fn tag(self) -> i32 {
        match self {
            VarType::Any => panic!(
                "ICE: IR->Wasm: Cannot query tag of Any - tag is the i32 value encoded in an Any"
            ),
            VarType::Unassigned => 0,
            VarType::Undefined => 1,
            VarType::Number => 2,
            VarType::Boolean => 3,
            VarType::String => 4,
            VarType::Func => 5,
            VarType::StructT { typeidx } => (NUM_PRIMITIVE_TAG_TYPES + typeidx) as i32,
        }
    }
}
pub const NUM_PRIMITIVE_TAG_TYPES: usize = 6; // does not include Any

#[derive(Eq, PartialEq, Ord, PartialOrd, Clone, Hash, Debug)]
pub struct Import {
    pub module_name: String,
    pub entity_name: String,
    pub params: Box<[ImportValType]>, // list of function parameters
    pub result: ImportValType,        // return type
}

// Types that can be imported (subset of VarType)
#[derive(Eq, PartialEq, Ord, PartialOrd, Copy, Clone, Hash, Debug)]
pub enum ImportValType {
    Undefined, // compiles into nothing
    Number,    // compiles into f64 parameter
    String, // compiles into i32(ptr) parameter, the host should look into our linear memory to figure out the length and the actual string content.
}

#[derive(Debug)]
pub struct Func {
    pub params: Box<[VarType]>, // list of function parameters (including closure)
    pub result: Option<VarType>, // if `None`, it means that this function never returns (e.g. it guarantees to trap or infinite loop, see the generated runtime error function)
    pub expr: Expr, // body of the function, must either return Void or return the correct result type
    pub signature_filter: Vec<(Box<[VarType]>, VarType, FuncIdx)>, // list of possibly acceptable signatures (param_types, return_type, constrained_func).
                                                                   // If a signature is not in this list, then it will be guaranteed to error;
                                                                   // but converse need not be true.  All entries must be a subtype of `params`.
                                                                   // `constrained_func` (funcidx) is a version of this function that has the specified param_types and return_type of this entry.
                                                                   // (i.e. if the caller can guarantee to have the correct types,
                                                                   // then it can emit code to call the constrained_func instead of the current one)
                                                                   // this list should not contain the entry where all the param types and return type are identical to the current one
                                                                   // (because there is no use for a self-reference)
}

#[derive(Debug)]
pub struct Expr {
    pub vartype: Option<VarType>, // the type set that this Expr is guaranteed to evaluate to (if unknown, just use ValType::Any).  Users of this expression will generate code that only works on this type.  It may also affect the memory layout of the expr.  Use `None` if this function is guaranteed to never return (aka it returns Void).
    pub kind: ExprKind,           // the variant kind of this expression
}

// Represents any lvalue (assignable value)
#[derive(Debug, Clone)]
pub enum TargetExpr {
    // if the field is a struct, then `next` can (but not necessarily) refer to a field inside the struct
    Global {
        globalidx: usize,
        next: Option<Box<StructField>>,
    }, // for targetting a global variable
    Local {
        localidx: usize,
        next: Option<Box<StructField>>,
    }, // for targetting a local variable
}

#[derive(Debug, Clone)]
pub struct StructField {
    pub typeidx: usize,                 // the struct type id (index into struct_types)
    pub fieldidx: usize,                // the index of the referred field in the struct
    pub next: Option<Box<StructField>>, // if the field is a struct, then this can (but not necessarily) refer to a field inside the struct
}

#[derive(Debug)]
pub enum ExprKind {
    PrimUndefined, // also functions as a "no-op"
    PrimNumber {
        val: f64,
    }, // e.g. `2`
    PrimBoolean {
        val: bool,
    }, // e.g. `true`
    PrimString {
        val: String,
    }, // e.g. `"hello world"`, may be placed in a region of memory immune to garbage collection
    PrimStructT {
        typeidx: usize,
    }, // a struct (Any will be set to Unassigned variant; String, Func::closure, StructT will be set to something that the GC can recognise as a "null pointer" for that VarType)
    PrimFunc {
        funcidx: FuncIdx,
        closure: Box<Expr>,
    }, // e.g. `() => {}`.  See encoding for the types that closure can take.
    TypeCast {
        /*
        Encodes a narrowing conversion; effectively does something like this:
        if typeof(test) == expected {
            let fresh = cast<expected>(test); // this line is only present if `create_narrow_local` is true
            <true_expr>
        }
        else {
            <false_expr>
        }

        We expect the TypeCast expression to be recognised and manipulated/eliminated by a type inference engine.
        */
        test: Box<Expr>,
        expected: VarType,
        create_narrow_local: bool,
        true_expr: Box<Expr>, // if `create_narrow_local` == true, will have an additional local implicitly declared and initialized to the one in `test`, but with the correct static type.
        false_expr: Box<Expr>,
    }, // returns bool.  Generated by compiler.  Also in is_number() etc builtins.
    VarName {
        source: TargetExpr,
    }, // something that is located somewhere in memory (i.e. an lvalue), e.g. `x`.  Note: even if `x` has type `any`, the Expr::vartype of this expr can still be more specific, if the compiler can prove its correctness.  i.e. we can load from a variant without checking the tag, if we already proved that the data must be of a particular VarType.
    PrimAppl {
        prim_inst: PrimInst,
        args: Box<[Expr]>,
    }, // primitive operations (e.g. number+number) hardcoded into the compiler.  Expr must have the correct VarType.  Should not be added directly by semantic analyser, because parser is not type-aware.
    Appl {
        func: Box<Expr>,
        args: Box<[Expr]>,
    }, // function application (operators are functions too).  Closure parameter is implicitly prepended to the argument list.
    DirectAppl {
        funcidx: FuncIdx,
        args: Box<[Expr]>,
    }, // direct function application (operators are functions too).  No closure will be prepended.
    Conditional {
        cond: Box<Expr>,
        true_expr: Box<Expr>,
        false_expr: Box<Expr>,
    }, // a ? b : c
    Declaration {
        local: VarType,  // local variable being declared
        expr: Box<Expr>, // expr in which the newly declared local is accessible
    }, // has the type of the `expr` (returns the value of the contained expr)
    Assign {
        target: TargetExpr,
        expr: Box<Expr>,
    }, // Assigment statement (can assign from specific type to Any, but not vice versa); has Undefined type
    Return {
        expr: Box<Expr>,
    }, // Return statmenent; has Void type
    Sequence {
        content: Vec<Expr>,
    }, // returns the value of the last expression, or `undefined` if there are zero expressions
    Trap {
        code: u32,
        location: SourceLocation,
    }, // has Void type
}

// enum of possible primitive functions, used by pre-declared operators, or added during type-checking optimisation
// these functions expect a particular type signature
// this is subject to change
// but the code that converts IR to Wasm needs to use this to generate the appropriate bytecode.

#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub enum PrimInst {
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
    StringEq,
    StringNeq,
    StringGt,
    StringLt,
    StringGe,
    StringLe,
}
pub const NUM_PRIM_INST: u8 = PrimInst::StringLe as u8 + 1;

// enum of pre-declared operators
#[derive(Eq, PartialEq, Copy, Clone, Debug)]
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
pub const NUM_BUILTINS: u8 = Builtin::UnaryMinus as u8 + 1;

/*
Represents a location in a source file.
*/
#[derive(Default, Debug)]
pub struct SourceLocation {
    pub file: u32,
    pub begin: u32,
    pub end: u32,
}

impl Program {
    // Creates an empty Program, and an array of pre-declared operators that can be used.
    // `funcs` will have pre-declared operators, but might also have other primitive functions (e.g. typed version of pre-declared operators).
    // The caller should only use the functions that match the funcidxs specified in the returned array of pre-declared operators.
    // Other things in the `funcs` array should not be used.
    pub fn new_with_imports(imports: Box<[Import]>) -> Program {
        let mut program = Program {
            struct_types: Default::default(),
            imports: imports,
            funcs: Default::default(),
            globals: Default::default(),
            entry_point: Default::default(),
        };
        //primfunc::add_prim_inst(program);
        program
        /*let (funcs, builtin_funcidxs) = primfunc::make_pregenerated_funcs(imports.len());
        let mut ir_imports: Vec<Import> = Vec::new();
        let mut returned_map: HashMap<String, FuncIdx> = HashMap::new();
        for (s, i) in imports {
            let idx: FuncIdx = ir_imports.len();
            ir_imports.push(i);
            returned_map.insert(s, idx);
        }
        (
            Program {
                struct_types: Default::default(),
                imports: ir_imports.into_boxed_slice(),
                funcs: funcs,
                globals: Default::default(),
                entry_point: Default::default(),
            },
            builtin_funcidxs,
            returned_map,
        )*/
    }
}

impl Func {
    pub fn new_with_params_and_result(params: &[VarType], result: VarType) -> Func {
        Func {
            params: params.into(),
            result: Some(result),
            expr: Expr {
                vartype: Some(VarType::Undefined),
                kind: ExprKind::PrimUndefined,
            },
            signature_filter: Default::default(),
        }
    }
    pub fn signature(&self) -> (&[VarType], Option<VarType>) {
        (&self.params, self.result)
    }
}

impl PrimInst {
    // returns the (param_types, result_type)
    // if result is None, then it is guaranteed to never return
    pub fn signature(&self) -> (&'static [VarType], Option<VarType>) {
        match self {
            Self::NumberAdd
            | Self::NumberSub
            | Self::NumberMul
            | Self::NumberDiv
            | Self::NumberRem => (&[VarType::Number, VarType::Number], Some(VarType::Number)),
            Self::NumberEq
            | Self::NumberNeq
            | Self::NumberGt
            | Self::NumberLt
            | Self::NumberGe
            | Self::NumberLe => (&[VarType::Number, VarType::Number], Some(VarType::Boolean)),
            Self::BooleanEq | Self::BooleanNeq | Self::BooleanAnd | Self::BooleanOr => (
                &[VarType::Boolean, VarType::Boolean],
                Some(VarType::Boolean),
            ),
            Self::BooleanNot => (&[VarType::Boolean], Some(VarType::Boolean)),
            Self::NumberNegate => (&[VarType::Number], Some(VarType::Number)),
            Self::StringAdd => (&[VarType::String, VarType::String], Some(VarType::String)),
            Self::StringEq
            | Self::StringNeq
            | Self::StringGt
            | Self::StringLt
            | Self::StringGe
            | Self::StringLe => (&[VarType::String, VarType::String], Some(VarType::Boolean)),
        }
    }
}

impl From<ImportValType> for VarType {
    fn from(x: ImportValType) -> Self {
        match x {
            ImportValType::Undefined => VarType::Undefined,
            ImportValType::Number => VarType::Number,
            ImportValType::String => VarType::String,
        }
    }
}

impl Expr {
    pub fn is_prim_undefined(&self) -> bool {
        if let Expr {
            vartype: Some(VarType::Undefined),
            kind: ExprKind::PrimUndefined,
        } = self
        {
            true
        } else {
            false
        }
    }
}
