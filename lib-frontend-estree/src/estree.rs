use serde::Deserialize;
use std::option::Option;

#[derive(Deserialize, Debug)]
pub struct SourceLocation {
    pub source: Option<String>,
    pub start: Position,
    pub end: Position,
}

#[derive(Deserialize, Debug, Copy, Clone)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

#[derive(Deserialize, Debug)]
pub struct Node {
    pub loc: Option<SourceLocation>,
    #[serde(flatten)]
    pub kind: NodeKind,
}

#[derive(Deserialize, Debug)]
#[serde(tag = "type")]
pub enum NodeKind {
    Identifier(Identifier),
    Literal(Literal),
    Program(Program),
    Directive(Directive),
    ExpressionStatement(ExpressionStatement),
    BlockStatement(BlockStatement),
    EmptyStatement(EmptyStatement),
    DebuggerStatement(DebuggerStatement),
    WithStatement(WithStatement),
    ReturnStatement(ReturnStatement),
    LabeledStatement(LabeledStatement),
    BreakStatement(BreakStatement),
    ContinueStatement(ContinueStatement),
    IfStatement(IfStatement),
    FunctionDeclaration(FunctionDeclaration),
    VariableDeclaration(VariableDeclaration),
    VariableDeclarator(VariableDeclarator),
    FunctionExpression(FunctionExpression),
    ArrowFunctionExpression(ArrowFunctionExpression),
    UnaryExpression(UnaryExpression),
    UpdateExpression(UpdateExpression),
    BinaryExpression(BinaryExpression),
    AssignmentExpression(AssignmentExpression),
    LogicalExpression(LogicalExpression),
    ConditionalExpression(ConditionalExpression),
    CallExpression(CallExpression),
    ImportDeclaration(ImportDeclaration),
    ImportSpecifier(ImportSpecifier),
    ImportDefaultSpecifier(ImportDefaultSpecifier),
    ImportNamespaceSpecifier(ImportNamespaceSpecifier),
    ExportNamedDeclaration(ExportNamedDeclaration),
    ExportSpecifier(ExportSpecifier),
}

#[derive(Deserialize, Debug)]
pub struct Identifier {
    pub name: String,
    #[serde(skip)]
    pub prevar: Option<PreVar>, // all names will get a PreVar after pre_parse.
}

#[derive(Deserialize, Debug)]
pub struct Literal {
    pub value: LiteralValue,
}

#[derive(Deserialize, Debug)]
#[serde(untagged)]
pub enum LiteralValue {
    String(String),
    Boolean(bool),
    Null,
    Number(f64),
    RegExp,
    /*#[serde(skip)]
    Undefined,*/
}

#[derive(Deserialize, Debug)]
pub struct Program {
    pub body: Vec<Node>,
    #[serde(skip)]
    pub direct_funcs: Vec<(String, Box<[ir::VarType]>)>, // list of direct functions, populated by pre_parse()
                                                         // todo! populate direct_funcs
}

#[derive(Deserialize, Debug)]
pub struct Directive {
    pub expression: Box<Node>,
    pub directive: String,
}

#[derive(Deserialize, Debug)]
pub struct ExpressionStatement {
    pub expression: Box<Node>,
}

#[derive(Deserialize, Debug)]
pub struct BlockStatement {
    pub body: Vec<Node>,
    #[serde(skip)]
    pub address_taken_vars: Vec<usize>, // list of address-taken vars, populated by pre_parse()
    #[serde(skip)]
    pub direct_funcs: Vec<(String, Box<[ir::VarType]>)>, // list of direct functions, populated by pre_parse()
                                                         // todo! populate direct_funcs
}

/*#[derive(Debug)]
pub struct StmtAttr {
    pub declared_vars: Vec<VarAttr>, // variables declared at this statement
}

#[derive(Debug)]
pub struct VarAttr {
    pub name: String,
    pub address_taken: bool,
}*/

pub type FunctionBody = BlockStatement;

#[derive(Deserialize, Debug)]
pub struct EmptyStatement {}

#[derive(Deserialize, Debug)]
pub struct DebuggerStatement {}

#[derive(Deserialize, Debug)]
pub struct WithStatement {
    pub object: Box<Node>,
    pub body: Box<Node>,
}

#[derive(Deserialize, Debug)]
pub struct ReturnStatement {
    pub argument: Option<Box<Node>>,
}

#[derive(Deserialize, Debug)]
pub struct LabeledStatement {
    pub label: Box<Node>,
    pub body: Box<Node>,
}

#[derive(Deserialize, Debug)]
pub struct BreakStatement {
    pub label: Option<Box<Node>>,
}

#[derive(Deserialize, Debug)]
pub struct ContinueStatement {
    pub label: Option<Box<Node>>,
}

#[derive(Deserialize, Debug)]
pub struct IfStatement {
    pub test: Box<Node>,
    pub consequent: Box<Node>,
    pub alternate: Option<Box<Node>>,
}

#[derive(Deserialize, Debug)]
pub struct FunctionDeclaration {
    pub id: Box<Node>,
    pub params: Vec<Node>,
    pub body: Box<Node>,
    #[serde(skip)]
    pub address_taken_vars: Vec<usize>, // list of address-taken vars, populated by pre_parse()
    #[serde(skip)]
    pub direct_funcs: Vec<(String, Box<[ir::VarType]>)>, // list of direct functions, populated by pre_parse()
    #[serde(skip)]
    pub captured_vars: Vec<VarLocId>,
    #[serde(skip)]
    pub direct_props: Option<(Box<[ir::VarType]>, ir::FuncIdx)>, // only set if this is a direct function, and it is set by post_parse().
}

#[derive(Deserialize, Debug)]
pub struct VariableDeclaration {
    pub kind: String,
    pub declarations: Vec<Node>,
}

#[derive(Deserialize, Debug)]
pub struct VariableDeclarator {
    pub id: Box<Node>,
    pub init: Option<Box<Node>>,
}

#[derive(Deserialize, Debug)]
pub struct FunctionExpression {
    pub params: Vec<Node>,
    pub body: Box<Node>,
}

#[derive(Deserialize, Debug)]
pub struct ArrowFunctionExpression {
    pub params: Vec<Node>,
    pub body: Box<Node>,
    pub expression: bool,
    #[serde(skip)]
    pub address_taken_vars: Vec<usize>, // list of address-taken vars, populated by pre_parse()
    #[serde(skip)]
    pub captured_vars: Vec<VarLocId>, // list of non-global variables captured by the function
}

#[derive(Deserialize, Debug)]
pub struct UnaryExpression {
    pub operator: String,
    pub prefix: bool,
    pub argument: Box<Node>,
}

#[derive(Deserialize, Debug)]
pub struct UpdateExpression {
    pub operator: String,
    pub prefix: bool,
    pub argument: Box<Node>,
}

#[derive(Deserialize, Debug)]
pub struct BinaryExpression {
    pub operator: String,
    pub left: Box<Node>,
    pub right: Box<Node>,
}

#[derive(Deserialize, Debug)]
pub struct AssignmentExpression {
    pub operator: String,
    pub left: Box<Node>,
    pub right: Box<Node>,
}

#[derive(Deserialize, Debug)]
pub struct LogicalExpression {
    pub operator: String,
    pub left: Box<Node>,
    pub right: Box<Node>,
}

#[derive(Deserialize, Debug)]
pub struct ConditionalExpression {
    pub test: Box<Node>,
    pub consequent: Box<Node>,
    pub alternate: Box<Node>,
}

#[derive(Deserialize, Debug)]
pub struct CallExpression {
    pub callee: Box<Node>,
    pub arguments: Vec<Node>,
}

#[derive(Deserialize, Debug)]
pub struct ImportDeclaration {
    pub specifiers: Vec<Node>,
    pub source: Box<Node>,
}

#[derive(Deserialize, Debug)]
pub struct ImportSpecifier {
    pub local: Box<Node>,
    pub source: Box<Node>,
}

#[derive(Deserialize, Debug)]
pub struct ImportDefaultSpecifier {
    pub local: Box<Node>,
}

#[derive(Deserialize, Debug)]
pub struct ImportNamespaceSpecifier {
    pub local: Box<Node>,
}

#[derive(Deserialize, Debug)]
pub struct ExportNamedDeclaration {
    pub declaration: Option<Box<Node>>,
    pub specifiers: Vec<Node>,
    pub source: Option<Box<Node>>,
}

#[derive(Deserialize, Debug)]
pub struct ExportSpecifier {
    pub local: Box<Node>,
    pub exported: Box<Node>,
}

pub trait Function: Scope {
    fn destructure_params_body(self) -> (Vec<Node>, Box<Node>);
    fn params_body_mut(&mut self) -> (&[Node], &mut Node);
    fn captured_vars_mut(&mut self) -> &mut Vec<VarLocId>; // captured variables, except globals
    fn params_mut(&mut self) -> &mut Vec<Node>;
}

impl Function for FunctionDeclaration {
    fn destructure_params_body(self) -> (Vec<Node>, Box<Node>) {
        (self.params, self.body)
    }
    fn params_body_mut(&mut self) -> (&[Node], &mut Node) {
        (&self.params, &mut *self.body)
    }
    fn captured_vars_mut(&mut self) -> &mut Vec<VarLocId> {
        &mut self.captured_vars
    }
    fn params_mut(&mut self) -> &mut Vec<Node> {
        &mut self.params
    }
}

/*impl Function for FunctionExpression {
    fn destructure_params_body(self) -> (Vec<Node>, Box<Node>) {
        return (self.params, self.body);
    }
}*/

impl Function for ArrowFunctionExpression {
    fn destructure_params_body(self) -> (Vec<Node>, Box<Node>) {
        (self.params, self.body)
    }
    fn params_body_mut(&mut self) -> (&[Node], &mut Node) {
        (&self.params, &mut *self.body)
    }
    fn captured_vars_mut(&mut self) -> &mut Vec<VarLocId> {
        &mut self.captured_vars
    }
    fn params_mut(&mut self) -> &mut Vec<Node> {
        &mut self.params
    }
}

pub trait Scope {
    fn address_taken_vars_mut(&mut self) -> &mut Vec<usize>;
    fn destructure(self) -> (Vec<Node>, Vec<usize>, Vec<(String, Box<[ir::VarType]>)>);
}

impl Scope for BlockStatement {
    fn address_taken_vars_mut(&mut self) -> &mut Vec<usize> {
        &mut self.address_taken_vars
    }
    fn destructure(self) -> (Vec<Node>, Vec<usize>, Vec<(String, Box<[ir::VarType]>)>) {
        (self.body, self.address_taken_vars, self.direct_funcs)
    }
}

impl Scope for FunctionDeclaration {
    fn address_taken_vars_mut(&mut self) -> &mut Vec<usize> {
        &mut self.address_taken_vars
    }
    fn destructure(self) -> (Vec<Node>, Vec<usize>, Vec<(String, Box<[ir::VarType]>)>) {
        (
            if let NodeKind::BlockStatement(block) = (*self.body).kind {
                block.body
            } else {
                panic!();
            },
            self.address_taken_vars,
            self.direct_funcs,
        )
    }
}

impl Scope for ArrowFunctionExpression {
    fn address_taken_vars_mut(&mut self) -> &mut Vec<usize> {
        &mut self.address_taken_vars
    }
    fn destructure(self) -> (Vec<Node>, Vec<usize>, Vec<(String, Box<[ir::VarType]>)>) {
        // self.body can be either a Block or an expression
        (
            if let NodeKind::BlockStatement(block) = (*self.body).kind {
                block.body
            } else {
                vec![*self.body]
            },
            self.address_taken_vars,
            Vec::new(),
        )
    }
}

impl Program {
    pub fn destructure(self) -> (Vec<Node>, Vec<(String, Box<[ir::VarType]>)>) {
        (self.body, self.direct_funcs)
    }
}

#[derive(PartialEq, Eq, Copy, Clone, PartialOrd, Ord, Hash, Debug)]
pub struct VarLocId {
    pub depth: usize, // depth of 0 means it is a global
    pub index: usize,
}

#[derive(PartialEq, Eq, Copy, Clone, PartialOrd, Ord, Hash, Debug)]
pub enum PreVar {
    Target(VarLocId),
    Direct,
}
