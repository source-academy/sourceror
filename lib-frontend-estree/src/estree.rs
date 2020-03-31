use serde::Deserialize;
use std::option::Option;

#[derive(Deserialize, Debug)]
pub struct SourceLocation {
    pub source: Option<String>,
    pub start: Position,
    pub end: Position,
}

#[derive(Deserialize, Debug)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

#[derive(Deserialize, Debug)]
pub struct Node {
    pub location: Option<SourceLocation>,
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
}

#[derive(Deserialize, Debug)]
pub struct Identifier {
    pub name: String,
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
}

#[derive(Deserialize, Debug)]
pub struct Program {
    pub body: Vec<Node>,
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
}

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

pub trait Function {
    fn destructure_params_body(self) -> (Vec<Node>, Box<Node>);
}

impl Function for FunctionDeclaration {
    fn destructure_params_body(self) -> (Vec<Node>, Box<Node>) {
        return (self.params, self.body);
    }
}

impl Function for FunctionExpression {
    fn destructure_params_body(self) -> (Vec<Node>, Box<Node>) {
        return (self.params, self.body);
    }
}

impl Function for ArrowFunctionExpression {
    fn destructure_params_body(self) -> (Vec<Node>, Box<Node>) {
        return (self.params, self.body);
    }
}
