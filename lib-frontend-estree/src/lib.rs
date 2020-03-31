mod estree;

use ir;
use projstd::log::Logger;
use projstd::log::Severity;
use serde_json;
use std::collections::HashMap;
use std::fmt;
use std::result::Result;

use estree::*;

#[derive(PartialEq, Eq, Copy, Clone, Debug)]
enum LogKind {
    ESTree,
    UndeclaredVariable,
    SourceRestriction,
}
impl fmt::Display for LogKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LogKind::ESTree => f.write_str("ESTree"),
            LogKind::UndeclaredVariable => f.write_str("UndeclaredVariable"),
            LogKind::SourceRestriction => f.write_str("SourceRestriction"),
        }
    }
}

struct FrontendLogger<L: Logger> {
    logger: L,
}
impl<L: Logger> FrontendLogger<L> {
    fn new(logger: L) -> Self {
        Self { logger: logger }
    }
    fn log<'a>(&self, kind: LogKind, severity: Severity, message: &'a str) {
        self.logger.log(severity, format!("{}: {}", kind, message));
    }
    fn log_string(&self, kind: LogKind, severity: Severity, message: String) {
        self.logger.log(severity, format!("{}: {}", kind, message));
    }
}

#[derive(Copy, Clone)]
enum VariableLocation {
    Closure(usize, usize), // (index in Context::closures, index in Context::closures[index])
    Local(usize, usize),   // (index in Context::locals, index in Context::locals[index])
}

#[derive(Copy, Clone)]
struct VariableInfo {
    pub typeidx: usize,
    pub len: usize,
}

struct Context<L: Logger> {
    ir_program: ir::Program,
    builtin_funcs: [ir::FuncIdx; ir::NUM_BUILTINS as usize],
    closures: Vec<VariableInfo>, // list of sizes of structs from outside this function
    closure_typeidx: Option<usize>,
    locals_offset: usize,      // number of ir params
    locals: Vec<VariableInfo>, // list of sizes of structs from this function, up to and including the current scope
    names: HashMap<String, VariableLocation>,
    names_buf: Vec<(String, Option<VariableLocation>)>, // list of insertions of names.  (name, old_location).
    logger: FrontendLogger<L>,
}

struct ContextRewindPoint {
    locals_len: usize,
    names_buf_len: usize,
}

impl<L: Logger> Context<L> {
    /**
     * Only for rewinding within the same function (rewinds `locals` and `names` only)
     */
    fn get_rewind_point(&self) -> ContextRewindPoint {
        return ContextRewindPoint {
            locals_len: self.locals.len(),
            names_buf_len: self.names_buf.len(),
        };
    }
    fn rewind(&mut self, rewind_point: ContextRewindPoint) {
        while self.locals.len() != rewind_point.locals_len {
            self.locals.pop();
        }
        while self.names_buf.len() != rewind_point.names_buf_len {
            let (name, loc) = self.names_buf.pop().unwrap();
            match loc {
                Some(variable_location) => self.names.insert(name, variable_location),
                None => self.names.remove(&name),
            };
        }
    }
    fn push_local(&mut self, variable_info: VariableInfo) -> usize {
        self.locals.returning_push(variable_info)
    }
    fn push_name(&mut self, name: String, location: VariableLocation) {
        let old = self.names.insert(name.clone(), location);
        self.names_buf.push((name, old));
    }
}

trait ReturningPush<T> {
    fn returning_push(&mut self, value: T) -> usize;
}

impl<T> ReturningPush<T> for Vec<T> {
    fn returning_push(&mut self, value: T) -> usize {
        let ret = self.len();
        self.push(value);
        ret
    }
}

type Error = ();
type ContextResult = Result<(), Error>;
pub type FrontendResult = Result<ir::Program, Error>;

fn write_error<L: Logger>(context: &Context<L>, message: &str) {
    context
        .logger
        .log(LogKind::ESTree, Severity::Error, message);
}
fn write_error_with_kind<L: Logger>(context: &Context<L>, kind: LogKind, message: &str) {
    context.logger.log(kind, Severity::Error, message);
}
fn write_owned_error<L: Logger>(context: &Context<L>, message: String) {
    context
        .logger
        .log_string(LogKind::ESTree, Severity::Error, message);
}
fn write_owned_error_with_kind<L: Logger>(context: &Context<L>, kind: LogKind, message: String) {
    context.logger.log_string(kind, Severity::Error, message);
}

fn make_error<T, L: Logger>(context: &Context<L>, message: &str) -> Result<T, ()> {
    write_error(context, message);
    Err(())
}
fn make_error_with_kind<T, L: Logger>(
    context: &Context<L>,
    kind: LogKind,
    message: &str,
) -> Result<T, ()> {
    write_error_with_kind(context, kind, message);
    Err(())
}
fn make_owned_error<T, L: Logger>(context: &Context<L>, message: String) -> Result<T, ()> {
    write_owned_error(context, message);
    Err(())
}
fn make_owned_error_with_kind<T, L: Logger>(
    context: &Context<L>,
    kind: LogKind,
    message: String,
) -> Result<T, ()> {
    write_owned_error_with_kind(context, kind, message);
    Err(())
}

trait AsIdentifierName {
    fn as_identifier_name(&self) -> Result<&str, ()>;
}

impl AsIdentifierName for Node {
    fn as_identifier_name(&self) -> Result<&str, ()> {
        match &self.kind {
            NodeKind::Identifier(ir_identifier) => Ok(ir_identifier.name.as_str()),
            _ => Err(()),
        }
    }
}

trait IntoIdentifierName {
    fn into_identifier_name(self) -> Result<String, ()>;
}

impl IntoIdentifierName for Node {
    fn into_identifier_name(self) -> Result<String, ()> {
        match self.kind {
            NodeKind::Identifier(ir_identifier) => Ok(ir_identifier.name),
            _ => Err(()),
        }
    }
}

pub fn run_frontend<L: Logger>(estree_str: &str, logger: L) -> FrontendResult {
    let (ir_program, builtin_funcs) = ir::Program::new();
    let es_program: estree::Node = serde_json::from_str(estree_str).unwrap();
    let mut context = Context::new(ir_program, builtin_funcs, FrontendLogger::new(logger));
    context.parse_program(es_program)?;
    Ok(context.build())
}

/*
Each parse_*() function should only ascertain the type of Node when needed.
They should not make any assumptions about the type of Node.
*/
impl<L: Logger> Context<L> {
    fn new(
        ir_program: ir::Program,
        builtin_funcs: [ir::FuncIdx; ir::NUM_BUILTINS as usize],
        logger: FrontendLogger<L>,
    ) -> Self {
        Self {
            ir_program: ir_program,
            builtin_funcs: builtin_funcs,
            closures: Default::default(),
            closure_typeidx: None,
            locals_offset: 0,
            locals: Default::default(),
            names: Default::default(),
            names_buf: Default::default(),
            logger: logger,
        }
    }
    fn build(self) -> ir::Program {
        self.ir_program
    }

    fn parse_program(&mut self, es_node: estree::Node) -> ContextResult {
        /*
        Toplevel func should return the result normally.  The backend should generate the proper convention so that the host can read the return value.
        */
        match es_node.kind {
            estree::NodeKind::Program(es_program) => {
                let es_funcexpr = transform_toplevel(es_program);
                self.parse_function_impl(es_funcexpr)?;
                return Result::Ok(());
            }
            _ => {
                self.logger.log(
                    LogKind::ESTree,
                    Severity::Error,
                    "root node of ESTree must be Program",
                );
                return Result::Err(());
            }
        }
    }

    /**
     * Creates a fresh local context my moving all the Context::locals into Context::closures, updating Context::names, synthesising the closure struct.
     * Returns an ir::Expr whose kind is ExprKind::PrimFunc.
     */
    fn prepare_context_and_parse_function<F: Function>(
        &mut self,
        es_func: F,
    ) -> Result<ir::Expr, Error> {
        unimplemented!();
    }

    /**
     * Adds ir::Function to the context.  The caller should have synthesised the closure struct for this Function, and set up all fields of the context to prepare for this function.
     */
    fn parse_function_impl<F: Function>(&mut self, es_func: F) -> Result<usize, Error> {
        let (es_params, es_body): (Vec<Node>, Box<Node>) = es_func.destructure_params_body();
        let es_param_strings: Box<[String]> = es_params
            .into_iter()
            .map(|ir_node| {
                ir_node.into_identifier_name().map_err(|e| {
                    write_error(self, "Expected ESTree Identifier here");
                    e
                })
            })
            .collect::<Result<Box<[String]>, _>>()?;
        let ir_logical_params: Box<[ir::VarType]> =
            es_param_strings.iter().map(|_| ir::VarType::Any).collect();
        let ir_params: Box<[ir::VarType]> = self
            .closure_typeidx
            .iter()
            .copied()
            .map(|typeidx| ir::VarType::StructT { typeidx: typeidx })
            .chain(ir_logical_params.iter().copied())
            .collect();
        let mut ir_function = ir::Func::new_with_params_and_result(&ir_params, ir::VarType::Any);

        self.locals_offset = ir_params.len();

        // make a new struct and copy all params into it
        let typeidx = self
            .ir_program
            .struct_types
            .returning_push(ir_logical_params.clone());
        ir_function
            .block
            .locals
            .push(ir::VarType::StructT { typeidx: typeidx });
        ir_function.block.statements.push(ir::Statement::Assign {
            target: ir::TargetExpr::Local {
                localidx: self.locals_offset + 0,
                next: None,
            },
            expr: ir::Expr {
                vartype: ir::VarType::StructT { typeidx: typeidx },
                kind: ir::ExprKind::PrimStructT { typeidx: typeidx },
            },
        });

        let rewind_point = self.get_rewind_point();

        let local_index = self.push_local(VariableInfo {
            typeidx: typeidx,
            len: ir_logical_params.len(),
        });
        assert!(local_index == 0);
        for (i, _) in ir_logical_params.iter().enumerate() {
            ir_function.block.statements.push(ir::Statement::Assign {
                target: ir::TargetExpr::Local {
                    localidx: self.locals_offset + local_index,
                    next: Some(Box::new(ir::StructField {
                        typeidx: typeidx,
                        fieldidx: i,
                        next: None,
                    })),
                },
                expr: ir::Expr {
                    vartype: ir::VarType::Any,
                    kind: ir::ExprKind::VarName {
                        source: ir::TargetExpr::Local {
                            localidx: self.closure_typeidx.iter().len() + i, // `closure_typeidx.iter().len()` is used as an offset in case the first parameter of this function is a closure
                            next: None,
                        },
                    },
                },
            });
            self.push_name(
                es_param_strings[i].clone(),
                VariableLocation::Local(local_index, i),
            );
        }

        // encode the body of the function
        match es_body.kind {
            NodeKind::BlockStatement(block_statement) => {
                ir_function.block.statements.push(ir::Statement::Block {
                    block: self.parse_block_statement(block_statement)?,
                });
            }
            _ => {
                self.logger.log(
                    LogKind::ESTree,
                    Severity::Error,
                    "body of ESTree function must be BlockStatement",
                );
                return Result::Err(());
            }
        };

        self.rewind(rewind_point);

        Ok(self.ir_program.funcs.returning_push(ir_function))
    }

    /**
     * Parse block statement, need to create a few local scope.
     */
    fn parse_block_statement(
        &mut self,
        es_block_statement: BlockStatement,
    ) -> Result<ir::Block, Error> {
        let es_declarations_names: Vec<String> = {
            let mut acc: Vec<String> = Vec::new();
            for node in &es_block_statement.body {
                match &node.kind {
                    NodeKind::VariableDeclaration(vardecl) => {
                        if vardecl.kind == "const" {
                            for declarator in &vardecl.declarations {
                                match &declarator.kind {
                                    NodeKind::VariableDeclarator(declarator) => {
                                        acc.push(
                                            declarator
                                                .id
                                                .as_identifier_name()
                                                .map_err(|e| {
                                                    write_error(
                                                        self,
                                                        "Expected ESTree Identifier here",
                                                    );
                                                    e
                                                })?
                                                .to_owned(),
                                        );
                                    }
                                    _ => {
                                        self.logger.log(
                                                LogKind::ESTree,
                                                Severity::Error,
                                                "ESTree VariableDeclaration must only contain VariableDeclarator children",
                                            );
                                        return Err(());
                                    }
                                };
                            }
                        } else {
                            self.logger.log(
                                LogKind::SourceRestriction,
                                Severity::Error,
                                "ESTree VariableDeclaration must be `const`",
                            );
                            return Err(());
                        }
                    }
                    NodeKind::FunctionDeclaration(funcdecl) => {
                        acc.push(
                            funcdecl
                                .id
                                .as_identifier_name()
                                .map_err(|e| {
                                    write_error(self, "Expected ESTree Identifier here");
                                    e
                                })?
                                .to_owned(),
                        );
                    }
                    _ => {}
                }
            }
            acc
        };

        let ir_logical_locals: Vec<ir::VarType> = es_declarations_names
            .iter()
            .map(|_| ir::VarType::Any)
            .collect();

        // make a new struct to store all the (uninitialized) locals
        let typeidx = self
            .ir_program
            .struct_types
            .returning_push(ir_logical_locals.clone().into_boxed_slice());

        let rewind_point = self.get_rewind_point();

        let local_index = self.push_local(VariableInfo {
            typeidx: typeidx,
            len: ir_logical_locals.len(),
        });
        for (i, _) in ir_logical_locals.iter().enumerate() {
            self.push_name(
                es_declarations_names[i].clone(),
                VariableLocation::Local(local_index, i),
            );
        }

        // encode the body of the block
        let ir_statements: Vec<ir::Statement> = es_block_statement
            .body
            .into_iter()
            .map(|node| self.parse_statement_node(node))
            .collect::<Result<Vec<Box<[ir::Statement]>>, _>>()?
            .into_iter()
            .flat_map(|b| b.into_vec())
            .collect();

        self.rewind(rewind_point);

        Ok(ir::Block {
            locals: vec![ir::VarType::StructT { typeidx: typeidx }],
            statements: ir_statements,
        })
    }

    fn parse_statement_node(
        &mut self,
        es_node: estree::Node,
    ) -> Result<Box<[ir::Statement]>, Error> {
        match es_node.kind {
            NodeKind::ExpressionStatement(stmt) => self
                .parse_expression_statement(stmt)
                .map(|x| Box::new([x]) as Box<[ir::Statement]>),
            NodeKind::BlockStatement(stmt) => self
                .parse_block_statement(stmt)
                .map(|ir_block| ir::Statement::Block { block: ir_block })
                .map(|x| Box::new([x]) as Box<[ir::Statement]>),
            NodeKind::ReturnStatement(stmt) => self
                .parse_return_statement(stmt)
                .map(|x| Box::new([x]) as Box<[ir::Statement]>),
            NodeKind::IfStatement(stmt) => self
                .parse_if_statement(stmt)
                .map(|x| Box::new([x]) as Box<[ir::Statement]>),
            NodeKind::FunctionDeclaration(func_declaration) => {
                let name = func_declaration
                    .id
                    .as_identifier_name()
                    .map_err(|e| {
                        write_error(self, "Expected ESTree Identifier here");
                        e
                    })?
                    .to_owned();
                let ir_expr = self.prepare_context_and_parse_function(func_declaration)?;
                self.make_assignment_statement(&name, ir_expr)
                    .map(|x| Box::new([x]) as Box<[ir::Statement]>)
            }
            NodeKind::VariableDeclaration(var_declaration) => {
                // parse variable declaration as if they are assignment statements
                var_declaration
                    .declarations
                    .into_iter()
                    .map(|decl| match decl.kind {
                        NodeKind::VariableDeclarator(VariableDeclarator { id, init }) => {
                            let name = id.into_identifier_name().map_err(|e| {
                                write_error(self, "Expected ESTree Identifier here");
                                e
                            })?;
                            let ir_expr = init.map_or(
                                make_error(self, "variable initializer must be present for Source"),
                                |expr_node| self.parse_expression_node(*expr_node),
                            )?;
                            self.make_assignment_statement(&name, ir_expr)
                        }
                        _ => make_error(
                            self,
                            "children of VariableDeclaration must only be VariableDeclarator",
                        ),
                    })
                    .collect::<Result<Box<[ir::Statement]>, Error>>()
            }
            NodeKind::EmptyStatement(_) => Ok(Box::new([])),
            NodeKind::DebuggerStatement(_)
            | NodeKind::WithStatement(_)
            | NodeKind::LabeledStatement(_)
            | NodeKind::BreakStatement(_)
            | NodeKind::ContinueStatement(_) => make_error_with_kind(
                self,
                LogKind::SourceRestriction,
                "This statement type is not allowed",
            ),
            _ => make_error(self, "Statement node expected in ESTree"),
        }
    }

    /**
     * This function should have additional logic to detect Assignment statements and make ir::Statement::Assign instead of ir::Statement::Expr
     */
    fn parse_expression_statement(
        &mut self,
        es_expression_statement: estree::ExpressionStatement,
    ) -> Result<ir::Statement, Error> {
        let es_expr_node: estree::Node = *es_expression_statement.expression;
        if let NodeKind::AssignmentExpression(AssignmentExpression {
            operator,
            left,
            right,
        }) = es_expr_node.kind
        {
            match operator.as_str() {
                "=" => {
                    let name = left.into_identifier_name().map_err(|e| {
                        write_error(
                            self,
                            "Expected ESTree Identifier at LHS of AssignmentExpression",
                        );
                        e
                    })?;
                    let ir_expr = self.parse_expression_node(*right)?;
                    self.make_assignment_statement(&name, ir_expr)
                }
                other => make_owned_error_with_kind(
                    self,
                    LogKind::SourceRestriction,
                    format!("Compound assignment operator \"{}\" not allowed", other),
                ),
            }
        } else {
            self.parse_expression_node(es_expr_node)
                .map(|expr| ir::Statement::Expr { expr: expr })
        }
    }

    fn parse_return_statement(
        &mut self,
        es_return_statement: estree::ReturnStatement,
    ) -> Result<ir::Statement, Error> {
        match es_return_statement.argument {
            Some(box_node) => self
                .parse_expression_node(*box_node)
                .map(|expr| ir::Statement::Return { expr: expr }),
            _ => {
                self.logger.log(
                    LogKind::SourceRestriction,
                    Severity::Error,
                    "return statement must have a value",
                );
                self.logger.log(
                    LogKind::SourceRestriction,
                    Severity::Hint,
                    "try `return undefined;` instead",
                );
                Err(())
            }
        }
    }

    fn parse_if_statement(
        &mut self,
        es_if_statement: estree::IfStatement,
    ) -> Result<ir::Statement, Error> {
        if let NodeKind::BlockStatement(es_true_block) = es_if_statement.consequent.kind {
            if let Some(es_false_node) = es_if_statement.alternate {
                if let NodeKind::BlockStatement(es_false_block) = es_false_node.kind {
                    Ok(ir::Statement::If {
                        cond: self.parse_expression_node(*es_if_statement.test)?,
                        true_block: self.parse_block_statement(es_true_block)?,
                        false_block: self.parse_block_statement(es_false_block)?,
                    })
                } else {
                    make_error_with_kind(
                        self,
                        LogKind::SourceRestriction,
                        "alternative of IfStatement must be Block",
                    )
                }
            } else {
                make_error_with_kind(
                    self,
                    LogKind::SourceRestriction,
                    "alternative of IfStatement must be present",
                )
            }
        } else {
            make_error_with_kind(
                self,
                LogKind::SourceRestriction,
                "consequent of IfStatement must be Block",
            )
        }
    }

    /**
     * This function parses a node that contains what is logically an ir::Expr (i.e. no assignment statements).
     */
    fn parse_expression_node(&mut self, es_node: estree::Node) -> Result<ir::Expr, Error> {
        unimplemented!();
    }

    fn make_assignment_statement(
        &self,
        name: &str,
        expr: ir::Expr,
    ) -> Result<ir::Statement, Error> {
        let var_loc: VariableLocation = self.names.get(name).map_or(
            make_owned_error_with_kind(
                self,
                LogKind::UndeclaredVariable,
                format!("Name \"{}\" is undeclared", name),
            ),
            |var_loc| Ok(*var_loc),
        )?;
        let ir_targetexpr: ir::TargetExpr = self.make_targetexpr_from_variable_location(var_loc);
        Ok(ir::Statement::Assign {
            target: ir_targetexpr,
            expr: expr,
        })
    }

    fn make_targetexpr_from_variable_location(&self, var_loc: VariableLocation) -> ir::TargetExpr {
        match var_loc {
            VariableLocation::Local(outer_idx, inner_idx) => ir::TargetExpr::Local {
                localidx: self.locals_offset + outer_idx,
                next: Some(Box::new(ir::StructField {
                    typeidx: self.locals[outer_idx].typeidx,
                    fieldidx: inner_idx,
                    next: None,
                })),
            },
            VariableLocation::Closure(outer_idx, inner_idx) => {
                // Note: if control comes here, then we are guaranteed that this function has a closure parameter (and it must be at param#0)
                ir::TargetExpr::Local {
                    localidx: 0, // the closure is at local#0 (i.e. param#0)
                    next: Some(Box::new(ir::StructField {
                        typeidx: self.closure_typeidx.unwrap(),
                        fieldidx: outer_idx,
                        next: Some(Box::new(ir::StructField {
                            typeidx: self.closures[outer_idx].typeidx,
                            fieldidx: inner_idx,
                            next: None,
                        })),
                    })),
                }
            }
        }
    }
}

fn transform_toplevel(es_program: estree::Program) -> ArrowFunctionExpression {
    return ArrowFunctionExpression {
        params: Vec::new(),
        body: Box::new(Node {
            location: None,
            kind: NodeKind::BlockStatement(BlockStatement {
                body: es_program.body,
            }),
        }),
        expression: false,
    };
}
