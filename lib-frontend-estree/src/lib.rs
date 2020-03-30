mod estree;

use ir;
use projstd::log::Logger;
use projstd::log::Severity;
use serde_json;
use std::fmt;
use std::result::Result;

use estree::*;

#[derive(PartialEq, Eq, Copy, Clone, Debug)]
enum LogKind {
    ESTree,
    UndeclaredVariable,
}
impl fmt::Display for LogKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ESTree => f.write_str("ESTree"),
            UndeclaredVariable => f.write_str("undeclared variable"),
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
}

struct Context<L: Logger> {
    ir_program: ir::Program,
    builtin_funcs: [ir::FuncIdx; ir::NUM_BUILTINS as usize],
    logger: FrontendLogger<L>,
}

type Error = ();
type ContextResult = Result<(), Error>;
pub type FrontendResult = Result<ir::Program, Error>;

pub fn run_frontend<L: Logger>(estree_str: &str, logger: L) -> FrontendResult {
    let (ir_program, builtin_funcs) = ir::Program::new();
    let es_program: estree::Node = serde_json::from_str(estree_str).unwrap();
    let mut context = Context::new(ir_program, builtin_funcs, FrontendLogger::new(logger));
    context.parse_program(es_program);
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
                let es_func = transform_toplevel(es_program);
                return Result::Ok(());
            }
            _ => {
                self.logger.log(
                    LogKind::ESTree,
                    Severity::Error,
                    "root node of ESTree should be Program",
                );
                return Result::Err(());
            }
        }
    }
}

fn transform_toplevel(es_program: estree::Program) -> FunctionExpression {
    return FunctionExpression {
        id: None,
        params: Vec::new(),
        body: Box::new(Node {
            location: None,
            kind: NodeKind::BlockStatement(BlockStatement {
                body: es_program.body,
            }),
        }),
    };
}
