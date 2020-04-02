#[macro_use]
extern crate lazy_static;

use ir;
use std::collections::HashMap;
use std::sync::Mutex;

lazy_static! {
    static ref IS_EXPRESSION: HashMap<&'static str, bool> = {
        let mut map = HashMap::new();
        map.insert("ThisExpression", true);
        map.insert("Identifier", true);
        map.insert("Literal", true);
        map.insert("ArrayExpression", true);
        map.insert("ObjectExpression", true);
        map.insert("FunctionExpression", true);
        map.insert("ArrowFunctionExpression", true);
        map.insert("TaggedTemplateExpression", true);
        map.insert("MemberExpression", true);
        map.insert("TaggedTemplateExpression", true);
        map.insert("Super", true);
        map.insert("MetaProperty", true);
        map.insert("NewExpression", true);
        map.insert("TaggedTemplateExpression", true);
        map.insert("CallExpression", true);
        map.insert("UpdateExpression", true);
        map.insert("AwaitExpression", true);
        map.insert("UnaryExpression", true);
        map.insert("BinaryExpression", true);
        map.insert("ConditionalExpression", true);
        map.insert("YieldExpression", true);
        map.insert("SequenceExpression", true);

        map
    };
}

/**
 * The structs functions serve as utility functions to extract information from the AST
 * into a suitable Intermediate Representation.
 * In particular:
 * * Entry points are zero-indexed. Main function will be #0
 * * For now, we will leave everything as type Any
 * Sample function that we will be decoding
 *
* *
* *
 */

// General TODOs:
// 1. Make all functions recursive
// 2. Change everything to be idiomatic
// 3. Remove all hardcoded vars

pub fn read_from_file(filename: Option<std::string::String>) -> serde_json::Value {
    let default_filename = "./ast.txt".to_string();
    let contents = std::fs::read_to_string(filename.unwrap_or(default_filename))
        .expect("Something went wrong reading the file");
    let json_contents: serde_json::Value = serde_json::from_str(&contents).unwrap();
    return json_contents;
}

pub fn populate_funcs() {
    todo!();
}

pub fn populate_func(ast: serde_json::Value) -> ir::Func {
    let mut VAR_TO_IDX: HashMap<String, usize> = HashMap::new();
    let func: ir::Func = ir::Func {
        params: populate_func_params(ast.clone()),
        result: populate_func_result(ast.clone()),
        locals: populate_func_locals(ast.clone(), &mut VAR_TO_IDX),
        statements: populate_func_statements(ast.clone(), VAR_TO_IDX),
        signature_filter: vec![],
    };

    return func;
}

pub fn populate_func_params(ast: serde_json::Value) -> Box<[ir::VarType]> {
    let mut func_params = Vec::<ir::VarType>::new();
    let prog_body = ast["body"][0].clone();
    if prog_body["type"] == "FunctionDeclaration" {
        for i in prog_body["params"].as_array().unwrap() {
            func_params.push(ir::VarType::Any);
        }
    }
    return func_params.into_boxed_slice();
}

pub fn populate_func_result(ast: serde_json::Value) -> Option<ir::VarType> {
    return Some(ir::VarType::Any);
}

pub fn populate_func_locals(
    ast: serde_json::Value,
    VAR_TO_IDX: &mut HashMap<String, usize>,
) -> Vec<ir::VarType> {
    let mut func_locals = Vec::<ir::VarType>::new();
    let prog_body = ast["body"][0].clone();
    let prog_locals = prog_body["body"]["body"][1]["declarations"].clone();
    let mut var_index = 0;

    for j in 0..prog_body["body"]["body"].as_array().unwrap().len() {
        if prog_body["body"]["body"][j]["type"] == "VariableDeclaration" {
            let var_name = prog_body["body"]["body"][j]["declarations"][0]["id"]["name"]
                .as_str()
                .unwrap()
                .to_string();
            VAR_TO_IDX.insert(var_name, var_index);
            func_locals.push(ir::VarType::Any);
            var_index += 1
        }
    }
    return func_locals;
}

pub fn populate_func_statements(
    ast: serde_json::Value,
    VAR_TO_IDX: HashMap<String, usize>,
) -> ir::Block {
    // Doesn't support nested functions
    // Statements are either Assign, Return, If, Expr, Void
    // If      -> Conditional Expression
    // Assign  -> AssignmentExpression
    // Return  -> Return Expression
    let mut func_statements = Vec::<ir::Statement>::new();
    let mut var_index: usize = 0;

    for i in 0..ast["body"][0]["body"]["body"].as_array().unwrap().len() {
        let statement = ast["body"][0]["body"]["body"][i].clone();
        let statement_type = statement["type"].clone();
        if statement_type == "ConditionalExpression" {
        } else if statement_type == "VariableDeclaration" {
            println!(
                "the statement is {}",
                statement["declarations"][0]["id"]["name"]
            );
            let assignment_statement = ir::Statement::Assign {
                target: ir::TargetExpr::Local {
                    localidx: 0,
                    next: None,
                },
                expr: ir::Expr {
                    vartype: ir::VarType::Any,
                    kind: ir::ExprKind::DirectAppl {
                        funcidx: 1,
                        args: Box::new([
                            ir::Expr {
                                vartype: ir::VarType::Any,
                                kind: ir::ExprKind::VarName {
                                    source: ir::TargetExpr::Local {
                                        localidx: var_index,
                                        next: None,
                                    },
                                },
                            },
                            ir::Expr {
                                vartype: ir::VarType::Any,
                                kind: ir::ExprKind::VarName {
                                    source: ir::TargetExpr::Local {
                                        localidx: var_index,
                                        next: None,
                                    },
                                },
                            },
                        ]),
                    },
                },
            };
            func_statements.push(assignment_statement);
        } else if statement_type == "ReturnStatement" {
            let var_name = statement["argument"]["name"].as_str().unwrap();
            let return_statement = ir::Statement::Return {
                expr: ir::Expr {
                    vartype: ir::VarType::Any,
                    kind: ir::ExprKind::VarName {
                        source: ir::TargetExpr::Local {
                            localidx: VAR_TO_IDX[var_name],
                            next: None,
                        },
                    },
                },
            };
            func_statements.push(return_statement);
        }
    }
    return func_statements;
}

// Test Case:
// ```
// function f(a, b) {
// const x = 4;
// const y = x * x;
// const z = y + 5;
// return z;
// }
// ```

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_can_populate_locals() {
        let VAR_TO_IDX: HashMap<String, usize> = HashMap::new();
        let ast = read_from_file(None);
        let actual_locals = populate_func_locals(ast, VAR_TO_IDX);
        let expected_locals = vec![ir::VarType::Any, ir::VarType::Any, ir::VarType::Any];
        assert_eq!(expected_locals, actual_locals);
    }

    #[test]
    fn it_can_populate_result() {
        let ast = read_from_file(None);
        let actual_result = populate_func_result(ast);
        let expected_result = Some(ir::VarType::Any);
        assert_eq!(actual_result, expected_result);
    }

    #[test]
    fn it_can_populate_func_params() {
        let ast = read_from_file(None);
        let actual_params = populate_func_params(ast);
        let expected_params = Box::new([ir::VarType::Any, ir::VarType::Any]);
        assert_eq!(*actual_params, *expected_params);
    }

    // #[test]
    // fn it_can_populate_func_statements() {
    //     let ast = read_from_file(None);
    //     let actual_statements = populate_func_statements(ast);
    //     // let expected_statements = vec![ir::Block;ir::Exp];
    //     // assert_eq!(*actual_params, *expected_params);
    //     todo!();
    // }
}
