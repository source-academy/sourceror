use ir;
use serde::{Deserialize, Serialize};
use std::env;
use std::fs;
/**
 * The structs functions serve as utility functions to extract information from the AST
 * into a suitable Intermediate Representation.
 * In particular:
 * * Entry points are zero-indexed. Main function will be #0
 * * For now, we will leave everything as type Any
 * Sample function that we will be decoding
```
function f() {
const x = 4;
const y = x * x;
const z = y + 5;
return z;
}
```
* *
* *
 */

pub fn read_from_file(filename: Option<std::string::String>) -> serde_json::Value {
    let default_filename = "./ast.txt".to_string();
    let contents = std::fs::read_to_string(filename.unwrap_or(default_filename))
        .expect("Something went wrong reading the file");
    let json_contents: serde_json::Value = serde_json::from_str(&contents).unwrap();
    // TODO: [Joel] (Find Semantic meaning for these constants)
    let program_code_excluding_imports = json_contents["body"][11]["expression"]["callee"]["body"]
        ["body"][0]["argument"]["callee"]["body"]["body"][0]["body"][58]
        .clone();
    println!(
        "User entered code section is {}",
        program_code_excluding_imports["body"][0]["body"][2]
    );
    populate_func_statements(program_code_excluding_imports["body"][0]["body"].clone());

    return program_code_excluding_imports;
}

pub fn populate_funcs() {
    todo!();
}

pub fn populate_func(ast: serde_json::Value) -> ir::Func {
    let func: ir::Func = ir::Func {
        params: populate_func_params(ast.clone()),
        result: populate_func_result(ast.clone()),
        locals: populate_func_locals(ast.clone()),
        statements: populate_func_statements(ast.clone()),
        signature_filter: vec![],
    };
    return func;
}

pub fn populate_func_params(ast: serde_json::Value) -> Box<[ir::VarType]> {
    // TODO [Joel] (Complete the logic for this function)
    return Box::new([]);
}

pub fn populate_func_result(ast: serde_json::Value) -> Option<ir::VarType> {
    let unwrapped_arr = ast.as_array().unwrap();
    for line in unwrapped_arr {
        println!("This value is {}", line);
    }
    return Some(ir::VarType::Any);
}

pub fn populate_func_locals(ast: serde_json::Value) -> Vec<ir::VarType> {
    // Loop through the array, look for "arguments" field to get declarations
    let unwrapped_arr = ast.as_array().unwrap();
    let mut func_local = Vec::<ir::VarType>::new();
    for line in unwrapped_arr {
        if line.get("declarations") != None {
            println!(
                "The value is {}",
                line.get("declarations").unwrap()[0]["id"]["name"]
            );
            func_local.push(ir::VarType::Any);
        }
    }

    return func_local;
}

pub fn populate_func_statements(ast: serde_json::Value) -> ir::Block {
    // Statements are either Assign, Return, If, Expr, Void
    // Sequentially classify each type of expression based on the node type
    let unwrapped_arr = ast.as_array().unwrap();
    let func_statements = Vec::<ir::Statement>::new();

    for line in unwrapped_arr {
        println!("func_statement is :{}", line);
    }
    return func_statements;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_can_read_default_file() {
        let ast = read_from_file(None);
        let expected = vec![ir::VarType::Any];
        let result = populate_func_locals(ast);
        assert_eq!(expected, result);
    }

    #[test]
    fn it_can_populate_funcs() {
        let ast = read_from_file(None);
        todo!();
    }

    #[test]
    fn it_can_populate_locals() {
        let ast = read_from_file(None);
        let expected_params = populate_func_params(ast);
        // assert_eq!(expected_params, Box::new([ir::VarType::Number, ir::VarType::Number,ir::VarType::Number]));
    }

    #[test]
    fn it_can_populate_result() {
        let ast = read_from_file(None);
        let expected_locals = populate_func_result(ast);

        assert_eq!(expected_locals, Some(ir::VarType::Any));
    }

    #[test]
    fn it_can_populate_func_params() {
        let ast = read_from_file(None);
        let expected_params = populate_func_params(ast);
        // assert_eq!(expected_params, Box::new([ir::VarType::Any, ir::VarType::Any]));
    }

    #[test]
    fn it_can_populate_func_statements() {
        todo!();
    }
}
