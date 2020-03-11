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
* *
 */

pub fn read_from_file(filename: Option<std::string::String>) -> serde_json::Value {
    let default_filename = "../ast.txt".to_string();
    let contents = std::fs::read_to_string(filename.unwrap_or(default_filename))
        .expect("Something went wrong reading the file");
    let json_contents: serde_json::Value = serde_json::from_str(&contents).unwrap();
    // TODO: [Joel] (Find Semantic meaning for these constants)
    let program_code_excluding_imports = json_contents["body"][11]["expression"]["callee"]["body"]
        ["body"][0]["argument"]["callee"]["body"]["body"][0]["body"][58]
        .clone();
    println!(
        "User entered code section is {}",
        program_code_excluding_imports["body"][0]["body"][0]
    );
    return program_code_excluding_imports;
}

pub fn populate_funcs() {
    // Read from file, extract
    todo!();
}

pub fn populate_func() {
    todo!();
}

pub fn populate_func_params(ast: serde_json::Value) -> Box<[ir::VarType]> {
    // Look through array one. Since function is global, it should not need
    todo!();
}

pub fn populate_func_result(ast: serde_json::Value) -> Option<ir::VarType> {
    // Check the last index of the array to get the result.
    return Some(ir::VarType::Any);
}

pub fn populate_func_locals(ast: serde_json::Value) -> Vec<ir::VarType> {
    // Go to body, loop through declarations to  get all variables
    let func_local = vec![ir::VarType::Any];
    return func_local;
}

pub fn populate_func_statements(ast: serde_json::Value) -> ir::Block {
    // Statements can be If, Expr, return
    todo!();
}

#[cfg(test)]
mod tests {
    use super::*;

    fn setup() {
        let ast = read_from_file(None);
    }

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
        todo!();
    }

    #[test]
    fn it_can_populate_result() {
        tests::setup();
        let ast = read_from_file(None);
        let expected_locals = populate_func_result(ast);
        assert_eq!(expected_locals, Some(ir::VarType::Any));
    }

    #[test]
    fn it_can_populate_func_params() {
        tests::setup();
        todo!();
    }

    #[test]
    fn it_can_populate_func_statements() {
        tests::setup();
        todo!();
    }
}
