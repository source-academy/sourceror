use std::env;
use std::fs;
use serde::{Deserialize, Serialize};

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

pub fn read_from_file(filename: Option<std::string::String>) -> std::string::String {
    let default_filename = "ast.txt".to_string();
    let contents = std::fs::read_to_string(filename.unwrap_or(default_filename)).expect("Something went wrong reading the file");
    let json_contents: serde_json::Value = serde_json::from_str(&contents).unwrap();
    // TODO: [Joel] (Find Semantic meaning for these constants)
    let program_code_excluding_imports = &json_contents["body"][11]["expression"]["callee"]["body"]["body"][0]["argument"]["callee"]["body"]["body"][0]["body"][58];
    println!("User entered code section is {}", program_code_excluding_imports["body"][0]["body"][0]);
    return contents;
}

pub fn populate_funcs() {
    // Read from file, extract
    todo!();
}


pub fn populate_func() {
    populate_func_params();
    populate_func_result();
    populate_func_locals();
    populate_func_statements();
    todo!();
}

pub fn populate_func_params() {
        // Look through array one. Since function is global, it should not need 
        // to be filled 
    todo!();
}


pub fn populate_func_result() {
    // Check the last index of the array to get the result.
}


pub fn populate_func_locals() {
    // Go to body, loop through declarations to  get all variables
    // Check index 1 of the code section, 
    todo!();
}

pub fn populate_func_statements() {
    // Statements can be If, Expr, return
    todo!();
}


#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }

    #[test]
    fn it_can_read_default_file() {
        let ast = read_from_file(None);
        assert_eq!(2 + 3, 4);
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
        // assert_eq!(2 + 3, 4);
    }

    #[test]
    fn it_can_populate_result() {
        let ast = read_from_file(None);
        todo!();
    }

    #[test]
    fn it_can_populate_func_params() {
        let ast = read_from_file(None);
        todo!();
    }

    #[test]
    fn it_can_populate_func_statements() {
        let ast = read_from_file(None);
        todo!();
    }
}
