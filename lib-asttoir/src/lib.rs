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
static FILENAME: &str = "ast.txt";

pub fn ReadFromFile() -> std::string::String {
    println!("In file {}", FILENAME);
    let contents = fs::read_to_string(FILENAME).expect("Something went wrong reading the file");
    let json_contents: serde_json::Value = serde_json::from_str(&contents).unwrap();
    // TODO: [Joel] (Find Semantic meaning for these constants)
    let program_code_excl_imports = &json_contents["body"][11]["expression"]["callee"]["body"]["body"][0]["argument"]["callee"]["body"]["body"][0]["body"][58];
    println!("User entered code section is {}", program_code_excl_imports["body"][0]["body"]);
    return contents;
}

pub fn PopulateFuncs() {
    todo!();
}

pub fn PopulateFunc() {
    PopulateParams::PopulateFuncParams();
    PopulateFuncResult::PopulateFuncResult();
    PopulateFuncLocals::PopulateFuncLocals();
    PopulateFuncStatements::PopulateFuncStatements();
    todo!();
}
mod PopulateParams {
    pub fn PopulateFuncParams() {
        // Using the JSON Passed in we want to populate the Func struct
        todo!();
    }
}

mod PopulateFuncResult {
    pub fn PopulateFuncResult() {
        // Using the struct passed in we want to populate the struct
        todo!();
    }
}

mod PopulateFuncLocals {
    pub fn PopulateFuncLocals() {
        // Populate the local variables based on struct passed in
        todo!();
    }

}

mod PopulateFuncStatements {
    pub fn PopulateFuncStatements() {
        // Populate Vector of statements
        todo!();
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
