use ir;

/**
 * The structs functions serve as utility functions to extract information from the AST
 * into a suitable Intermediate Representation.
 * In particular:
 * * Entry points are zero-indexed. Main function will be #0
 * * For now, we will leave everything as type Any
 * Sample function that we will be decoding

* *
* *
 */

pub fn read_from_file(filename: Option<std::string::String>) -> serde_json::Value {
    let default_filename = "../ast.txt".to_string();
    let contents = std::fs::read_to_string(filename.unwrap_or(default_filename))
        .expect("Something went wrong reading the file");
    let json_contents: serde_json::Value = serde_json::from_str(&contents).unwrap();
    // TODO: [Joel] (Find Semantic meaning for these constants)
    // let program_code_excluding_imports = json_contents;
    println!(
        "User entered code section is {}",
        json_contents.clone()
    );
    //["body"][0]["body"][2]
    //program_code_excluding_imports["body"][0]["body"]
    // populate_func_statements(program_code_excluding_imports.clone());

    return json_contents;
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
    // Traverse until you get Function Declaration
    // Look inside function declaration to get Params. Convert param to typ and then append
    // TODO [Joel] (Complete the logic for this function)
    return Box::new([ir::VarType::Any, ir::VarType::Any]);
}

pub fn populate_func_result(ast: serde_json::Value) -> Option<ir::VarType> {
    let unwrapped_arr = ast.as_array().unwrap();
    return Some(ir::VarType::Any);
}

pub fn populate_func_locals(ast: serde_json::Value) -> Vec<ir::VarType> {
    // Loop through the array, look for "arguments" field to get declarations
    // Look for variable Declarator and then Identifier field and then the name.
    // ["id"]["type"] = "Identifier" then check ["id"]["name"]

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
    // Doesn't support nested functions
    // Statements are either Assign, Return, If, Expr, Void
    // Sequentially classify each type of expression based on the node type
    // Return, If, Expr, Void,
    // Expressions can be any one of 
    // type Expression = ThisExpression | Identifier | Literal |
    // ArrayExpression | ObjectExpression | FunctionExpression | ArrowFunctionExpression | ClassExpression |
    // TaggedTemplateExpression | MemberExpression | Super | MetaProperty |
    // NewExpression | CallExpression | UpdateExpression | AwaitExpression | UnaryExpression |
    // BinaryExpression | LogicalExpression | ConditionalExpression |
    // YieldExpression  | SequenceExpression;
    // If it's a function we loop through body and then classify each of the statements
    // If      -> Conditional Expression
    // Assign  -> AssignmentExpression
    // Return  -> Return Expression
    // If it
    let unwrapped_arr = ast["body"].as_array().unwrap();
    let func_statements = Vec::<ir::Statement>::new();

    return func_statements;
}


// Test Case:
// ```
// function f() {
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
        let ast = read_from_file(None);
        let actual_locals = populate_func_locals(ast);
        let expected_locals = vec![ir::VarType::Number, ir::VarType::Number];
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

    #[test]
    fn it_can_populate_func_statements() {
        let ast = read_from_file(None);
        let actual_statements = populate_func_statements(ast);
        // let expected_statements = vec![ir::Block;ir::Exp];
        // assert_eq!(*actual_params, *expected_params);
        todo!();
    }
}
