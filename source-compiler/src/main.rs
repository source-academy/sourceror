use asttoir;
use ir;
use serde::{Deserialize, Serialize};
use std::vec::Vec;

#[derive(Serialize, Deserialize, Debug)]
struct Point {
    x: i32,
    y: i32,
}

fn main() {
    let point = Point { x: 1, y: 2 };
    let contents = asttoir::ReadFromFile();
    // Convert the Point to a JSON string.
    // let serialized = serde_json::to_string(&point).unwrap();
    let a = ir::Func::new_with_params_and_result(&[ir::VarType::Any], ir::VarType::Any);

    let json_contents: serde_json::Value = serde_json::from_str(&contents).unwrap();

    println!("contents are : {:?}", json_contents);
    println!("Contents are : {}\n", contents);

    // Prints deserialized = Point { x: 1, y: 2 }
}
