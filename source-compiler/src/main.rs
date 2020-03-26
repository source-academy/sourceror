use asttoir;
use ir;
use serde::{Deserialize, Serialize};
use std::vec::Vec;

fn main() {
    let contents = asttoir::read_from_file(None);
    let func = asttoir::populate_func(contents);
    println!("{:?}", func);
}
