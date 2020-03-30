use wasmgen;

use backend_wasm;

use projstd;

const SOURCE_CODE: &'static str = r#"
{"type":"Program","start":0,"end":72,"body":[{"type":"FunctionDeclaration","start":0,"end":72,"id":{"type":"Identifier","start":9,"end":10,"name":"f"},"expression":false,"generator":false,"async":false,"params":[{"type":"Identifier","start":11,"end":12,"name":"a"},{"type":"Identifier","start":14,"end":15,"name":"b"}],"body":{"type":"BlockStatement","start":17,"end":72,"body":[{"type":"VariableDeclaration","start":18,"end":30,"declarations":[{"type":"VariableDeclarator","start":24,"end":29,"id":{"type":"Identifier","start":24,"end":25,"name":"x"},"init":{"type":"Literal","start":28,"end":29,"value":4,"raw":"4"}}],"kind":"const"},{"type":"VariableDeclaration","start":30,"end":46,"declarations":[{"type":"VariableDeclarator","start":36,"end":45,"id":{"type":"Identifier","start":36,"end":37,"name":"y"},"init":{"type":"BinaryExpression","start":40,"end":45,"left":{"type":"Identifier","start":40,"end":41,"name":"x"},"operator":"*","right":{"type":"Identifier","start":44,"end":45,"name":"x"}}}],"kind":"const"},{"type":"VariableDeclaration","start":46,"end":62,"declarations":[{"type":"VariableDeclarator","start":52,"end":61,"id":{"type":"Identifier","start":52,"end":53,"name":"z"},"init":{"type":"BinaryExpression","start":56,"end":61,"left":{"type":"Identifier","start":56,"end":57,"name":"y"},"operator":"+","right":{"type":"Literal","start":60,"end":61,"value":5,"raw":"5"}}}],"kind":"const"},{"type":"ReturnStatement","start":62,"end":71,"argument":{"type":"Identifier","start":69,"end":70,"name":"z"}}]}}],"sourceType":"script"}
"#;
/*const SOURCE_CODE: &'static str = r#"
{"type":"Identifier","start":0,"end":72,"name":"str"}
"#;*/
/*const SOURCE_CODE: &'static str = r#"
{"type":"FunctionDeclaration","start":0,"end":72,"id":{"type":"Identifier","start":9,"end":10,"name":"f"},"expression":false,"generator":false,"async":false,"params":[],"body":{"type":"BlockStatement","start":17,"end":72,"body":[]}}
"#;*/

fn severity_value(severity: projstd::log::Severity) -> i32 {
    match severity {
        projstd::log::Severity::Info => 0,
        projstd::log::Severity::Warning => 1,
        projstd::log::Severity::Error => 2,
    }
}

pub struct MainLogger {}
impl projstd::log::Logger for MainLogger {
    fn log(&self, severity: projstd::log::Severity, message: String) {
        print!("Error level {}: {}", severity_value(severity), &message);
    }
}

fn main() {
    {
        use std::io;
        use std::io::prelude::*;
        let _ = io::stdin().read(&mut [0u8]).unwrap();
    }
    use wasmgen::WasmSerialize;
    match frontend_estree::run_frontend(SOURCE_CODE, MainLogger {}) {
        Ok(ir_program) => {
            let wasm_module =
                backend_wasm::run_backend(&ir_program, backend_wasm::Options::default());
            let mut receiver = std::vec::Vec::<u8>::new();
            wasm_module.wasm_serialize(&mut receiver);
            receiver.into_boxed_slice();
        }
        Err(()) => {}
    }
}
