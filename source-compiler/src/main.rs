use projstd::log;

/*const SOURCE_CODE: &'static str = r#"
{"type":"Program","start":0,"end":72,"body":[{"type":"FunctionDeclaration","start":0,"end":72,"id":{"type":"Identifier","start":9,"end":10,"name":"f"},"expression":false,"generator":false,"async":false,"params":[{"type":"Identifier","start":11,"end":12,"name":"a"},{"type":"Identifier","start":14,"end":15,"name":"b"}],"body":{"type":"BlockStatement","start":17,"end":72,"body":[{"type":"VariableDeclaration","start":18,"end":30,"declarations":[{"type":"VariableDeclarator","start":24,"end":29,"id":{"type":"Identifier","start":24,"end":25,"name":"x"},"init":{"type":"Literal","start":28,"end":29,"value":4,"raw":"4"}}],"kind":"const"},{"type":"VariableDeclaration","start":30,"end":46,"declarations":[{"type":"VariableDeclarator","start":36,"end":45,"id":{"type":"Identifier","start":36,"end":37,"name":"y"},"init":{"type":"BinaryExpression","start":40,"end":45,"left":{"type":"Identifier","start":40,"end":41,"name":"x"},"operator":"*","right":{"type":"Identifier","start":44,"end":45,"name":"x"}}}],"kind":"const"},{"type":"VariableDeclaration","start":46,"end":62,"declarations":[{"type":"VariableDeclarator","start":52,"end":61,"id":{"type":"Identifier","start":52,"end":53,"name":"z"},"init":{"type":"BinaryExpression","start":56,"end":61,"left":{"type":"Identifier","start":56,"end":57,"name":"y"},"operator":"+","right":{"type":"Literal","start":60,"end":61,"value":5,"raw":"5"}}}],"kind":"const"},{"type":"ReturnStatement","start":62,"end":71,"argument":{"type":"Identifier","start":69,"end":70,"name":"z"}}]}}],"sourceType":"script"}
"#;*/
const SOURCE_CODE: &'static str = r#"
{"type":"Program","start":0,"end":173,"loc":{"start":{"line":1,"column":0},"end":{"line":11,"column":6}},"body":[{"type":"VariableDeclaration","start":0,"end":12,"loc":{"start":{"line":1,"column":0},"end":{"line":1,"column":12}},"declarations":[{"type":"VariableDeclarator","start":6,"end":11,"loc":{"start":{"line":1,"column":6},"end":{"line":1,"column":11}},"id":{"type":"Identifier","start":6,"end":7,"loc":{"start":{"line":1,"column":6},"end":{"line":1,"column":7}},"name":"d"},"init":{"type":"Identifier","start":10,"end":11,"loc":{"start":{"line":1,"column":10},"end":{"line":1,"column":11}},"name":"e"}}],"kind":"const"}],"sourceType":"module"}
"#;
/*const SOURCE_CODE: &'static str = r#"
{"type":"Identifier","start":0,"end":72,"name":"str"}
"#;*/
/*const SOURCE_CODE: &'static str = r#"
{"type":"FunctionDeclaration","start":0,"end":72,"id":{"type":"Identifier","start":9,"end":10,"name":"f"},"expression":false,"generator":false,"async":false,"params":[],"body":{"type":"BlockStatement","start":17,"end":72,"body":[]}}
"#;*/

#[derive(Copy, Clone)]
pub struct MainLogger {}
impl log::Logger for MainLogger {
    fn log(&self, message: String) {
        print!("logger: {}", message);
    }
}

async fn fetch_dep_proxy(name: String) -> Option<String> {
    None
}

fn main() {
    {
        use std::io;
        use std::io::prelude::*;
        let _ = io::stdin().read(&mut [0u8]).unwrap();
    }

    let _: () = futures::executor::block_on((|| async {
        use wasmgen::WasmSerialize;

        //let ir_imports = frontend_estree::parse_imports(import_spec, MainLogger::new(context))?;
        let ir_program =
            frontend_estree::run_frontend(SOURCE_CODE.to_owned(), fetch_dep_proxy, MainLogger {})
                .await?;
        let ir_program_opt = ir::opt::optimize_mandatory(ir_program);
        println!("{:#?}", &ir_program_opt);
        {
            use std::io::prelude::*;
            let mut file = std::fs::File::create("out.ir").unwrap();
            file.write_all(format!("{:#?}", &ir_program_opt).as_bytes())
                .unwrap();
        }
        let wasm_module =
            backend_wasm::run_backend(&ir_program_opt, backend_wasm::Options::default());
        let mut receiver = std::vec::Vec::<u8>::new();
        wasm_module.wasm_serialize(&mut receiver);
        {
            use std::io::prelude::*;
            let mut file = std::fs::File::create("out.wasm").unwrap();
            file.write_all(&receiver).unwrap();
        }
        Ok(())
    })())
    .unwrap_or_else(|_: ()| panic!("Frontend errored out"));
}
