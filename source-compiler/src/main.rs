use projstd::log;

/*const SOURCE_CODE: &'static str = r#"
{"type":"Program","start":0,"end":72,"body":[{"type":"FunctionDeclaration","start":0,"end":72,"id":{"type":"Identifier","start":9,"end":10,"name":"f"},"expression":false,"generator":false,"async":false,"params":[{"type":"Identifier","start":11,"end":12,"name":"a"},{"type":"Identifier","start":14,"end":15,"name":"b"}],"body":{"type":"BlockStatement","start":17,"end":72,"body":[{"type":"VariableDeclaration","start":18,"end":30,"declarations":[{"type":"VariableDeclarator","start":24,"end":29,"id":{"type":"Identifier","start":24,"end":25,"name":"x"},"init":{"type":"Literal","start":28,"end":29,"value":4,"raw":"4"}}],"kind":"const"},{"type":"VariableDeclaration","start":30,"end":46,"declarations":[{"type":"VariableDeclarator","start":36,"end":45,"id":{"type":"Identifier","start":36,"end":37,"name":"y"},"init":{"type":"BinaryExpression","start":40,"end":45,"left":{"type":"Identifier","start":40,"end":41,"name":"x"},"operator":"*","right":{"type":"Identifier","start":44,"end":45,"name":"x"}}}],"kind":"const"},{"type":"VariableDeclaration","start":46,"end":62,"declarations":[{"type":"VariableDeclarator","start":52,"end":61,"id":{"type":"Identifier","start":52,"end":53,"name":"z"},"init":{"type":"BinaryExpression","start":56,"end":61,"left":{"type":"Identifier","start":56,"end":57,"name":"y"},"operator":"+","right":{"type":"Literal","start":60,"end":61,"value":5,"raw":"5"}}}],"kind":"const"},{"type":"ReturnStatement","start":62,"end":71,"argument":{"type":"Identifier","start":69,"end":70,"name":"z"}}]}}],"sourceType":"script"}
"#;*/
const SOURCE_CODE: &'static str = r#"
{"type":"Program","start":0,"end":2,"loc":{"start":{"line":1,"column":0},"end":{"line":1,"column":2}},"body":[{"type":"ExpressionStatement","start":0,"end":2,"loc":{"start":{"line":1,"column":0},"end":{"line":1,"column":2}},"expression":{"type":"Literal","start":0,"end":1,"loc":{"start":{"line":1,"column":0},"end":{"line":1,"column":1}},"value":2,"raw":"2"}}],"sourceType":"module"}
"#;
/*const SOURCE_CODE: &'static str = r#"
{"type":"Identifier","start":0,"end":72,"name":"str"}
"#;*/
/*const SOURCE_CODE: &'static str = r#"
{"type":"FunctionDeclaration","start":0,"end":72,"id":{"type":"Identifier","start":9,"end":10,"name":"f"},"expression":false,"generator":false,"async":false,"params":[],"body":{"type":"BlockStatement","start":17,"end":72,"body":[]}}
"#;*/

const IMPORT_SPEC: &'static str = "";

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

    let source_code: String = r#"{"type":"Program","start":0,"end":67,"loc":{"start":{"line":1,"column":0},"end":{"line":5,"column":0}},"body":[{"type":"VariableDeclaration","start":31,"end":43,"loc":{"start":{"line":3,"column":0},"end":{"line":3,"column":12}},"declarations":[{"type":"VariableDeclarator","start":37,"end":42,"loc":{"start":{"line":3,"column":6},"end":{"line":3,"column":11}},"id":{"type":"Identifier","start":37,"end":38,"loc":{"start":{"line":3,"column":6},"end":{"line":3,"column":7}},"name":"x"},"init":{"type":"Literal","start":41,"end":42,"loc":{"start":{"line":3,"column":10},"end":{"line":3,"column":11}},"value":2,"raw":"2"}}],"kind":"const"},{"type":"VariableDeclaration","start":44,"end":66,"loc":{"start":{"line":4,"column":0},"end":{"line":4,"column":22}},"declarations":[{"type":"VariableDeclarator","start":50,"end":65,"loc":{"start":{"line":4,"column":6},"end":{"line":4,"column":21}},"id":{"type":"Identifier","start":50,"end":51,"loc":{"start":{"line":4,"column":6},"end":{"line":4,"column":7}},"name":"y"},"init":{"type":"BinaryExpression","start":54,"end":65,"loc":{"start":{"line":4,"column":10},"end":{"line":4,"column":21}},"left":{"type":"Identifier","start":54,"end":55,"loc":{"start":{"line":4,"column":10},"end":{"line":4,"column":11}},"name":"x"},"operator":"*","right":{"type":"BinaryExpression","start":59,"end":64,"loc":{"start":{"line":4,"column":15},"end":{"line":4,"column":20}},"left":{"type":"Identifier","start":59,"end":60,"loc":{"start":{"line":4,"column":15},"end":{"line":4,"column":16}},"name":"x"},"operator":"+","right":{"type":"Literal","start":63,"end":64,"loc":{"start":{"line":4,"column":19},"end":{"line":4,"column":20}},"value":1,"raw":"1"}}}}],"kind":"const"}],"sourceType":"module"}"#.to_owned();

    let _: () = futures::executor::block_on((|| async {
        use wasmgen::WasmSerialize;

        //let ir_imports = frontend_estree::parse_imports(import_spec, MainLogger::new(context))?;
        let ir_program =
            frontend_estree::run_frontend(source_code, fetch_dep_proxy, MainLogger {}).await?;
        println!("{:#?}", &ir_program);
        let wasm_module = backend_wasm::run_backend(&ir_program, backend_wasm::Options::default());
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
