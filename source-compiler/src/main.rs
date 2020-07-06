use projstd::log;

/*const SOURCE_CODE: &'static str = r#"
{"type":"Program","start":0,"end":72,"body":[{"type":"FunctionDeclaration","start":0,"end":72,"id":{"type":"Identifier","start":9,"end":10,"name":"f"},"expression":false,"generator":false,"async":false,"params":[{"type":"Identifier","start":11,"end":12,"name":"a"},{"type":"Identifier","start":14,"end":15,"name":"b"}],"body":{"type":"BlockStatement","start":17,"end":72,"body":[{"type":"VariableDeclaration","start":18,"end":30,"declarations":[{"type":"VariableDeclarator","start":24,"end":29,"id":{"type":"Identifier","start":24,"end":25,"name":"x"},"init":{"type":"Literal","start":28,"end":29,"value":4,"raw":"4"}}],"kind":"const"},{"type":"VariableDeclaration","start":30,"end":46,"declarations":[{"type":"VariableDeclarator","start":36,"end":45,"id":{"type":"Identifier","start":36,"end":37,"name":"y"},"init":{"type":"BinaryExpression","start":40,"end":45,"left":{"type":"Identifier","start":40,"end":41,"name":"x"},"operator":"*","right":{"type":"Identifier","start":44,"end":45,"name":"x"}}}],"kind":"const"},{"type":"VariableDeclaration","start":46,"end":62,"declarations":[{"type":"VariableDeclarator","start":52,"end":61,"id":{"type":"Identifier","start":52,"end":53,"name":"z"},"init":{"type":"BinaryExpression","start":56,"end":61,"left":{"type":"Identifier","start":56,"end":57,"name":"y"},"operator":"+","right":{"type":"Literal","start":60,"end":61,"value":5,"raw":"5"}}}],"kind":"const"},{"type":"ReturnStatement","start":62,"end":71,"argument":{"type":"Identifier","start":69,"end":70,"name":"z"}}]}}],"sourceType":"script"}
"#;*/
const SOURCE_CODE: &'static str = r#"
{"type":"Program","start":0,"end":92,"loc":{"start":{"line":1,"column":0},"end":{"line":5,"column":12}},"body":[{"type":"FunctionDeclaration","start":0,"end":79,"loc":{"start":{"line":1,"column":0},"end":{"line":4,"column":1}},"id":{"type":"Identifier","start":9,"end":10,"loc":{"start":{"line":1,"column":9},"end":{"line":1,"column":10}},"name":"f"},"expression":false,"generator":false,"params":[{"type":"Identifier","start":11,"end":12,"loc":{"start":{"line":1,"column":11},"end":{"line":1,"column":12}},"name":"x"}],"body":{"type":"BlockStatement","start":14,"end":79,"loc":{"start":{"line":1,"column":14},"end":{"line":4,"column":1}},"body":[{"type":"ReturnStatement","start":68,"end":77,"loc":{"start":{"line":3,"column":4},"end":{"line":3,"column":13}},"argument":{"type":"Literal","start":75,"end":76,"loc":{"start":{"line":3,"column":11},"end":{"line":3,"column":12}},"value":2,"raw":"2"}}]}},{"type":"ExpressionStatement","start":80,"end":92,"loc":{"start":{"line":5,"column":0},"end":{"line":5,"column":12}},"expression":{"type":"BinaryExpression","start":80,"end":91,"loc":{"start":{"line":5,"column":0},"end":{"line":5,"column":11}},"left":{"type":"CallExpression","start":80,"end":84,"loc":{"start":{"line":5,"column":0},"end":{"line":5,"column":4}},"callee":{"type":"Identifier","start":80,"end":81,"loc":{"start":{"line":5,"column":0},"end":{"line":5,"column":1}},"name":"f"},"arguments":[{"type":"Literal","start":82,"end":83,"loc":{"start":{"line":5,"column":2},"end":{"line":5,"column":3}},"value":1,"raw":"1"}]},"operator":"+","right":{"type":"CallExpression","start":87,"end":91,"loc":{"start":{"line":5,"column":7},"end":{"line":5,"column":11}},"callee":{"type":"Identifier","start":87,"end":88,"loc":{"start":{"line":5,"column":7},"end":{"line":5,"column":8}},"name":"f"},"arguments":[{"type":"Literal","start":89,"end":90,"loc":{"start":{"line":5,"column":9},"end":{"line":5,"column":10}},"value":1,"raw":"1"}]}}}],"sourceType":"module"}
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

    let _: () = futures::executor::block_on((|| async {
        use wasmgen::WasmSerialize;

        //let ir_imports = frontend_estree::parse_imports(import_spec, MainLogger::new(context))?;
        let ir_program =
            frontend_estree::run_frontend(SOURCE_CODE.to_owned(), fetch_dep_proxy, MainLogger {})
                .await?;
        let ir_program_opt = ir::opt::optimize(ir_program);
        println!("{:#?}", &ir_program_opt);
        println!("{:#?}", &ir_program_opt.funcs[26]);
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
