/*
import init from "./webloader.js";

async function run() {
    let wasm = await init("test.wasm");
    let ret = await wasm.test(10);
    alert(ret);
}

run();
*/

import * as compiler from "./source_compiler.js";
import load_binary from "./loader.js";

async function run() {
    await compiler.default(); // init
    const binary = compiler.compile("int x=4;");
    const imports = { /* platform support libraries go here */ };
    const instance = await load_binary(binary, imports);
    const result = instance.main();
    alert(result);
}

run();
