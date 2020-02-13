import init from "./loader.js";

async function run() {
    let wasm = await init("test.wasm");
    let ret = await wasm.test(10);
    alert(ret);
}

run();
