import init, { greet } from "./hello_wasm.js";

async function run() {
    await init();
    await greet("WebAssembly");
}

run();
