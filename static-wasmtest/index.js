/*
import init from "./webloader.js";

async function run() {
    let wasm = await init("test.wasm");
    let ret = await wasm.test(10);
    alert(ret);
}

run();
*/

import * as compiler from "./wasm_test_driver.js";
import load_binary from "./loader.js";

async function run() {
    await compiler.default(); // init
    const binary = compiler.build_tests();
    download(binary);
    let failed_assert = false;
    let left,right;
    let failed_test = false;
    const imports = { platform: {
        assert_fail: (l, r) => {failed_assert = true; alert("Assertion failed: " + l + "==" + r); },
        test_fail: (n) => {failed_test = true; },
    } };
    const instance = await load_binary(binary, imports);
    const result = instance.main();
    if (failed_assert) {
        
    } else if (failed_test) {
        alert("Test failed after " + result.toString() + " tests");
    } else {
        alert("Success: " + result.toString() + " tests passed");
    }
}

function download(buffer) {
    const a = document.createElement("a");
    const file = new Blob([buffer], {});
    const url = URL.createObjectURL(file);
    a.href = url;
    a.download = "tmp";
    document.body.appendChild(a);
    a.click();
}

run();
