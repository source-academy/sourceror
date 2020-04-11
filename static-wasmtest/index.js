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
    const binaries = [];
    window.add_test = (binary) => {
        binaries.push(binary);
    }
    compiler.build_tests();
    //download(binary);
    let test_index = 0;
    let num_asserts = 0;
    for (const binary of binaries) {
        const imports = {
            platform: {
                assert_fail: (l, r, n) => { alert("Assertion " + (n+1) + " failed on test " + (test_index+1) + ": " + l + "==" + r); throw ""; },
                test_fail: () => { alert("Test failed on test " + (test_index+1) + "."); throw ""; },
            }, core: {
                error: (e, a, i, l, c) => { alert("Error code " + e + " raised"); throw ""; },
                test_fail: () => { alert("Test failed on test " + (test_index+1) + "."); throw ""; },
            }
        };
        const instance = await load_binary(binary, imports);
        const result = instance.main();
        ++test_index;
        num_asserts += result;
    }
    alert("Success: " + test_index + " tests, " + num_asserts + " assertions passed");
    
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
