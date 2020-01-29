wasm-pack build --target web
copy pkg\hello_wasm_bg.wasm stage /Y
copy pkg\hello_wasm.js stage /Y
copy static\* stage /Y