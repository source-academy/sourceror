cd ..
wasm-pack build --target web
copy pkg\source_compiler_bg.wasm stage /Y
copy pkg\source_compiler.js stage /Y
copy static\* stage /Y
