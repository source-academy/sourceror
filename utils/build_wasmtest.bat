cd ..
rustup run nightly wasm-pack build --debug --target web --out-dir "../pkg" wasm-test-driver -- -Z package-features --features backend-wasm/wasmtest
copy pkg\wasm_test_driver_bg.wasm stage-wasmtest /Y
copy pkg\wasm_test_driver.js stage-wasmtest /Y
copy static-wasmtest\* stage-wasmtest /Y
