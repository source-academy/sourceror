let wasm;

function init(module_bytecode, imports) {
    if (!imports) imports = {};
    return WebAssembly.instantiate(module_bytecode, imports)
    .then(({instance, module}) => {
        wasm = instance.exports;
        //init.__wbindgen_wasm_module = module;
        return wasm;
    });
}

export default init;
