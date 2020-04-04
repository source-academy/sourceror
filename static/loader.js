function init(module_bytecode, imports) {
    if (!imports) imports = {};
    return WebAssembly.instantiate(module_bytecode, imports)
    .then(({instance, module}) => {
        let ret = {};
        Object.assign(ret, instance.exports);
        ret.__read_js_result = (wasm_result) => {
            const mem = new DataView(ret.linear_memory.buffer);
            const tag = mem.getUint32((1 << 20) - 12, true);
            const data_offset = ((1 << 20) - 8);
            switch (tag) {
                case 0:
                    return "(unassigned variable was returned)";
                case 1:
                    return undefined;
                case 2:
                    return mem.getFloat64(data_offset, true);
                case 3:
                    return mem.getUint32(data_offset, true) !== 0;
                case 4:
                    return "(string was returned)";
                case 5:
                    return "(function was returned)";
                case 6:
                    return "(struct was returned)";
                default:
                    return "(invalid type was returned)";
            }
        };
        return ret;
    });
}

export default init;
