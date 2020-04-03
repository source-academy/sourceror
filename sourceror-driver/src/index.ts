import {compile as wasm_compile} from '@btzy/source-compiler/source_compiler'

/*class CompileError extends Error {
    constructor(message: string) {
        super(message)
    }
}*/

export class RuntimeError extends Error {
    constructor(message: string) {
        super(message)
    }
}

export class Result {
    status: string;
    value: any;
    constructor(status: string, value: any) {
        this.status = status;
        this.value = value;
    }
}

export async function compile(code: string, options: any): Promise<WebAssembly.Module> {
    return WebAssembly.compile(wasm_compile(code));
    //const compiler = require('@btzy/source-compiler/source_compiler');
    //compiler();
    //console.log(compiler.__exports.compile.constructor.name);
}


function read_js_result(linear_memory: WebAssembly.Memory): any {
    const mem = new DataView(linear_memory.buffer);
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
}


export async function run(wasm_module: WebAssembly.Module): Promise<Result> {
    let err: any = undefined;
    return WebAssembly.instantiate(wasm_module, { core: { error: (x: number) => { err = x } } })
        .then(instance => {
            (instance.exports.main as Function)();
            if (err) {
                return Promise.reject(new RuntimeError(err.toString()));
            } else {
                return new Result("finished", read_js_result(instance.exports.linear_memory as WebAssembly.Memory))
            }
        });

}