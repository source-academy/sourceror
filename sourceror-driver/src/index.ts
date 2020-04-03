import {compile as wasm_compile} from '@btzy/source-compiler/source_compiler'

/*class CompileError extends Error {
    constructor(message: string) {
        super(message)
    }
}*/

class RuntimeError extends Error {
    constructor(message: string) {
        super(message)
    }
}

class Result {
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


export async function run(wasm_module: WebAssembly.Module): Promise<Result> {
    let err: any = undefined;
    return WebAssembly.instantiate(wasm_module, { core: { error: (x: number) => { err = x } } })
        .then(instance => {
            const return_val = (instance.exports.main as Function)();
            if (err) {
                return Promise.reject(new RuntimeError(err.toString()));
            } else {
                return new Result("finished", return_val)
            }
        });

}