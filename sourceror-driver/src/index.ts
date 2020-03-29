class CompileError {
    desc: string;
    constructor(desc: string) {
        this.desc = desc;
    }
}

class RuntimeError {
    desc: string;
    constructor(desc: string) {
        this.desc = desc;
    }
}

export async function compile(code: string, options: any): Promise<WebAssembly.Module> {
    return new CompileError("err");
}


export async function run(wasm_module: WebAssembly.Module) {
    return new RuntimeError("test");
}