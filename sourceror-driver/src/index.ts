import { compile as wasm_compile } from '@btzy/source-compiler'
import { Context } from 'js-slang'
import { ErrorType, ErrorSeverity } from 'js-slang/dist/types'
import { parse as slang_parse } from 'js-slang/dist/parser/parser'
import * as es from 'estree'

class CompileError extends Error {
    constructor(message: string) {
        super(message)
    }
}

export class RuntimeError extends Error {
    constructor(message: string) {
        super(message)
    }
}

export async function compile(code: string, context: Context): Promise<WebAssembly.Module> {
    let estree: es.Program | undefined = slang_parse(code, context);
    if (!estree) {
        return Promise.reject(new CompileError("js-slang cannot parse the program"));
    }
    let es_str: string = JSON.stringify(estree);
    let compile_promise = wasm_compile(es_str);
    return compile_promise.then((wasm_binary: Uint8Array) => {
        return WebAssembly.compile(wasm_binary);
    }).catch((err: string) => {
        context.errors.push({
            type: ErrorType.SYNTAX,
            severity: ErrorSeverity.ERROR,
            location: {
                source: null, start: {
                    line: 0, column: 0
                }, end: {
                    line: 0, column: 0
                }
            },
            explain: (): string => err,
            elaborate: (): string => err,
        });
        return Promise.reject(new CompileError("sourceror cannot compile the program"));
    });
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


export async function run(wasm_module: WebAssembly.Module, context: Context): Promise<any> {
    return WebAssembly.instantiate(wasm_module, {
        core: {
            error: (code: number, detail: number, file: number, line: number, column: number) => {
                context.errors.push({
                    type: ErrorType.RUNTIME,
                    severity: ErrorSeverity.ERROR,
                    location: {
                        source: null, start: {
                            line, column
                        }, end: {
                            line, column: column + 1
                        }
                    },
                    explain: (): string => code.toString(),
                    elaborate: (): string => code.toString(),
                });
                throw ""; // to stop the webassembly binary immediately
            }
        }
    })
    .then(instance => {
        try {
            (instance.exports.main as Function)();
            return read_js_result(instance.exports.linear_memory as WebAssembly.Memory);
        } catch (_) {
            return Promise.reject(new RuntimeError("runtime error"));
        }
    });
}