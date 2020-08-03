import * as Sourceror from "./wrapper";
import { Context } from "js-slang";
import { ErrorType, ErrorSeverity } from "js-slang/dist/types";
import { parse as slang_parse } from "js-slang/dist/parser/parser";
import { Options as AcornOptions, Parser as AcornParser } from "acorn";
import * as es from "estree";
export { makePlatformImports } from "./platform";
import { Transcoder } from "./transcoder";
export { Transcoder };

export class CompileError extends Error {
  constructor(message: string) {
    super(message);
  }
}

export class RuntimeError extends Error {
  constructor(message: string) {
    super(message);
  }
}

class SyntacticParser extends AcornParser {
  raiseRecoverable(pos: any, message: string) {
    if (message.includes("Identifier ") && message.includes(" has already been declared")) return;
    (AcornParser.prototype as any).raiseRecoverable.call(this, pos, message);
  }
}

function createImportOptions(err_handler: () => void): AcornOptions {
  return {
    sourceType: 'module',
    ecmaVersion: 6,
    locations: true,
    // tslint:disable-next-line:no-any
    onInsertedSemicolon(end: any, loc: any) {
      err_handler();
    },
    // tslint:disable-next-line:no-any
    onTrailingComma(_end: any, _loc: any) {
      err_handler();
    }
  }
}

function parseImport(code: string): es.Program | undefined {
  let program: es.Program | undefined;
  let has_errors = false;
  try {
    // we directly use acorn, because js-slang's parse is too restrictive
    // with attributes and exports
    program = (SyntacticParser.parse(code, createImportOptions(() => { has_errors = true; })) as unknown) as es.Program
  } catch (error) {
    if (error instanceof SyntaxError) {
      has_errors = true;
    } else {
      throw error;
    }
  }
  if (program && !has_errors) return program;
  return undefined;
}

export async function compile(
  code: string,
  context: Context
): Promise<WebAssembly.Module> {
  context.chapter = 3;
  let estree: es.Program | undefined = slang_parse(code, context);
  if (!estree) {
    return Promise.reject(
      new CompileError("js-slang cannot parse the program")
    );
  }
  let es_str: string = JSON.stringify(estree);
  let wasm_context: number = Sourceror.createContext((message: string) => {
    context.errors.push({
      type: ErrorType.SYNTAX,
      /*severity: severity_code >= 4 ? ErrorSeverity.ERROR : ErrorSeverity.WARNING, // Sourceror supports other severity levels, but js-slang does not
      location: {
        source: null,
        start: {
          line,
          column,
        },
        end: {
          line,
          column: column + 1,
        },
      },*/
      severity: ErrorSeverity.ERROR,
      location: undefined as unknown as es.SourceLocation, // shhhh
      explain: (): string => message,
      elaborate: (): string => "",
    });
  }, (name: string): Promise<string> => {
      return fetch(name)
        .then(rsp => rsp.text())
        .then(rsptxt => {
          if (rsptxt.split('\n', 1)[0] == "@SourceImports") {
            // it is an import file, return it as is
            return rsptxt;
          }
          else {
            // it is a Source file, need to parse it with acorn
            const estree: es.Program | undefined = parseImport(rsptxt);
            if (!estree) {
              return Promise.reject(
                new CompileError("js-slang cannot parse the program")
              );
            }
            return JSON.stringify(estree);
          }
        });
  });
  return Sourceror.compile(wasm_context, es_str)
    .then((wasm_binary: Uint8Array) => {
      if (wasm_binary.byteLength > 0) {
        return WebAssembly.compile(wasm_binary).catch((err: string) => {
          context.errors.push({
            type: ErrorType.SYNTAX,
            severity: ErrorSeverity.ERROR,
            location: {
              source: null,
              start: {
                line: 0,
                column: 0,
              },
              end: {
                line: 0,
                column: 0,
              },
            },
            explain: (): string => err,
            elaborate: (): string => "Your browser's WebAssembly engine is unable to compile the WebAssembly binary produced by Sourceror.  This is probably a bug in Sourceror; please report it.",
          });
          throw new CompileError("WebAssembly compilation error");
        });
      }
      else {
        throw new CompileError("Syntax error");
      }
    })
    .finally(() => {
      Sourceror.destroyContext(wasm_context);
    });
}

function read_js_result(linear_memory: WebAssembly.Memory): any {
  const mem = new DataView(linear_memory.buffer);
  const tag = mem.getUint32((1 << 20) - 12, true);
  const data_offset = (1 << 20) - 8;
  switch (tag) {
    case 0:
      return "(unassigned variable was returned)";
    case 1:
      return undefined;
    case 2:
      return mem.getFloat64(data_offset, true);
    case 3:
      return mem.getUint32(data_offset, true) !== 0;
    case 4: {
      const ptr = mem.getUint32(data_offset, true);
      const len = mem.getUint32(ptr, true);
      const decoder = new TextDecoder();
      const res = decoder.decode(
        new Uint8Array(linear_memory.buffer, ptr + 4, len)
      );
      return res;
    }
    case 5:
      return "(function was returned)";
    default:
      return "(struct or invalid type (" + tag + ") was returned)";
  }
}

function stringifySourcerorRuntimeErrorCode(code: number): [string, string] {
  switch (code) {
    case 0x0:
      return ["General runtime error", ""];
    case 0x1:
      return [
        "Out of memory",
        "Strings and objects are allocated on the heap.  You have exhausted the available heap space.  Try recompiling your program with increased heap space.",
      ];
    case 0x10:
      return ["General runtime type error", ""];
    case 0x11:
      return ["Function called with incorrect parameter type", ""];
    case 0x12:
      return ["Unary operator called with incorrect parameter type", ""];
    case 0x13:
      return ["Binary operator called with incorrect parameter type", ""];
    case 0x16:
      return ["Function call operator applied on a non-function", ""];
    case 0x17:
      return ["If statement has a non-boolean condition", ""];
    case 0x1A:
      return ["Variable used before initialization", ""];
    default:
      return [
        "Unknown runtime error",
        "This is probably a bug in Sourceror; please report it.",
      ];
  }
}

// Just a unique identifier used for throwing exceptions while running the webassembly code
const propagationToken = {};

export async function run(
  wasm_module: WebAssembly.Module,
  platform: any,
  transcoder: Transcoder,
  context: Context,
): Promise<any> {
  const real_imports = Object.assign({}, platform);
  real_imports.core = {
    error: (
      code: number,
      detail: number,
      file: number,
      line: number,
      column: number,
    ) => {
      const [explain, elaborate] = stringifySourcerorRuntimeErrorCode(code);
      context.errors.push({
        type: ErrorType.RUNTIME,
        severity: ErrorSeverity.ERROR,
        location: {
          source: null,
          start: {
            line,
            column,
          },
          end: {
            line,
            column: column + 1,
          },
        },
        explain: (): string => explain,
        elaborate: (): string => elaborate,
      });
      throw propagationToken; // to stop the webassembly binary immediately
    },
    abort: () => {
      context.errors.push({
        type: ErrorType.RUNTIME,
        severity: ErrorSeverity.ERROR,
        location: {
          source: null,
          start: {
            line: 0,
            column: 0,
          },
          end: {
            line: 0,
            column: 0,
          },
        },
        explain: (): string => "Execution aborted by call to error()",
        elaborate: (): string => "",
      });
      throw propagationToken; // to stop the webassembly binary immediately
    },
  };
  return WebAssembly.instantiate(wasm_module, real_imports).then((instance) => {
    transcoder.setMem(new DataView((instance.exports.linear_memory as WebAssembly.Memory).buffer));
    transcoder.setAllocateStringFunc(instance.exports.allocate_string as (len: number) => number);
    try {
      (instance.exports.main as Function)();
      return read_js_result(
        instance.exports.linear_memory as WebAssembly.Memory
      );
    } catch (e) {
      if (e === propagationToken) {
        throw new RuntimeError("runtime error");
      } else {
        context.errors.push({
          type: ErrorType.RUNTIME,
          severity: ErrorSeverity.ERROR,
          location: {
            source: null,
            start: {
              line: 0,
              column: 0,
            },
            end: {
              line: 0,
              column: 0,
            },
          },
          explain: (): string => e.toString(),
          elaborate: (): string => e.toString(),
        });
        throw e;
      }
    }
  }, (err: string) => {
    context.errors.push({
      type: ErrorType.SYNTAX,
      severity: ErrorSeverity.ERROR,
      location: {
        source: null,
        start: {
          line: 0,
          column: 0,
        },
        end: {
          line: 0,
          column: 0,
        },
      },
      explain: (): string => err,
      elaborate: (): string => "Your browser's WebAssembly engine is unable to instantiate the WebAssembly module.",
    });
      throw new CompileError("WebAssembly instantiation error");
  });
}
