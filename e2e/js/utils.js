import { Parser as AcornParser } from "acorn";
import { cachedGetFile } from "./cache";
import * as Sourceror from "./wrapper";

class SyntacticParser extends AcornParser {
  raiseRecoverable(pos, message) {
    if (
      message.includes("Identifier ") &&
      message.includes(" has already been declared")
    )
      return;
    AcornParser.prototype.raiseRecoverable.call(this, pos, message);
  }
}

export function createImportOptions(err_handler) {
  return {
    sourceType: "module",
    ecmaVersion: 6,
    locations: true,
    onInsertedSemicolon(end, loc) {
      err_handler();
    },
    onTrailingComma(_end, _loc) {
      err_handler();
    },
  };
}

export function parseImports(code) {
  let program;
  let has_errors = false;
  try {
    // we directly use acorn, because js-slang's parse is too restrictive
    // with attributes and exports
    program = SyntacticParser.parse(
      code,
      createImportOptions(() => {
        has_errors = true;
      })
    );
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

export function compile(code, context) {
  let estree = parseImports(code);
  if (!estree) {
    return Promise.reject(
      new Error("acorn cannot parse the program")
    );
  }
  let es_str = JSON.stringify(estree);
  let wasm_context = Sourceror.createContext(
    (
      severity,
      location_file,
      location_start_line,
      location_start_column,
      location_end_line,
      location_end_column,
      message
    ) => {
      context.errors.push({
        type: "Syntax Error",
        severity: severity >= 4 ? "Error" : "Warning", // Sourceror supports other severity levels, but js-slang does not
        location: {
          source: location_file ? location_file : undefined,
          start: {
            line: location_start_line,
            column: location_start_column,
          },
          end: {
            line: location_end_line,
            column: location_end_column,
          },
        },
        explain: () => message,
        elaborate: () => "",
      });
    },
    (name) => {
      return cachedGetFile(name, (name) =>
        fetch(name)
          .then((rsp) => rsp.text())
          .then((rsptxt) => {
            if (rsptxt.split("\n", 1)[0] == "@SourceImports") {
              // it is an import file, return it as is
              return rsptxt;
            } else {
              // it is a Source file, need to parse it with acorn
              const estree = parseImports(rsptxt);
              if (!estree) {
                return Promise.reject(
                  new Error("js-slang cannot parse the program")
                );
              }
              return JSON.stringify(estree);
            }
          })
      );
    }
  );
  return Sourceror.compile(wasm_context, es_str)
    .then((wasm_binary) => {
      if (wasm_binary.byteLength > 0) {
        return WebAssembly.compile(wasm_binary).catch((err) => {
          context.errors.push({
            type: "Syntax",
            severity: "Error",
            location: {
              source: undefined,
              start: {
                line: 0,
                column: 0,
              },
              end: {
                line: 0,
                column: 0,
              },
            },
            explain: () => err,
            elaborate: () =>
              "Your browser's WebAssembly engine is unable to compile the WebAssembly binary produced by Sourceror.  This is probably a bug in Sourceror; please report it.",
          });
          throw new Error("WebAssembly compilation error: " + err);
        });
      } else {
        throw new Error("Syntax error");
      }
    })
    .finally(() => {
      Sourceror.destroyContext(wasm_context);
    });
}

function stringifySourcerorRuntimeErrorCode(code) {
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
    case 0x1a:
      return ["Variable used before initialization", ""];
    default:
      return [
        "Unknown runtime error",
        "This is probably a bug in Sourceror; please report it.",
      ];
  }
}

const propagationToken = {};

export async function run(wasm_module, platform, transcoder, context) {
  const real_imports = Object.assign({}, platform);
  real_imports.core = {
    error: (
      code,
      detail,
      file,
      start_line,
      start_column,
      end_line,
      end_column
    ) => {
      const [explain, elaborate] = stringifySourcerorRuntimeErrorCode(code);
      context.errors.push({
        type: "Runtime",
        severity: "Error",
        location: {
          source: undefined,
          start: {
            line: start_line,
            column: start_column,
          },
          end: {
            line: end_line,
            column: end_column,
          },
        },
        explain: () => explain,
        elaborate: () => elaborate,
      });
      console.error(context.errors);
      throw propagationToken; // to stop the webassembly binary immediately
    },
    abort: () => {
      context.errors.push({
        type: "Runtime",
        severity: "Error",
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
        explain: () => "Execution aborted by call to error()",
        elaborate: () => "",
      });
      throw propagationToken; // to stop the webassembly binary immediately
    },
  };
  return WebAssembly.instantiate(wasm_module, real_imports).then(
    (instance) => {
      transcoder.setMem(new DataView(instance.exports.linear_memory.buffer));
      transcoder.setAllocateStringFunc(instance.exports.allocate_string);
      try {
        instance.exports.main();
        return read_js_result(instance.exports.linear_memory);
      } catch (e) {
        if (e === propagationToken) {
          throw new RuntimeError("runtime error");
        } else {
          context.errors.push({
            type: "Runtime",
            severity: "Error",
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
            explain: () => e.toString(),
            elaborate: () => e.toString(),
          });
          throw e;
        }
      }
    },
    (err) => {
      context.errors.push({
        type: "Syntax",
        severity: "Error",
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
        explain: () => err,
        elaborate: () =>
          "Your browser's WebAssembly engine is unable to instantiate the WebAssembly module.",
      });
      throw new Error("WebAssembly instantiation error");
    }
  );
}

export function read_js_result(linear_memory) {
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
