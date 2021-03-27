import { parseCode } from "./utils";

export function compileAndRun(name, index, code, expected) {
  import("../pkg").then(module => {
    const parsed = parseCode(code);

    // module.compile(1, `{ "type": "Program", "start": 0, "end": 3, "body": [ { "type": "ExpressionStatement", "start": 0, "end": 2, "expression": { "type": "Literal", "start": 0, "end": 1, "value": 1, "raw": "1" } } ], "sourceType": "script" }`).then(arr => {
    module.compile(1, JSON.stringify(parsed)).then(arr => {
      var config = {
        env: {
          memoryBase: 0,
          tableBase: 0,
          memory: new WebAssembly.Memory({ initial: 256 }),
          table: new WebAssembly.Table({ initial: 0, element: 'anyfunc' })
        },
        imports: {
          imported_func: function(arg) {
            console.log(arg);
          }
        },
        core: {
          error: (
            code,
            detail,
            file,
            start_line,
            start_column,
            end_line,
            end_column,
          ) => {
            throw "Oh crap"; // to stop the webassembly binary immediately
          },
          abort: () => {
            throw "Oh crap"; // to stop the webassembly binary immediately
          }
        }
      };
      WebAssembly.instantiate(arr, config)
        .then(({ instance, module }) => {
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
          const result = ret.__read_js_result(ret.main());
          console.log("%c" + name + " " + index + ": ", "color: orange");
          if (result == expected) {
            console.log("%cPASS", "color: green")
          } else {
            console.log("%cFAIL", "color: red")
            console.log(`%coutput: ${result}`, "color: red");
            console.log(`%cexpected: ${expected}`, "color: red");
          }
        }).catch(err => {
          console.log("%c" + name + " " + index + ": ", "color: orange");
          console.log("%cFAIL", "color: red")
          console.error(err)
          console.log(`%cexpected: ${expected}`, "color: red");
        });
    });
  }).catch(console.error)
}
