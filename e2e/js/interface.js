import { parseCode } from "./utils";
import Transcoder from "./transcoder";

function read_js_result(linear_memory) {
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

export function compileAndRun(code, logger) {
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
              console.error(code);
              console.error(detail);
              console.error(file);
              console.error(start_line);
              console.error(start_column);
              console.error(end_line);
              console.error(end_column);
              throw 'Error'; // to stop the webassembly binary immediately
            },
            abort: () => {
              throw "Aborting"; // to stop the webassembly binary immediately
            }
          }
        };
        const t0 = Date.now();
        WebAssembly.instantiate(arr, config)
          .then(({ instance, module }) => {
            const t1 = Date.now() 
            const result = t1 - t0;
            console.log(`Log: time: ${result}`)
            const transcoder = new Transcoder();
            transcoder.setMem(new DataView((instance.exports.linear_memory).buffer));
            transcoder.setAllocateStringFunc(instance.exports.allocate_string);
            try {
              (instance.exports.main)();
              const result =  read_js_result(
                instance.exports.linear_memory
              );
              logger(result, undefined);
            } catch (e) {
              if (e === {}) {
                logger(undefined, "Runtime error");
              } else {
                logger(undefined, e);
              }
            }
          }, (err) => {
            logger(undefined, err);
          }).catch(err => {
            logger(undefined, err);
          });
      });
    }).catch(console.error)
  }

