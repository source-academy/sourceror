import { compile, run } from "./utils";
import Transcoder from "./transcoder";
import { makePlatformImports } from  "./platform";

export function compileAndRun(code, logger) {
  const context = {
    errors: [],
  }

  compile(code, context)
    .then((wasm_module) => {
      const transcoder = new Transcoder();
      return run(wasm_module, makePlatformImports(context, transcoder), transcoder, context)
    }) .then((result) => logger(result, undefined))
    .catch((err) => logger(undefined, err));

  // compile(code, context).then(arr => {
  //   const startTime = Date.now();
  //   WebAssembly.instantiate(arr, config)
  //     .then(({ instance, module }) => {
  //       const transcoder = new Transcoder();
  //       transcoder.setMem(new DataView((instance.exports.linear_memory).buffer));
  //       transcoder.setAllocateStringFunc(instance.exports.allocate_string);
  //       try {
  //         (instance.exports.main)();
  //         const result = read_js_result(
  //           instance.exports.linear_memory
  //         );
  //         const endTime = Date.now()
  //         const latency = endTime - startTime;
  //         console.log(`Log: time: ${latency}`)
  //         logger(result, undefined);
  //       } catch (e) {
  //         if (e === {}) {
  //           logger(undefined, "Runtime error");
  //         } else {
  //           logger(undefined, e);
  //         }
  //       }
  //     }, (err) => {
  //       logger(undefined, err);
  //     }).catch(err => {
  //       logger(undefined, err);
  //     });
  // });
}

