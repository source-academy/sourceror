
let wasm;

let cachegetUint8Memory0 = null;
function getUint8Memory0() {
    if (cachegetUint8Memory0 === null || cachegetUint8Memory0.buffer !== wasm.memory.buffer) {
        cachegetUint8Memory0 = new Uint8Array(wasm.memory.buffer);
    }
    return cachegetUint8Memory0;
}

function getArrayU8FromWasm0(ptr, len) {
    return getUint8Memory0().subarray(ptr / 1, ptr / 1 + len);
}

function logError(e) {
    let error = (function () {
        try {
            return e instanceof Error ? `${e.message}\n\nStack:\n${e.stack}` : e.toString();
        } catch(_) {
            return "<failed to stringify thrown value>";
        }
    }());
    console.error("wasm-bindgen: imported JS function that was not marked as `catch` threw an error:", error);
    throw e;
}
/**
*/
export function build_tests() {
    wasm.build_tests();
}

function init(module) {
    if (typeof module === 'undefined') {
        module = import.meta.url.replace(/\.js$/, '_bg.wasm');
    }
    let result;
    const imports = {};
    imports.wbg = {};
    imports.wbg.__wbg_addtest_aa51307eb9597027 = function(arg0, arg1) {
        try {
            var v0 = getArrayU8FromWasm0(arg0, arg1).slice();
            wasm.__wbindgen_free(arg0, arg1 * 1);
            add_test(v0);
        } catch (e) {
            logError(e)
        }
    };

    if ((typeof URL === 'function' && module instanceof URL) || typeof module === 'string' || (typeof Request === 'function' && module instanceof Request)) {

        const response = fetch(module);
        if (typeof WebAssembly.instantiateStreaming === 'function') {
            result = WebAssembly.instantiateStreaming(response, imports)
            .catch(e => {
                return response
                .then(r => {
                    if (r.headers.get('Content-Type') != 'application/wasm') {
                        console.warn("`WebAssembly.instantiateStreaming` failed because your server does not serve wasm with `application/wasm` MIME type. Falling back to `WebAssembly.instantiate` which is slower. Original error:\n", e);
                        return r.arrayBuffer();
                    } else {
                        throw e;
                    }
                })
                .then(bytes => WebAssembly.instantiate(bytes, imports));
            });
        } else {
            result = response
            .then(r => r.arrayBuffer())
            .then(bytes => WebAssembly.instantiate(bytes, imports));
        }
    } else {

        result = WebAssembly.instantiate(module, imports)
        .then(result => {
            if (result instanceof WebAssembly.Instance) {
                return { instance: result, module };
            } else {
                return result;
            }
        });
    }
    return result.then(({instance, module}) => {
        wasm = instance.exports;
        init.__wbindgen_wasm_module = module;

        return wasm;
    });
}

export default init;

