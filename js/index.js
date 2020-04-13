let contexts = [];

// called by frontend
export function create_context(log_callback) {
    const ret = contexts.length;
    contexts.push(log_callback);
    return ret;
}

// called by frontend
export function destroy_context(context) {
    contexts[context] = undefined;
}

// called by frontend
export function compile(context, code) {
    return import("../pkg/index.js").then(module => module.compile(context, code));
}

// called by the compiler to log stuff
function compiler_log(context, severity_code, message, line, column) {
    contexts[context](severity_code, message, line, column);
}

global.sourcerorLogCallback = compiler_log;
