export function compile(code) {
    return import("../pkg/index.js").then(module => module.compile(code));
}

export function sourceror_log(severity, text) {
    console.log(severity + " " + text);
}