declare function LoadWasm(): Promise<typeof import("../wasm/source_compiler")>;

export = LoadWasm;
