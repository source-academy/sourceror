import LoadWasm from './load-wasm';

export type Context = number;
export type LogCallback = (severity: number, location_file: string, location_start_line: number, location_start_column: number, location_end_line: number, location_end_column: number, message: string) => void;
export type FetchCallback = (name: string) => Promise<string>;

const contexts: Array<[LogCallback, FetchCallback]> = [];

export function createContext(logCallback: LogCallback, fetchCallback: FetchCallback): Context {
  const ret = contexts.length;
  contexts.push([logCallback, fetchCallback]);
  return ret;
}

export function destroyContext(context: Context) {
  delete contexts[context];
}

export function compile(context: Context, code: string) {
  return LoadWasm().then(module => module.compile(context, code));
}

function compilerLog(context: Context, severity: number, location_file: string, location_start_line: number, location_start_column: number, location_end_line: number, location_end_column: number, message: string) {
  contexts[context][0](severity, location_file, location_start_line, location_start_column, location_end_line, location_end_column, message);
}

async function compilerFetchDep(context: Context, name: string): Promise<string> {
  return contexts[context][1](name);
}

(global as any).sourcerorLogCallback = compilerLog;
(global as any).sourcerorFetchDepCallback = compilerFetchDep;
