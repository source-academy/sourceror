import LoadWasm from './load-wasm';

export type Context = number;
export type LogCallback = (severityCode: number, message: string, line: number, column: number) => void;
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

function compilerLog(context: Context, severityCode: number, message: string, line: number, column: number) {
  contexts[context][0](severityCode, message, line, column);
}

async function compilerFetchDep(context: Context, name: string): Promise<string> {
  return contexts[context][1](name);
}

(global as any).sourcerorLogCallback = compilerLog;
(global as any).sourcerorFetchDepCallback = compilerFetchDep;
