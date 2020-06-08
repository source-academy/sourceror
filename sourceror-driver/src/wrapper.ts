import LoadWasm from './load-wasm';

export type Context = number;
export type LogCallback = (severityCode: number, message: string, line: number, column: number) => void;

const contexts: Array<LogCallback> = [];

export function createContext(logCallback: LogCallback): Context {
  const ret = contexts.length;
  contexts.push(logCallback);
  return ret;
}

export function destroyContext(context: Context) {
  delete contexts[context];
}

export function compile(context: Context, code: string, imports: string) {
  return LoadWasm().then(module => module.compile(context, code, imports));
}

function compilerLog(context: Context, severityCode: number, message: string, line: number, column: number) {
  contexts[context](severityCode, message, line, column);
}

(global as any).sourcerorLogCallback = compilerLog;
