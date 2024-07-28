const contexts = [];

export function createContext(logCallback, fetchCallback) {
  const ret = contexts.length;
  contexts.push([logCallback, fetchCallback]);
  return ret;
}

export function destroyContext(context) {
  delete contexts[context];
}

export function compile(context, code) {
  return import("../pkg").then(module => module.compile(context, code));
}

function compilerLog(context, severity, location_file, location_start_line, location_start_column, location_end_line, location_end_column, message) {
  contexts[context][0](severity, location_file, location_start_line, location_start_column, location_end_line, location_end_column, message);
}

async function compilerFetchDep(context, name) {
  return contexts[context][1](name);
}

global.sourcerorLogCallback = compilerLog;
global.sourcerorFetchDepCallback = compilerFetchDep;

