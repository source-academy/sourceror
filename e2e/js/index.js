import { compileAndRun } from './compileAndRun';
import tailCallTests from './tests/tailCalls';
import functionTests from './tests/functions';

function harnessStart(inputs) {
  inputs.testCases.forEach((input, index) => {
    compileAndRun(inputs.name, index + 1, input.code, input.expect);
  });
}

harnessStart(tailCallTests);
harnessStart(functionTests);
