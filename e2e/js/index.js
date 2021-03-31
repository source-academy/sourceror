import Harness from './harness';

const harness = new Harness;
const code = `
function f(x) {
  return x;
}
2;
`;
harness.runCode(code, 1);
