import Harness from './harness';

const harness = new Harness;
const code = `
__attributes = "direct";
function x(n) {
  return 0;
}
x(10000000);
`;
harness.runCode(code, 0);
