import Harness from './harness';

const harness = new Harness;
const code = `
function x() {
  return 2;
}
x();
`;
harness.runCode(code, 2);
