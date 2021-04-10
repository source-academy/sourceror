import Harness from './harness';

const harness = new Harness;
const code = `
function y() {
  return 10;
}
function x() {
  return true ? y() : 3;
}
x();
`;
harness.runCode(code, 2);
