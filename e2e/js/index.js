import Harness from './harness';

const harness = new Harness;
const code = `
function y() {
  return 10;
}
function x(n) {
  return n ? y() + 1 : y();
}
x(false);
`;
harness.runCode(code, 2);
