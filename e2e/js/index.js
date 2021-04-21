import Harness from './harness';

const harness = new Harness;
const code = `
import {math_sin} from "https://gist.githubusercontent.com/yzia2000/79338fc9790c36647c80be5155c53a7a/raw/bfe65096964f8be9d743752199270327d3360490/math.ffi";
function x(n) {
  return n === 0 ? 0 : math_sin(n);
}
x(30);
`;
harness.runCode(code, 1);
