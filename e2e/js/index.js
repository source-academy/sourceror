import Harness from './harness';

const harness = new Harness;
const code = `
import {math_sin, math_PI} from "https://btzy.github.io/libsourceror/std/math.source";

function x(n) {
  return n === 0 ? 0 : math_sin(n);
}
x(30);
`;
harness.runCode(code, 1);
