import Harness from './harness';

const harness = new Harness;
const code = `
import {math_sin, math_PI } from "https://btzy.github.io/libsourceror/std/math.source";

function x() {
  return math_sin(1);
}
x();
`;
harness.runCode(code, 1);
