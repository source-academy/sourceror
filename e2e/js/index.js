import Harness from './harness';

const harness = new Harness;
const code = `
import {math_max} from "https://btzy.github.io/libsourceror/std/math.source";

math_max(4, 2);
`;
harness.runCode(code, 1);
