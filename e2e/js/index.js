import Harness from './harness';

const harness = new Harness;
const code = `
import {math_log2} from "https://btzy.github.io/libsourceror/std/math.source";

math_log2(4);
`;
harness.runCode(code, 1);
