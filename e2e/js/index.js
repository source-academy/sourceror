import Harness from './harness';

const harness = new Harness;
const code = `
function factorial(n) { 
    function facloop(n, acc) {
        return n === 1 
               ? acc
               : facloop(n - 1, acc * n);
    }
    return facloop(n, 1); 
}
factorial(10000);

`;
harness.runCode(code, 100055055460.09229);
