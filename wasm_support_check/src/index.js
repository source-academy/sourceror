// import * as check from 'wasm-check';
const check = require('wasm-check');

console.log(`WASM Support: ${check.support()}`);
console.log(`WASM 2 Support: ${check.support(2)}`);
console.log(`Features:`);
console.log({...check.feature});
