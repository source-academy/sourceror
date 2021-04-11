import Harness from './harness';

const harness = new Harness;
const code = `
function abs(x) {
  return x >= 0 ? x : 0 - x;
}
function square(x) {
  return x * x;
}
function average(x,y) {
  return (x + y) / 2;
}
function sqrt(x) {
  function good_enough(guess, x) {
    return abs(square(guess) - x) < 0.001;
  }
  function improve(guess, x) {
    return average(guess, x / guess);
  }
  function sqrt_iter(guess, x) {
    return good_enough(guess, x)
      ? guess
      : sqrt_iter(improve(
        guess, x), x);
  }
  return sqrt_iter(1.0, x);
}

sqrt(10011014123122145131234);  
`;
harness.runCode(code, 100055055460.09229);
