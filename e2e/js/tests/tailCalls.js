export default {
  name: "Proper Tail Calls",
  testCases: [
    {
      code: `
      function ok(n) {
        return n === 0 ? 0 : ok(n-1);
      }
      ok(2);
      `,
      expect: `0`,
    },
    {
      code: `
      function ok(n) {
        return n === 0 ? 0 : ok(n-1);
      }
      ok(20000);
      `,
      expect: `0`,
    },
    {
      code: `
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
          // here comes the tail call
            : sqrt_iter(improve(
              guess, x), x);
        }
        return sqrt_iter(1.0, x);
      }

      sqrt(10011014123122145131234);  
      `,
      expect: `100055055460.09229`,
    },
    {
      code: `
      function factorial(n) { 
        function facloop(n, acc) {
          return n === 1 
            ? acc
            : facloop(n - 1, acc * n);
        }
        return facloop(n, 1); 
      }
      factorial(4);
      `,
      expect: `24`,
    },
    {
      code: `
      function factorial(n) { 
        function facloop(n, acc) {
          return n === 1 
            ? acc
            : facloop(n - 1, acc * n);
        }
        return facloop(n, 1); 
      }
      factorial(1000000);
      `,
      expect: `Infinity`,
    },
    {
      code: `
      function factorial(n) { 
        function facloop(n, acc) {
          return n === 2 
            ? facloop(n - 1, 1)
            : n === 1
            ? acc
            : facloop(n - 1, acc * n);
        }
        return facloop(n, 1); 
      }
      factorial(1000000);
      `,
      expect: `1`,
    },
  ],
};
