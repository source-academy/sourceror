export default {
  name: "Proper Tail Calls",
  testCases: [{
    code: `
      function ok(n) {
        return n === 0 ? 0 : ok(n-1);
      }
      ok(2);
    `,
    expect: `0`
  },
  {
    code: `
      function ok(n) {
        return n === 0 ? 0 : ok(n-1);
      }
      ok(20000);
    `,
    expect: `0`
  }]
};
