export default {
  name: "Functions",
  testCases: [{
    code: `
    function ok(n) {
      return n;
    }
    ok(1);
    `,
    expect: `1`
  }]
};
