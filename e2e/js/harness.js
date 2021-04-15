import { compileAndRun } from './interface';

function Harness() {
  this.testSuites = ['tailCalls', 'functions'];

  this.runSuite = (suite) => {
    suite.testCases.forEach((testCase, index) => {
      compileAndRun(testCase.code, (output, err) => {
        console.log("%c" + suite.name + " " + index + ": ", "color: orange");
        if (err) {
          console.log("%cFAIL", "color: red")
          console.error(err);
          console.log(`%cexpected: ${testCase.expect}`, "color: red");
        } else {
          if (output == testCase.expect) {
            console.log("%cPASS", "color: green")
          } else {
            console.log("%cFAIL", "color: red")
            console.log(`%coutput: ${output}`, "color: red");
            console.log(`%cexpected: ${testCase.expect}`, "color: red");
          }
        }
      });
    });
  }

  this.runCode = (code, expected) => {
    compileAndRun(code, (output, err) => {
      if (err) {
        console.log("%cFAIL", "color: red")
        console.error(err);
        console.log(`%cexpected: ${expected}`, "color: red");
      } else {
        if (output == expected) {
          console.log("%cPASS", "color: green")
        } else {
          console.log("%cFAIL", "color: red")
          console.log(`%coutput: ${output}`, "color: red");
          console.log(`%cexpected: ${expected}`, "color: red");
        }
      }
    });
  }

  this.start = () => {
    this.testSuites.forEach(async suiteName => {
      const suite =  await import(`./tests/${suiteName}`);
      this.runSuite(suite.default);
    });
  }
}

export default Harness;
