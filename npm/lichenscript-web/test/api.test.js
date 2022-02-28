const assert = require('assert');
const lichenscript = require('..');

describe('Array', function () {
  describe('#indexOf()', function () {
    it('should return -1 when the value is not present', function () {
      const result = lichenscript.compile(`
      function main() {
        print("Hello World");
      }
      `);
      const fun = new Function(result)
      fun();
    });
  });
});
