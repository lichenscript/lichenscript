const assert = require('assert');
const lichenscript = require('..');

describe('Compiler', function () {
  describe('#indexOf()', function () {

    it('compile api', function () {
      const result = lichenscript.compile(`
      function main() {
        print("Hello World");
      }
      `);
      const fun = new Function(result)
      fun();
    });

    it('parse error', function () {
      try {
        const result = lichenscript.compile(`
        function main() {
          print("Hello World;
        }
        `);
        const fun = new Function(result)
        fun();
      } catch (err) {
        assert.strictEqual(Array.isArray(err.errors), true)
        console.log(JSON.stringify(err.errors));
      }
    });

  });
});
