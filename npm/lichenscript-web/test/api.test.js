const assert = require('assert');
const lichenscript = require('..');

describe('Compiler', function () {
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

  it('resolve error', function () {
    try {
      const result = lichenscript.compile(`
      import "xxx";

      function main() {
        print("Hello World");
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

describe('Intellisense', function() {

  it('createInstance', function() {
    const instance = lichenscript.createIntellisenseInstance({}, {
      findPaths: [],
      runtimeDir: "",
    });
    assert.strictEqual(typeof instance, 'object');
    let arr = instance.parseAndCache("/some_path", `
      import "xxx";

      function main() {
        print("Hello World");
      }
    `);
    assert.strictEqual(Array.isArray(arr), true);
    assert.strictEqual(arr.length === 0, true);
    arr = instance.parseAndCache("/some_path", `
      import "xxx;

      function main() {
        print("Hello World");
      }
    `);
    assert.strictEqual(Array.isArray(arr), true);
    assert.strictEqual(arr.length > 0, true);
  });

});
