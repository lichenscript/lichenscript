const assert = require('assert');
const lichenscript = require('..');

describe('Compiler', function () {
  it('compile api', function () {
    const program = lichenscript.compile(`
    function main() {
      print("Hello World");
    }
    `);
    program.execute();
  });

  it('parse error', function () {
    try {
      const program = lichenscript.compile(`
      function main() {
        print("Hello World;
      }
      `);
      program.execute();
    } catch (err) {
      assert.strictEqual(Array.isArray(err.errors), true)
      console.log(JSON.stringify(err.errors));
    }
  });

  it('resolve error', function () {
    try {
      const program = lichenscript.compile(`
      import "xxx";

      function main() {
        print("Hello World");
      }
      `);
      program.execute();
    } catch (err) {
      assert.strictEqual(Array.isArray(err.errors), true)
      console.log(JSON.stringify(err.errors));
    }
  });

  it('get arguments', function () {
    const program = lichenscript.compile(`
    function main() {
      const args = getArgs();
      print(args[0]);
    }
    `);
    program.execute(["Hello"]);
  });

  it('static field', function () {
    const program = lichenscript.compile(`

class Foo {

  static Name = "Foo"

}

function main() {
  print(Foo.Name);
  Foo.Name = "Bar";
  print(Foo.Name);
}

    `);
    program.execute(["Hello"]);
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
