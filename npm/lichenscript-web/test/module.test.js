const lichenscript = require('..');

describe("Module", function () {

	it("provider", function () {
    lichenscript.registerModule({
      name: "my-module",
      files: [
        {
          name: "lib.lc",
          content: `

          public function myFunction() {
            print("myFunction");
          }


          `
        }
      ]
    });
    const program = lichenscript.compile(`
    import m from "my-module";

    function main() {
      m.myFunction();
    }
    `);
    program.execute([]);
	});

});
