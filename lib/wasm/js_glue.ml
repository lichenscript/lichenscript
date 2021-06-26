open Core_kernel
open Codegen_env

let generate_glue filename = "
  const fs = require('fs');
  const path = require('path');

  var importObject = {
    imports: {
      imported_func: function(arg) {
        console.log(arg);
      }
    },
    env: {
      abort: () => {},
    },
  };

  const bytes = fs.readFileSync(path.join(__dirname, '" ^ filename ^ ".wasm'));
  WebAssembly.instantiate(bytes, importObject).then(function (result) {
    let tmp = result.instance.exports.main(1, 2);
    console.log(tmp);
  })
  "


let dump_js_glue env =
  generate_glue env.output_filename
