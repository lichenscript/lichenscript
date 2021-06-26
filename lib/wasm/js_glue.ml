open Core_kernel
open Codegen_env

let generate_glue filename = "
  const fs = require('fs');

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

  const bytes = fs.readFileSync('" ^ filename ^ ".wasm');
  WebAssembly.instantiate(bytes, importObject)
  "


let dump_js_glue env =
  generate_glue env.output_filename
