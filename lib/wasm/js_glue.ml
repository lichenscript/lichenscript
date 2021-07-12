open Core_kernel
open Codegen_env

let generate_glue filename = "
  const fs = require('fs');
  const path = require('path');

  var Module = typeof Module !== 'undefined' ? Module : {};
  var wasmMemory;

  var HEAP,
  /** @type {ArrayBuffer} */
  buffer,
  /** @type {Int8Array} */
  HEAP8,
  /** @type {Uint8Array} */
  HEAPU8,
  /** @type {Int16Array} */
  HEAP16,
  /** @type {Uint16Array} */
  HEAPU16,
  /** @type {Int32Array} */
  HEAP32,
  /** @type {Uint32Array} */
  HEAPU32,
  /** @type {Float32Array} */
  HEAPF32,
  /** @type {Float64Array} */
  HEAPF64;

  function updateGlobalBufferAndViews(buf) {
    buffer = buf;
    Module['HEAP8'] = HEAP8 = new Int8Array(buf);
    Module['HEAP16'] = HEAP16 = new Int16Array(buf);
    Module['HEAP32'] = HEAP32 = new Int32Array(buf);
    Module['HEAPU8'] = HEAPU8 = new Uint8Array(buf);
    Module['HEAPU16'] = HEAPU16 = new Uint16Array(buf);
    Module['HEAPU32'] = HEAPU32 = new Uint32Array(buf);
    Module['HEAPF32'] = HEAPF32 = new Float32Array(buf);
    Module['HEAPF64'] = HEAPF64 = new Float64Array(buf);
  }

  function wtfStringToJsString(ptr) {
    var length = HEAP32[(ptr + 16) >> 2];

    var str = '';

    for (var i = 0; i < length; i++) {
      str += String.fromCharCode(HEAP16[((ptr + 24) >> 1) + i]);
    }

    return str;
  }

  function main() {

    function receiveInstance(instance, module) {
      var exports = instance.exports;

      Module['asm'] = exports;

      wasmMemory = Module['asm']['memory'];
      updateGlobalBufferAndViews(wasmMemory.buffer);

      exports['main']();
    }

    var env = {
      console_log: function (ptr) {
        var str = wtfStringToJsString(ptr);
        console.log(str);
      },
      memory_fill: function (dest, value, size) {
        for (var i = 0; i < size; i++) {
          HEAPU8[dest + i] = value;
        }
      },
      memory_copy: function (dest, src, num) {
        HEAPU8.copyWithin(dest, src, src + num);
      },
    }

    var info = {
      env,
    };

    const bytes = fs.readFileSync('" ^ filename ^ "');
    WebAssembly.instantiate(bytes, info).then(function (result) {
      receiveInstance(result.instance);
    }).catch(err => console.error(err));
  }

  main();
  "


let dump_js_glue env =
  generate_glue env.output_filename
