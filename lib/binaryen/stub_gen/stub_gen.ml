open Core
open Bindings

let codegen =
  let sys_argvs = Sys.get_argv() in
  let last = Array.last sys_argvs in
  if String.equal last "-c" then (
    printf "gen C\n";
    Format.fprintf Format.str_formatter "#include <binaryen-c.h>
    #include <stdlib.h>
    static void clean_binary_result(BinaryenModuleAllocateAndWriteResult result) {
      if (result.binary) {
        free(result.binary);
      }
      if (result.sourceMap) {
        free(result.sourceMap);
      }
    }\n";
    Cstubs.write_c Format.str_formatter ~prefix:"binaryen_stub" (module Bindings);
    let c_content = Format.flush_str_formatter () in
    Out_channel.write_all "b.c" ~data:c_content
  ) else (
    Cstubs.write_ml Format.str_formatter ~prefix:"binaryen_stub" (module Bindings);
    let ml_content = Format.flush_str_formatter () in
    Out_channel.write_all "b.ml" ~data:ml_content
  )
