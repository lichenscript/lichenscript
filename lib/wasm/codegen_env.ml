open Core_kernel
open Binaryen.Dsl

let allocator_facility_flag = 0b00000001
let string_facility_flag    = 0b00000010

type js_snippet = {
  js_fun_def: string;
  js_add_env_def: string option;
}

type t = {
  module_: Bound.module_;
  output_filename: string;
  config: Config.t;
  mutable facilities_flags: int;
  data_segment: Data_segment_allocator.t;
  mutable js_snippets: js_snippet list;
  program: Waterlang_typing.Program.t;
  mutable while_label: string option;
}

let create ?output_filename config program =
  {
    module_ = module_create ();
    output_filename = Option.value output_filename ~default:"test";
    config;
    facilities_flags = 0;
    data_segment = Data_segment_allocator.init_with_begin_offset config.data_segment_offset;
    js_snippets = [];
    program;
    while_label = None;
  }

let turn_on_allocator env =
  env.facilities_flags <- env.facilities_flags lor allocator_facility_flag

let turn_on_string env =
  turn_on_allocator env;
  env.facilities_flags <- env.facilities_flags lor string_facility_flag

let needs_allocator env =
  not (Int.equal (env.facilities_flags land allocator_facility_flag) 0)

let needs_string env =
  not (Int.equal (env.facilities_flags land string_facility_flag) 0)

let ptr_ty env =
  match env.config.arch with
  | Config.ARCH_WASM32 -> i32

  | Config.ARCH_WASM64 -> i64

let ptr_size env =
  match env.config.arch with
  | Config.ARCH_WASM32 -> 4
  | Config.ARCH_WASM64 -> 8

let add_js_snippet env snippet =
  env.js_snippets <- snippet::env.js_snippets
