open Core_kernel

type t = {
  module_: C_bindings.m;
  output_filename: string;
  config: Config.t;
  mutable last_expr: C_bindings.exp option;
}

let create ?output_filename config =
  {
    module_ = C_bindings.make_module();
    output_filename = Option.value output_filename ~default:"test";
    config;
    last_expr = None;
  }
