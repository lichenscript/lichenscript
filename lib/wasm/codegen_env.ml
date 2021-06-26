
type t = {
  module_: C_bindings.m;
  config: Config.t;
  mutable last_expr: C_bindings.exp option;
}

let create config =
  {
    module_ = C_bindings.make_module();
    config;
    last_expr = None;
  }
