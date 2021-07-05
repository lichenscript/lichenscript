open Core_kernel

module StaticStringPool = Map.Make(String)

let allocator_facility_flag = 0b00000001
let string_facility_flag    = 0b00000010

type t = {
  module_: C_bindings.m;
  output_filename: string;
  config: Config.t;
  mutable facilities_flags: int;
  mutable static_string_pool: int StaticStringPool.t;
  mutable last_expr: C_bindings.exp option;
}

let create ?output_filename config =
  {
    module_ = C_bindings.make_module();
    output_filename = Option.value output_filename ~default:"test";
    config;
    facilities_flags = 0;
    static_string_pool = StaticStringPool.empty;
    last_expr = None;
  }

let add_static_string env str =
  let opt = StaticStringPool.find env.static_string_pool str in
  match opt with
  | Some result -> result
  | None ->
    let id = StaticStringPool.length env.static_string_pool in
    let next = StaticStringPool.set env.static_string_pool ~key:str ~data:id in
    env.static_string_pool <- next;
    id

let turn_on_allocator env =
  env.facilities_flags <- env.facilities_flags lor allocator_facility_flag

let turn_on_string env =
  turn_on_allocator env;
  env.facilities_flags <- env.facilities_flags lor string_facility_flag