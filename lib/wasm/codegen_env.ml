open Core_kernel
open Binaryen.Dsl

module T = Waterlang_typing

let allocator_facility_flag = 0b00000001
let string_facility_flag    = 0b00000010

type js_snippet = {
  js_fun_def: string;
  js_add_env_def: string option;
}

module CodegenScope : sig
  type t

  val root: t

  val next_label: t -> string

  val next_temp_value_id: t -> T.Core_type.TypeValue.t -> int

  val make: T.Scope.t -> t
  
end = struct
  module TempValueMap = Map.Make(Int)

  type t = {
    typed_scope: T.Scope.t option;
    mutable name_counter: int;
    mutable temp_value_distributor: int;
    mutable temp_value_type: T.Core_type.TypeValue.t TempValueMap.t;
  }

  let root = {
    typed_scope = None;
    name_counter = 0;
    temp_value_distributor = 0;
    temp_value_type = TempValueMap.empty;
  }

  let next_label scope =
    let current_count = scope.name_counter in
    scope.name_counter <- current_count + 1;
    (* TODO: use name of function *)
    let base_name = "scope_" ^ (string_of_int current_count) in
    base_name

  let next_temp_value_id scope ty =
    let current_value = scope.temp_value_distributor in
    scope.temp_value_type <- TempValueMap.set scope.temp_value_type ~key:current_value ~data:ty;
    scope.temp_value_distributor <- current_value + 1;
    current_value

  let make scope =
    let open T.Scope in
    let vars_count = SymbolTable.length scope.var_symbols in
    {
      typed_scope = Some scope;
      name_counter = 0;
      temp_value_distributor = vars_count;
      temp_value_type = TempValueMap.empty;
    }
  
end

type t = {
  module_: Bound.module_;
  output_filename: string;
  config: Config.t;
  mutable facilities_flags: int;
  data_segment: Data_segment_allocator.t;
  mutable js_snippets: js_snippet list;
  program: T.Program.t;
  mutable while_label: string option;
  scopes: CodegenScope.t Stack.t;
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
    scopes = Stack.of_list [ CodegenScope.root ]
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

let gen_label_name env =
  let scope = Stack.top_exn env.scopes in
  CodegenScope.next_label scope

let with_scope env scope callback =
  Stack.push env.scopes scope;
  let result = callback env in
  let _ = Stack.pop_exn env.scopes in
  result
