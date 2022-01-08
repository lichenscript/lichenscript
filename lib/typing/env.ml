open Base
open Core_kernel

type t = {
  ctx: Type_context.t;
  module_scope: Scope.t;
  open_domains: string array list;
  type_provider: Type_provider.provider;
  mutable current_scope: Scope.t;
  mutable errors: Type_error.t list;
  mutable scope_counter: int;

  (*
   * collect all the return types
   * a function can have multiple return
   *)
  mutable return_types: int list;
}

let create ?(open_domains=[]) ~type_provider ctx =
  let mod_scope = Scope.create ~prev:(Type_context.root_scope ctx) () in
  {
    ctx;
    open_domains;
    type_provider;
    module_scope = mod_scope;
    current_scope = mod_scope;
    errors = [];
    scope_counter = 1;
    return_types = [];
  }

let ctx env = env.ctx

let set_current_scope env scope =
  env.current_scope <- scope

let peek_scope env = env.current_scope

let module_scope env = env.module_scope

let add_error env err =
  env.errors <- err::env.errors

let errors env = List.rev env.errors

let with_new_scope env scope callback =
  let prev_scope = env.current_scope in
  env.current_scope <- scope;
  let result = callback env in
  env.current_scope <- prev_scope;
  result

let add_return_type env return_type =
  env.return_types <- return_type::env.return_types

let take_return_types env = 
  let result = env.return_types in
  env.return_types <- [];
  result

let get_global_type_val env =
  Scope.find_type_symbol (Type_context.root_scope env.ctx)

let unwrap_global_type name env =
  Option.value_exn (get_global_type_val env name)

let ty_u32 = unwrap_global_type "u32"

let ty_i32 = unwrap_global_type "i32"

let ty_f32 = unwrap_global_type "f32"

let ty_char = unwrap_global_type "char"

let ty_string = unwrap_global_type "string"

let ty_boolean = unwrap_global_type "boolean"
