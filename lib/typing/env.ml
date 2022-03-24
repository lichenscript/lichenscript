open Base
open Core_kernel
open Scope

type external_resolver = string -> name:string -> int option

type t = {
  prog: Program.t;
  file_scope: scope;
  external_resolver: external_resolver;
  mutable current_scope: scope;
  mutable errors: Type_error.t list;
  mutable scope_counter: int;
  mutable in_lambda: bool;
  mutable in_declare: bool;
  mutable before_eval_fun_call: string list;
  mutable allow_external: bool;
}

let create ~external_resolver ~file_scope ~allow_external prog =
  {
    prog;
    external_resolver;
    file_scope;
    current_scope = file_scope;
    errors = [];
    scope_counter = 1;
    in_lambda = false;
    in_declare = false;
    before_eval_fun_call = [];
    allow_external;
  }

let prog env = env.prog

let allow_external env = env.allow_external

let in_lambda env = env.in_lambda

let set_in_lambda env v = env.in_lambda <- v

let in_declare env = env.in_declare

let set_in_declare env v = env.in_declare <- v

let capture_variable env ~name =
  if env.in_lambda then 
    ignore (env.current_scope#set_variable_captured 0 name)
  else ()

let set_current_scope env scope =
  env.current_scope <- scope

let peek_scope env = env.current_scope

let file_scope env = env.file_scope

let add_error env err =
  env.errors <- err::env.errors

let errors env = List.rev env.errors

let with_new_scope env scope callback =
  let prev_scope = env.current_scope in
  env.current_scope <- scope;
  let result = callback env in
  env.current_scope <- prev_scope;
  result

let external_resolver env = env.external_resolver

let add_before_eval_fun_call env calls =
  env.before_eval_fun_call <- List.append env.before_eval_fun_call calls

let before_eval_fun_call env = env.before_eval_fun_call

let get_global_type_val env =
  let root_scope = Program.root_scope env.prog in
  root_scope#find_type_symbol

let unwrap_global_type name env =
  Option.value_exn (get_global_type_val env name)

let ty_u32 = unwrap_global_type "u32"

let ty_i32 = unwrap_global_type "i32"

let ty_f32 = unwrap_global_type "f32"

let ty_char = unwrap_global_type "char"

let ty_boolean = unwrap_global_type "boolean"
