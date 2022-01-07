open Base
open Core_kernel
open Core_type

type t = {
  ctx: Type_context.t;
  root_scope: Scope.t;
  open_domains: string array list;
  mutable current_scope: Scope.t;
  mutable errors: Type_error.t list;
  mutable scope_counter: int;
  mutable return_type: TypeValue.t option;
}

let make_default_type_sym ctx scope =
  let open TypeSym in
  let names = [|
    ("u32", Primitive);
    ("i32", Primitive);
    ("u64", Primitive);
    ("i64", Primitive);
    ("f32", Primitive);
    ("f64", Primitive);
    ("char", Primitive);
    ("string", Object);
    ("boolean", Primitive);
  |] in
  Array.iter
    ~f:(fun (name, spec) ->
      let sym = TypeSym.create ~builtin:true  name spec in
      let node = {
        value = TypeValue.TypeDef sym;
        loc = Waterlang_lex.Loc.none;
        deps = [];
        check = none;
      } in
      let id = Type_context.new_id ctx node in
      Scope.insert_type_symbol scope name id;
    )
    names

let has_make_default = ref false

let create ?(open_domains=[]) ctx =
  let mod_scope = Scope.create ~prev:(Scope.root_scope) () in
  let env =
    {
      ctx;
      root_scope = Scope.root_scope;
      open_domains;
      current_scope = mod_scope;
      errors = [];
      scope_counter = 1;
      return_type = None;
    }
  in
  if not !has_make_default then (
    make_default_type_sym ctx Scope.root_scope;
    has_make_default := true
  );
  env

let ctx env = env.ctx

let root_scope env = env.root_scope

let set_current_scope env scope =
  env.current_scope <- scope

let peek_scope env = env.current_scope

let add_error env err =
  env.errors <- err::env.errors

let errors env = List.rev env.errors

let with_new_scope env scope callback =
  let prev_scope = env.current_scope in
  env.current_scope <- scope;
  let result = callback env in
  env.current_scope <- prev_scope;
  result

let set_return_type env return_type =
  env.return_type <- return_type

let return_type env = env.return_type

let get_global_type_val env =
  Scope.find_type_symbol env.root_scope

let unwrap_global_type name env =
  Option.value_exn (get_global_type_val env name)

let ty_u32 = unwrap_global_type "u32"

let ty_i32 = unwrap_global_type "i32"

let ty_f32 = unwrap_global_type "f32"

let ty_char = unwrap_global_type "char"

let ty_string = unwrap_global_type "string"

let ty_boolean = unwrap_global_type "boolean"
