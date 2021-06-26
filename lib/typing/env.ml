open Core_kernel
open Core_type

type t = {
  root_scope: Scope.t;
  mutable current_scope: Scope.t;
  mutable errors: Type_error.t list;
  mutable scope_counter: int;
}

let default_symbols = [|
  ("u32", TypeValue.Numeric Num_u32);
  ("i32", TypeValue.Numeric Num_i32);
  ("u64", TypeValue.Numeric Num_u64);
  ("i64", TypeValue.Numeric Num_i64);
  ("f32", TypeValue.Numeric Num_f32);
  ("f64", TypeValue.Numeric Num_f64);
  ("string", String);
|]

let add_default_symbols (scope: Scope.t) =
  Array.iter
    ~f:(fun (name, ty) ->
      let sym = TypeSym.mk_builtin_global ~scope_id:(Scope.id scope) name in
      TypeSym.bind_value sym ty;
      Scope.set_type_symbol scope name sym;
    )
    default_symbols

let create () =
  let root_scope = Scope.create 0 in
  let env =
    {
      root_scope;
      current_scope = root_scope;
      errors = [];
      scope_counter = 1;
    }
  in
  add_default_symbols root_scope;
  env

let set_current_scope env scope =
  env.current_scope <- scope

let peek_scope env = env.current_scope

let add_error env err =
  env.errors <- err::env.errors

let with_new_scope env scope callback =
  let prev_scope = env.current_scope in
  env.current_scope <- scope;
  let result = callback env in
  env.current_scope <- prev_scope;
  result
