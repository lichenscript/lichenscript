open Core_kernel
open Core_type

type t = {
  root_scope: Scope.t;
  scope_stack: Scope.t Stack.t;
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
      scope_stack = Stack.create ();
      errors = [];
      scope_counter = 1;
    }
  in
  add_default_symbols root_scope;
  env

let peek_scope env =
  Stack.top_exn env.scope_stack

let push_scope env scope =
  Stack.push env.scope_stack scope

let pop_scope env =
  Stack.pop env.scope_stack

let find_or_create_var_symbol env name =
  Scope.find_or_create_var_symbol env.root_scope name

let find_or_create_type_symbol env name =
  Scope.find_or_create_type_symbol env.root_scope name

let find_type_symbol env name =
  Scope.find_type_symbol env.root_scope name

let add_error env err =
  env.errors <- err::env.errors
