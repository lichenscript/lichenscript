open Core_kernel
open Core_type

module TypeBindingMap = Map.Make(Int)

type t = {
  root_scope: Scope.t;
  scope_stack: Scope.t Stack.t;
  mutable scope_counter: int;
  mutable type_map: Core_type.core_type TypeBindingMap.t;
}

let bind_type env ~sym_id ty =
  env.type_map <- TypeBindingMap.set env.type_map ~key:sym_id ~data:ty;;

let default_symbols = [|
  ("u32", Numeric Num_u32);
  ("i32", Numeric Num_i32);
  ("u64", Numeric Num_u64);
  ("i64", Numeric Num_i64);
  ("f32", Numeric Num_f32);
  ("f64", Numeric Num_f64);
  ("string", String);
|]

let add_default_symbols env (scope: Scope.t) =
  Array.iter
    ~f:(fun (name, ty) ->
      let sym = Symbol.mk_builtin_global ~scope_id:(Scope.id scope) name in
      bind_type env ~sym_id:sym.id ty;
      Scope.set_type_symbol scope name sym;
    )
    default_symbols

let create () =
  let root_scope = Scope.create 0 in
  let env =
    {
      root_scope;
      scope_stack = Stack.create ();
      scope_counter = 1;
      type_map = TypeBindingMap.empty;
    }
  in
  add_default_symbols env root_scope;
  env

let peek_scope env =
  Stack.top_exn env.scope_stack

let push_scope env scope =
  Stack.push env.scope_stack scope

let pop_scope env =
  Stack.pop env.scope_stack

let find_or_create_var_symbol env name =
  Scope.find_or_create_var_symbol env.root_scope name
