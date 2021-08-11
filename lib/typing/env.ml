open Core_kernel
open Core_type

type t = {
  root_scope: Scope.t;
  mutable current_scope: Scope.t;
  mutable errors: Type_error.t list;
  mutable scope_counter: int;
  mutable return_type: TypeValue.t option;
}

let make_default_type_sym scope =
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
    ("Array", Object);
    ("boolean", Primitive);
  |] in
  Array.iter
    ~f:(fun (name, spec) ->
      let scope_id = Scope.id scope in
      let sym = TypeSym.create ~builtin:true  ~scope_id name spec in
      Scope.insert_type_symbol scope sym;
    )
    names

let create () =
  let root_scope = Scope.create 0 "root" in
  let env =
    {
      root_scope;
      current_scope = root_scope;
      errors = [];
      scope_counter = 1;
      return_type = None;
    }
  in
  make_default_type_sym root_scope;
  env

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
