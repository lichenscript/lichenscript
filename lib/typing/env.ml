open Core_kernel
open Core_type

type t = {
  root_scope: Scope.t;
  mutable current_scope: Scope.t;
  mutable errors: Type_error.t list;
  mutable scope_counter: int;
  mutable return_type: TypeValue.t option;
}

let make_default_module_sym scope =
  let module_t = TypeSym.create_module() in
  let str_ty = Option.value_exn (Scope.find_type_symbol scope "string")in
  PropsMap.set module_t.props ~key:"log" ~data:(Core_type.TypeValue.(Function {
    tfun_params = [("content", Ctor str_ty)];
    tfun_ret = Unit;
  }));
  let console = TypeSym.create ~builtin:true ~kind:Global ~scope_id:(Scope.id scope) "console" (TypeSym.Module_ module_t) in
  Scope.insert_type_symbol scope console;
  let extern_module = PropsMap.create() in
  let log_method =
    { VarSym.
      id_in_scope = Scope.next_var_id scope;
      name = "log";
      def_type = TypeValue.Unknown;
      def_loc = None;
      kind = Local;
      scope_id = Scope.id scope;
      builtin = true;
      spec = Core_type.VarSym.ExternalMethod "__wtl_console_log"
    }
  in
  PropsMap.set extern_module ~key:"log" ~data:log_method;
  let var_sym =
    { VarSym.
      id_in_scope = Scope.next_var_id scope;
      name = "console";
      def_type = Ctor console;
      def_loc = None;
      kind = Global;
      scope_id = Scope.id scope;
      builtin = true;
      spec = Core_type.VarSym.ExternalModule(extern_module);
    }
  in
  Scope.insert_var_symbol scope var_sym

let make_default_type_sym scope =
  let names = [|
    "u32";
    "i32";
    "u64";
    "i64";
    "f32";
    "f64";
    "char";
    "string";
    "boolean";
  |] in
  Array.iter
    ~f:(fun name ->
      let sym = TypeSym.create ~builtin:true ~kind:Global ~scope_id:(Scope.id scope) name TypeSym.Primitive in
      Scope.insert_type_symbol scope sym;
    )
    names
  ;
  make_default_module_sym scope

let create () =
  let root_scope = Scope.create 0 in
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
