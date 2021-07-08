open Core_kernel
open Core_type

module SymbolTable = Hashtbl.Make_binable(String)

type t = {
  prev: t option;
  id: int;
  var_symbols: VarSym.t SymbolTable.t;
  type_symbols: TypeSym.t SymbolTable.t;
  mutable var_counter: int;
}

let id scope = scope.id

let create ?prev id =
  {
    prev;
    id;
    var_symbols = SymbolTable.create ();
    type_symbols = SymbolTable.create ();
    var_counter = 0;
  }

let rec find_var_symbol scope name =
  let tmp = SymbolTable.find scope.var_symbols name in
  match tmp with
  | Some _ -> tmp
  | None ->
    Option.(scope.prev >>= (fun parent -> find_var_symbol parent name))

let rec find_type_symbol scope name =
  let tmp = SymbolTable.find scope.type_symbols name in
  match tmp with
  | Some _ -> tmp
  | None ->
    Option.(scope.prev >>= (fun parent -> find_type_symbol parent name))

let insert_var_symbol (scope: t) (sym: VarSym.t) =
  let name = sym.name in
  SymbolTable.set scope.var_symbols ~key:name ~data:sym

let insert_type_symbol (scope: t) (sym: TypeSym.t) =
  let name = TypeSym.name sym in
  SymbolTable.set scope.type_symbols ~key:name ~data:sym

let next_var_id scope =
  let id = scope.var_counter in
  scope.var_counter <- scope.var_counter + 1;
  id

let create_var_symbol scope name =
  let id = next_var_id scope in
  let scope_id = scope.id in
  let sym = VarSym.mk_local ~id_in_scope:id ~scope_id name in
  insert_var_symbol scope sym;
  sym
