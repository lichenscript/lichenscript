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

let find_var_symbol scope name =
  SymbolTable.find scope.var_symbols name

let find_type_symbol scope name =
  SymbolTable.find scope.type_symbols name

let set_var_symbol scope name sym =
  SymbolTable.set scope.var_symbols ~key:name ~data:sym

let set_type_symbol scope name sym =
  SymbolTable.set scope.type_symbols ~key:name ~data:sym

let create_var_symbol scope name =
  let id = scope.var_counter in
  scope.var_counter <- scope.var_counter + 1;
  let scope_id = scope.id in
  let sym = VarSym.mk_local ~id_in_scope:id ~scope_id name in
  set_var_symbol scope name sym;
  sym
