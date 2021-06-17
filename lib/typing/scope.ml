open Core_kernel
open Core_type

module SymbolTable = Hashtbl.Make_binable(String)

type t = {
  prev: t option;
  id: int;
  var_symbols: VarSym.t SymbolTable.t;
  type_symbols: TypeSym.t SymbolTable.t;
}

let id scope = scope.id

let create ?prev id =
  {
    prev;
    id;
    var_symbols = SymbolTable.create ();
    type_symbols = SymbolTable.create ();
  }

let find_var_symbol scope name =
  SymbolTable.find scope.var_symbols name

let find_type_symbol scope name =
  SymbolTable.find scope.type_symbols name

let set_var_symbol scope name sym =
  SymbolTable.set scope.var_symbols ~key:name ~data:sym

let set_type_symbol scope name sym =
  SymbolTable.set scope.type_symbols ~key:name ~data:sym;
