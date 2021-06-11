open Core_kernel

module SymbolTable = Hashtbl.Make_binable(String)

type t = {
  id: int;
  var_symbols: Symbol.var_sym SymbolTable.t;
  type_symbols: Symbol.type_sym SymbolTable.t;
}

let id scope = scope.id

let create id =
  {
    id;
    var_symbols = SymbolTable.create ();
    type_symbols = SymbolTable.create ();
  }

let find_or_create_var_symbol scope name =
  let opt = SymbolTable.find scope.var_symbols name in
  match opt with
  | Some sym -> sym
  | None ->
    let sym = Symbol.mk_local_vsym ~scope_id:scope.id name in
    let _ =  SymbolTable.add scope.var_symbols ~key:name ~data:sym in
    sym

let find_or_create_type_symbol scope name =
  let opt = SymbolTable.find scope.type_symbols name in
  match opt with
  | Some sym -> sym
  | None ->
    let sym = Symbol.mk_local_tsym ~scope_id:scope.id name in
    let _ =  SymbolTable.add scope.type_symbols ~key:name ~data:sym in
    sym

let set_type_symbol scope name sym =
  SymbolTable.set scope.type_symbols ~key:name ~data:sym;
