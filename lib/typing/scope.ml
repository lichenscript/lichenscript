open Core_kernel

module SymbolTable = Hashtbl.Make_binable(String)

type t = {
  prev: t option;
  var_symbols: int SymbolTable.t;
  type_symbols: int SymbolTable.t;
  mutable var_counter: int;
}

let create ?prev () =
  {
    prev;
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

let insert_var_symbol (scope: t) name (sym: int) =
  SymbolTable.set scope.var_symbols ~key:name ~data:sym

let insert_type_symbol (scope: t) name (sym: int) =
  SymbolTable.set scope.type_symbols ~key:name ~data:sym

let next_var_id scope =
  let id = scope.var_counter in
  scope.var_counter <- scope.var_counter + 1;
  id

let vars scope =
  SymbolTable.to_alist scope.var_symbols

let pp formatter _scope =
  Format.fprintf formatter "Scope"
