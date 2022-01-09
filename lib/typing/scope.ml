open Core_kernel

module SymbolTable = Hashtbl.Make_binable(String)

class scope ?prev () = object
  val var_symbols = SymbolTable.create ()
  val type_symbols = SymbolTable.create ()
  val mutable var_counter = 0

  method find_var_symbol (name: string): int option =
    let tmp = SymbolTable.find var_symbols name in
    match tmp with
    | Some _ -> tmp
    | None ->
      Option.(prev >>= (fun parent -> parent#find_var_symbol name))

  method find_type_symbol (name: string): int option =
    let tmp = SymbolTable.find type_symbols name in
    match tmp with
    | Some _ -> tmp
    | None ->
      Option.(prev >>= (fun parent -> parent#find_type_symbol name))

  method insert_var_symbol name (sym: int) =
    SymbolTable.set var_symbols ~key:name ~data:sym

  method insert_type_symbol name (sym: int) =
    SymbolTable.set type_symbols ~key:name ~data:sym

  method next_var_id =
    let id = var_counter in
    var_counter <- var_counter + 1;
    id

  method vars =
    SymbolTable.to_alist var_symbols

end

let pp_scope formatter _scope =
  Format.fprintf formatter "Scope"
