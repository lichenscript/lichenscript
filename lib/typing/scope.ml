open Core_kernel
open Lichenscript_parsing

module SymbolTable = Hashtbl.Make_binable(String)
module ExportTable = Map.Make(String)

type variable = {
  var_id: int;
  var_kind: Ast.var_kind;
}

class scope ?prev () = object
  val var_symbols = SymbolTable.create ()
  val type_symbols = SymbolTable.create ()
  val mutable export_map: Asttypes.visibility option ExportTable.t = ExportTable.empty;
  val mutable var_counter = 0

  method find_var_symbol (name: string): variable option =
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

  method insert_var_symbol name (var: variable) =
    SymbolTable.set var_symbols ~key:name ~data:var

  method insert_type_symbol name (sym: int) =
    SymbolTable.set type_symbols ~key:name ~data:sym

  method next_var_id =
    let id = var_counter in
    var_counter <- var_counter + 1;
    id

  method vars =
    SymbolTable.to_alist var_symbols

  method set_visibility name (visibility: Lichenscript_parsing.Asttypes.visibility option): unit =
    export_map <- ExportTable.set export_map ~key:name ~data:visibility

  method get_visibility name: Lichenscript_parsing.Asttypes.visibility option =
    Option.join (ExportTable.find export_map name)

end

let pp_scope formatter _scope =
  Format.fprintf formatter "Scope"
