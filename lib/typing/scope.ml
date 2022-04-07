open Core_kernel
open Lichenscript_lex
open Lichenscript_parsing

module SymbolTable = Hashtbl.Make_binable(String)
module ExportTable = Map.Make(String)
module CapturingVarMap = Map.Make(String)

type variable = {
  var_id: int;
  var_kind: Ast.var_kind;
  var_captured: bool ref;
  var_init: bool ref;
  var_loc: Loc.t;
}

type scope_type =
  | ST_Root
  | ST_Normal
  | ST_Class of int
  | ST_Function
  | ST_File
  | ST_Module
  | ST_While
  | ST_Lambda

module ClsElm = struct

  type spec =
    | Property
    | Method
    | Getter
    | Setter

  type t = {
    name: (string * int);
    spec: spec;
    visibility: Asttypes.visibility option;
  }
  
end

type property = {
  prop_id: int;
  prop_visibility: Asttypes.visibility option;
}

class scope ?prev () = object(self)
  val var_symbols = SymbolTable.create ()
  val type_symbols = SymbolTable.create ()
  val generic_type_symbol = Hash_set.create (module String)
  val mutable export_map: Asttypes.visibility option ExportTable.t = ExportTable.empty;
  val mutable capturing_variables = CapturingVarMap.empty
  val mutable var_counter = 0
  val mutable children = []

  method scope_type = ST_Normal

  (* only add blocks, do NOT add a lambda *)
  method add_child (child: scope) =
    children <- child::children

  method children = List.rev children

  method insert_generic_type_symbol (name: string) =
    Hash_set.add generic_type_symbol name

  method is_generic_type_symbol (name: string) =
    let test = Hash_set.mem generic_type_symbol name in
    if test then test
    else (
      match prev with
      | Some prev -> prev#is_generic_type_symbol name
      | None -> false
    )

  method find_local_var_symbol (name: string): variable option =
    SymbolTable.find var_symbols name

  method find_var_symbol (name: string): variable option =
    let tmp = SymbolTable.find var_symbols name in
    match tmp with
    | Some _ -> tmp
    | None ->
      Option.(prev >>= (fun parent -> parent#find_var_symbol name))

  method capturing_variables = capturing_variables

  method set_variable_captured level (name: string): bool =
    let variable = self#find_local_var_symbol name in
    match variable with
    | Some var ->
      if level > 0 then (
        var.var_captured := true;
        true
      ) else
        false

    | None -> (
      (* is a captured *)
      match prev with
      | Some prev_scope -> (
        if prev_scope#set_variable_captured (level+1) name then (
          let id = CapturingVarMap.length capturing_variables in
          capturing_variables <-
            (match CapturingVarMap.add capturing_variables ~key:name ~data:id with
            | `Ok v -> v
            | `Duplicate -> capturing_variables);
          true
        ) else false
      )

      | None -> false
    )

  method find_type_symbol (name: string): int option =
    let tmp = SymbolTable.find type_symbols name in
    match tmp with
    | Some _ -> tmp
    | None ->
      Option.(prev >>= (fun parent -> parent#find_type_symbol name))

  method insert_var_symbol name (var: variable) =
    SymbolTable.set var_symbols ~key:name ~data:var

  method new_var_symbol ~id ~kind ~loc name =
    SymbolTable.add var_symbols ~key:name ~data:{
      var_id = id;
      var_kind = kind;
      var_captured = ref false;
      var_init = ref false;
      var_loc = loc;
    }

  method init_symbol name =
    let var_symbol = SymbolTable.find_exn var_symbols name in
    var_symbol.var_init := true

  method print_type_symbols =
    Option.iter ~f:(fun prev -> prev#print_type_symbols) prev;
    SymbolTable.iteri
      ~f:(fun ~key ~data ->
        Format.eprintf "%s: %d\n" key data;
      )
      type_symbols

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

  method find_cls_element (_name: string) (_spec: ClsElm.spec): ClsElm.t option =
    failwith "only allowed in class scope"

  method insert_cls_element (_var: ClsElm.t) : unit =
    failwith "only allowed in class scope"

  method this_expr : Core_type.TypeExpr.t =
    match prev with
    | Some prev -> prev#this_expr
    | None -> failwith "only allowed in class scope"

  method test_in_class: int option =
    match self#scope_type with
    | ST_Class id -> Some id
    | _ ->
      let open Option in
      prev >>= (fun prev -> prev#test_in_class)

end

(*
 * a enum is also using a class_scope
 *)
class class_scope ?prev cls_id this_expr = object
  inherit scope ?prev ()

  val mutable elements = []

  method! find_cls_element name spec : ClsElm.t option =
    List.find
      ~f:(fun elm ->
        let elm_name, _ = ClsElm.(elm.name) in
        (String.equal elm_name name) && (phys_equal spec elm.spec)
      )
      elements

  method! insert_cls_element elm =
    elements <- elm::elements

  method! this_expr = this_expr

  method! scope_type = ST_Class cls_id

end

class function_scope ?prev () = object
  inherit scope ?prev ()

  method! scope_type = ST_Function

end

class while_scope ?prev () = object
  inherit scope ?prev ()

  method! scope_type = ST_While

end

class lambda_scope ?prev () = object
  inherit function_scope ?prev ()

  method! scope_type = ST_Lambda

end

let pp_scope formatter _scope =
  Format.fprintf formatter "Scope"
