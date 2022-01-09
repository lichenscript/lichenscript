open Core_kernel
open Lichenscript_typing

let make_new_name_map ~preserve_names (type_scope: Scope.scope) =
  let name_map = Hashtbl.create (module String) in
  let counter = ref 0 in

  List.iter
    ~f:(fun name -> Hashtbl.set name_map ~key:name ~data:name)
    preserve_names;

  let names = type_scope#vars in
  List.iter
    ~f:(fun (name, _) ->
      if Hashtbl.mem name_map name then (
        let new_name = name ^ (Int.to_string !counter) in
        counter := !counter + 1;
        Hashtbl.set name_map ~key:name ~data:new_name
      ) else (
        Hashtbl.set name_map ~key:name ~data:name
      )
    )
    names;

  name_map

class scope ~preserve_names type_scope = object

  val name_map = make_new_name_map ~preserve_names type_scope

  method codegen_name name =
    match Hashtbl.find name_map name with
    | Some name -> name
    | None -> name
  
end
