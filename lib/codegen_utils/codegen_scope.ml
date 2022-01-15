open Core_kernel

class scope ~preserve_names () = object

  val id_map = Hashtbl.create (module Int)
  val used_name = Hash_set.of_list (module String) preserve_names
  val generated_name = Hashtbl.create (module String)

  method codegen_name name =
    Hash_set.add used_name name;
    name
  
  method codegen_id (name, id): string =
    (match Hashtbl.find id_map id with
    | Some name -> name
    | None ->
      let final_name = name ^ "_" ^ (Int.to_string id) in
      Hashtbl.set id_map ~key:id ~data:final_name;
      Hashtbl.set generated_name ~key:name ~data:final_name;
      final_name
    )

  method id_name_exn name =
    Hashtbl.find_exn generated_name name
  
end
