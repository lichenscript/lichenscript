open Core_kernel
(*
 * record the dependency of types
 **)

(* num -> [] *)
type t = ResizableArray.t

let create () = ResizableArray.make 1024

let new_id ctx ty =
  let id = ResizableArray.size ctx in
  ResizableArray.push ctx ty;
  id

let update_node ctx id node =
  ResizableArray.set ctx id node

let size ctx = ResizableArray.size ctx

let get_node ctx id =
  ResizableArray.get ctx id

let print ctx =
  let rec print_item_by_id id =
    let item = get_node ctx id in
    let open Core_type.TypeValue in
    match item.value with
    | Unknown -> "unkonwn"
    | Any -> "any"
    | Unit -> "unit"
    | Ctor (name, []) -> (
      print_item_by_id name
    )
    | Ctor _ -> "ctor"
    (*
    | Ctor (name, _list) -> (
      name ^ "<>"
    ) *)

    | Class _ -> "class"
    | Function _ -> "function"
    | Module _ -> "module"
    | Array _ -> "array"
    | TypeDef sym ->
      (Core_type.TypeSym.name sym)

  in

  let arr_size = size ctx in
  for i = 0 to (arr_size - 1) do
    let item = get_node ctx i in
    let deps = Buffer.create 64 in
    List.iter ~f:(fun item -> Buffer.add_string deps (Int.to_string item); Buffer.add_string deps " ") item.deps ;
    Format.printf "%d: %s\n" i (Buffer.contents deps);
    Format.printf "\t%s\n\n" (print_item_by_id i);
  done
