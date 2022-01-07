open Core_kernel
(*
 * record the dependency of types
 **)

(* num -> [] *)
let type_env: ResizableArray.t = ResizableArray.make 1024

let new_id ty =
  let id = ResizableArray.size type_env in
  ResizableArray.push type_env ty;
  id

let update_node id node =
  ResizableArray.set type_env id node

let size () = ResizableArray.size type_env

let get_node id =
  ResizableArray.get type_env id

let print () =
  let print_item_by_id id =
    let item = get_node id in
    let open Core_type.TypeValue in
    match item.value with
    | Unknown -> "unkonwn"
    | Any -> "any"
    | Unit -> "unit"
    | Ctor (name, []) -> (
      name
    )
    | Ctor (name, _list) -> (
      name ^ "<>"
    )

    | Class _ -> "class"
    | Function _ -> "function"
    | Module _ -> "module"
    | Array _ -> "array"
  in

  let arr_size = size() in
  for i = 0 to (arr_size - 1) do
    let item = get_node i in
    let deps = Buffer.create 64 in
    List.iter ~f:(fun item -> Buffer.add_string deps (Int.to_string item); Buffer.add_string deps " ") item.deps ;
    Format.printf "%d: %s\n" i (Buffer.contents deps);
    Format.printf "\t%s\n\n" (print_item_by_id i);
  done
