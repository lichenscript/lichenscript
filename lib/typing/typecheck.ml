(*
 * Type check deeply, check every expressions
 **)
open Core_kernel

module IntHash = Hashtbl.Make(Int)

let type_check ctx ?(debug=false) () =
  let size = Type_context.size ctx in
  let visited_mark = Array.create ~len:size false in
  let reversed_map = Array.create ~len:size [] in

  let no_deps = ref [] in

  (**
   * iterate the array,
   * get the reversed map,
   *
   * and get nodes depend nothing
   *)
  for i = 0 to (size - 1) do
    let node = Type_context.get_node ctx i in
    let deps = node.deps in
    match deps with
    | [] -> (
      no_deps := i::(!no_deps)
    )
    | _ ->
      List.iter
        ~f:(fun dep ->
          let exist = Array.get reversed_map dep in
          Array.set reversed_map dep (i::exist)
        )
        deps
  done;

  let is_all_id_satisfied ids =
    List.fold
      ~init:true
      ~f:(fun acc id ->
        if not acc then
          acc
        else
          Array.get visited_mark id
      )
      ids
  in

  let rec iterate_node node_id =
    if not (Array.get visited_mark node_id) then (
      let node = Type_context.get_node ctx node_id in
      node.check node_id;
      Array.set visited_mark node_id true;

      let arr = Array.get reversed_map node_id in
      List.iter
      ~f:(fun id ->
        let node = Type_context.get_node ctx id in
        let deps = node.deps in
        if is_all_id_satisfied deps then (
          iterate_node id
        )
      )
      arr
    )
  in

  (try
    List.iter ~f:iterate_node !no_deps;
  with
  | e -> (
    if debug then
      Type_context.print ctx
    );
    raise e
  );
  []
