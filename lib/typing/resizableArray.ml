
type t = {
  mutable arr: Core_type.node array;
  mutable size: int;
}

exception Out_of_bounds_set of string

let make size =
  {
    arr = Array.make size Core_type.unknown;
    (* 0, not the given `size`. See the comment for this function in the `.mli` file. *)
    size = 0;
  }

let get arr i =
  if i < 0 || i >= arr.size then
    Core_type.unknown
  else
    arr.arr.(i)

let change_capacity arr new_capacity =
  let new_array = Array.make new_capacity Core_type.unknown in
  Array.blit arr.arr 0 new_array 0 arr.size;
  arr.arr <- new_array

let expand_if_needed arr =
  let old_capacity = Array.length arr.arr in
  if arr.size = old_capacity then
    let new_capacity = max (old_capacity * 2) 1 in
    change_capacity arr new_capacity

let set arr i x =
  if i >= arr.size || i < 0 then
    raise (Out_of_bounds_set (Printf.sprintf "Index: %d, size: %d" i arr.size));
  arr.arr.(i) <- x

let push arr elt =
  expand_if_needed arr;
  arr.arr.(arr.size) <- elt;
  arr.size <- arr.size + 1

let shrink arr = if arr.size <> Array.length arr.arr then change_capacity arr arr.size

let size arr = arr.size

let underlying_array_size_do_not_use arr = Array.length arr.arr

(* let to_hashtbl arr =
  let tbl = Hashtbl.create arr.size in
  for i = 0 to arr.size - 1 do
    match arr.arr.(i) with
    | Some v -> Hashtbl.add tbl v i
    | None -> ()
  done;
  tbl *)
