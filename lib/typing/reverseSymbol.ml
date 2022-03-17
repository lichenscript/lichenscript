open Lichenscript_lex
open Core_kernel

type symbol_log_item = (Loc.t * int)

type file = {
  mutable symbol_log: symbol_log_item list;
  mutable sorted_symbol: symbol_log_item array option;
}

type t = {
  file_map: (string, file) Hashtbl.t;
}

let create () = {
  file_map = Hashtbl.create (module String);
}

let create_file () = {
  symbol_log = [];
  sorted_symbol = None;
}

let sort_log file =
  let arr = List.to_array file.symbol_log in
  file.symbol_log <- [];  (* release the memory *)
  Array.sort arr ~compare:(fun (left, _) (right, _) ->
    let open Loc in
    left.start.offset - right._end.offset
  );
  file.sorted_symbol <- Some arr;
  arr

let log_file file (loc: Loc.t) id =
  file.symbol_log <- (loc, id)::(file.symbol_log)

let log env (loc: Loc.t) id =
  let source = Option.value_exn loc.source in
  let path = Format.asprintf "%a" File_key.pp source in
  match Hashtbl.find env.file_map path with
  | Some file -> log_file file loc id
  | None -> (
    let file = create_file () in
    log_file file loc id;
    ignore (Hashtbl.add env.file_map ~key:path ~data:file)
  )

let rec find_symbol_in_sorted_array arr find_start find_end offset =
  if find_start > find_end then
    None
  else (
    let mid = find_start + ((find_end - find_start) / 2) in
    let mid_item = Array.get arr mid in
    let open Loc in
    let mid_item_loc, mid_item_value = mid_item in
    if offset >= mid_item_loc.start.offset && offset <= mid_item_loc._end.offset then
      Some mid_item_value
    else if offset < mid_item_loc.start.offset then
      find_symbol_in_sorted_array arr find_start (mid - 1) offset
    else
      find_symbol_in_sorted_array arr (mid + 1) find_end offset
  )

let find_symbol env path offset =
  let open Option in
  Hashtbl.find env.file_map path >>= fun file ->
  let sorted_symbol =
    match file.sorted_symbol with
    | Some s -> s
    | None -> (
      sort_log file
    )
  in
  let find_end = Array.length sorted_symbol in
  find_symbol_in_sorted_array sorted_symbol 0 (find_end - 1) offset
