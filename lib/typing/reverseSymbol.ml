(*
 * Copyright 2022 Vincent Chan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *)
open Lichenscript_lex
open Lichenscript_common
open Core_kernel
open Scope

type symbol_log_item = (Loc.t * int)

type file = {
  mutable symbol_log: symbol_log_item list;
  mutable sorted_symbol: symbol_log_item array option;
  mutable scope_range: scope RangeTree.node;
  member_access: (int, Core_type.TypeExpr.t) Hashtbl.t;
}

type t = {
  file_map: (string, file) Hashtbl.t;
}

let create () = {
  file_map = Hashtbl.create (module String);
}

let create_file scope total_lines =
  let scope_range = RangeTree.create_root_node { left = 1; right = total_lines; data = scope } in
  {
    symbol_log = [];
    sorted_symbol = None;
    scope_range;
    member_access = Hashtbl.create (module Int);
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

let create_log_for_file env path scope total_lines =
  let file = create_file scope total_lines in
  Hashtbl.set env.file_map ~key:path ~data:file

let log env (loc: Loc.t) id =
  let source = Option.value_exn loc.source in
  let path = Format.asprintf "%a" File_key.pp source in
  let file = Hashtbl.find_exn env.file_map path in
  log_file file loc id

let log_range env (loc: Loc.t) scope =
  let source = Option.value_exn loc.source in
  let path = Format.asprintf "%a" File_key.pp source in
  let file = Hashtbl.find_exn env.file_map path in
  file.scope_range <- RangeTree.insert_value file.scope_range { data = scope; left = loc.start.offset; right = loc._end.offset }

let log_member_access env (loc: Loc.t) ty =
  let source = Option.value_exn loc.source in
  let path = Format.asprintf "%a" File_key.pp source in
  let file = Hashtbl.find_exn env.file_map path in
  Hashtbl.set file.member_access ~key:loc._end.offset ~data:ty

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

let find_scope_in_range env path offset : Scope.scope option =
  Hashtbl.find env.file_map path
  |> Option.map
  ~f:(fun file ->
    let scope = RangeTree.find_value file.scope_range offset in
    scope
  )

let find_member_access env path offset : Core_type.TypeExpr.t option =
  let open Option in
  Hashtbl.find env.file_map path >>= fun file ->
  Hashtbl.find file.member_access offset
