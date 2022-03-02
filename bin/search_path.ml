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
open Core

module SearchPathGenerator = struct

  type t = {
    mutable parts: string list;
  }

  let create entry_dir =
    let parts = Filename.parts (Filename.realpath entry_dir) in
    { parts }

  let next env =
    if List.is_empty env.parts then
      None
    else
      let current_parts = env.parts in
      env.parts <- List.drop_last_exn env.parts;
      Some (Filename.of_parts (List.append current_parts ["node_modules"]))
  
end

let get_search_path_from_node entry_dir: string list =
  let generator = SearchPathGenerator.create entry_dir in
  let ptr = ref (SearchPathGenerator.next generator) in
  let result = ref [] in

  while Option.is_some !ptr do (
    let value = Option.value_exn !ptr in
    result := value::(!result);

    ptr := SearchPathGenerator.next generator;

  ) done;

  List.rev !result
