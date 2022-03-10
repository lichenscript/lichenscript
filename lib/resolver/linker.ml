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
open Core_kernel
open Lichenscript_lex
open Lichenscript_typing

(*
 * Collect the references between all top-level declarations,
 * traverse from entry.
 *
 * Which is called tree-shaking.
 *)

module ModuleMap = Hashtbl.Make(String)
module VisitPathSet = Set.Make(Int)

type t = {
  prog: Program.t;
  module_map: Module.t ModuleMap.t;
  mutable modules_in_order: Module.t list;
  top_level_deps: (int list) Array.t;
  mutable external_resources: string list;
}

let create ~prog () =
  let open Program in
  let top_level_deps = Array.create ~len:(ResizableArray.size prog.ty_map) [] in
  {
    prog;
    module_map = ModuleMap.create ();
    modules_in_order = [];
    top_level_deps;
    external_resources = [];
  }

let make_declare_of_decl id (decl: Typedtree.Declaration.t) =
  let open Typedtree in
  match decl.spec with
  | Declaration.Function_ _fun -> (
    let decl_spec = Declaration.Declare {
      decl_visibility = None;
      decl_spec = Declaration.DeclFunction _fun.header;
      decl_loc = Loc.none;
      decl_ty_var = id;
    } in
    Some { decl with
      spec = decl_spec
    }
  )

  | _ -> None

(*
 * 1. for normal function, the parent should be added after the children
 *
 *    function A() -> function B()
 *
 *    the generated order should be:
 *
 *    functionB() { ... }
 *    functionA() { ... }
 *
 * 2. for cyclic function, a declaration should be added:
 *
 *    function A() -> function B() -> function A()
 *
 *    the generated order should be:
 *
 *    declare functionA()
 *    functionB() { ... }
 *    functionA() { ... }
 *
 * 3. for more complex function
 *
 *    function A() -> function D()
 *                 \
 *                  \-> function B() -> function C() -> function D()
 *
 *    the generated order should be:
 *
 *    function D()
 *    function C()
 *    function B()
 *    function A()
 *
 *    the deeper branch should be linked firstly
 *
 *)
let rec link_from_entry env ~verbose entry : Typedtree.Declaration.t list =
  link_from_entry_internal env ~verbose entry

and link_from_entry_internal env ~verbose entry : Typedtree.Declaration.t list =
  let needs_decl = Array.create ~len:(ResizableArray.size env.prog.ty_map) false in

  (* remove duplicate nodes *)
  let normalize_deps deps =
    let reached_nodes = Hash_set.create (module Int) in
    Array.filter
      ~f:(fun dep ->
        if Hash_set.mem reached_nodes dep then
          false
        else (
          Hash_set.add reached_nodes dep;
          true
        )
      )
      deps
  in

  let rec iterate_node path id =
    if VisitPathSet.mem path id then (
      (* it's a cyclic reference, set decls in the children *)
      Array.set needs_decl id true;
      []
    ) else (
      let open Program in

      let node = ResizableArray.get env.prog.ty_map id in
      let next_path = VisitPathSet.add path id in

      let children_orders =
        node.deps
        |> List.map ~f:(iterate_node next_path)
        |> List.concat
      in

      (* add this declaration to order after children *)
      let open Core_type.TypeExpr in
      let result = 
        match node.value with
        | TypeDef _ -> id::children_orders;
        | _ -> children_orders
      in

      if Array.get needs_decl id then
        List.append result [id * (-1)]
      else
        result
    )
  in

  let orders =
    iterate_node VisitPathSet.empty entry
    |> List.to_array
  in

  Array.rev_inplace orders;
  let orders = normalize_deps orders in

  if verbose then (
    Format.eprintf "- entry %d\n" entry;
    Array.iteri
      ~f:(fun index id ->
        Format.printf "- %d: %d\n" index id
      )
      orders
  );

  (*
   * if a declaration is tagged as @builtin()
   * do NOT generate it
   *)
  let is_builtin (decl: Typedtree.Declaration.t) =
    let open Lichenscript_parsing.Ast in
    decl.attributes
    |> List.find ~f:(function
    | { attr_name = { txt = "builtin"; _ }; _ } -> true
    | _ -> false
    )
    |> Option.is_some
  in

  let declarations =
    orders
    |> Array.fold
      ~init:[]
      ~f:(fun acc id->
        if id >= 0 then (
          let open Program in
          match Hashtbl.find env.prog.declarations id with
          | None -> acc
          | Some decl ->
            if is_builtin decl then
              acc
            else
              decl::acc
        ) else (
          let positive = id * (-1) in
          let open Program in
          match Hashtbl.find env.prog.declarations positive with
          | None -> acc
          | Some decl -> (
            if is_builtin decl then
              acc
            else (
              match make_declare_of_decl positive decl with
              | Some d -> d::acc
              | None -> acc
            )
          )
        )
      )
    |> List.rev
  in
  declarations

let set_module env key _mod =
  env.modules_in_order <- _mod::env.modules_in_order;
  ModuleMap.set env.module_map ~key ~data:_mod

let get_module env key =
  ModuleMap.find env.module_map key

let has_module env key =
  ModuleMap.mem env.module_map key

let iter_modules env ~f =
  env.modules_in_order
  |> List.iter ~f

let add_external_resource env res =
  env.external_resources <- res::(env.external_resources)

let external_resources env = List.rev env.external_resources
