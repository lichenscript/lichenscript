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
  ctx: Type_context.t;
  module_map: Module.t ModuleMap.t;
  mutable modules_in_order: Module.t list;
  top_level_deps: (int list) Array.t;
}

let create ~ctx () =
  let open Type_context in
  let top_level_deps = Array.create ~len:(ResizableArray.size ctx.ty_map) [] in
  {
    ctx;
    module_map = ModuleMap.create ();
    modules_in_order = [];
    top_level_deps;
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

let link_from_entry env ~verbose entry =
  let reach_nodes = Array.create ~len:(ResizableArray.size env.ctx.ty_map) false in
  let needs_decl = Array.create ~len:(ResizableArray.size env.ctx.ty_map) false in

  let rec iterate_node path id =
    if VisitPathSet.mem path id then (
      (* it's a cyclic reference *)
      Array.set needs_decl id true
    );
    if Array.get reach_nodes id then
      []
    else (
      let open Type_context in
      Array.set reach_nodes id true;
      let node = ResizableArray.get env.ctx.ty_map id in
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

  let orders = (iterate_node VisitPathSet.empty entry) in

  if verbose then (
    Format.eprintf "- entry %d\n" entry;
    List.iteri
      ~f:(fun index id ->
        Format.printf "- %d: %d\n" index id
      )
      (List.rev orders)
  );

  let declarations =
    List.fold
    ~init:[]
    ~f:(fun acc id->
      if id >= 0 then (
        let open Type_context in
        match Hashtbl.find env.ctx.declarations id with
        | None -> acc
        | Some decl ->
          decl::acc
      ) else (
        let positive = id * (-1) in
        let open Type_context in
        match Hashtbl.find env.ctx.declarations positive with
        | None -> acc
        | Some decl -> (
          match make_declare_of_decl positive decl with
          | Some d -> d::acc
          | None -> acc
        )
      )
    )
    orders
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
