(*
 * Collect the references between all top-level declarations,
 * traverse from entry.
 *
 * Which is called tree-shaking.
 *)
open Core_kernel
open Lichenscript_typing

module ModuleMap = Hashtbl.Make(String)

type t = {
  ctx: Type_context.t;
  module_map: Module.t ModuleMap.t;
  top_level_deps: (int list) Array.t;
}

let create ~ctx () =
  let open Type_context in
  let top_level_deps = Array.create ~len:(ResizableArray.size ctx.ty_map) [] in
  {
    ctx;
    module_map = ModuleMap.create ();
    top_level_deps;
  }

let link_from_entry env entry =
  let reach_nodes = Array.create ~len:(ResizableArray.size env.ctx.ty_map) false in

  let orders = ref [] in

  let rec iterate_node id =
    if Array.get reach_nodes id then ()
    else (
      let open Type_context in
      Array.set reach_nodes id true;
      let node = ResizableArray.get env.ctx.ty_map id in

      let deps = node.deps in
      List.iter ~f:iterate_node deps;

      (* after children *)
      let open Core_type.TypeExpr in
      (match node.value with
      | TypeDef _ -> orders := id::!orders;
      | _ -> ()
      )
    )
  in

  iterate_node entry;

  Format.printf "- entry %d\n" entry;
  List.iteri
    ~f:(fun index id ->
      Format.printf "- %d: %d\n" index id
    )
    (List.rev !orders);

  let declarations =
    List.fold
    ~init:[]
    ~f:(fun acc id->
      let open Type_context in
      match Hashtbl.find env.ctx.declarations id with
      | None -> acc
      | Some decl ->
        decl::acc
    )
    (!orders)
  in
  declarations

let set_module env key _mod =
  ModuleMap.set env.module_map ~key ~data:_mod

let get_module env key =
  ModuleMap.find env.module_map key

let has_module env key =
  ModuleMap.mem env.module_map key

let iter_modules env ~f =
  ModuleMap.iter
    ~f env.module_map
