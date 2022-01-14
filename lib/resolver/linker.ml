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

      let open Core_type.TypeExpr in
      (match node.value with
      | TypeDef _ -> orders := id::!orders;
      | _ -> ()
      );

      let deps = node.deps in
      List.iter ~f:iterate_node deps
    )
  in

  iterate_node entry;

  Format.printf "- entry %d\n" entry;
  List.iteri
    ~f:(fun index id ->
      Format.printf "- %d: %d\n" index id
    )
    (!orders)

let set_module env key _mod =
  ModuleMap.set env.module_map ~key ~data:_mod

let get_module env key =
  ModuleMap.find env.module_map key

let has_module env key =
  ModuleMap.mem env.module_map key

let iter_modules env ~f =
  ModuleMap.iter
    ~f env.module_map

(* let rec collect_deps env =
  Hashtbl.iteri
    ~f:(collect_declaration env)
    env.ctx.declarations

and collect_declaration env ~key:_ ~data:decl =
  let open Declaration in
  match decl.spec with
  | Class _ -> ()

  | Function_ _fun -> (
    let id = _fun.header.id in
    let header_refs = collect_function_header _fun.header in

    Array.set env.top_level_deps id header_refs
  )

  | Declare _decare -> ()

  | Enum _ -> (
    failwith "enum"
  )

  | Import _ -> ()

and collect_function_header header =
  let open Function in
  let refs = ref [] in
  let { params; _ } = header in

  List.iter
    ~f:(fun param ->
      refs := param.param_ty::(!refs)
    )
    params.params_content;

  List.rev !refs


(*
 * Figure out all reached nodes
 *)
let assemble_optimize_tree env (entry: int) =
  let reach_node = Array.create ~len:(ResizableArray.size env.ctx.ty_map) false in

  let rec iterate_node id = 
    if Array.get reach_node id then
      ()
    else (
      Array.set reach_node id true;
      let deps = Array.get env.top_level_deps id in
      List.iter ~f:iterate_node deps
    )
  in

  iterate_node entry;

  reach_node *)
