open Core_kernel
open Scope
(*
 * record the dependency of types
 **)

(* num -> [] *)
type t = {
  ty_map: ResizableArray.t;
  external_symbol: (int, string) Hashtbl.t;
  root_scope: scope;
  declarations: (int, Typedtree.Declaration.t) Hashtbl.t;
}

let new_id ctx ty =
  let id = ResizableArray.size ctx.ty_map in
  ResizableArray.push ctx.ty_map ty;
  (* if phys_equal id 17 then (
    failwith "fuckit";
  ); *)
  id

let make_default_type_sym ctx scope =
  let open Core_type in
  let open Core_type.TypeDef in
  let names = [|
    ("unit", Primitive);
    ("u32", Primitive);
    ("i32", Primitive);
    ("u64", Primitive);
    ("i64", Primitive);
    ("f32", Primitive);
    ("f64", Primitive);
    ("char", Primitive);
    ("string", Primitive);  (* alias for String *)
    ("boolean", Primitive);
  |] in
  Array.iter
    ~f:(fun (name, spec) ->
      let sym = TypeDef.create ~builtin:true  name spec in
      let node = {
        value = TypeExpr.TypeDef sym;
        loc = Lichenscript_lex.Loc.none;
        deps = [];
        check = none;
      } in
      let id = new_id ctx node in
      scope#insert_type_symbol name id;
    )
    names

let create () =
  let root_scope = new scope () in
  let ctx = {
    ty_map = ResizableArray.make 1024;
    external_symbol = Hashtbl.create (module Int);
    root_scope;
    declarations = Hashtbl.create (module Int);
  } in
  make_default_type_sym ctx root_scope;
  ctx

let update_node ctx id node =
  ResizableArray.set ctx.ty_map id node

let map_node ctx ~f id =
  let node = ResizableArray.get ctx.ty_map id in
  let new_node = f node in
  update_node ctx id new_node

let update_node_type ctx id ty =
  let old_node = ResizableArray.get ctx.ty_map id in
  update_node ctx id { old_node with value = ty }

let set_external_symbol ctx id symbol =
  Hashtbl.set ctx.external_symbol ~key:id ~data:symbol

let find_external_symbol ctx id =
  Hashtbl.find ctx.external_symbol id

let size ctx = ResizableArray.size ctx.ty_map

let get_node ctx id =
  ResizableArray.get ctx.ty_map id

let rec print_type_by_id ctx id =
  let item = get_node ctx id in
  print_type_value ctx item.value

(* first level *)
and print_type_value ctx ty_value =
  let open Core_type.TypeExpr in
  match ty_value with
  | Unknown -> "unknown"
  | Any -> "any"
  | Ctor (var, []) -> (
    let node = get_node ctx var in
    match node.value with
    | TypeDef type_sym ->
      (Format.asprintf "%a" Core_type.TypeDef.pp type_sym)

    | _ ->
      print_type_value ctx node.value

  )
  | Ctor _ -> "ctor"
  | Ref id -> (
    let node = get_node ctx id in
    print_type_value ctx node.value
  )
  | Function _ -> "function"
  | Array _ -> "array"
  | TypeDef type_sym ->
    (Format.asprintf "(typeof %a)" Core_type.TypeDef.pp type_sym)

let print ctx =

  let arr_size = size ctx in
  for i = 0 to (arr_size - 1) do
    let item = get_node ctx i in
    let deps = Buffer.create 64 in
    List.iter ~f:(fun item -> Buffer.add_string deps (Int.to_string item); Buffer.add_string deps " ") item.deps ;
    Format.printf "%d: %s %s\n" i (Buffer.contents deps)
      (Option.value ~default:"None"
        (Option.map ~f:(fun key -> Format.asprintf "\"%a\" %d:%d" Lichenscript_lex.File_key.pp key item.loc.start.line item.loc.start.column) item.loc.source));
    Format.printf "\t%s\n\n" (print_type_by_id ctx i);
  done

let root_scope ctx = ctx.root_scope
