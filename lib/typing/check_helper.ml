open Core_kernel
open Core_type

let rec deref_type ctx ty =
  let open TypeExpr in
  match ty with
  | Ref c ->
    let node = Type_context.get_node ctx c in
    deref_type ctx node.value

  | _ -> ty

(* recursive find type *)
let rec find_construct_of ctx c1 =
  let open TypeExpr in
  let node = Type_context.get_node ctx c1 in 
  match node.value with
  | TypeDef sym -> Some (sym, c1)
  | Ctor (c, []) -> (
    let node = Type_context.get_node ctx c in
    let value = deref_type ctx node.value in
    match value with
    | TypeDef sym -> Some (sym, c)
    | _ -> None
  )

  | Ref c ->
    find_construct_of ctx c
      
  | _ -> None

let type_assinable ctx left right =
  let open TypeExpr in
  let left = deref_type ctx left in
  let right = deref_type ctx right in
  match (left, right) with
  | (Any, _)
  | (_, Any) -> false
  | (Ctor (c1, []), Ctor (c2, [])) -> (
    let c1_def = find_construct_of ctx c1 in
    let c2_def = find_construct_of ctx c2 in
    match (c1_def, c2_def) with
    | (Some (left_sym, _), Some (right_sym, _)) ->
      if TypeDef.(left_sym == right_sym) then
        true
      else
        false
    | _ ->
      false

  )

  (* | (Unknown, Unknown) *)
  | _ ->
    false

let check_is_primitive_type ~group ctx (left: TypeExpr.t) (right: TypeExpr.t) =
  let left = deref_type ctx left in
  let right = deref_type ctx right in
  let open Core_type.TypeExpr in
  match (left, right) with
  | (Ctor (left_id, []), (Ctor (right_id, []))) -> (
    let left_def_opt = find_construct_of ctx left_id in
    let right_def_opt = find_construct_of ctx right_id in
    (match (left_def_opt, right_def_opt) with
      | (Some (left, _), Some (right, _)) -> (
        let open TypeDef in
        left.builtin && right.builtin && (String.equal left.name right.name) &&
        (Array.mem group ~equal:String.equal left.name)
      )
      | _ -> false
    )
  )
  | _ -> false

let type_addable =
  check_is_primitive_type ~group:[| "i32"; "u32"; "u64"; "i64"; "f32"; "f64"; "string" |]

let type_arithmetic =
  check_is_primitive_type ~group:[| "i32"; "u32"; "u64"; "i64"; "f32"; "f64"; |]

let type_arithmetic_integer =
  check_is_primitive_type ~group:[| "i32"; "u32"; "u64"; "i64"; |]

let type_logic_compareable =
  check_is_primitive_type ~group:[| "i32"; "u32"; "u64"; "i64"; "f32"; "f64"; "string" |]

let try_unwrap_array ctx expr =
  let expr = deref_type ctx expr in
  let open TypeExpr in
  match expr with
  | Array t -> Some t
  | _ -> None
