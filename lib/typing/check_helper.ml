open Core_kernel
open Core_type

(* recursive find type *)
let rec find_to_typedef ctx c1 =
  let open TypeValue in
  let node = Type_context.get_node ctx c1 in 
  match node.value with
  | TypeDef sym -> Some (sym, c1)
  | Ctor (c, []) ->
    find_to_typedef ctx c
      
  | _ -> None

let type_assinable ctx left right =
  let open TypeValue in
  match (left, right) with
  | (Any, _)
  | (_, Any) -> false
  | (Ctor (c1, []), Ctor (c2, [])) -> (
    let c1_def = find_to_typedef ctx c1 in
    let c2_def = find_to_typedef ctx c2 in
    match (c1_def, c2_def) with
    | (Some (left_sym, _), Some (right_sym, _)) ->
      if TypeSym.(left_sym == right_sym) then
        true
      else
        false
    | _ ->false
  )

  (* | (Unknown, Unknown) *)
  | _ ->
    false

let type_addable left right =
  let open TypeSym in
  left.builtin && right.builtin && (String.equal left.name right.name) &&
  (Array.mem [| "i32"; "u32"; "u64"; "i64"; "f32"; "f64"; "string" |] ~equal:String.equal left.name)
