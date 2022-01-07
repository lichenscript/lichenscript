(* open Core_type *)

(* let type_assinable left right =
  let open TypeValue in
  match (left, right) with
  | (Any, _)
  | (_, Any)
  | (Unknown, Unknown) -> true
  | (Ctor (c1, []), Ctor (c2, [])) ->
    if TypeSym.(left_sym == right_sym) then
      true
    else
      false

  | _ ->
    false *)
