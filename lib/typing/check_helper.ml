open Core_kernel
open Core_type

(* recursive find type *)
let find_construct_of ctx type_expr =
  let open TypeExpr in
  let type_expr = Type_context.deref_type ctx type_expr in
  match type_expr with
  | Ctor (c, _) -> (
    let value = Type_context.deref_type ctx c in
    match value with
    | TypeDef sym -> Some sym
    | _ -> None
  )

  | _ -> None

let find_typedef_of ctx type_expr = 
  let open TypeExpr in
  let type_expr = Type_context.deref_type ctx type_expr in
  match type_expr with
  | TypeDef sym -> Some sym
  | _ -> None

let type_assinable ctx left right =
  let open TypeExpr in
  let left = Type_context.deref_type ctx left in
  let right = Type_context.deref_type ctx right in
  match (left, right) with
  | (Any, _) -> true
  | (_, Any) -> false
  | _ -> (
    let c1_def = find_construct_of ctx left in
    let c2_def = find_construct_of ctx right in
    match (c1_def, c2_def) with
    | (Some (left_sym, _), Some (right_sym, _)) ->
      if TypeDef.(left_sym == right_sym) then
        true
      else
        false
    | _ ->
      false
  )


let check_is_primitive_type ~group ctx (left: TypeExpr.t) (right: TypeExpr.t) =
  let left = Type_context.deref_type ctx left in
  let right = Type_context.deref_type ctx right in
  let left_def_opt = find_construct_of ctx left in
  let right_def_opt = find_construct_of ctx right in
  (match (left_def_opt, right_def_opt) with
    | (Some (left, _), Some (right, _)) -> (
      let open TypeDef in
      left.builtin && right.builtin && (String.equal left.name right.name) &&
      (Array.mem group ~equal:String.equal left.name)
    )
    | _ -> false
  )

let type_addable =
  check_is_primitive_type ~group:[| "i32"; "u32"; "u64"; "i64"; "f32"; "f64"; "string" |]

let type_arithmetic =
  check_is_primitive_type ~group:[| "i32"; "u32"; "u64"; "i64"; "f32"; "f64"; |]

let type_arithmetic_integer =
  check_is_primitive_type ~group:[| "i32"; "u32"; "u64"; "i64"; |]

let type_logic_compareable =
  check_is_primitive_type ~group:[| "i32"; "u32"; "u64"; "i64"; "f32"; "f64"; "string" |]

let try_unwrap_array ctx expr =
  let expr = Type_context.deref_type ctx expr in
  let open TypeExpr in
  match expr with
  | Array t -> Some t
  | _ -> None

let contruct_enum_case ctx enum_ctor _param_types =
  let open Core_type.TypeDef in
  let super_id = enum_ctor.enum_ctor_super_id in
  let _super_node = Type_context.get_node ctx super_id in
  failwith "not implemented"
  (* match super_node.value with
  | TypeDef { spec = Enum enum; _ } -> (
    if List.is_empty enum.enum_params then
      Ok (TypeExpr.Ctor (super_id, []))
    else (
      let provided_types = List.to_array param_types in
      let params = List.foldi
        ~init:(Ok [])
        ~f:(fun index acc param_name ->
          let open Result in
          acc >>= fun acc ->
          if index >= Array.length provided_types then (
            let err = Type_error.(CannotContructParamOfType(param_name, super_node.value)) in
            Error err
          ) else (
            let provided = Array.get provided_types index in
            let node = Type_context.get_node ctx provided in
            Ok (node.value::acc)
          )
        )
        enum_ctor.enum_ctor_params
      in
      Result.map ~f:(fun params -> TypeExpr.Ctor (super_id, params)) params
    )
  )
  | _ -> failwith "super of enum case is not a enum" *)
