(*
 * This file is part of LichenScript Compiler.
 *
 * LichenScript Compiler is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 *
 * LichenScript Compiler is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with LichenScript Compiler. If not, see <https://www.gnu.org/licenses/>.
 *)
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

let rec type_assinable ctx left right =
  let open TypeExpr in
  let left = Type_context.deref_type ctx left in
  let right = Type_context.deref_type ctx right in
  match (left, right) with
  | (Any, _) -> true
  | (_, Any) -> false
  | (String, String) -> true

  | (Array left_arr, Array right_arr) ->
    type_equal ctx left_arr right_arr

  | (Lambda (params1, rt1), Lambda (params2, rt2)) -> (
    let test_params =
      List.fold2_exn
        ~init:true
        ~f:(fun acc (_, item1) (_, item2) ->
          if not acc then acc
          else
            type_assinable ctx item2 item1
        )
        params1.params_content
        params2.params_content
    in

    let test_rest =
      match (params1.params_rest, params2.params_rest) with
      | Some (_, r1), Some (_, r2) ->
        type_assinable ctx r2 r1

      | None, None -> true

      | _, _ -> false
    in

    let test_rt =
      if not (type_assinable ctx rt1 rt2) then (
        false
      ) else 
        true
    in
    test_params && test_rest && test_rt
  )

  | _ -> (
    let c1_def = find_construct_of ctx left in
    let c2_def = find_construct_of ctx right in
    match (c1_def, c2_def) with
    | (Some { id = enum_id; spec = Enum _; _ }, Some { spec = EnumCtor { enum_ctor_super_id; _ }; _}) ->
      enum_id = enum_ctor_super_id

    | (Some left_sym, Some right_sym) ->
      if TypeDef.(left_sym == right_sym) then
        true
      else
        false
    
    | _ ->
      false
  )

and type_equal ctx left right =
  let open TypeExpr in
  let left = Type_context.deref_type ctx left in
  let right = Type_context.deref_type ctx right in
  match (left, right) with
  | (Any, Any) -> true
  | (String, String) -> true
  | (Array left_arr, Array right_arr) ->
    type_equal ctx left_arr right_arr
  | _, _ -> (
    let c1_def = find_construct_of ctx left in
    let c2_def = find_construct_of ctx right in
    match (c1_def, c2_def) with
    | (Some { id = enum_id; spec = Enum _; _ }, Some { spec = EnumCtor { enum_ctor_super_id; _ }; _}) ->
      enum_id = enum_ctor_super_id

    | (Some left_sym, Some right_sym) ->
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
    | (Some left, Some right) -> (
      let open TypeDef in
      left.builtin && right.builtin && (String.equal left.name right.name) &&
      (Array.mem group ~equal:String.equal left.name)
    )
    | _ -> false
  )

let type_should_not_release ctx expr =
  let group = [| "unit"; "i32"; "u32"; "u64"; "i64"; "f32"; "f64"; "char"; "boolean" |] in
  let expr = Type_context.deref_type ctx expr in
  let expr_def_opt = find_construct_of ctx expr in
  (match expr_def_opt with
    | Some def -> (
      let open TypeDef in
      def.builtin &&
      (Array.mem group ~equal:String.equal def.name)
    )
    | _ -> false
  )

let type_addable ctx left right =
  let left = Type_context.deref_type ctx left in
  let right = Type_context.deref_type ctx right in
  match (left, right) with
  | TypeExpr.String, TypeExpr.String -> true
  | _ ->
    check_is_primitive_type ~group:[| "i32"; "u32"; "u64"; "i64"; "f32"; "f64" |] ctx left right

let type_arithmetic =
  check_is_primitive_type ~group:[| "i32"; "u32"; "u64"; "i64"; "f32"; "f64"; |]

let type_arithmetic_integer =
  check_is_primitive_type ~group:[| "i32"; "u32"; "u64"; "i64"; |]

let type_logic_compareable ctx left right =
  let left = Type_context.deref_type ctx left in
  let right = Type_context.deref_type ctx right in
  match (left, right) with
  | TypeExpr.String, TypeExpr.String -> true
  | _ ->
    check_is_primitive_type ~group:[| "i32"; "u32"; "u64"; "i64"; "f32"; "f64" |] ctx left right

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

let find_classname_of_type ctx type_expr =
  let type_expr = Type_context.deref_type ctx type_expr in
  match type_expr with
  | Ctor(type_expr, _) -> (
    let type_expr = Type_context.deref_type ctx type_expr in
    let open TypeDef in
    match type_expr with
    | TypeDef { spec = Class cls; _ } -> (
      Some cls.tcls_name
    )
    | _ -> None
  )

  | _ -> None

let is_array ctx type_expr =
  let type_expr = Type_context.deref_type ctx type_expr in
  match type_expr with
  | Array _ -> true
  | _ -> false

let is_i32 ctx type_expr =
  let type_expr = Type_context.deref_type ctx type_expr in
  let expr_def_opt = find_construct_of ctx type_expr in
  (match expr_def_opt with
    | Some def -> (
      let open TypeDef in
      def.builtin &&
      String.equal def.name "i32"
    )
    | _ -> false
  )

(*
  * TODO: namespace
  * class/enum static function
  * object's property/method
  *
  * Special case:
  * 1. for array: T[]
  *    share the member of Array<T>
  *
  * 2. for string
  *    share the member of String
  *
  *)
let rec find_member_of_type ctx ~scope type_expr member_name =
  let type_expr = Type_context.deref_type ctx type_expr in
  match type_expr with
  | Ctor(type_expr, _) -> (
    let type_expr = Type_context.deref_type ctx type_expr in
    let open TypeDef in
    match type_expr with
    | TypeDef { spec = Class cls; _ } -> (
      let result =
        List.find ~f:(fun (elm_name, _) -> String.equal elm_name member_name)
        cls.tcls_elements
      in
      match result with
      | Some (_, member_id) ->
        let node = Type_context.get_node ctx member_id in
        Some (node.value, member_id)

      | _ -> None
    )
    | _ -> None
  )

  (* it's type def itself, find the static member *)
  | TypeDef { spec = Class { tcls_static_elements; _ }; _ } -> (
    let result =
      List.find ~f:(fun (static_memeber_name, _) -> String.equal static_memeber_name member_name)
      tcls_static_elements
    in

    match result with
    | Some (_, member_id) ->
      let node = Type_context.get_node ctx member_id in
      Some (node.value, member_id)

    | _ -> None
  )

  | Array t -> (
    let ty_int_opt = scope#find_type_symbol "Array" in
    match ty_int_opt with
    | Some ty_int -> (
      let array_node = Type_context.get_node ctx ty_int in
      (* building type Array<T> *)
      let ctor_type = TypeExpr.Ctor(array_node.value, [t]) in
      find_member_of_type ctx ~scope ctor_type member_name
    )
    | None -> failwith "Can not find Array type in current scope"
  )

  | String -> (
    let ty_int_opt = scope#find_type_symbol "String" in
    match ty_int_opt with
    | Some ty_int -> (
      let array_node = Type_context.get_node ctx ty_int in
      (* building type Array<T> *)
      let ctor_type = TypeExpr.Ctor(array_node.value, []) in
      find_member_of_type ctx ~scope ctor_type member_name
    )
    | None -> failwith "Can not find String type in current scope"
  )

  | _ -> None

