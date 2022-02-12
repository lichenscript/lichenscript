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
open Core_type

module TypeVarMap = Map.Make(String)

(* recursive find type *)
let find_construct_of ctx type_expr: (TypeDef.t * TypeExpr.t list) option =
  let open TypeExpr in
  let type_expr = Type_context.deref_type ctx type_expr in
  match type_expr with
  | Ctor (c, args) -> (
    let value = Type_context.deref_type ctx c in
    match value with
    | TypeDef sym -> Some (sym, args)
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
  | (Method _, _)
  | (_, Method _) -> false
  | (String, String) -> true

  (* Array _ <- Array<T> *)
  | (Array _, Array(TypeSymbol _)) -> true

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
    | (Some({ id = enum_id; spec = Enum _; _ }, _), Some({ spec = EnumCtor { enum_ctor_super_id; _ }; _}, _)) ->
      enum_id = enum_ctor_super_id

    (* assigning class to interface *)
    | (Some({ id = intf_id; spec = Interface _ ; _ }, _), Some({ spec = Class { tcls_implements; _ }; _ }, _)) -> (
      let test =
        List.find
        ~f:(fun (impl, _) ->
          let ctor_id = find_construct_of ctx impl in
          match ctor_id with
          | Some({ TypeDef. id; _ }, _) -> id = intf_id
          | None -> false
        )
        tcls_implements in
      Option.is_some test
    )

    | (Some(left_def, left_args), Some(right_def, right_args)) ->
      TypeDef.(left_def == right_def) && (
        let result = List.fold2
          ~init:true
          ~f:(fun acc left right ->
            if (not acc) then
              acc
            else
              type_equal ctx left right
          )
          left_args right_args
        in
        match result with
        | List.Or_unequal_lengths.Ok r -> r
        | List.Or_unequal_lengths.Unequal_lengths -> false
      )
    
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
    | (Some({ id = enum_id; spec = Enum _; _ }, _), Some({ spec = EnumCtor { enum_ctor_super_id; _ }; _}, _)) ->
      enum_id = enum_ctor_super_id

    | (Some(left_sym, _), Some(right_sym, _)) ->
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
    | (Some(left, []), Some(right, [])) -> (
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
    | Some(def, []) -> (
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
    check_is_primitive_type ~group:[| "i32"; "u32"; "u64"; "i64"; "f32"; "f64"; "char" |] ctx left right

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

let is_primitive_with_name ctx ~name:expect_name type_expr =
  let type_expr = Type_context.deref_type ctx type_expr in
  let expr_def_opt = find_construct_of ctx type_expr in
  (match expr_def_opt with
    | Some(def, []) -> (
      let open TypeDef in
      def.builtin &&
      String.equal def.name expect_name
    )
    | _ -> false
  )

let is_unit = is_primitive_with_name ~name:"unit"

let is_i32 = is_primitive_with_name ~name:"i32"

let is_f32 = is_primitive_with_name ~name:"f32"

let is_char = is_primitive_with_name ~name:"char"

let is_boolean = is_primitive_with_name ~name:"boolean"

let rec replace_type_vars_with_maps ctx type_map type_expr =
  let open Core_type.TypeExpr in
  match type_expr with
  | TypeSymbol sym_name -> (
    match TypeVarMap.find type_map sym_name with
    | Some v -> v
    | None -> type_expr
  )

  | Unknown
  | Any -> type_expr

  | Ctor (m, list) -> (
    let m = replace_type_vars_with_maps ctx type_map m in
    let list = List.map ~f:(replace_type_vars_with_maps ctx type_map) list in
    Ctor(m, list)
  )

  | Ref ref_id -> (
    let node = Type_context.get_node ctx ref_id in
    replace_type_vars_with_maps ctx type_map node.value
  )

  | Lambda _ -> type_expr

  | Method _ -> type_expr

  | Array arr ->
    Array (replace_type_vars_with_maps ctx type_map arr)

  | String -> type_expr
  | TypeDef _ -> type_expr

and replace_params_with_type ctx type_map params =
  let open TypeExpr in
  let { params_content; params_rest } = params in
  let params_content =
    List.map
    ~f:(fun (name, expr) -> (name, replace_type_vars_with_maps ctx type_map expr))
    params_content
  in
  let params_rest =
    Option.map
    ~f:(fun (name, expr) -> (name, replace_type_vars_with_maps ctx type_map expr))
    params_rest
  in
  { params_content; params_rest }

let get_visibility_of_class_elm class_elm =
  let open Core_type.TypeDef in
  match class_elm with
  | Cls_elm_prop (v, _, _) -> v
  | Cls_elm_method(v, _) -> v
  | Cls_elm_get_set(v, _, _) -> v

let rec find_member_of_class ctx ~scope type_expr member_name type_vars =
  let type_expr = Type_context.deref_type ctx type_expr in
  let open TypeDef in
  match type_expr with
  | TypeDef { id = cls_id; spec = Class cls; _ } -> (
    let test_scope = scope#test_class_scope in
    let outside_finder = (
      fun (elm_name, elm) ->
        let visibility = get_visibility_of_class_elm elm in
        String.equal elm_name member_name && (Visibility.access_in_module visibility)
      )
    in
    let finder =
      match test_scope with
      | Some id -> (
        (* in the current class *)
        if id = cls_id then
          (fun (elm_name, _) -> String.equal elm_name member_name)
        else
          outside_finder
      )
      | None -> outside_finder
    in
    let result = List.find ~f:finder cls.tcls_elements in

    let types_map =
      List.fold2_exn
        ~init:TypeVarMap.empty
        ~f:(fun acc def_var given_var ->
          TypeVarMap.set acc ~key:def_var ~data:given_var
        )
        cls.tcls_vars type_vars
    in

    let open Option in
    let open Core_type in
    match result with
    | Some (_, cls_elm) -> (
      match cls_elm with
      | Cls_elm_method (_, ({ TypeDef. id = member_id; spec = ClassMethod { method_params; method_return; _ }; _ } as def)) -> (
        let params = replace_params_with_type ctx types_map method_params in
        let rt = replace_type_vars_with_maps ctx types_map method_return in
        let expr = TypeExpr.Method(def, params, rt) in
        Some (expr, member_id)
      )

      | Cls_elm_get_set(_, Some ({ TypeDef. id = member_id; spec = ClassMethod { method_params; method_return; _ }; _ } as def), _) -> (
        let params = replace_params_with_type ctx types_map method_params in
        let rt = replace_type_vars_with_maps ctx types_map method_return in
        let expr = TypeExpr.Method(def, params, rt) in
        Some (expr, member_id)
      )

      | Cls_elm_prop (_, member_id, value) ->
        Some (replace_type_vars_with_maps ctx types_map value, member_id)

      | _ -> None
    )
    | None -> (
      cls.tcls_extends >>= fun ancester ->
      find_member_of_type ctx ~scope ancester member_name
    )
  )
  | _ -> failwith "unrechable"

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
and find_member_of_type ctx ~scope type_expr member_name : (TypeExpr.t * int) option =
  let type_expr = Type_context.deref_type ctx type_expr in
  match type_expr with
  | Ctor(type_expr, type_vars) -> (
    let type_expr = Type_context.deref_type ctx type_expr in
    let open TypeDef in
    match type_expr with
    | TypeDef { spec = Class _; _ } ->
      find_member_of_class ctx ~scope type_expr member_name type_vars

    | TypeDef { spec = Interface intf; _ } -> (
      let find_method =
        List.find intf.intf_methods
        ~f:(fun (method_name, _) -> String.equal method_name member_name)
      in
      let open Option in
      find_method >>= (fun (_, intf_elm) ->
        match intf_elm with
        | Cls_elm_method (_, ({ TypeDef. id = member_id; spec = ClassMethod { method_params; method_return; _ }; _ } as def)) -> (
          (* let params = replace_params_with_type ctx types_map method_params in
          let rt = replace_type_vars_with_maps ctx types_map method_return in *)
          let expr = TypeExpr.Method(def, method_params, method_return) in
          Some (expr, member_id)
        )
        | _ -> None
      )
    )

    | _ -> None
  )

  (* it's type def itself, find the static member *)
  | TypeDef { spec = Class { tcls_static_elements; _ }; _ } -> (
    let result =
      List.find ~f:(fun (static_memeber_name, _) -> String.equal static_memeber_name member_name)
      tcls_static_elements
    in

    let open Core_type.TypeDef in
    match result with
    | Some (_, Cls_elm_method(_, _method)) ->
      let member_id = _method.id in
      let node = Type_context.get_node ctx member_id in
      Some (node.value, member_id)

    | Some (_, Cls_elm_prop(_, id, prop)) ->
      Some (prop, id)

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

type pattern_exhausted =
  | Pat_exausted
  | Pat_begin
  | Pat_boolean of bool * bool  (* has true, has false *)
  | Pat_enum_branch of (Typedtree.identifier * Typedtree.Pattern.t) list
[@@deriving show]
