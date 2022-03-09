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
open Lichenscript_lex
open Lichenscript_parsing

module IntHash = Hashtbl.Make(Int)
module T = Typedtree

type import_checker = Ast.Import.t -> unit

(*
 * Type check deeply, check every expressions
 **)
type env = {
  ctx: Program.t;
  mutable scope: Scope.scope;

  (*
   * collect all the return types
   * a function can have multiple return
   *)
  mutable return_types: (TypeExpr.t * Loc.t) list;

  (*
  * Check all the import symbols if they conflict with
  * the symbol in this package
  *
  * But it needs the relationship with other modules,
  * only the resolver has that informations.
  *)
  import_checker: import_checker;
}

let add_return_type env ret = env.return_types <- ret::env.return_types

let take_return_type env =
  let types = env.return_types in
  env.return_types <- [];
  types

let get_global_type_val env =
  let root_scope = Program.root_scope env.ctx in
  root_scope#find_type_symbol

let unwrap_global_type name env =
  Option.value_exn (get_global_type_val env name)

let[@warning "-unused-value-declaration"]
  ty_u32 = unwrap_global_type "u32"

let[@warning "-unused-value-declaration"]
  ty_i32 = unwrap_global_type "i32"

let[@warning "-unused-value-declaration"]
  ty_f32 = unwrap_global_type "f32"

let[@warning "-unused-value-declaration"]
  ty_char = unwrap_global_type "char"

let[@warning "-unused-value-declaration"]
  ty_boolean = unwrap_global_type "boolean"

let with_scope env scope cb =
  let prev = env.scope in
  env.scope <- scope;
  let result = cb env in
  env.scope <- prev;
  result

let rec typecheck_module ctx ~import_checker ~verbose (typedtree: T.program) =
  try
    let { T. tprogram_declarations; tprogram_scope; _ } = typedtree in
    let env = {
      ctx;
      scope = tprogram_scope;
      return_types = [];
      import_checker;
    } in
    List.iter ~f:(check_declaration env) tprogram_declarations;
    if verbose then (
      Program.print ctx
    );
  with e ->
    if verbose then (
      Program.print ctx
    );
    raise e

and check_declaration env decl =
  let open T.Declaration in
  match decl.spec with
  | Class cls -> check_class env cls
  | Function_ _fun -> check_function env _fun
  | Interface intf -> check_interface env intf
  | Declare _ -> ()
  | Enum enum -> check_enum env enum
  | Import import -> env.import_checker import

and check_enum env enum =
  let open T.Enum in
  let { elements; _ } = enum in

  List.iter
    ~f:(fun elm ->
      match elm with
      | Method _method -> check_method_content env _method
      | _ -> ()
    )
    elements

(*
 * TODO: check if the use of the type is safe
 * maybe it reference some illegal type
 *)
and check_interface _env _intf =
  (* failwith "intf" *)
  ()

and check_function env _fun =
  let open T.Function in
  let { header; scope; body; _ } = _fun in
  let _, name_id = header.name in
  with_scope env scope (fun env ->
    check_block env body;

    let type_node = Program.get_node env.ctx name_id in
    let typedef = Option.value_exn (Check_helper.find_typedef_of env.ctx type_node.value) in
    let unwrap_function =
      match typedef with
      | { TypeDef. spec = Function _fun; _ } -> _fun
      | _ -> failwith "unwrap function failed"
    in

    check_function_return_type env unwrap_function.fun_return body
  )

and check_function_return_type env fun_return_ty (block: T.Block.t) =
  let block_return_type = Program.deref_node_type env.ctx block.return_ty in

  let is_last_stmt_return (block: T.Block.t) =
    let last_stmt = List.last block.body in
    match last_stmt with
    | Some { T.Statement. spec = Return _; _ } -> true
    | _ -> false
  in

  if not (is_last_stmt_return block) && not (Check_helper.type_assinable env.ctx fun_return_ty block_return_type) then (
    let open Diagnosis in
    let spec = Type_error.CannotReturn(fun_return_ty, block_return_type) in
    let err = make_error env.ctx block.loc spec in
    raise (Error err)
  );

  let return_types = take_return_type env in

  (* there are return statements, check every statments *)
  List.iter
    ~f:(fun (return_ty, return_loc) ->
      if not (Check_helper.type_assinable env.ctx fun_return_ty return_ty) then (
        let open Diagnosis in
        let spec = Type_error.CannotReturn(fun_return_ty, return_ty) in
        let err = make_error env.ctx return_loc spec in
        raise (Error err)
      )
    )
    return_types

and check_block env blk =
  let open T.Block in
  let { body; scope; return_ty; _ } = blk in
  with_scope env scope (fun env ->
    List.iter
      ~f:(check_statement env)
      body;
    let last_opt = List.last body in
    match last_opt with
    | Some { Typedtree.Statement. spec = Expr expr ; _ } -> (
      let ty_var = Typedtree.Expression.(expr.ty_var) in
      Program.update_node_type env.ctx return_ty (TypeExpr.Ref ty_var)
    )

    | _ -> (
      Program.update_node_type env.ctx return_ty (TypeExpr.Unit)
    )
  )

and check_statement env stmt =
  let open T.Statement in
  match stmt.spec with
  | Expr expr -> check_expression env expr
  | Semi expr ->
    (* TODO: check return is unit *)
    check_expression env expr

  | While { while_test; while_block; _ } -> (
    check_expression env while_test;
    let test_type = Program.deref_node_type env.ctx while_test.ty_var in
    if not (Check_helper.is_boolean env.ctx test_type) then (
      let open Diagnosis in
      let spec = Type_error.WhileTestShouldBeBoolean test_type in
      let err = make_error env.ctx while_test.loc spec in
      raise (Error err)
    );
    List.iter ~f:(check_statement env) while_block.body
  )

  | Binding binding -> (
    let { binding_pat; binding_init; _ } = binding in
    check_expression env binding_init;
    match binding_pat.spec with
    | T.Pattern.Underscore -> ()
    | T.Pattern.Symbol(_, ty_var) -> (
      let expr_node = Program.get_node env.ctx binding_init.ty_var in
      Program.update_node_type env.ctx ty_var expr_node.value
    )
    | _ -> failwith "unreachable"
  )

  | Break _ -> ()

  | Continue _ -> ()

  | Debugger -> ()

  | Return None -> (
    add_return_type env (TypeExpr.Unit, stmt.loc)
  )

  | Return (Some expr) -> (
    check_expression env expr;
    let node_type = Program.deref_node_type env.ctx expr.ty_var in
    add_return_type env (node_type, expr.loc)
  )

  | Empty -> ()

and check_expression_if env if_spec =
  let open T.Expression in
  let { if_test; if_consequent; if_alternative; if_ty_var; if_loc; _ } = if_spec in
  check_expression env if_test;

  let test_type = Program.deref_node_type env.ctx if_test.ty_var in
  if not (Check_helper.is_boolean env.ctx test_type) then (
    let open Diagnosis in
    let spec = Type_error.IfTestShouldBeBoolean test_type in
    let err = make_error env.ctx if_test.loc spec in
    raise (Error err)
  );

  check_block env if_consequent;

  let node = Program.get_node env.ctx if_consequent.return_ty in

  let if_ty =
    match if_alternative with
    | Some (If_alt_block blk) -> (
      check_block env blk;

      let blk_ty = Program.deref_node_type env.ctx blk.return_ty in
      if Check_helper.type_assinable env.ctx node.value blk_ty then
        node.value
      else if Check_helper.type_assinable env.ctx node.value blk_ty then
        blk_ty
      else (
        let err = Diagnosis.(make_error env.ctx blk.loc (NotAssignable(node.value, blk_ty))) in
        raise (Diagnosis.Error err)
      )
    )
    | Some (If_alt_if desc) -> (
      check_expression_if env desc;

      let alt_ty = Program.deref_node_type env.ctx desc.if_ty_var in
      if Check_helper.type_assinable env.ctx node.value alt_ty then
        node.value
      else if Check_helper.type_assinable env.ctx node.value alt_ty then
        alt_ty
      else (
        let err = Diagnosis.(make_error env.ctx desc.if_loc (NotAssignable(node.value, alt_ty))) in
        raise (Diagnosis.Error err)
      )
    )
    (*
     * if there is no alternative, this if expression is regarded as 'unit' type,
     *)
    | None -> (
      let result = TypeExpr.Unit in
      if not (Check_helper.type_assinable env.ctx result node.value) then (
        let err = Diagnosis.(make_error env.ctx if_loc (NotAssignable(result, node.value))) in
        raise (Diagnosis.Error err)
      );

      result
    )
  in

  Program.update_node_type env.ctx if_ty_var if_ty

and check_expression env expr =
  let open T.Expression in
  let expr_loc = expr.loc in
  match expr.spec with
  | Constant _ -> ()

  (*
   * if it's a enum constructor, auto construct it
   *)
  | Identifier (_, name_id) -> (
    let node = Program.get_node env.ctx name_id in
    let test_typedef = Check_helper.find_typedef_of env.ctx node.value in
    Option.iter
      ~f:(fun typedef ->
        match typedef with
        | { id; spec = EnumCtor { enum_ctor_params = []; _ }; _ } -> (
          Program.update_node_type env.ctx name_id (TypeExpr.Ctor(Ref id, []))
        )
        | _ -> ()
      )
      test_typedef
  )

  | Lambda lambda -> check_lambda env lambda

  | If if_desc -> check_expression_if env if_desc

  | Array arr_list -> (
    let init_sym = TypeExpr.TypeSymbol "T" in
    let arr_type =
      List.fold
        ~init:init_sym
        ~f:(fun acc expr ->
          check_expression env expr;
          let item_type = Program.deref_node_type env.ctx expr.ty_var in
          match acc with
          | TypeExpr.TypeSymbol _ -> item_type
          | _ -> (
            if Check_helper.type_assinable env.ctx acc item_type then
              acc
            else if Check_helper.type_assinable env.ctx item_type acc then
              item_type
            else (
              let err = Diagnosis.(make_error env.ctx expr.loc (NotAssignable(acc, item_type))) in
              raise (Diagnosis.Error err)
            )
          )
        )
        arr_list
    in
    Program.update_node_type env.ctx expr.ty_var TypeExpr.(Array arr_type)
  )

  | Tuple children -> (
    let children_types = List.map
      ~f:(fun child ->
        check_expression env child;
        Program.deref_node_type env.ctx child.ty_var
      )
      children
    in
    Program.update_node_type env.ctx expr.ty_var TypeExpr.(Tuple children_types)
  )

  | Map map_entries -> (
    let map_opt = env.scope#find_type_symbol "Map" in
    let map_ty = Option.value_exn ~message:"Cannot found Map in the current scope" map_opt in
    if List.is_empty map_entries then (
      Program.update_node_type
        env.ctx
        expr.ty_var
        TypeExpr.(Ctor (Ref map_ty, [(TypeSymbol "K"); (TypeSymbol "V")]))
    ) else (
      (* TODO: check every expressions are the same *)
      List.iter
        ~f:(fun { map_entry_value; _ } ->
          check_expression env map_entry_value;
        )
        map_entries;

      let key_ty, value_ty =
        List.fold
        ~init:(TypeExpr.TypeSymbol "K", TypeExpr.TypeSymbol "V")
        ~f:(fun (_acc_key, acc_value) entry ->
          check_expression env entry.map_entry_value;
          let key_ty =
            match entry.map_entry_key with
            | Ast.Literal.String _ -> TypeExpr.String
            (* | Ast.Literal.Char _ -> TypeExpr.Ref(ty_char env)
            | Ast.Literal.Integer _ -> TypeExpr.Ref(ty_i32 env)
            | Ast.Literal.Boolean _ -> TypeExpr.Ref(ty_boolean env)
            | Ast.Literal.Float _ ->  *)
            | _ ->
              let ty = TypeExpr.Ref(ty_f32 env) in
              let err = Diagnosis.(make_error env.ctx entry.map_entry_loc (CannotUsedAsKeyOfMap ty)) in
              raise (Diagnosis.Error err)
          in
          let node_type = Program.deref_node_type env.ctx entry.map_entry_value.ty_var in
          let value_ty =
            match (acc_value, node_type) with
            | (TypeExpr.TypeSymbol "V", _) -> node_type
            | _ ->
              if Check_helper.type_assinable env.ctx acc_value node_type then
                acc_value
              else if Check_helper.type_assinable env.ctx node_type acc_value then
                node_type
              else (
                let err = Diagnosis.(make_error env.ctx entry.map_entry_loc (NotAssignable(acc_value, node_type))) in
                raise (Diagnosis.Error err)
              )
          in
          (key_ty, value_ty)
        )
        map_entries
      in
      Program.update_node_type env.ctx expr.ty_var TypeExpr.(Ctor (Ref map_ty, [key_ty; value_ty]))
    )
  )

  | Call { callee; call_params; _ } -> (
    check_expression env callee;
    List.iter ~f:(check_expression env) call_params;
    let ctx = env.ctx in
    let ty_int = callee.ty_var in
    let deref_type_expr = Program.deref_node_type ctx ty_int  in

    let check_params symbol_map (expected_params: TypeExpr.params) =
      let { TypeExpr. params_content; params_rest } = expected_params in
      let pass_params = List.to_array call_params in
      let expected_params = List.to_array params_content in
      let symbol_map = ref symbol_map in
      for i = 0 to ((Array.length expected_params) - 1) do
        let expect_param_name, expect_param = Array.get expected_params i in
        if i >= (Array.length pass_params) then (
          let err = Diagnosis.(make_error env.ctx expr_loc (ParamDoesNotProvided expect_param_name)) in
          raise (Diagnosis.Error err)
        );
        let pass_param = Array.get pass_params i in
        let pass_param_type = Program.deref_node_type env.ctx pass_param.ty_var in
        let test_map, test_result = Check_helper.type_assinable_with_maps env.ctx !symbol_map expect_param pass_param_type in
        symbol_map := test_map;
        if not test_result then (
          let err = Diagnosis.(make_error env.ctx expr_loc (CannotPassParam(expect_param_name, expect_param, pass_param_type))) in
          raise (Diagnosis.Error err)
        )
      done;
      if (Array.length pass_params) > (Array.length expected_params) then (
        match params_rest with
        | Some (rest_name, rest_ty) -> (
          let array_content = match Check_helper.try_unwrap_array env.ctx rest_ty with
            | Some v -> v
            | None -> (
              let err = Diagnosis.(make_error env.ctx expr_loc RestShouldBeArray) in
              raise (Diagnosis.Error err)
            )
          in
          let pass_rest = Array.slice pass_params (Array.length expected_params) (Array.length pass_params) in
          Array.iter
            ~f:(fun param ->
              let pass_param_type = Program.deref_node_type env.ctx param.ty_var in
              if not (Check_helper.type_assinable env.ctx array_content pass_param_type) then (
                let err = Diagnosis.(make_error env.ctx expr_loc (CannotPassParam(rest_name, array_content, pass_param_type))) in
                raise (Diagnosis.Error err)
              )
            )
            pass_rest
        )

        | _ ->
          let expected = Array.length expected_params in
          let actual = Array.length pass_params in
          let err = Diagnosis.(make_error env.ctx expr_loc (UnexpectedParams(expected, actual))) in
          raise (Diagnosis.Error err)
      );
      !symbol_map
    in

    let check_enum_ctor_params symbol_map (params: TypeExpr.t list) =
      let pass_params = List.to_array call_params in
      let expected_params = List.to_array params in

      let symbol_map = ref symbol_map in

      for i = 0 to ((Array.length expected_params) - 1) do
        let expect_param = Array.get expected_params i in
        let pass_param = Array.get pass_params i in
        let pass_param_type = Program.deref_node_type env.ctx pass_param.ty_var in
        match expect_param with
        | TypeExpr.TypeSymbol sym_name ->
          symbol_map := Check_helper.TypeVarMap.set !symbol_map ~key:sym_name ~data:pass_param_type

        | _ ->
          let expect_param_name = Int.to_string i in
          if i >= (Array.length pass_params) then (
            let err = Diagnosis.(make_error env.ctx expr_loc (ParamDoesNotProvided expect_param_name)) in
            raise (Diagnosis.Error err)
          );
          let pass_param_type = Program.deref_node_type env.ctx pass_param.ty_var in
          if not (Check_helper.type_assinable env.ctx expect_param pass_param_type) then (
            let err = Diagnosis.(make_error env.ctx expr_loc (CannotPassParam(expect_param_name, expect_param, pass_param_type))) in
            raise (Diagnosis.Error err)
          )
      done;

      !symbol_map
    in

    match deref_type_expr with
    | TypeExpr.Lambda(params, ret) -> (
      ignore (check_params Check_helper.TypeVarMap.empty params);
      Program.update_node_type ctx expr.ty_var ret
    )

    | TypeExpr.Method(_, params, ret) -> (
      let symbol_map = check_params Check_helper.TypeVarMap.empty params in
      Program.update_node_type ctx expr.ty_var (Check_helper.replace_type_vars_with_maps env.ctx symbol_map ret)
    )

    | _ ->
      begin
        let _ty_def = Check_helper.find_construct_of ctx deref_type_expr in
        match deref_type_expr with
        | TypeExpr.TypeDef { TypeDef. spec = Function _fun; _ } ->
          let symbol_map = check_params Check_helper.TypeVarMap.empty _fun.fun_params in
          Program.update_node_type ctx expr.ty_var (Check_helper.replace_type_vars_with_maps env.ctx symbol_map _fun.fun_return)

        | TypeExpr.TypeDef { TypeDef. spec = EnumCtor enum_ctor; _} -> (
          let super_id = enum_ctor.enum_ctor_super_id in

          let symbol_map = check_enum_ctor_params Check_helper.TypeVarMap.empty enum_ctor.enum_ctor_params in

          let enum_base_node = Program.deref_node_type env.ctx super_id in
          let unwrap_enum_base =
            match enum_base_node with
            | TypeExpr.TypeDef { TypeDef. spec = Enum enum; _ } -> enum
            | _ -> failwith "unrechable"
          in

          let params_types =
            List.map
              ~f:(fun name ->
                match Check_helper.TypeVarMap.find symbol_map name with
                | Some ty -> ty
                | None -> TypeExpr.TypeSymbol name
              )
              unwrap_enum_base.enum_params
          in

          Program.update_node_type ctx expr.ty_var (TypeExpr.Ctor (Ref super_id, params_types))
        )

        | _ -> (
          let err = Diagnosis.(make_error ctx expr_loc (NotCallable deref_type_expr)) in
          raise (Diagnosis.Error err)
        )
      end

  )

  | Member (main_expr, name) -> (
    let handle_member_typpe expr_node member_type_opt =
      let open Core_type.TypeExpr in
      match member_type_opt with
      | Some (Method({ TypeDef. spec = ClassMethod { method_get_set = Some _; method_return; _ }; _ }, _params, _rt), _) -> (
        Program.update_node_type env.ctx expr.ty_var method_return
      )

      | Some (Method({ TypeDef. spec = ClassMethod { method_get_set = None; _ }; _ }, _params, _rt), _) -> (
        let ty, _ = Option.value_exn member_type_opt in
        Program.update_node_type env.ctx expr.ty_var ty
      )

      (* it's a property *) 
      | Some (ty_expr, _) ->
        Program.update_node_type env.ctx expr.ty_var ty_expr

      | None ->
        let err = Diagnosis.(make_error env.ctx expr_loc (CannotReadMember(name.pident_name, expr_node.value))) in
        raise (Diagnosis.Error err)

    in

    let default_clause () =
      check_expression env main_expr;
      let expr_node = Program.get_node env.ctx main_expr.ty_var in
      let member_type_opt =
        Check_helper.find_member_of_type env.ctx ~scope:env.scope expr_node.value name.pident_name
      in
      handle_member_typpe expr_node member_type_opt
    in

    match main_expr with

    (*
     * Call a static function from another lib
     *
     * import lib from "xxx";
     *
     * lib.function(...)
     *
     *)
    | { T.Expression. spec = Identifier _; ty_var; _ } -> (
      let node_type = Program.deref_node_type env.ctx ty_var in
      match node_type with
      | TypeExpr.TypeDef { TypeDef. spec = Namespace _; _ } ->
        ()

      | _ ->
        default_clause ()

    )

    | _ ->
      default_clause ()

  )

  | Index (main_expr, index_expr) -> (
    check_expression env index_expr;
    check_expression env main_expr;

    let node_type = Program.deref_node_type env.ctx main_expr.ty_var in
    match node_type with
    | String -> (
      let ty_char = ty_char env in
      let t = TypeExpr.Ctor(Ref ty_char, []) in
      Program.update_node_type env.ctx expr.ty_var t
    )

    | _ ->
      begin
        match (Check_helper.try_unwrap_array env.ctx node_type) with
        | Some t ->
          Program.update_node_type env.ctx expr.ty_var t

        | None -> (
          let err = Diagnosis.(make_error env.ctx expr_loc (CannotGetIndex node_type)) in
          raise (Diagnosis.Error err)
        )
      end
  )

  | Unary(op, child) -> (
    check_expression env child;
    let node_type = Program.deref_node_type env.ctx child.ty_var in
    let raise_err () =
      let err = Diagnosis.(make_error env.ctx expr_loc (CannotApplyUnary(op, node_type))) in
      raise (Diagnosis.Error err)
    in
    match op with
    | Asttypes.UnaryOp.Not -> (
      let ctor = Check_helper.find_construct_of env.ctx node_type in
      match ctor with
      | Some({ TypeDef. builtin = true; name = "boolean"; _ }, []) -> (
        Program.update_node_type env.ctx expr.ty_var node_type
      )
      | _ -> raise_err()
    )

    | Asttypes.UnaryOp.Minus ->
      if not (Check_helper.is_i32 env.ctx node_type) && not (Check_helper.is_f32 env.ctx node_type) then (
        raise_err ()
      );
      Program.update_node_type env.ctx expr.ty_var node_type
    | Asttypes.UnaryOp.Plus -> (
      if
        not (Check_helper.is_i32 env.ctx node_type) &&
        not (Check_helper.is_f32 env.ctx node_type) &&
        not (Check_helper.is_string node_type)
      then (
        raise_err ()
      );
      Program.update_node_type env.ctx expr.ty_var node_type
    )

    (* TODO: check other operations *)
    | _ -> raise_err ()
  )

  | Binary (op, left, right) -> (
    check_expression env left;
    check_expression env right;
    check_binary_op env op expr left right;
  )

  | Assign (op, left, right) -> (
    check_expression env left;
    check_expression env right;
    let ctx = env.ctx in
    let scope = env.scope in
    (match left with
    | { T.Expression. spec = Identifier (name, _); _ } -> (
      let variable = Option.value_exn (scope#find_var_symbol name) in

      (match variable.var_kind with
      | Ast.Pvar_const -> (
        let err = Diagnosis.(make_error ctx expr_loc CannotAssignToConstVar) in
        raise (Diagnosis.Error err)
      )
      | _ -> ());
    )

    | { spec = Member(main_expr, name) ; _ } -> (
      let main_expr_type = Program.deref_node_type ctx main_expr.ty_var in
      let member_opt = Check_helper.find_member_of_type ctx ~scope:env.scope main_expr_type name.pident_name in
      match member_opt with
      | Some _ -> ()
      | None -> (
        let err = Diagnosis.(make_error ctx expr_loc (CannotReadMember(name.pident_name, main_expr_type))) in
        raise (Diagnosis.Error err)
      )
    )

    | { spec = Index(main_expr, value_expr) ; _ } -> (
      let main_expr_type = Program.deref_node_type ctx main_expr.ty_var in
      let value_type = Program.deref_node_type ctx value_expr.ty_var in
      if not (Check_helper.is_array ctx main_expr_type) then (
        let err = Diagnosis.(make_error ctx main_expr.loc OnlyAssignArrayIndexAlpha) in
        raise (Diagnosis.Error err)
      );
      if not (Check_helper.is_i32 ctx value_type) then (
        let err = Diagnosis.(make_error ctx value_expr.loc OnlyI32InIndexAlpha) in
        raise (Diagnosis.Error err)
      )
    )

    | _ -> ()
    );

    match op with
    | None -> (
      let sym_node = Program.get_node ctx left.ty_var in
      let expr_node = Program.get_node ctx right.ty_var in
      if not (Check_helper.type_assinable ctx sym_node.value expr_node.value) then (
        let err = Diagnosis.(make_error ctx expr_loc (NotAssignable(sym_node.value, expr_node.value))) in
        raise (Diagnosis.Error err)
      )
    )

    | Some op ->
      check_binary_op env (Asttypes.AssignOp.to_binary op) expr left right
  )

  | Block blk -> check_block env blk

  | Init { init_name = cls_name, name_id;  init_elements; _ } -> (
    let node = Program.deref_node_type env.ctx name_id in
    let def = Option.value_exn (Check_helper.find_typedef_of env.ctx node) in
    let unwrap_class =
      match def.spec with
      | Class cls -> cls
      | _ -> failwith "unrechable"
    in

    let module PropMap = Map.Make (String) in

    let type_map, init_map =
      List.fold
        ~init:(PropMap.empty, PropMap.empty)
        ~f:(fun (type_map, init_map) (prop_name, elm) ->
          match elm with
          | Cls_elm_prop (_, _id, elm_node) ->
            PropMap.set type_map ~key:prop_name ~data:elm_node,
            PropMap.set init_map ~key:prop_name ~data:(ref false)

          | _ -> (type_map, init_map)
        )
        unwrap_class.tcls_elements
    in

    List.iter
      ~f:(fun elm ->
        match elm with
        | InitEntry { init_entry_key; init_entry_value; _ } -> (
          check_expression env init_entry_value;
          let expected_type = PropMap.find_exn type_map init_entry_key.pident_name in
          let actual_type = Program.deref_node_type env.ctx init_entry_value.ty_var in
          if not (Check_helper.type_assinable env.ctx expected_type actual_type) then (
            let err = Diagnosis.(make_error env.ctx expr_loc (ClassInitNotAssignable(cls_name, init_entry_key.pident_name, expected_type, actual_type))) in
            raise Diagnosis.(Error err)
          );
          let init_flag = PropMap.find_exn init_map init_entry_key.pident_name in
          init_flag := true
        )

        | InitSpread spread ->
          check_expression env spread

      )
      init_elements;

    PropMap.iteri
      ~f:(fun ~key ~data ->
        if not !data then (
          let err = Diagnosis.(make_error env.ctx expr_loc (ClassPropNotInit(cls_name, key))) in
          raise Diagnosis.(Error err)
        )
      )
      init_map
  )

  | Match _match -> (
    let rec check_clause_pattern expr_type (pat: Typedtree.Pattern.t) =
      match pat.spec with
      | Underscore -> ()
      | Symbol (name, name_id) -> (
        if not (Annotate.is_name_enum_or_class name) then (
          Program.update_node_type env.ctx name_id expr_type
        );
      )

      | Literal Ast.Literal.Unit -> (
        if not (Check_helper.is_unit expr_type) then (
          let err = Diagnosis.(make_error env.ctx pat.loc (UnexpectedPatternType("unit", expr_type))) in
          raise (Diagnosis.Error err)
        )
      )

      | Literal (Ast.Literal.Boolean _) -> (
        if not (Check_helper.is_boolean env.ctx expr_type) then (
          let err = Diagnosis.(make_error env.ctx pat.loc (UnexpectedPatternType("boolean", expr_type))) in
          raise (Diagnosis.Error err)
        )
      )
      | Literal (Ast.Literal.Char _) -> (
        if not (Check_helper.is_char env.ctx expr_type) then (
          let err = Diagnosis.(make_error env.ctx pat.loc (UnexpectedPatternType("char", expr_type))) in
          raise (Diagnosis.Error err)
        )
      )
      | Literal (Ast.Literal.Float _) -> (
        if not (Check_helper.is_f32 env.ctx expr_type) then (
          let err = Diagnosis.(make_error env.ctx pat.loc (UnexpectedPatternType("float", expr_type))) in
          raise (Diagnosis.Error err)
        )
      )
      | Literal (Ast.Literal.Integer _) -> (
        if not (Check_helper.is_i32 env.ctx expr_type) then (
          let err = Diagnosis.(make_error env.ctx pat.loc (UnexpectedPatternType("number", expr_type))) in
          raise (Diagnosis.Error err)
        )
      )
      | Literal (Ast.Literal.String _) -> (
        match expr_type with
        | TypeExpr.String -> ()
        | _ ->
          let err = Diagnosis.(make_error env.ctx pat.loc (UnexpectedPatternType("string", expr_type))) in
          raise (Diagnosis.Error err)
      )
      | EnumCtor((ctor_name, ctor_id), child_pat) -> (
        let raise_err () =
          let err = Diagnosis.(make_error env.ctx pat.loc (UnexpectedPatternType(ctor_name, expr_type))) in
          raise (Diagnosis.Error err)
        in
        let enum_ctor_typedef =
          match Program.deref_node_type env.ctx ctor_id with
          | TypeDef { TypeDef. spec = EnumCtor enum_ctor ; _ } -> enum_ctor
          | _ -> failwith "unrechable"
        in
        let contruct_type_vars_map params args = 
          let tmp =
            List.fold2
            ~init:Check_helper.TypeVarMap.empty
            ~f:(fun acc item item2 ->
              Check_helper.TypeVarMap.set acc ~key:item ~data:item2
            )
            params args
          in
          match tmp with
          | List.Or_unequal_lengths.Ok t -> t
          | List.Or_unequal_lengths.Unequal_lengths -> raise_err ()
        in
        let ctor_opt = Check_helper.find_construct_of env.ctx expr_type in
        match ctor_opt with
        | Some({ TypeDef. id = enum_id;  spec = Enum { enum_params; _}; _ }, args) -> (
          if enum_id <> enum_ctor_typedef.enum_ctor_super_id then (
            raise_err ()
          );
          let enum_ctor_typedef =
            match Program.deref_node_type env.ctx ctor_id with
            | TypeDef { TypeDef. spec = EnumCtor c; _ } -> c
            | _ -> failwith "unrechable"
          in
          let type_vars_map = contruct_type_vars_map enum_params args in
          match enum_ctor_typedef.enum_ctor_params with
          | [arg] ->
            check_clause_pattern
              (Check_helper.replace_type_vars_with_maps env.ctx type_vars_map arg)
              child_pat

          | _ -> raise_err ()

        )

        | _ ->
          raise_err ()
      )
      | Tuple children -> (
        let raise_err () =
          let err = Diagnosis.(make_error env.ctx pat.loc (UnexpectedPatternType("tuple", expr_type))) in
          raise (Diagnosis.Error err)
        in
        match expr_type with
        | TypeExpr.Tuple children_types -> (
          let tmp =
            List.map2
            ~f:(fun child_pat child_type ->
              check_clause_pattern child_type child_pat;
            )
            children children_types
          in
          match tmp with
          | List.Or_unequal_lengths.Ok _ -> ()
          | List.Or_unequal_lengths.Unequal_lengths -> raise_err ()
        )

        | _ -> raise_err ()
      )
      | Array { elements; rest } -> (
        let raise_err () =
          let err = Diagnosis.(make_error env.ctx pat.loc (UnexpectedPatternType("array", expr_type))) in
          raise (Diagnosis.Error err)
        in
        match expr_type with
        | TypeExpr.Array child_type -> (
          List.iter ~f:(check_clause_pattern child_type) elements;
          match rest with
          | Some rest ->
            check_clause_pattern expr_type rest

          | None -> ()
        )

        | _ -> raise_err ()
      )

    in

    let { match_expr; match_clauses; match_loc } = _match in

    check_expression env match_expr;
    let match_expr_type = Program.deref_node_type env.ctx match_expr.ty_var in

    let ty =
      if List.is_empty match_clauses then (
        TypeExpr.Unit
      ) else (
        (* TODO: better way to check every clauses *)
        List.fold
          ~init:TypeExpr.Unknown
          ~f:(fun acc clause ->
            check_clause_pattern match_expr_type clause.clause_pat;
            let consequent = clause.clause_consequent in
            check_expression env consequent;
            let node_expr = Program.deref_node_type env.ctx consequent.ty_var in
            match (acc, node_expr) with
            | TypeExpr.Unknown, _ ->
              node_expr

            | (TypeExpr.Ctor(c1, [])), (TypeExpr.Ctor (c2, [])) ->  (
              let c1_def = Program.deref_type env.ctx c1 in
              let c2_def = Program.deref_type env.ctx c2 in
              (match (c1_def, c2_def) with
              | (TypeExpr.TypeDef left_sym, TypeExpr.TypeDef right_sym) -> (
                if TypeDef.(left_sym == right_sym) then ()
                else
                  let err = Diagnosis.(make_error env.ctx match_loc (NotAllTheCasesReturnSameType(c1_def, c2_def))) in
                  raise (Diagnosis.Error err)
              )
              | _ -> (
                let err = Diagnosis.(make_error env.ctx match_loc (NotAllTheCasesReturnSameType(c2_def, c2_def))) in
                raise (Diagnosis.Error err)
              ));

              acc
            )

            | _ -> (
              acc
            )
          )
          match_clauses
      )
    in

    check_match_exhausted env _match;

    Program.update_node_type env.ctx expr.ty_var ty
  )

  | Try try_expr -> (
    check_expression env try_expr;
    let try_expr_type = Program.deref_node_type env.ctx try_expr.ty_var in
    let ctor_opt = Check_helper.find_construct_of env.ctx try_expr_type in
    let raise_err () =
      let err = Diagnosis.(make_error env.ctx expr_loc (CannotUsedForTryExpression try_expr_type)) in
      raise (Diagnosis.Error err)
    in
    match ctor_opt with
    | Some ({ TypeDef. name = "Result"; spec = Enum { enum_params; _ } ; _ }, args) -> (
      let tmp =
        List.fold2
        ~init:Check_helper.TypeVarMap.empty
        ~f:(fun acc item item2 ->
          Check_helper.TypeVarMap.set acc ~key:item ~data:item2
        )
        enum_params args
      in
      let type_maps=
        match tmp with
        | List.Or_unequal_lengths.Ok t -> t
        | List.Or_unequal_lengths.Unequal_lengths -> raise_err ()
      in
      let ok_type = Check_helper.TypeVarMap.find_exn type_maps "A" in
      Program.update_node_type env.ctx expr.ty_var ok_type;

      let error_type = Check_helper.TypeVarMap.find_exn type_maps "B" in

      (* TODO: do NOT find result in current scope, will have name conflict *)
      let error_ty_var = Option.value_exn (env.scope#find_type_symbol "Result") in
      let return_type = TypeExpr.Ctor(Ref error_ty_var, [TypeSymbol "A"; error_type]) in
      add_return_type env (return_type, expr_loc);
    )

    |_ -> raise_err ()
  )

  | TypeCast(expr, _type) -> (
    check_expression env expr;
    
    let expr_type = Program.deref_node_type env.ctx expr.ty_var in

    if not (Check_helper.type_castable env.ctx expr_type _type) then (
      let err = Diagnosis.(make_error env.ctx expr.loc (CannotCastType (expr_type, _type))) in
      raise (Diagnosis.Error err)
    );

    ()
  )

  | This
  | Super -> ()

and check_binary_op env op expr left right =
  let left_node = Program.get_node env.ctx left.ty_var in
  let right_node = Program.get_node env.ctx right.ty_var in
  let ctx = env.ctx in
  let loc = expr.loc in
  let id = expr.ty_var in
  let open Asttypes in
  match op with
  | BinaryOp.Plus -> (
    if not (Check_helper.type_addable ctx left_node.value right_node.value) then (
      let err = Diagnosis.(make_error ctx loc (CannotApplyBinary (op, left_node.value, right_node.value))) in
      raise (Diagnosis.Error err)
    );
    Program.update_node_type ctx id left_node.value;
  )

  | BinaryOp.Minus
  | BinaryOp.Mult
  | BinaryOp.Div
    -> (
    if not (Check_helper.type_arithmetic ctx left_node.value right_node.value) then (
      let err = Diagnosis.(make_error ctx loc (CannotApplyBinary (op, left_node.value, right_node.value))) in
      raise (Diagnosis.Error err)
    );
    Program.update_node_type ctx id left_node.value;
  )

  | BinaryOp.Mod
  | BinaryOp.BitAnd
  | BinaryOp.Xor
  | BinaryOp.BitOr
    -> (
    if not (Check_helper.type_arithmetic_integer ctx left_node.value right_node.value) then (
      let err = Diagnosis.(make_error ctx loc (CannotApplyBinary (op, left_node.value, right_node.value))) in
      raise (Diagnosis.Error err)
    );
    Program.update_node_type ctx id left_node.value;
  )

  | BinaryOp.Equal
  | BinaryOp.NotEqual
  | BinaryOp.LessThan
  | BinaryOp.LessThanEqual
  | BinaryOp.GreaterThan
  | BinaryOp.GreaterThanEqual
    -> (
      if not (Check_helper.type_logic_compareable ctx left_node.value right_node.value) then (
        let err = Diagnosis.(make_error ctx loc (CannotApplyBinary (op, left_node.value, right_node.value))) in
        raise (Diagnosis.Error err)
      );
      let bool_ty = ty_boolean env in
      Program.update_node_type ctx id (TypeExpr.Ctor (Ref bool_ty, []));
    )

  | BinaryOp.And
  | BinaryOp.Or
    -> (
      if (not (Check_helper.is_boolean ctx left_node.value)) || (not (Check_helper.is_boolean ctx right_node.value)) then (
        let err = Diagnosis.(make_error ctx loc (CannotApplyBinary (op, left_node.value, right_node.value))) in
        raise (Diagnosis.Error err)
      );
      let bool_ty = ty_boolean env in
      Program.update_node_type ctx id (TypeExpr.Ctor (Ref bool_ty, []));
    )

  | _ -> (
      if not (Check_helper.type_logic_compareable ctx left_node.value right_node.value) then (
        let err = Diagnosis.(make_error ctx loc (CannotApplyBinary (op, left_node.value, right_node.value))) in
        raise (Diagnosis.Error err)
      );
  )

and check_match_exhausted env _match =
  let open T.Expression in

  let check_tuples_exausted slots patterns =
    let open Check_helper in
    List.iteri
      ~f:(fun index child ->
        let exist = Array.get slots index in
        Array.set slots index (child::exist)
      )
      patterns;

    Pat_tuple slots
  in

  let rec check_patterns_exhausted patterns =
    let open Check_helper in
    let result =
      List.fold
        ~init:Pat_begin
        ~f:(fun acc pattern ->
          let open T.Pattern in
          match pattern.spec with
          | Underscore
          | Symbol _ -> (
            Pat_exausted
          )

          | Literal (Ast.Literal.Boolean v) -> (
            match acc with
            | Pat_begin ->
              if v then
                Pat_boolean(true, false)
              else
                Pat_boolean(false, true)

            | Pat_boolean(has_true, has_false) ->
              if v then
                Pat_boolean(true, has_false)
              else
                Pat_boolean(has_true, true)

            | _ -> acc
          )

          | EnumCtor (id, child_pat) -> (
            match acc with
            | Pat_begin ->
              Pat_enum_branch [(id, child_pat)]

            | Pat_enum_branch exist ->
              Pat_enum_branch ((id, child_pat)::exist)

            | _ -> acc
          )

          | Tuple children -> (
            let children_len = List.length children in
            match acc with
            | Pat_begin -> (
              let slots = Array.create ~len:children_len [] in
              check_tuples_exausted slots children
            )

            | Pat_tuple slots when (Array.length slots) = children_len ->
              check_tuples_exausted slots children

            | _ -> acc
          )

          | _ -> acc
        )
        patterns
    in
    match result with
    | Pat_boolean(true, true)
    | Pat_exausted -> ()
    | Pat_enum_branch items -> (
      let branches_map = Hashtbl.create (module String) in

      List.iter
        ~f:(fun item ->
          let ((name, _), _) = item in
          match Hashtbl.find branches_map name with
          | Some list ->
            Hashtbl.set branches_map ~key:name ~data:(item::list)
          | None ->
            Hashtbl.set branches_map ~key:name ~data:[item]
        )
        items;

      Hashtbl.iter
        ~f:(fun items ->
          let patterns = List.map ~f:(fun (_, pat) -> pat) items in
          check_patterns_exhausted patterns
        )
        branches_map;

      let (_ctor_name, ctor_id), _ = List.hd_exn items in

      let def = Program.deref_node_type env.ctx ctor_id in
      let first_enum_ctor =
        match def with
        | TypeExpr.TypeDef { Core_type.TypeDef. spec = EnumCtor ctor;_ } -> ctor
        | _ -> failwith "unrechable"
      in

      let enum_super_id = first_enum_ctor.enum_ctor_super_id in
      let super_def = Program.deref_node_type env.ctx enum_super_id in
      let unwrap_super_def =
        match super_def with
        | TypeExpr.TypeDef { Core_type.TypeDef. spec = Enum enum;_ } -> enum
        | _ -> failwith "unrechable"
      in

      let visited_map =
        unwrap_super_def.enum_members
        |> List.map
          ~f:(fun (member_name, _) ->
            member_name
          )
        |> Hash_set.of_list (module String)
      in

      List.iter
        ~f:(fun item ->
          let ((name, _), _) = item in
          Hash_set.remove visited_map name
        )
        items;

      if not (Hash_set.is_empty visited_map) then (
        let err = Diagnosis.(make_error env.ctx _match.match_loc PatternNotExausted) in
        raise (Diagnosis.Error err)
      )
    )

    | Pat_tuple tuples -> (
      Array.iter
        ~f:(check_patterns_exhausted)
        tuples;
    )

    | _ -> (
      let err = Diagnosis.(make_error env.ctx _match.match_loc PatternNotExausted) in
      raise (Diagnosis.Error err)
    )
  in

  let { match_clauses; _ } = _match in

  let patterns =
    List.map
    ~f:(fun clause -> clause.clause_pat)
    match_clauses
  in

  check_patterns_exhausted patterns

and check_lambda env lambda =
  let { T.Expression. lambda_return_ty; lambda_body; _ } = lambda in
  let prev_return_type = take_return_type env in

  check_expression env lambda_body;
  let lambda_return_types = take_return_type env in

  let expr_type = Program.deref_node_type env.ctx lambda_body.ty_var in

  if not (Check_helper.type_assinable env.ctx lambda_return_ty expr_type) then (
    let open Diagnosis in
    let spec = Type_error.CannotReturn(lambda_return_ty, expr_type) in
    let err = make_error env.ctx lambda_body.loc spec in
    raise (Error err)
  );

  (* there are return statements, check every statments *)
  List.iter
    ~f:(fun (return_ty, return_loc) ->
      if not (Check_helper.type_assinable env.ctx lambda_return_ty return_ty) then (
        let open Diagnosis in
        let spec = Type_error.CannotReturn(lambda_return_ty, return_ty) in
        let err = make_error env.ctx return_loc spec in
        raise (Error err)
      )
    )
    lambda_return_types;

  env.return_types <- prev_return_type;

and check_class env cls =
  let open T.Declaration in
  let { cls_id = _, cls_id; cls_body; _ } = cls in
  let { cls_body_elements; _ } = cls_body in

  let node = Program.get_node env.ctx cls_id in
  let cls_node = Check_helper.find_typedef_of env.ctx node.value in
  let unwrap_class =
    match cls_node with
    | Some { Core_type.TypeDef. spec = Class cls; _ } -> cls
    | _ -> failwith "unrechable"
  in

  let check_class_implements (impl_expr, loc) =
    let typedef = Check_helper.find_construct_of env.ctx impl_expr in
    match typedef with
    | Some({ TypeDef. name = intf_name; spec = Interface { intf_methods } ; _ }, _) -> (
      (* TODO: check args *)
      List.iter
        ~f:(fun (method_name, elm) -> 
          match elm with
          | Cls_elm_method(_, _method) -> (
            let test_method =
              List.find
              ~f:(fun (test_name, elm) ->
                match elm with
                | Cls_elm_method _ -> String.equal test_name method_name
                | _ -> false
              )
              unwrap_class.tcls_elements
            in
            if Option.is_none test_method then (
              let err = Diagnosis.(make_error env.ctx loc (MissingMethodForInterface(intf_name, method_name))) in
              raise (Diagnosis.Error err)
            )
          )
          | _ -> ()
        )
        intf_methods
    )
    | _ ->
      let open Diagnosis in
      let spec = Type_error.CannotImplement impl_expr in
      let err = make_error env.ctx loc spec in
      raise (Error err)
  in

  List.iter ~f:check_class_implements unwrap_class.tcls_implements;

  List.iter
    ~f:(fun elm ->
      match elm with
      | Cls_method _method -> (
        let is_static =
          match _method.cls_method_modifier with
          | (Some Cls_modifier_static) -> true
          | _ -> false
        in
        if not is_static then (
          check_method_for_class env unwrap_class _method
        );
        check_method_content env _method
      )

      | Cls_property _
      | Cls_declare _ -> ()
    )
    cls_body_elements

(*
 * 1. if a method is virtual, check if it's parent has a virtual method with the same name, add a warning to make it override
 *    check the params and return type, if they are different, throw an error
 * 2. if a method is override, check if it's parent has a virtual/override method with the same name,
 *    throw an error if there are not.
 *    check the params and return type, if they are different, throw an error
 * 3. if a method is neither virtual nor override. check if it's parent has a virtual/override method with the same name,
 *    add a warning
 *)
and check_method_for_class env cls_type _method =
  let open T.Declaration in
  let { cls_method_name = method_name, method_id;
    cls_method_modifier; cls_method_visibility;
    cls_method_loc; _
  } = _method in
  let is_private =
    match cls_method_visibility with
    | Some Asttypes.Pvisibility_private -> true
    | _ -> false
  in
  let is_virtual =
    match cls_method_modifier with
    | Some Cls_modifier_virtual -> true
    | _ -> false
  in
  let is_override =
    match cls_method_modifier with
    | Some Cls_modifier_override -> true
    | _ -> false
  in

  let check_private_virtual () =
    if is_private then (
      let open Diagnosis in
      let spec = Type_error.PrivateVirtualMethod method_name in
      let err = make_error env.ctx cls_method_loc spec in
      raise (Error err)
    );
  in

  let raise_no_method_to_be_overrided () =
    let open Diagnosis in
    let spec = Type_error.NoMethodToBeOverride(method_name, cls_type.tcls_name)in
    let err = make_error env.ctx cls_method_loc spec in
    raise (Error err)
  in

  let scope = env.scope in

  let method_node = Program.get_node env.ctx method_id in
  let method_def, unwrap_method =
    match method_node.value with
    | TypeExpr.TypeDef ({ Core_type.TypeDef. spec = ClassMethod method_type; _ } as def) ->
      (def, method_type)
    | _ -> failwith ("unrechable: " ^ method_name)
  in

  let find_member_in_ancester method_name =
    let open Option in
    cls_type.tcls_extends >>= fun parent_type ->
    Check_helper.find_member_of_type env.ctx ~scope parent_type method_name
  in

  let find_member_in_interface method_name =
    List.find_map
      ~f:(fun (intf_type_expr, _) ->
        Check_helper.find_member_of_type env.ctx ~scope intf_type_expr method_name
      )
      cls_type.tcls_implements
  in

  if is_virtual then (
    check_private_virtual ();
    match cls_type.tcls_extends with
    | Some parent_type -> (
      let _method_opt = Check_helper.find_member_of_type env.ctx ~scope parent_type method_name in
      ()
    )
    | None -> ()
  ) else if is_override then (
    check_private_virtual ();
    let method_opt = find_member_in_ancester method_name in
    let method_opt =
      if Option.is_none method_opt then
        find_member_in_interface method_name
      else
        method_opt
    in
    match method_opt with
    | Some (found_type_expr, _) -> (
      let this_type_expr = TypeExpr.Method(method_def, unwrap_method.method_params, unwrap_method.method_return) in
      if not (Check_helper.method_sig_type_equal env.ctx this_type_expr found_type_expr) then (
        let open Diagnosis in
        let spec = Type_error.OverrideFunctionNotMatch(found_type_expr, this_type_expr)in
        let err = make_error env.ctx cls_method_loc spec in
        raise (Error err)
      )
    )
    | None -> raise_no_method_to_be_overrided ()

  ) else
    (* TODO: add warning *)
    ()

and check_method_content env _method =
  let open T.Declaration in
  let { cls_method_name = _, name_id; cls_method_scope; cls_method_body; cls_method_modifier; _ } = _method in
  with_scope env (Option.value_exn cls_method_scope) (fun env ->
    check_block env cls_method_body;

    let is_static =
      match cls_method_modifier with
      | (Some Cls_modifier_static) -> true
      | _ -> false
    in

    if is_static then (
      let type_node = Program.get_node env.ctx name_id in
      let typedef = Option.value_exn (Check_helper.find_typedef_of env.ctx type_node.value) in
      let unwrap_function =
        match typedef with
        | { TypeDef. spec = Function _fun; _ } -> _fun
        | _ -> failwith "unwrap class method failed"
      in

      check_function_return_type env unwrap_function.fun_return cls_method_body
    ) else (
      let type_node = Program.get_node env.ctx name_id in
      let typedef = Option.value_exn (Check_helper.find_typedef_of env.ctx type_node.value) in
      let unwrap_method =
        match typedef with
        | { TypeDef. spec = ClassMethod _method; _ } -> _method
        | _ -> failwith "unwrap class method failed"
      in

      check_function_return_type env unwrap_method.method_return cls_method_body
    )
  )
