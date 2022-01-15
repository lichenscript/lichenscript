(*
 * 1. annotate parsed tree to typed tree
 * 2. resolve top-level variables internally
 * 
 * deeper variables resolution remains to type-check phase
 *)
open Core_kernel
open Core_type
open Scope
open Lichenscript_parsing

module T = Typedtree

let rec annotate_statement ~(prev_deps: int list) env (stmt: Ast.Statement.t) =
  let open Ast.Statement in
  let { spec; loc; attributes; _ } = stmt in
  let deps, spec =
    match spec with
    | Expr expr -> (
      let expr = annotate_expression ~prev_deps env expr in 
      let ty_var = T.Expression.(expr.ty_var) in

      (* the expression may not depends on prev_deps, so next
       * statement should continue to depends on prev deps
       *)
      (List.append prev_deps [ty_var]), (T.Statement.Expr expr)
    )

    | Semi expr -> (
      let expr = annotate_expression ~prev_deps env expr in 
      let ty_var = T.Expression.(expr.ty_var) in

      let node = {
        value = TypeExpr.Ctor((Env.ty_unit env), []);
        loc;
        deps = List.append prev_deps [ty_var];
        check = none;  (* TODO: check expr is empty *)
      } in

      let id = Type_context.new_id (Env.ctx env) node in
      [id], (T.Statement.Semi expr)
    )

    | While _while -> (
      let { while_test; while_block; while_loc } = _while in
      let while_test = annotate_expression ~prev_deps env while_test in
      let while_block = annotate_block ~prev_deps:[while_test.ty_var] env while_block in
      let next_desp = [ while_block.return_ty ] in
      next_desp, T.Statement.While { while_test; while_block; while_loc }
    )

    | Binding binding -> (
      let { binding_kind; binding_pat; binding_init; binding_loc; _ } = binding in

      let binding_pat, sym_id = annotate_pattern env binding_pat in
      let name =
        let open T.Pattern in
        match binding_pat.spec with
        | Symbol (name, _) -> name
      in

      let scope = Env.peek_scope env in
      (* TODO: check redefinition? *)
      scope#insert_var_symbol name { var_id = sym_id; var_kind = binding_kind };

      let binding_init = annotate_expression ~prev_deps env binding_init in

      let ctx = Env.ctx env in
      let node = Type_context.get_node ctx sym_id in
      Type_context.update_node ctx sym_id {
        node with
        deps = List.concat [node.deps; [binding_init.ty_var]; prev_deps ];
        check = (fun id ->
          let expr_node = Type_context.get_node ctx binding_init.ty_var in
          Type_context.update_node_type ctx id expr_node.value
        )
      };

      [sym_id], T.Statement.Binding { T.Statement.
        binding_kind;
        binding_pat;
        binding_init;
        binding_ty_var = sym_id;
        binding_loc;
      }
    )

    | Block block -> (
      let block = annotate_block ~prev_deps env block in
      let dep = block.return_ty in
      [dep], (T.Statement.Block block)
    )

    | Break _
    | Continue _
    | Debugger -> prev_deps, failwith "not implment"

    | Return ret_opt -> (
      match ret_opt with
      | Some expr -> (
        let expr = annotate_expression ~prev_deps env expr in
        let ty_var = expr.ty_var in

        Env.add_return_type env ty_var;

        [ty_var], (T.Statement.Return (Some expr))
      )

      | None -> [], (T.Statement.Return None)
    )

    | Empty ->
      prev_deps, T.Statement.Empty
  in
  deps, { T.Statement. spec; loc; attributes }

and annotate_expression ~prev_deps env expr : T.Expression.t =
  let open Ast.Expression in
  let { spec; loc; attributes; } = expr in
  let root_scope = Type_context.root_scope (Env.ctx env) in
  let ty_var, spec = 
    match spec with
    | Constant cnst -> (
      let open Ast.Literal in
      let ty_var =
        match cnst with
        | Integer _ ->
          Option.value_exn (root_scope#find_type_symbol "i32")

        | Char _ ->
          Option.value_exn (root_scope#find_type_symbol "char")

        (* 'c' *)
        | String _ ->
          Option.value_exn (root_scope#find_type_symbol "string")

        | Float _ ->
          Option.value_exn (root_scope#find_type_symbol "f32")

        | Boolean _ -> 
          Option.value_exn (root_scope#find_type_symbol "boolean")

      in

      let node = {
        value = TypeExpr.Ctor(ty_var, []);
        loc = loc;
        check = none;
        deps = [ty_var];
      } in

      let node_id = Type_context.new_id (Env.ctx env) node in

      node_id, (T.Expression.Constant cnst)
    )

    | Identifier id -> (
      let ty_var_opt = (Env.peek_scope env)#find_var_symbol id.pident_name in
      match ty_var_opt with
      | Some variable ->
        variable.var_id, (T.Expression.Identifier (id.pident_name, variable.var_id))
      | _ ->
        let err_spec = Type_error.CannotFindName id.pident_name in
        let err = Type_error.make_error (Env.ctx env) id.pident_loc err_spec in
        raise (Type_error.Error err)
    )

    | Lambda _
    | If _
    | Array _  ->
      -1, failwith "not implemented"

    | Call call -> (
      let { callee; call_params; call_loc } = call in

      let callee = annotate_expression ~prev_deps env callee in
      let call_params = List.map ~f:(annotate_expression ~prev_deps env) call_params in

      let ty_var = Type_context.new_id (Env.ctx env) {
        value = TypeExpr.Unknown;
        loc;
        deps = [ T.Expression.(callee.ty_var) ];
        check = (fun id ->
          let ctx = Env.ctx env in
          let ty_int = callee.ty_var in
          let ty_def = Check_helper.find_construct_of ctx ty_int in
          match ty_def with
          | Some ({ TypeDef. spec = Function _fun; _ }, _) ->
            (* TODO: check call params *)
            Type_context.update_node_type ctx id _fun.fun_return

          | _ -> (
            let _val = Type_context.get_node ctx ty_int in
            let err = Type_error.(make_error ctx call_loc (NotCallable _val.value)) in
            raise (Type_error.Error err)
          )
        );
      } in

      ty_var, (T.Expression.Call { callee; call_params; call_loc })
    )

    (*
     * TODO: namespace
     * class/enum static function
     * object's property/method
     *)
    | Member (expr, name) -> (
      let expr = annotate_expression ~prev_deps env expr in
      let ctx = Env.ctx env in
      let member_name = name.pident_name in
      let node = {
        value = TypeExpr.Unknown;
        loc;
        deps = List.append prev_deps [expr.ty_var];
        check = (fun id ->
          let expr_node = Type_context.get_node ctx expr.ty_var in
          let open TypeExpr in
          let raise_error () =
            let err = Type_error.(make_error ctx loc (CannotReadMember(member_name, expr_node.value))) in
            raise (Type_error.Error err)
          in
          match expr_node.value with
          (* instance of type *)
          | Ctor(ty_id, []) -> (
            let ctor_node = Type_context.get_node ctx ty_id in
            let open TypeDef in
            match ctor_node.value with
            | TypeDef { spec = Class cls; _ } -> (
              let result =
                List.find ~f:(fun (elm_name, _) -> String.equal elm_name member_name)
                cls.tcls_elements
              in
              match result with
              | Some (_, member_id) ->
                Type_context.update_node_type ctx id (TypeExpr.Ref member_id)

              | _ -> raise_error ()
            )
            | _ ->
              raise_error ()
          )

          (* type def itself *)
          | TypeDef { spec = Class { tcls_static_elements; _ }; _ } -> (
            let result =
              List.find ~f:(fun (static_memeber_name, _) -> String.equal static_memeber_name member_name)
              tcls_static_elements
            in

            match result with
            | Some (_, member_id) ->
              Type_context.update_node_type ctx id (TypeExpr.Ref member_id)

            | _ -> raise_error ()
          )

          | _ -> raise_error ()

        );
      } in
      let id = Type_context.new_id ctx node in
      id, T.Expression.Member(expr, name)
    )

    | Unary _ -> -1, failwith "not implemented"

    | Binary (op, left, right) -> (
      let left = annotate_expression ~prev_deps env left in
      let right = annotate_expression ~prev_deps env right in
      let open T.Expression in
      let node = {
        value = TypeExpr.Unknown;
        deps = [left.ty_var; right.ty_var];
        loc;
        check = (fun id ->
          let ctx = Env.ctx env in
          let left_def_opt = Check_helper.find_construct_of ctx left.ty_var in
          let right_def_opt = Check_helper.find_construct_of ctx right.ty_var in
          match (left_def_opt, right_def_opt) with
          | (Some (left_sym, left_id), Some (right_sym, _)) -> (
            if not (Check_helper.type_addable left_sym right_sym) then (
              let err = Type_error.(make_error ctx loc (NotAddable (left_sym, right_sym))) in
              raise (Type_error.Error err)
            );
            Type_context.update_node_type ctx id (TypeExpr.Ctor(left_id, []))
          )
          | _ -> (
            let err = Type_error.(make_error ctx loc CannotResolveTypeOfExpression) in
            raise (Type_error.Error err)
          )
        );
      } in
      let id = Type_context.new_id (Env.ctx env) node in

      id, (T.Expression.Binary(op, left, right))
    )

    | Update _ -> -1, failwith "not implemented"

    | Assign (id, expr) -> (
      let expr = annotate_expression ~prev_deps env expr in
      let scope = Env.peek_scope env in
      let ctx = Env.ctx env in
      let name, ty_int =
        match scope#find_var_symbol id.pident_name with
        | Some var -> (id.pident_name, var.var_id)
        | None -> (
          let err = Type_error.(make_error ctx loc (CannotFindName id.pident_name)) in
          raise (Type_error.Error err)
        )
      in
      let value = (TypeExpr.Ctor ((Env.ty_unit env), [])) in
      let next_id = Type_context.new_id ctx {
        value;
        deps = [ expr.ty_var; ty_int ];
        loc;
        check = (fun _ ->
          let variable = Option.value_exn (scope#find_var_symbol name) in
          (match variable.var_kind with
          | Ast.Pvar_const -> (
            let err = Type_error.(make_error ctx loc CannotAssignToConstVar) in
            raise (Type_error.Error err)
          )
          | _ -> ());
          let sym_node = Type_context.get_node ctx ty_int in
          let expr_node = Type_context.get_node ctx expr.ty_var in
          if not (Check_helper.type_assinable ctx sym_node.value expr_node.value) then (
            let err = Type_error.(make_error ctx loc (NotAssignable(sym_node.value, expr_node.value))) in
            raise (Type_error.Error err)
          )
        );
      } in
      next_id, Assign((name, ty_int), expr)
    )

    | Block block -> (
      let block = annotate_block ~prev_deps env block in
      T.Block.(block.return_ty), (T.Expression.Block block)
    )

    | Init init -> (
      let { init_loc; init_name; _ } = init in
      let ctx = Env.ctx env in
      let type_int = (Env.peek_scope env)#find_type_symbol init_name.pident_name in
      match type_int with
      | Some v -> (
        let node = {
          value = TypeExpr.Ctor(v, []);
          loc = init_loc;
          deps = [];
          (* TODO: check props and expressions *)
          check = none;
        } in
        let node_id = Type_context.new_id ctx node in
        node_id, T.Expression.Init init
      )
      | None -> (
        let err = Type_error.(make_error ctx init_loc (CannotFindName init_name.pident_name)) in
        raise (Type_error.Error err)
      )

    )

    | Match _ -> failwith "not implement"

  in
  { T.Expression.
    spec;
    loc;
    attributes;
    ty_var;
  }

and annotate_block ~prev_deps env block : T.Block.t =
  let open Ast.Block in
  let { body; loc } = block in
  let body_dep, body_stmts =
    List.fold_map
      ~init:prev_deps
      ~f:(fun prev_deps stmt ->
        annotate_statement ~prev_deps env stmt
      )
      body
  in
  let node = {
    value = TypeExpr.Unknown;
    deps = body_dep;
    loc;
    check = (fun id ->
      let ctx = Env.ctx env in
      let last_opt = List.last body_stmts in
      match last_opt with
      | Some { Typedtree.Statement. spec = Expr expr ; _ } -> (
        let ty_var = Typedtree.Expression.(expr.ty_var) in
        Type_context.update_node_type ctx id (TypeExpr.Ref ty_var)
      )

      | _ -> (
        let unit_type = Env.ty_unit env in
        Type_context.update_node_type ctx id (TypeExpr.Ctor(unit_type, []))
      )
    );
  } in
  let return_ty = Type_context.new_id (Env.ctx env) node in
  { T.Block.
    body = body_stmts;
    loc;
    return_ty;
  }

and annotate_declaration env decl : T.Declaration.t =
  let open Ast.Declaration in
  let { spec; loc; attributes } = decl in
  let ty_var, spec =
    match spec with
    | Class _class -> (
      let open Typedtree.Declaration in
      let _class = annotate_class env _class in
      let _, ty_int = _class.cls_id in
      ty_int, T.Declaration.Class _class
    )

    | Function_ _fun -> (
      let open Typedtree.Function in
      let _fun = annotate_function env _fun in
      _fun.ty_var, T.Declaration.Function_ _fun
    )

    | Declare declare -> (
      let { decl_spec; decl_visibility; decl_loc } = declare in
      match decl_spec with
      | DeclFunction declare_fun -> (
        let { Ast.Function. id; params; return_ty; _ } = declare_fun in

        let params, params_types = annotate_function_params env params in

        let scope = Env.peek_scope env in
        let ty_id =
          match scope#find_var_symbol id.pident_name with
          | Some v -> v.var_id
          | None -> failwith (Format.sprintf "unexpected: %s is not added to scope\n" id.pident_name)
        in

        let fun_return, fun_return_deps =
          match return_ty with
          | Some ty -> (
            annotate_type env ty
          )
          | None -> (
            let unit_ty = Env.ty_unit env in
            TypeExpr.Ctor(unit_ty, []), []
          )
        in

        let ty_def = {
          TypeDef.
          builtin = false;
          name = id.pident_name;
          spec = Function {
            fun_params = [];
            fun_return;
          }
        } in

        Type_context.update_node (Env.ctx env) ty_id {
          value = TypeExpr.TypeDef ty_def;
          deps = List.append params_types fun_return_deps;
          loc = decl_loc;
          check = none;
        };

        (match List.last attributes with
        | Some { Ast. attr_name = { txt = "external"; _ }; attr_payload = ext_name::_; _ } ->
          Type_context.set_external_symbol (Env.ctx env) ty_id ext_name

        | _ ->
          let open Type_error in
          let err = make_error (Env.ctx env) loc DeclareFunctionShouldSpecificExternal in
          raise (Error err)
        );

        let header = {
          T.Function.
          id = ty_id;
          name = Identifier.(id.pident_name);
          params;
        } in

        ty_id, (T.Declaration.Declare {
          T.Declaration.
          decl_visibility;
          decl_ty_var = ty_id;
          decl_spec = T.Declaration.DeclFunction header;
          decl_loc;
        })
      )
    )

    | Enum enum -> -1, T.Declaration.Enum enum

    | Import import -> -1, T.Declaration.Import import

  in
  let result = { T.Declaration. spec; loc; attributes } in

  (* record the declaration for linking stage *)
  if ty_var >= 0 then (
    let open Type_context in
    let ctx = Env.ctx env in
    Hashtbl.set ctx.declarations ~key:ty_var ~data:result
  );

  result

(*
 * class annotation is done in two phase
 * 1. scan all methods and properties, infer `this`
 * 2. annotate all methods and collect dependencies
 *
 *)
and annotate_class env cls =
  let open Ast.Declaration in

  let cls_var = Option.value_exn ((Env.peek_scope env)#find_var_symbol cls.cls_id.pident_name) in

  let prev_scope = Env.peek_scope env in
  let class_scope = new class_scope ~prev:prev_scope () in

  let tcls_static_elements = ref [] in
  let tcls_elements = ref [] in
  let props_deps = ref [] in
  let method_deps = ref [] in

  let ctx = Env.ctx env in
  (* prescan class property and method *)
  List.iter
    ~f:(fun item ->
      match item with
      | Cls_method _method -> (
        let { cls_method_name; cls_method_visibility; cls_method_loc; cls_method_modifier; _ } = _method in
        let node = {
          value = TypeExpr.Unknown;
          deps = [];
          loc = cls_method_loc;
          check = none;
        } in
        let node_id = Type_context.new_id ctx node in
        match cls_method_modifier with
        | (Some Cls_modifier_static) -> (
          tcls_static_elements := (cls_method_name.pident_name, node_id)::!tcls_static_elements;
        )

        | _ -> (
          tcls_elements := ((cls_method_name.pident_name, node_id)::!tcls_elements);
          class_scope#insert_cls_element
            cls_method_name.pident_name
            (Scope.Cls_method {
              method_id = node_id;
              method_visibility = cls_method_visibility;
            })
        )
      )
      | Cls_property property -> (
        let { cls_property_name; cls_property_type; cls_property_loc; cls_property_visibility; _  } = property in
        let property_ty, deps = annotate_type env cls_property_type in
        let node = {
          value = property_ty;
          deps;
          loc = cls_property_loc;
          check = none;
        } in
        let node_id = Type_context.new_id ctx node in
        tcls_elements := ((cls_property_name.pident_name, node_id)::!tcls_elements);

        (*
         * the class itself depends on all the properties
         * all the method depends on the class
         *)
         props_deps := node_id::(!props_deps);

        class_scope#insert_cls_element
          cls_property_name.pident_name
          (Scope.Cls_property {
            prop_id = node_id;
            prop_visibility = cls_property_visibility;
          })
      )
    )
    cls.cls_body.cls_body_elements;

  (* depend on the base class *)
  Option.iter
    ~f:(fun extend ->
      let var = (Env.peek_scope env)#find_var_symbol extend.pident_name in
      match var with
      | Some var ->
        props_deps := (var.var_id)::!props_deps;

      | None -> (
        let err = Type_error.(make_error (Env.ctx env) extend.pident_loc (CannotFindName extend.pident_name)) in
        raise (Type_error.Error err)
      )
    )
    cls.cls_extends;

  let annotate_class_body body =
    let { cls_body_elements; cls_body_loc; } = body in
    let cls_body_elements =
      List.map ~f:(fun elm ->
        match elm with
        | Cls_method _method -> (
          let { cls_method_attributes; cls_method_visibility; cls_method_modifier; cls_method_name; cls_method_params; cls_method_loc; cls_method_body; cls_method_return_ty; _ } = _method in
          let method_id =
            match cls_method_modifier with
            | Some Ast.Declaration.Cls_modifier_static -> (
              let result = List.find ~f:(fun (name, _) -> String.equal name cls_method_name.pident_name) !tcls_static_elements in
              match result with
              | Some (_, id) -> id
              | None ->
                failwith (Format.sprintf "unexpected: can not find static class method %s" cls_method_name.pident_name)
            )

            | _ -> (
              match (class_scope#find_cls_element cls_method_name.pident_name) with
              | Some (Scope.Cls_method { method_id; _ }) -> method_id
              | Some _ -> failwith "unexpected: expect class method, but got property"
              | None -> failwith (Format.sprintf "unexpected: can not find class method %s" cls_method_name.pident_name)
            )
          in
          let cls_method_params, cls_method_params_deps = annotate_function_params env cls_method_params in
          let cls_method_body = Option.map ~f:(annotate_block ~prev_deps:cls_method_params_deps env) cls_method_body in

          let this_deps = ref !props_deps in

          Option.iter
            ~f:(fun body_block ->
              let t = Typedtree.Block.(body_block.return_ty) in
              this_deps := t::(!this_deps);
            )
            cls_method_body;

          (* check return *)
          let _collected_returns = Env.take_return_types env in

          let fun_return, return_ty_deps =
            match cls_method_return_ty with
            | Some ty -> annotate_type env ty
            | None -> (
              let unit_type = Env.ty_unit env in
              TypeExpr.(Ctor (unit_type, [])), [unit_type]
            )
          in

          let new_type = {
            TypeDef.
            builtin = false;
            name = cls_method_name.pident_name;
            spec = Function {
              fun_params = [];
              fun_return;
            };
          } in

          (* class method deps *)
          method_deps := List.append !method_deps [method_id];

          this_deps := List.append !this_deps return_ty_deps;

          Type_context.map_node ctx
            ~f:(fun node -> {
              node with
              deps = List.rev !this_deps
              |> List.filter
                ~f:(fun id -> id <> cls_var.var_id)
              ;
              value = (TypeExpr.TypeDef new_type);
            })
            method_id
            ;
          T.Declaration.Cls_method {
            T.Declaration.
            cls_method_attributes;
            cls_method_visibility;
            cls_method_modifier;
            cls_method_params;
            cls_method_name = (cls_method_name.pident_name, method_id);
            cls_method_body;
            cls_method_loc;
          }
        )
        | Cls_property prop -> (
          let { cls_property_visibility; cls_property_loc; cls_property_name; _ } = prop in
          T.Declaration.Cls_property {
            T.Declaration.
            cls_property_loc;
            cls_property_visibility;
            cls_property_name;
          }
        )
      )
      cls_body_elements
    in
    { T.Declaration. cls_body_elements; cls_body_loc}
  in

  Env.with_new_scope env class_scope (fun _env ->
    let { cls_id; cls_visibility; cls_type_vars = _; cls_loc; cls_body; cls_comments; _ } = cls in
    let tcls_name = cls_id.pident_name in
    let cls_id = tcls_name, cls_var.var_id in
    let cls_body = annotate_class_body cls_body in

    Type_context.map_node ctx 
      ~f:(fun node -> {
        node with
        value = TypeExpr.TypeDef (
          { TypeDef.
            builtin = false;
            name = cls.cls_id.pident_name;
            spec = Class {
              tcls_name;
              tcls_extends = None;
              tcls_elements = List.rev !tcls_elements;
              tcls_static_elements = List.rev !tcls_static_elements;
            };
          }
        );
        loc = cls.cls_loc;
        deps = 
          (* remove self-reference *)
          List.filter
          ~f:(fun id -> id <> cls_var.var_id)
          (if List.is_empty !method_deps then List.rev !props_deps else List.rev !method_deps);
      })
      cls_var.var_id;

    { T.Declaration. cls_id; cls_visibility; cls_body; cls_loc; cls_comments; }
  )

and annotate_an_def_identifer env ident =
  let open Identifier in
  let { pident_name; pident_loc } = ident in
  let node = {
    Core_type.
    loc = pident_loc;
    value = TypeExpr.Unknown;
    check = none;
    deps = [];
  } in
  let id = Type_context.new_id (Env.ctx env) node in
  pident_name, id

and annotate_pattern env pat =
  let open Ast.Pattern in
  let { spec; loc } = pat in
  let id, spec =
    match spec with
    | Identifier ident -> (
      let name, id = annotate_an_def_identifer env ident in
      id, (T.Pattern.Symbol (name, id))
    )

    | _ -> failwith "unimplemented pattern"
  in
  { T.Pattern. spec; loc }, id

(* only collect deps, construct value in type check *)
and annotate_type env ty : (TypeExpr.t * int list) =
  let open Ast.Type in
  let { spec; _ } = ty in
  let deps = ref [] in
  match spec with
  | Ty_any -> TypeExpr.Any, []
  | Ty_ctor(ctor, params) -> (
    let { Identifier. pident_name; pident_loc } = ctor in
    let ty_var_opt = (Env.peek_scope env)#find_type_symbol pident_name in
    match ty_var_opt with
    | Some ty_var -> (
      (* TODO: find ctor in the scope *)
      let params, params_deps = List.map ~f:(annotate_type env) params |> List.unzip in
      deps := List.concat (!deps::params_deps);
      TypeExpr.Ctor (ty_var, params), [ty_var]
    )

    | None -> (
      let ctx = Env.ctx env in
      let err_spec = Type_error.CannotFindName pident_name in
      let err = Type_error.make_error ctx pident_loc err_spec in
      raise (Type_error.Error err)
    )

  )
  | Ty_arrow (params, result) -> (
    let params, params_types_deps = List.map ~f:(annotate_type env) params |> List.unzip in
    let return_type, return_type_deps = annotate_type env result in
    deps := List.concat ((!deps)::return_type_deps::params_types_deps);
    TypeExpr.Function(params, return_type), !deps
  )
  (* !deps *)

and annotate_function_params env params = 
  let open Ast.Function in
  let annoate_param param =
    let { param_pat; param_ty; param_loc; param_rest } = param in
    let param_pat, param_id = annotate_pattern env param_pat in
    let deps = ref [] in
    let value = ref TypeExpr.Unknown in
    Option.iter
      ~f:(fun ty ->
        let param_ty, param_ty_deps = annotate_type env ty in
        deps := List.append param_ty_deps !deps;
        value := param_ty;
      )
      param_ty
    ;
    let node = {
      loc = param_loc;
      value = !value;
      deps = !deps;
      check = none;  (* check init *)
    } in
    Type_context.update_node (Env.ctx env) param_id node;
    { T.Function.
      param_pat;
      param_ty = param_id;
      param_loc;
      param_rest;
    }, param_id
  in

  let { params_content; params_loc } = params in
  let params, params_types = List.map ~f:annoate_param params_content |> List.unzip in
  { T.Function. params_content = params; params_loc }, params_types

and annotate_function env fun_ =
  let open Ast.Function in

  let prev_scope = Env.peek_scope env in
  let fun_scope = new scope ~prev:prev_scope () in

  Env.with_new_scope env fun_scope (fun env ->
    let { visibility = _visibility; header; body; loc; comments; } = fun_ in

    let fun_id_opt = prev_scope#find_var_symbol header.id.pident_name in
    if Option.is_none fun_id_opt then (
      failwith (Format.sprintf "unexpected: function id %s is not added in parsing stage" header.id.pident_name)
    );
    let fun_id = (Option.value_exn fun_id_opt).var_id in
    let name_node = Type_context.get_node (Env.ctx env) fun_id in
    let fun_deps = ref [] in

    let name_node = {
      name_node with
      loc;
    } in
    Type_context.update_node (Env.ctx env) fun_id name_node;

    (*
     * differnt from TypeScript
     * if no return type is defined, use 'unit' type
     * do not try to infer from block, that's too complicated
     *)
    let return_ty, return_ty_Deps =
      match header.return_ty with
      | Some type_expr ->
        annotate_type env type_expr
      | None ->
        let _unit = Env.ty_unit env in
        TypeExpr.Ctor(_unit, []), []
    in

    fun_deps := List.append !fun_deps return_ty_Deps;

    let params, params_types = annotate_function_params env header.params in

    (* add all params into scope *)
    List.iter
      ~f:(fun param -> 
        let pat = param.param_pat in
        let name = 
          match pat.spec with
          | T.Pattern.Symbol (name, _) -> name
        in
        fun_scope#insert_var_symbol name {
          var_id = param.param_ty;
          var_kind = Ast.Pvar_let;
        };
      )
      params.params_content;

    let body = annotate_block ~prev_deps:params_types env body in
    let collected_returns = Env.take_return_types env in

    (* defined return *)
    fun_deps := body.return_ty::(!fun_deps);
    fun_deps := List.append !fun_deps collected_returns;

    Type_context.update_node (Env.ctx env) fun_id {
      name_node with
      value = TypeExpr.Unknown;  (* it's an typedef *)
      (* deps = return_id::params_types; *)
      deps = !fun_deps;
      check = (fun id ->
        let ctx = Env.ctx env in
        let block_node = Type_context.get_node ctx body.return_ty in
        (* if no return statements, use last statement of block *)
        if List.is_empty collected_returns then (
          if not (Check_helper.type_assinable ctx return_ty block_node.value) then (
            let open Type_error in
            let spec = CannotReturn(return_ty, block_node.value) in
            let err = make_error ctx block_node.loc spec in
            raise (Error err)
          );
          (* Type_context.update_node_type ctx id return_ty *)
        ) else (
          (* there are return statements, check every statments *)
          List.iter
            ~f:(fun return_ty_var ->
              let return_ty_node = Type_context.get_node ctx return_ty_var in
              if not (Check_helper.type_assinable ctx return_ty return_ty_node.value) then (
                let open Type_error in
                let spec = CannotReturn(return_ty, return_ty_node.value) in
                let err = make_error ctx return_ty_node.loc spec in
                raise (Error err)
              )
            )
            collected_returns
        );
        let type_def = { TypeDef.
          builtin = false;
          name = fun_.header.id.pident_name;
          spec = Function {
            fun_params = [];
            fun_return = return_ty;
          };
        } in
        Type_context.update_node_type ctx id (TypeExpr.TypeDef type_def)
      );
    };
    { T.Function.
      header = {
        id = fun_id;
        name = header.id.pident_name;
        params;
      };
      scope = fun_scope;
      body;
      ty_var = fun_id;
      comments;
    }
  )

let annotate_program env (program: Ast.program) =
  let { Ast. pprogram_declarations; pprogram_top_level = _; pprogram_loc; _; } = program in

  (* Hashtbl.iter_keys
    ~f:(fun key ->
      let node = {
        value = TypeExpr.Unknown;
        loc = Lichenscript_lex.Loc.none;
        deps = [];
        check = none;
      } in
      let new_id = Type_context.new_id (Env.ctx env) node in
      (Env.peek_scope env)#insert_var_symbol key new_id
    )
    pprogram_top_level.names
    ; *)

  let tprogram_declarations = List.map ~f:(annotate_declaration env) pprogram_declarations in

  let deps =
    List.fold
      ~init:[]
      ~f:(fun acc decl -> 
        let open T.Declaration in
        let { spec; _ } = decl in
        match spec with
        | Class cls -> (
          let { cls_id = (_, ty_var); _} = cls in
          ty_var::acc
        )

        | Function_ _fun -> (
          let open T.Function in
          let { ty_var; _ } = _fun in
          ty_var::acc
        )

        | Declare declare -> (
          let { decl_ty_var; _ } = declare in
          decl_ty_var::acc
        )

        | Enum _ -> acc

        | Import _ -> acc
        
      )
      tprogram_declarations
  in

  let val_ =
    {
      value = TypeExpr.Unknown;
      loc = pprogram_loc;
      deps;
      check = none;
    }
  in
  let ty_var = Type_context.new_id (Env.ctx env) val_ in
  let tree = { T.
    tprogram_declarations;
    tprogram_scope = Env.module_scope env;
    ty_var
  } in
  tree
