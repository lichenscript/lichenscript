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
open Scope
open Lichenscript_parsing

(*
 * 1. annotate parsed tree to typed tree
 * 2. resolve top-level variables internally
 * 
 * deeper variables resolution remains to type-check phase
 *)

module T = Typedtree

let annotate_visibility (visibility: Asttypes.visibility option): Core_type.Visibility.t =
  let open Asttypes in
  match visibility with
  | Some Pvisibility_public -> Visibility.Public
  | Some Pvisibility_protected -> Visibility.Protected
  | Some Pvisibility_private -> Visibility.Private
  | Some Pvisibility_internal
  | None
    -> Visibility.Internal

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
        value = TypeExpr.Ctor(Ref (Env.ty_unit env), []);
        loc;
        deps = List.append prev_deps [ty_var];
      } in

      let id = Type_context.new_id (Env.ctx env) node in
      [id], (T.Statement.Semi expr)
    )

    | While _while -> (
      let { while_test; while_block; while_loc } = _while in
      let while_test = annotate_expression ~prev_deps env while_test in
      let while_scope = new while_scope ~prev:(Env.peek_scope env) () in
      let while_block = annotate_block_expr
        ~prev_deps:[while_test.ty_var] ~scope:while_scope env while_block
      in
      let next_deps = [ while_block.return_ty ] in
      next_deps, T.Statement.While { while_test; while_block; while_loc }
    )

    | Binding binding -> (
      let { binding_kind; binding_pat; binding_init; binding_loc; _ } = binding in

      let binding_init = annotate_expression ~prev_deps env binding_init in

      let binding_pat, pat_deps = annotate_pattern ~pat_id:0 env binding_pat in
      let open T.Pattern in
      match binding_pat.spec with
      | Underscore -> (
        [], T.Statement.Binding { T.Statement.
          binding_kind;
          binding_pat;
          binding_init;
          binding_loc;
        }
      )

      | Symbol (_, sym_id) -> (
        let ctx = Env.ctx env in
        let node = Type_context.get_node ctx sym_id in
        Type_context.update_node ctx sym_id {
          node with
          deps = List.concat [node.deps; [binding_init.ty_var]; prev_deps ];
        };

        let deps = List.append pat_deps [sym_id] in
        deps, T.Statement.Binding { T.Statement.
          binding_kind;
          binding_pat;
          binding_init;
          binding_loc;
        }
      )

      | Literal _
      | Array _
      | EnumCtor _ -> (
        let err = Type_error.(make_error (Env.ctx env) binding_loc (CannotBindingOfPattern "enum")) in
        raise (Type_error.Error err)
      )
    )

    | Break label ->
      prev_deps, (T.Statement.Break label)

    | Continue label ->
      prev_deps, (T.Statement.Continue label)

    | Debugger -> prev_deps, failwith "not implment"

    | Return ret_opt -> (
      match ret_opt with
      | Some expr -> (
        let expr = annotate_expression ~prev_deps env expr in
        let ty_var = expr.ty_var in

        [ty_var], (T.Statement.Return (Some expr))
      )

      | None -> prev_deps, (T.Statement.Return None)
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
      let deps, value =
        match cnst with
        | Integer _ ->
          let ty_var = Option.value_exn (root_scope#find_type_symbol "i32") in
          [ty_var], TypeExpr.Ctor(Ref ty_var, [])

        | Char _ ->
          let ty_var = Option.value_exn (root_scope#find_type_symbol "char") in
          [ty_var], TypeExpr.Ctor(Ref ty_var, [])

        (* 'c' *)
        | String _ ->
          [], TypeExpr.String

        | Float _ ->
          let ty_var = Option.value_exn (root_scope#find_type_symbol "f32") in
          [ty_var], TypeExpr.Ctor(Ref ty_var, [])

        | Boolean _ -> 
          let ty_var = Option.value_exn (root_scope#find_type_symbol "boolean") in
          [ty_var], TypeExpr.Ctor(Ref ty_var, [])

      in

      let node = {
        value;
        loc = loc;
        deps;
      } in

      let node_id = Type_context.new_id (Env.ctx env) node in

      node_id, (T.Expression.Constant cnst)
    )

    | Identifier id -> (
      let ty_var_opt = (Env.peek_scope env)#find_var_symbol id.pident_name in
      match ty_var_opt with
      | Some variable -> (
        if not !(variable.var_init) then (
          let err = Type_error.(make_error (Env.ctx env) expr.loc (CannotAccessBeforeInit id.pident_name)) in
          raise (Type_error.Error err)
        );
        (*
         * it's class or enum, create a ref to it,
         * for example, a None identifer, actually represents a constructor of None
         *)
        if is_name_enum_or_class id.pident_name then (
          let node = {
            value = TypeExpr.Ref variable.var_id;
            loc = loc;
            deps = [variable.var_id];
          } in
          let ty_id = Type_context.new_id (Env.ctx env) node in
          ty_id, (T.Expression.Identifier (id.pident_name, ty_id))
        ) else (
          Env.capture_variable env ~name:id.pident_name;
          variable.var_id, (T.Expression.Identifier (id.pident_name, variable.var_id))
        )
      )

      | _ ->
        let err_spec = Type_error.CannotFindName id.pident_name in
        let err = Type_error.make_error (Env.ctx env) id.pident_loc err_spec in
        raise (Type_error.Error err)
    )

    | Lambda lambda -> (
      let lambda_scope = new lambda_scope ~prev:(Env.peek_scope env) () in
      Env.with_new_scope env lambda_scope (fun env ->
        let prev_in_lambda = Env.in_lambda env in

        Env.set_in_lambda env true;
        let { lambda_params; lambda_return_ty; lambda_body } = lambda in

        let params, params_type, deps = annotate_function_params env lambda_params in

        let lambda_return_ty, ret_deps =
          match lambda_return_ty with
          | Some t ->
            let t, deps = annotate_type env t in
            t, deps
          | None -> (
            let none_type = Env.ty_unit env in
            TypeExpr.Ctor(Ref none_type, []), []
          )
        in

        let lambda_body = annotate_expression ~prev_deps:(List.append deps ret_deps) env lambda_body in

        let node = {
          value = TypeExpr.Lambda(params_type, lambda_return_ty);
          loc = loc;
          deps = [lambda_body.ty_var];
        } in

        let node_id = Type_context.new_id (Env.ctx env) node in

        Env.set_in_lambda env prev_in_lambda;

        node_id, T.Expression.Lambda {
          lambda_params = params;
          lambda_body;
          lambda_return_ty;
          lambda_scope;
        }
      )
    )

    | If _if -> (
      let id, spec = annotate_expression_if ~prev_deps env _if in
      id, T.Expression.If spec
    )

    | Array arr_list  -> (
      let a_list = List.map ~f:(annotate_expression ~prev_deps env) arr_list in
      let deps = List.map ~f:(fun expr -> expr.ty_var) a_list in

      let ty_var = Type_context.new_id (Env.ctx env) {
        value = TypeExpr.Unknown;
        loc;
        deps;
      } in

      ty_var, (T.Expression.Array a_list)
    )

    | Map entries -> (
      let entries, deps =
        entries
        |> List.map ~f:(fun { map_entry_key; map_entry_value; map_entry_loc } ->
          let map_entry_value = annotate_expression ~prev_deps env map_entry_value in
          let dep = map_entry_value.ty_var in
          { T.Expression.
            map_entry_key;
            map_entry_value;
            map_entry_loc
          }, dep
        )
        |> List.unzip
      in

      let ty_var = Type_context.new_id (Env.ctx env) {
        value = TypeExpr.Unknown;
        loc;
        deps;
      } in

      ty_var, T.Expression.Map entries
    )

    | Call call ->
      let ty_var, spec = annotate_expression_call ~prev_deps env loc call in
      ty_var, (T.Expression.Call spec)

    | Member (expr, name) -> (
      let expr = annotate_expression ~prev_deps env expr in
      let ctx = Env.ctx env in
      let node = {
        value = TypeExpr.Unknown;
        loc;
        deps = List.append prev_deps [expr.ty_var];
      } in
      let id = Type_context.new_id ctx node in
      id, T.Expression.Member(expr, name)
    )

    | Index(expr, index_expr) -> (
      let expr = annotate_expression ~prev_deps env expr in
      let index_expr = annotate_expression ~prev_deps env index_expr in
      let node = {
        value = TypeExpr.Unknown;
        deps = [ expr.ty_var; index_expr.ty_var ];
        loc;
      } in

      let id = Type_context.new_id (Env.ctx env) node in

      id, (T.Expression.Index(expr, index_expr))
    )

    | Unary(op, child_expr) -> (
      let child_expr = annotate_expression ~prev_deps env child_expr in
      let node = {
        value = TypeExpr.Unknown;
        deps = [child_expr.ty_var];
        loc;
      } in
      let id = Type_context.new_id (Env.ctx env) node in
      id, T.Expression.Unary(op, child_expr)
    )

    | Binary (op, left, right) -> (
      let left = annotate_expression ~prev_deps env left in
      let right = annotate_expression ~prev_deps env right in
      let open T.Expression in
      let node = {
        value = TypeExpr.Unknown;
        deps = [left.ty_var; right.ty_var];
        loc;
      } in
      let id = Type_context.new_id (Env.ctx env) node in

      id, (T.Expression.Binary(op, left, right))
    )

    (*
     * 1. assigning to local vars: a = 3
     * 2. assigning to member:
     *    this.a = 3
     *    some().a = 3
     * 3. assinging to index: a[0] = 3
     *)
    | Assign (op, left, expr) -> (
      let expr = annotate_expression ~prev_deps env expr in
      let scope = Env.peek_scope env in
      let ctx = Env.ctx env in
      let left' =
        match left with
        | { spec = Identifier id; _; } -> (
          match scope#find_var_symbol id.pident_name with
          | Some var ->
            (* check all find_var_symbol to captured *)
            Env.capture_variable env ~name:id.pident_name;
            { T.Expression.
              spec = Identifier (id.pident_name, var.var_id);
              loc = id.pident_loc;
              ty_var = var.var_id;
              attributes = [];
            }
          | None -> (
            let err = Type_error.(make_error ctx loc (CannotFindName id.pident_name)) in
            raise (Type_error.Error err)
          )
        )

        | { spec = Member _ ; _ }
        | { spec = Index _ ; _ } ->
          annotate_expression ~prev_deps:[expr.ty_var] env left

        | _ -> (
          let err = Type_error.(make_error ctx loc InvalidAssign) in
          raise (Type_error.Error err)
        )
      in
      let unit_type = Env.ty_unit env in
      let value = (TypeExpr.Ctor (Ref unit_type, [])) in
      let next_id = Type_context.new_id ctx {
        value;
        deps = [ expr.ty_var; left'.ty_var ];
        loc;
      } in
      next_id, Assign(op, left', expr)
    )

    | Block block -> (
      let block = annotate_block_expr ~prev_deps env block in
      T.Block.(block.return_ty), (T.Expression.Block block)
    )

    | Init init -> (
      let { init_loc; init_name; init_elements } = init in
      let ctx = Env.ctx env in
      let type_int = (Env.peek_scope env)#find_type_symbol init_name.pident_name in
      let deps = ref [] in

      let annotate_element elm =
        match elm with
        | InitSpread expr -> (
          let expr = annotate_expression ~prev_deps env expr in
          deps := expr.ty_var::!deps;
          T.Expression.InitSpread expr
        )
        | InitEntry { init_entry_loc; init_entry_key; init_entry_value } -> (
          let init_entry_value =
            match init_entry_value with
            | Some expr ->
              let expr' = annotate_expression ~prev_deps env expr in
              deps := expr'.ty_var::!deps;
              expr'

            (* no value of key, find variable in current scope *)
            | None -> (
              let key_name = init_entry_key.pident_name in
              let ty_var_opt = (Env.peek_scope env)#find_var_symbol key_name in
              match ty_var_opt with
              | Some variable -> (
                Env.capture_variable env ~name:key_name;
                deps := variable.var_id::!deps;
                { T.Expression.
                  spec = Identifier (key_name, variable.var_id);
                  loc = init_entry_key.pident_loc;
                  ty_var = variable.var_id;
                  attributes = [];
                }
              )

              | _ ->
                let err_spec = Type_error.CannotFindName key_name in
                let err = Type_error.make_error (Env.ctx env) init_entry_key.pident_loc err_spec in
                raise (Type_error.Error err)

            )
          in
          T.Expression.InitEntry {
            init_entry_loc;
            init_entry_key;
            init_entry_value;
          }
        )
      in

      match type_int with
      | Some v -> (
        let node = {
          value = TypeExpr.Ctor(Ref v, []);
          loc = init_loc;
          deps = v::(List.rev !deps);
          (* TODO: check props and expressions *)
        } in
        let node_id = Type_context.new_id ctx node in
        node_id, T.Expression.Init{
          init_loc;
          init_name = (init_name.pident_name, v);
          init_elements = List.map ~f:annotate_element init_elements;
        } 
      )
      | None -> (
        let err = Type_error.(make_error ctx init_loc (CannotFindName init_name.pident_name)) in
        raise (Type_error.Error err)
      )

    )

    | Match _match -> annotate_expression_match ~prev_deps env _match

    | This -> (
      let scope = Env.peek_scope env in
      let this_expr = scope#this_expr in
      (* TODO: generic *)
      let node = {
        value = this_expr;
        loc;
        deps = [];
      } in

      let node_id = Type_context.new_id (Env.ctx env) node in
      node_id, T.Expression.This
    )

    | Super -> failwith "not implemented this"

  in
  { T.Expression.
    spec;
    loc;
    attributes;
    ty_var;
  }

and annotate_expression_match ~prev_deps env _match =
  let open Ast.Expression in
  let { match_expr; match_clauses; match_loc } = _match in
  let match_expr = annotate_expression ~prev_deps env match_expr in
  let parent_scope = Env.peek_scope env in

  let annotate_clause clause =
    let open Ast.Expression in
    let scope = new scope ~prev:parent_scope () in
    parent_scope#add_child scope;
    Env.with_new_scope env scope (fun env ->
      let { clause_pat; clause_consequent; clause_loc } = clause in
      prescan_pattern_for_scope ~kind:Ast.Pvar_const ~scope env clause_pat;
      let clause_pat, clause_deps = annotate_pattern ~pat_id:0 env clause_pat in
      let clause_consequent = annotate_expression ~prev_deps:clause_deps env clause_consequent in
      { T.Expression.
        clause_pat;
        clause_consequent;
        clause_loc;
        clause_scope = scope;
      }
    )
  in

  let match_clauses = List.map ~f:annotate_clause match_clauses in
  let clauses_deps = List.map ~f:(fun clause -> T.Expression.(clause.clause_consequent.ty_var)) match_clauses in

  let node = {
    Core_type.
    value = Unknown;
    deps = List.append [match_expr.ty_var] clauses_deps;
    loc = match_loc;
  } in

  let id = Type_context.new_id (Env.ctx env) node in

  id, (T.Expression.Match { T.Expression.
    match_expr;
    match_clauses;
    match_loc;
  })

and annotate_expression_call ~prev_deps env loc call =
  let open Ast.Expression in
  let { callee; call_params; call_loc } = call in

  let callee = annotate_expression ~prev_deps env callee in
  let call_params = List.map ~f:(annotate_expression ~prev_deps env) call_params in

  let params_deps = List.map ~f:(fun expr -> expr.ty_var) call_params in

  let ty_var = Type_context.new_id (Env.ctx env) {
    value = TypeExpr.Unknown;
    loc;
    deps = List.append [ T.Expression.(callee.ty_var) ] params_deps;
  } in

  ty_var, { T.Expression. callee; call_params; call_loc }

and annotate_expression_if ~prev_deps env _if =
  let open Ast.Expression in
  let { if_test; if_consequent; if_alternative; if_loc } = _if in

  let if_test = annotate_expression env ~prev_deps if_test in
  let if_consequent = annotate_block_expr ~prev_deps:[if_test.ty_var] env if_consequent in
  let alt_deps = ref [] in
  let if_alternative =
    Option.map
    ~f:(fun alt ->
      match alt with
      | If_alt_block block ->
        let blk = annotate_block_expr ~prev_deps:[if_test.ty_var] env block in
        alt_deps := [blk.return_ty];
        T.Expression.If_alt_block blk

      | If_alt_if else_if ->
        let id, else_if = annotate_expression_if ~prev_deps:[if_test.ty_var] env else_if in
        alt_deps := [id];
        T.Expression.If_alt_if else_if

    )
    if_alternative
  in

  let node = {
    value = TypeExpr.Unknown;
    loc = if_loc;
    deps = List.append [if_consequent.return_ty] !alt_deps;
  } in

  let node_id = Type_context.new_id (Env.ctx env) node in
  node_id, { T.Expression.
    if_test;
    if_consequent;
    if_alternative;
    if_ty_var = node_id;
    if_loc;
  }

and prescan_pattern_for_scope ~kind ~(scope: Scope.scope) env pat =
  let open Ast.Pattern in
  match pat.spec with
  | Identifier { Identifier. pident_name; pident_loc } -> (
    if String.equal pident_name "_" then ()
    else if is_name_enum_or_class pident_name then ()
    else (
      let node = {
        Core_type.
        loc = pident_loc;
        value = TypeExpr.Unknown;
        deps = [];
      } in
      let id = Type_context.new_id (Env.ctx env) node in
      (match scope#new_var_symbol pident_name ~id ~kind with
      | `Duplicate -> (
        let err = Type_error.(make_error (Env.ctx env) pident_loc (Redefinition pident_name)) in
        raise Type_error.(Error err)
      )
      | _ -> ());
    )
  )

  | EnumCtor (_, child_pat) -> prescan_pattern_for_scope ~kind ~scope env child_pat
  | Array { elements; rest } ->
    List.iter ~f:(prescan_pattern_for_scope ~kind ~scope env) elements;
    Option.iter ~f:(prescan_pattern_for_scope ~kind ~scope env) rest

  | _ -> ()

and annotate_block_impl ~prev_deps env block : T.Block.t =
  let open Ast.Block in
  let scope = Env.peek_scope env in

  let prescan_local_vars body =
    List.iter
      ~f:(fun stmt ->
        let open Ast.Statement in
        match stmt.spec with
        | Binding { binding_kind; binding_pat; _ } ->
          prescan_pattern_for_scope ~kind:binding_kind ~scope env binding_pat
        | _ -> ()
      )
      body
  in

  let { body; loc } = block in

  prescan_local_vars body;

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
  } in
  let return_ty = Type_context.new_id (Env.ctx env) node in
  { T.Block.
    body = body_stmts;
    loc;
    scope = Env.peek_scope env;
    return_ty;
  }

and annotate_block_expr ~prev_deps ?scope env block : T.Block.t =
  let prev_scope = Env.peek_scope env in
  let block_scope =
    match scope with
    | Some s -> s
    | _ -> new scope ~prev:prev_scope ()
  in
  prev_scope#add_child block_scope;

  Env.with_new_scope env block_scope (fun env ->
    annotate_block_impl ~prev_deps env block
  )

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

    | Interface intf -> (
      let intf = annotate_interface env intf in
      let _, ty_int = intf.intf_name in
      ty_int, T.Declaration.Interface intf
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
        let parent_scope = Env.peek_scope env in
        let ty_id =
          match parent_scope#find_var_symbol id.pident_name with
          | Some v -> v.var_id
          | None -> failwith (Format.sprintf "unexpected: %s is not added to scope\n" id.pident_name)
        in

        let scope = new scope ~prev:parent_scope () in

        let params, params_type, params_deps = Env.with_new_scope env scope (fun env ->
          annotate_function_params env params
        ) in

        let fun_return, fun_return_deps =
          match return_ty with
          | Some ty -> (
            annotate_type env ty
          )
          | None -> (
            let unit_ty = Env.ty_unit env in
            TypeExpr.Ctor(Ref unit_ty, []), []
          )
        in

        let ty_def = {
          TypeDef.
          id = ty_id;
          builtin = false;
          name = id.pident_name;
          spec = Function {
            fun_params = params_type;
            fun_return;
          }
        } in

        Type_context.update_node (Env.ctx env) ty_id {
          value = TypeExpr.TypeDef ty_def;
          deps = List.append params_deps fun_return_deps;
          loc = decl_loc;
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
          name = (Identifier.(id.pident_name), ty_id);
          name_loc= Identifier.(id.pident_loc) ;
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

    | Enum enum ->
      let enum = annotate_enum env enum in
      let _, ty_var = T.Enum.(enum.name) in
      ty_var, T.Declaration.Enum enum

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
 *
 * What should a class depends?
 * - Everything in property/method's sigature(not including the body).
 *
 * What depends on a class?
 * - Everything in the method body
 *
 *)
and annotate_class env cls =
  let open Ast.Declaration in

  let cls_var = Option.value_exn ((Env.peek_scope env)#find_var_symbol cls.cls_id.pident_name) in

  let prev_scope = Env.peek_scope env in

  let this_expr = TypeExpr.Ctor(Ref cls_var.var_id, List.map ~f:Identifier.(fun id -> TypeExpr.TypeSymbol id.pident_name) cls.cls_type_vars) in
  let class_scope = new class_scope ~prev:prev_scope cls_var.var_id this_expr in

  List.iter
    ~f:(fun ident ->
      class_scope#insert_generic_type_symbol ident.pident_name;
    )
    cls.cls_type_vars;

  let tcls_static_elements = ref [] in
  let tcls_elements = ref [] in
  let props_deps = ref [] in
  let method_deps = ref [] in

  let ctx = Env.ctx env in

  let base_deps = ref None in

  let add_tcls_element elm loc =
    let name, elm_spec = elm in
    let exist =
      match elm_spec with
      | TypeDef.Cls_elm_get_set (_, Some _, _) ->
        !tcls_elements
        |> List.find ~f:(fun (item_name, item_spec) ->
          let is_getter = match item_spec with
            | TypeDef.Cls_elm_get_set (_, Some _, _) -> true
            | _ -> false
          in
          (String.equal item_name name) && is_getter
        )
        |> Option.is_some

      | TypeDef.Cls_elm_get_set (_, _, Some _) ->
        !tcls_elements
        |> List.find ~f:(fun (item_name, item_spec) ->
          let is_getter = match item_spec with
            | TypeDef.Cls_elm_get_set (_, _, Some _) -> true
            | _ -> false
          in
          (String.equal item_name name) && is_getter
        )
        |> Option.is_some

      |_ ->
        !tcls_elements
        |> List.find ~f:(fun (item_name, _) -> String.equal item_name name)
        |> Option.is_some
    in
    if exist then (
      let err = Type_error.(make_error (Env.ctx env) loc (ClassPropRedefinition(cls.cls_id.pident_name, name))) in
      raise (Type_error.Error err)
    );
    tcls_elements := elm::(!tcls_elements)
  in

  let add_tcls_static_element elm loc =
    let name, _ = elm in
    let exist =
      !tcls_static_elements
      |> List.find ~f:(fun (item_name, _) -> String.equal item_name name)
      |> Option.is_some
    in
    if exist then (
      let err = Type_error.(make_error (Env.ctx env) loc (ClassPropRedefinition(cls.cls_id.pident_name, name))) in
      raise (Type_error.Error err)
    );
    tcls_static_elements := elm::(!tcls_static_elements)
  in

  (* depend on the base class *)
  let tcls_extends =
    Option.map
    ~f:(fun extend ->
      let ext_type, deps = annotate_type env extend in
      base_deps := Some deps;
      ext_type
    )
    cls.cls_extends;
  in

  let tcls_implements =
    List.map
    ~f:(fun impl ->
      let impl_type, _deps = annotate_type env impl in
      (impl_type, impl.loc)
    )
    cls.cls_implements
  in

  let annotate_class_body body =
    let { cls_body_elements; cls_body_loc; } = body in
    let cls_body_elements =
      List.map ~f:(fun elm ->
        match elm with
        | Cls_method _method -> (
          let method_scope = new function_scope ~prev:(Env.peek_scope env) () in
          let { cls_method_attributes; cls_method_visibility; cls_method_modifier; cls_method_name; cls_method_params; cls_method_loc; cls_method_body; cls_method_return_ty; _ } = _method in
          let type_visibility = annotate_visibility cls_method_visibility in
          let is_static =
            match cls_method_modifier with
            | Some Cls_modifier_static -> true
            | _ -> false
          in
          let method_is_virtual =
            match cls_method_modifier with
            | Some Ast.Declaration.Cls_modifier_virtual
            | Some Ast.Declaration.Cls_modifier_override -> true
            | _ -> false
          in
          Env.with_new_scope env method_scope (fun env ->
            let cls_method_params, method_params, cls_method_params_deps = annotate_function_params env cls_method_params in

            let cls_method_body = annotate_block_impl ~prev_deps:[cls_var.var_id] env cls_method_body in

            let this_deps = ref !props_deps in

            this_deps := Typedtree.Block.(cls_method_body.return_ty)::(!this_deps);

            let method_return, return_ty_deps =
              match cls_method_return_ty with
              | Some ty -> annotate_type env ty
              | None -> (
                let unit_type = Env.ty_unit env in
                TypeExpr.(Ctor (Ref unit_type, [])), [unit_type]
              )
            in

            let method_id = Type_context.size (Env.ctx env) in
            let new_type =
              match _method.cls_method_modifier with
              | Some Cls_modifier_static -> { TypeDef.
                id = method_id;
                builtin = false;
                name = cls_method_name.pident_name;
                spec = Function {
                  fun_params = method_params;
                  fun_return = method_return;
                };
              }
              | _ -> { TypeDef.
                id = method_id;
                builtin = false;
                name = cls_method_name.pident_name;
                spec = ClassMethod {
                  method_cls_id = cls_var.var_id;
                  method_get_set = None;
                  method_is_virtual;
                  method_params = method_params;
                  method_return;
                };
              }
            in

            (* class method deps *)
            method_deps := List.append !method_deps [method_id];

            this_deps := List.append !this_deps return_ty_deps;

            let typedef = TypeExpr.TypeDef new_type in
            ignore (Type_context.new_id ctx
              { Core_type.
                deps = List.concat [ List.rev !this_deps; cls_method_params_deps ]
                |> List.filter
                  ~f:(fun id -> id <> cls_var.var_id)
                ;
                loc = _method.cls_method_loc;
                value = typedef;
              }
            );
            let ty_elm = Core_type.TypeDef.Cls_elm_method(type_visibility, new_type) in
            if is_static then
              add_tcls_static_element (cls_method_name.pident_name, ty_elm) cls_method_loc
            else (
              add_tcls_element (cls_method_name.pident_name, ty_elm) _method.cls_method_loc;
              class_scope#insert_cls_element
                { Scope.ClsElm.
                  name = (cls_method_name.pident_name, method_id);
                  spec = Method;
                  visibility = cls_method_visibility;
                }
            );
            T.Declaration.Cls_method {
              T.Declaration.
              cls_method_attributes;
              cls_method_visibility;
              cls_method_modifier;
              cls_method_params;
              cls_method_name = (cls_method_name.pident_name, method_id);
              cls_method_scope = Some method_scope;
              cls_method_body;
              cls_method_loc;
            }
          )
        )
        | Cls_property prop -> (
          let { cls_property_visibility; cls_property_loc; cls_property_name; cls_property_type; _ } = prop in
          let type_visibility = annotate_visibility cls_property_visibility in
          let property_ty, deps = annotate_type env cls_property_type in
          let node = {
            value = property_ty;
            deps;
            loc = cls_property_loc;
          } in
          let node_id = Type_context.new_id ctx node in
          let ty_elm = Core_type.TypeDef.Cls_elm_prop (type_visibility, node_id, property_ty) in
          add_tcls_element (cls_property_name.pident_name, ty_elm) cls_property_loc;

          (*
          * the class itself depends on all the properties
          * all the method depends on the class
          *)
          props_deps := node_id::(!props_deps);

          class_scope#insert_cls_element
            { Scope.ClsElm.
              name = (cls_property_name.pident_name, node_id);
              spec = Property;
              visibility = cls_property_visibility;
            };

          T.Declaration.Cls_property {
            T.Declaration.
            cls_property_loc;
            cls_property_visibility;
            cls_property_name;
          }
        )

        | Cls_declare declare -> (
          let { cls_decl_method_attributes; cls_decl_method_name; cls_decl_method_params; cls_decl_method_loc; cls_decl_method_return_ty; cls_decl_method_get_set; _ } = declare in
          let type_visibility = Visibility.Public in

          let declare_id = Type_context.size ctx in

          (match List.last cls_decl_method_attributes with
          | Some { Ast. attr_name = { txt = "external"; _ }; attr_payload = ext_name::_; _ } ->
            Type_context.set_external_symbol (Env.ctx env) declare_id ext_name

          | _ ->
            let open Type_error in
            let err = make_error (Env.ctx env) cls_decl_method_loc DeclareFunctionShouldSpecificExternal in
            raise (Error err)
          );

          let parent_scope = Env.peek_scope env in
          let scope = new scope ~prev:parent_scope () in

          let cls_decl_method_params, method_params, cls_method_params_deps =
            Env.with_new_scope env scope (fun env ->
              annotate_function_params env cls_decl_method_params
            )
          in

          let method_return, return_ty_deps =
            match cls_decl_method_return_ty with
            | Some ty -> annotate_type env ty
            | None -> (
              let unit_type = Env.ty_unit env in
              TypeExpr.(Ctor (Ref unit_type, [])), [unit_type]
            )
          in

          let method_get_set =
            Option.map
            ~f:(fun get_set ->
              match get_set with
              | Ast.Declaration.Cls_getter -> TypeDef.Getter
              | Ast.Declaration.Cls_setter -> TypeDef.Setter
            )
            cls_decl_method_get_set
          in

          let new_type =
            { TypeDef.
              id = declare_id;
              builtin = false;
              name = cls_decl_method_name.pident_name;
              spec = ClassMethod {
                method_cls_id = cls_var.var_id;
                method_get_set;
                method_is_virtual = false;
                method_params = method_params;
                method_return;
              };
            }
          in

          let cls_elm = 
            match cls_decl_method_get_set with
            | Some Ast.Declaration.Cls_getter ->
              Core_type.TypeDef.Cls_elm_get_set(type_visibility, Some new_type, None)

            | Some Ast.Declaration.Cls_setter ->
              Core_type.TypeDef.Cls_elm_get_set(type_visibility, None, Some new_type)

            | None ->
              Core_type.TypeDef.Cls_elm_method(type_visibility, new_type)

          in

          add_tcls_element (cls_decl_method_name.pident_name, cls_elm) cls_decl_method_loc;

          ignore (Type_context.new_id ctx
            { Core_type.
              deps = (List.append cls_method_params_deps return_ty_deps)
              |> List.filter
                ~f:(fun id -> id <> cls_var.var_id)
              ;
              value = (TypeExpr.TypeDef new_type);
              loc = cls_decl_method_loc;
            });

          (match cls_decl_method_get_set with
          | Some Cls_getter -> 
            class_scope#insert_cls_element
              { Scope.ClsElm.
                name = (cls_decl_method_name.pident_name, declare_id);
                spec = Getter;
                (* temporary use public here *)
                visibility = Some Asttypes.Pvisibility_public;
              }

          | Some Cls_setter ->
            class_scope#insert_cls_element
              { Scope.ClsElm.
                name = cls_decl_method_name.pident_name, declare_id;
                spec = Setter;
                (* temporary use public here *)
                visibility = Some Asttypes.Pvisibility_public;
              }

          | None ->
            class_scope#insert_cls_element
              { Scope.ClsElm.
                name = cls_decl_method_name.pident_name, declare_id;
                spec = Method;
                (* temporary use public here *)
                visibility = Some Asttypes.Pvisibility_public;
              }
          );

          T.Declaration.Cls_declare {
            cls_decl_method_attributes;
            cls_decl_method_loc;
            cls_decl_method_name = cls_decl_method_name.pident_name, declare_id;
            cls_decl_method_params;
          }
        )
      )
      cls_body_elements
    in
    { T.Declaration. cls_body_elements; cls_body_loc}
  in

  Env.with_new_scope env class_scope (fun _env ->
    let { cls_id; cls_visibility; cls_type_vars; cls_loc; cls_body; cls_comments; _ } = cls in
    let tcls_name = cls_id.pident_name in
    let cls_id = tcls_name, cls_var.var_id in
    let cls_body = annotate_class_body cls_body in

    (* reduced all method and elements here *)
    Type_context.map_node ctx 
      ~f:(fun _ -> {
        value = TypeExpr.TypeDef { TypeDef.
          id = cls_var.var_id;
          builtin = false;
          name = cls.cls_id.pident_name;
          spec = Class {
            tcls_name;
            tcls_vars = List.map ~f:(fun id -> Identifier.(id.pident_name)) cls_type_vars;
            tcls_extends;
            tcls_implements;
            tcls_elements = List.rev !tcls_elements;
            tcls_static_elements = List.rev !tcls_static_elements;
          };
        };
        loc = cls.cls_loc;
        deps = 
          (* remove self-reference *)
          List.concat [
            [ List.rev !method_deps; List.rev !props_deps; Option.to_list !base_deps |> List.concat ]
            |> List.concat
            |> List.filter ~f:(fun id -> id <> cls_var.var_id)
          ]
      })
      cls_var.var_id;

    { T.Declaration. cls_id; cls_visibility; cls_body; cls_loc; cls_comments; }
  )

and annotate_an_def_identifer env ident =
  let open Identifier in
  let scope = Env.peek_scope env in
  let { pident_name; _ } = ident in
  let variable = scope#find_var_symbol pident_name in
  let variable = Option.value_exn ~message:"identifier not found" variable in
  variable.var_init := true;
  pident_name, variable.var_id

and annotate_function_param env ident =
  let open Identifier in
  let { pident_name; pident_loc } = ident in
  let node = {
    Core_type.
    loc = pident_loc;
    value = TypeExpr.Unknown;
    deps = [];
  } in
  let id = Type_context.new_id (Env.ctx env) node in
  let scope = Env.peek_scope env in
  (match scope#new_var_symbol ~id ~kind:Pvar_const pident_name  with
  | `Duplicate -> (
    let err = Type_error.(make_error (Env.ctx env) pident_loc (Redefinition pident_name)) in
    raise Type_error.(Error err)
  )
  | _ -> ());
  scope#init_symbol pident_name;
  pident_name, id

and is_name_enum_or_class name =
  let first_char = String.get name 0 in
  Char.is_uppercase first_char

and annotate_pattern ~pat_id env pat : (T.Pattern.t * int list) =
  let open Ast.Pattern in
  let { spec; loc } = pat in
  let scope = Env.peek_scope env in
  let deps = ref [] in
  let spec =
    match spec with
    | Literal l -> T.Pattern.Literal l
    | Identifier ident -> (
      (* It's a enum contructor *)
      if String.equal ident.pident_name "_" then (
        T.Pattern.Underscore
      ) else if is_name_enum_or_class ident.pident_name then (
        Env.capture_variable env ~name:ident.pident_name;

        let ctor_var = scope#find_var_symbol ident.pident_name in
        if Option.is_none ctor_var then (
          let err = Type_error.(make_error (Env.ctx env) ident.pident_loc (NotAEnumConstructor ident.pident_name)) in
          raise (Type_error.Error err)
        );
        let ctor = Option.value_exn ctor_var in
        deps := List.append !deps [ctor.var_id];
        (T.Pattern.Symbol (ident.pident_name, ctor.var_id))
      ) else (
        (* local var *)
        let name, id = annotate_an_def_identifer env ident in
        deps := List.append !deps [id];
        (T.Pattern.Symbol (name, id))
      )
    )

    | EnumCtor(id, pat) -> (
      let ctor_var = scope#find_var_symbol id.pident_name in
      if Option.is_none ctor_var then (
        let err = Type_error.(make_error (Env.ctx env) loc (CannotFindName id.pident_name)) in
        raise (Type_error.Error err)
      );
      let ctor_var = Option.value_exn ctor_var in

      let param_pat, param_deps = annotate_pattern ~pat_id:(pat_id + 1) env pat in
      deps := List.concat [ param_deps; !deps; [ctor_var.var_id]];

      (T.Pattern.EnumCtor (
        (id.pident_name, ctor_var.var_id),
        param_pat
      ))
    )

    | Array array_pat -> (
      let { elements; rest; } = array_pat in
      
      let elements, element_deps =
        elements
        |> List.map ~f:(annotate_pattern ~pat_id:(pat_id + 1) env)
        |> List.unzip
      in

      let rest, rest_deps =
        match rest with
        | Some rest_elm ->
          let rest, rest_deps = annotate_pattern ~pat_id:(pat_id + 1) env rest_elm in
          (Some rest), rest_deps

        | None -> None, []
      in

      deps := List.concat [ element_deps |> List.concat; rest_deps; !deps; ];

      T.Pattern.Array { elements; rest }
    )

  in
  { T.Pattern. spec; loc; pat_id }, List.rev !deps

(* only collect deps, construct value in type check *)
and annotate_type env ty : (TypeExpr.t * int list) =
  let open Ast.Type in
  let { spec; loc } = ty in
  let deps = ref [] in
  let scope = Env.peek_scope env in
  match spec with
  | Ty_any -> TypeExpr.Any, []
  | Ty_ctor({ pident_name = "string"; _ }, []) -> TypeExpr.String, []
  | Ty_ctor({ pident_name = "any"; _ }, []) -> TypeExpr.Any, []
  | Ty_ctor(ctor, params) -> (
    let { Identifier. pident_name; pident_loc } = ctor in

    let params, params_deps = List.map ~f:(annotate_type env) params |> List.unzip in
    deps := List.concat (!deps::params_deps);

    if scope#is_generic_type_symbol pident_name then (
      if not (List.is_empty params) then (
        let err = Type_error.(make_error (Env.ctx env) loc (IsNotGeneric pident_name)) in
        raise (Type_error.Error err)
      );
      TypeExpr.TypeSymbol pident_name, !deps
    ) else (
      let ty_var_opt = (Env.peek_scope env)#find_type_symbol pident_name in
      match ty_var_opt with
      | Some ty_var -> (
        (* TODO: find ctor in the scope *)
        TypeExpr.Ctor (Ref ty_var, params), ty_var::!deps
      )

      | None -> (
        (Env.peek_scope env)#print_type_symbols;
        let ctx = Env.ctx env in
        let err_spec = Type_error.CannotFindName pident_name in
        let err = Type_error.make_error ctx pident_loc err_spec in
        raise (Type_error.Error err)
      )
    )

  )
  | Ty_array target -> (
    let target_type, target_type_deps = annotate_type env target in
    TypeExpr.Array target_type, target_type_deps
  )

  | Ty_arrow (params, result) -> (
    let _params, params_type, params_deps = annotate_function_params env params in
    let return_type, return_type_deps = annotate_type env result in
    deps := List.concat [ !deps; return_type_deps; params_deps ];
    TypeExpr.Lambda(params_type, return_type), !deps
  )

and annotate_function_params env params : T.Function.params * TypeExpr.params * int list = 
  let open Ast.Function in
  let annoate_param param =
    let { param_name; param_ty; param_loc; param_rest } = param in
    let param_name = annotate_function_param env param_name in
    let _, param_id = param_name in
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
    } in
    Type_context.update_node (Env.ctx env) param_id node;
    let param_name', _ = param_name in
    { T.Function.
      param_name;
      param_ty = param_id;
      param_loc;
      param_rest;
    }, (param_name', !value), param_id
  in

  let params_rest = ref None in

  let { params_content; params_loc } = params in
  let params_len = List.length params_content in
  let params, params_content, params_deps =
    params_content
    |> List.mapi ~f:(fun index param ->
      if param.param_rest && (index <> params_len - 1) then (
        let err = Type_error.(make_error (Env.ctx env) param.param_loc RestParamsMustAtLast) in
        raise Type_error.(Error err)
      );

      let t, type_tuple, dep_id = annoate_param param in
      if param.param_rest then (
        params_rest := Some type_tuple;
        t, None, dep_id
      ) else
        t, Some type_tuple, dep_id
    )
    |> List.unzip3
  in

  let param_type = { Core_type.TypeExpr.
    params_content = List.filter_opt params_content;
    params_rest = !params_rest;
  } in

  { T.Function. params_content = params; params_loc }, param_type, params_deps

and annotate_function env fun_ =
  let open Ast.Function in

  let prev_scope = Env.peek_scope env in
  let fun_scope = new function_scope ~prev:prev_scope () in

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
    let return_ty, return_ty_deps =
      match header.return_ty with
      | Some type_expr ->
        annotate_type env type_expr
      | None ->
        let unit_type = Env.ty_unit env in
        TypeExpr.Ctor(Ref unit_type, []), []
    in

    fun_deps := List.append !fun_deps return_ty_deps;

    let params, params_type, params_deps = annotate_function_params env header.params in

    let body = annotate_block_impl ~prev_deps:params_deps env body in

    (* defined return *)
    fun_deps := body.return_ty::(!fun_deps);

    let type_def = { Core_type.TypeDef.
      id = fun_id;
      builtin = false;
      name = fun_.header.id.pident_name;
      spec = Function {
        fun_params = params_type;
        fun_return = return_ty;
      };
    } in
    Type_context.update_node (Env.ctx env) fun_id {
      name_node with
      value = TypeExpr.TypeDef type_def;
      (* deps = return_id::params_types; *)
      deps = !fun_deps;
    };
    { T.Function.
      header = {
        name = (header.id.pident_name, fun_id);
        name_loc = header.id.pident_loc;
        params;
      };
      scope = fun_scope;
      body;
      ty_var = fun_id;
      comments;
    }
  )

and annotate_enum env enum =
  let open Ast.Enum in
  let { visibility; name; loc; cases; type_vars } = enum in
  let ctx = Env.ctx env in
  let scope = Env.peek_scope env in
  let variable = Option.value_exn (scope#find_var_symbol name.pident_name) in

  let scope = new scope ~prev:scope () in

  let type_vars_names =
    List.map
      ~f:(fun ident ->
        scope#insert_generic_type_symbol ident.pident_name;

        ident.pident_name
      )
      type_vars
  in

  Env.with_new_scope env scope (fun env ->
    let enum_members = ref [] in
    let annotate_case index _case =
      let { case_name; case_fields; case_loc } = _case in
      let member_var = Option.value_exn (scope#find_var_symbol case_name.pident_name) in

      let fields_types, deps = List.map ~f:(annotate_type env) case_fields |> List.unzip in

      let first_char = String.get case_name.pident_name 0 in
      if not (Char.is_uppercase first_char) then (
        let err = Type_error.(make_error ctx case_name.pident_loc (CapitalizedEnumMemeber case_name.pident_name)) in
        raise (Type_error.Error err)
      );

      let ctor_ty_def = {
        TypeDef.
        enum_ctor_tag_id = index;
        enum_ctor_name = case_name.pident_name;
        enum_ctor_super_id = variable.var_id;
        enum_ctor_params = fields_types;
      } in

      let ty_def = { TypeDef.
        id = member_var.var_id;
        builtin = false;
        name = case_name.pident_name;
        spec = EnumCtor ctor_ty_def;
      } in

      enum_members := (case_name.pident_name, ty_def)::(!enum_members);

      Type_context.map_node
        ctx
        ~f:(fun _ -> {
          deps = List.concat ([variable.var_id]::deps);
          loc = case_loc;
          value = TypeExpr.TypeDef ty_def;
        })
        member_var.var_id
      ;

      { T.Enum.
        case_name = (case_name.pident_name, member_var.var_id);
        case_fields = fields_types;
        case_loc;
      }, member_var.var_id 
    in

    let cases, _cases_deps = List.mapi ~f:annotate_case cases |> List.unzip in

    let enum_params =
      List.map
      ~f:(fun id -> Identifier.(id.pident_name))
      type_vars
    in

    let ty_def = {
      TypeDef.
      enum_members = List.rev !enum_members;
      enum_params;
    } in

    Type_context.map_node
      ctx
      ~f:(fun _ ->
        {
          (* deps = cases_deps; *)
          deps = [];
          loc;
          value = (TypeExpr.TypeDef {
            id = variable.var_id;
            builtin = false;
            name = name.pident_name;
            spec = Enum ty_def;
          });
        }
      )
      variable.var_id;

    { T.Enum.
      visibility;
      name = (name.pident_name, variable.var_id);
      type_vars = type_vars_names;
      cases;
      loc;
    }
  )

and annotate_interface env intf: T.Declaration.intf =
  let { Ast.Declaration. intf_visibility; intf_name; intf_type_vars; intf_methods; _ } = intf in
  let type_visibility = annotate_visibility intf_visibility in
  let scope = Env.peek_scope env in

  let intf_id_opt = scope#find_var_symbol intf_name.pident_name in
  if Option.is_none intf_id_opt then (
    failwith (Format.sprintf "unexpected: interface %s is not added in parsing stage" intf_name.pident_name)
  );
  let intf_id = (Option.value_exn intf_id_opt).var_id in
  let intf_name_tuple = intf_name.pident_name, intf_id in
  let deps = ref [] in
  let intf_method_tuples = ref [] in

  let annotate_method (_method: Ast.Declaration.intf_method) =
    let { Ast.Declaration. intf_method_name; intf_method_loc; intf_method_params; intf_method_return_ty; _ } = _method in
    let intf_method_params, type_params, params_deps = annotate_function_params env intf_method_params in
    let intf_method_return_ty, ret_deps =
      match intf_method_return_ty with
      | Some t ->
        let t, deps = annotate_type env t in
        t, deps
      | None -> (
        let none_type = Env.ty_unit env in
        TypeExpr.Ctor(Ref none_type, []), []
      )
    in

    let node_id = Type_context.size (Env.ctx env) in

    let cls_method = { Core_type.TypeDef.
      method_cls_id = intf_id;
      method_get_set = None;
      method_is_virtual = true;
      method_params = type_params;
      method_return = intf_method_return_ty;
    } in

    let typedef = { Core_type.TypeDef.
      id = node_id;
      builtin = false;
      name = intf_method_name.pident_name;
      spec = ClassMethod cls_method;
    } in

    intf_method_tuples :=
      (intf_method_name.pident_name, Core_type.TypeDef.Cls_elm_method(type_visibility, typedef))::(!intf_method_tuples);

    let node = { Core_type.
      value = TypeExpr.TypeDef typedef;
      loc = intf_method_loc;
      deps = List.append params_deps  ret_deps;
    } in

    let node_id = Type_context.new_id (Env.ctx env) node in
    deps := node_id::!deps;

    { T.Declaration.
      intf_method_name = intf_method_name.pident_name, node_id;
      intf_method_params;
      intf_method_loc;
    }
  in

  let intf_methods = List.map ~f:annotate_method intf_methods in

  let typedef = { Core_type.TypeDef.
    id = intf_id;
    builtin = false;
    name = intf_name.pident_name;
    spec = Interface {
      intf_methods = List.rev !intf_method_tuples;
    };
  } in

  Type_context.map_node
    (Env.ctx env)
    ~f:(fun _ -> {
      deps = List.rev !deps;
      loc = intf_name.pident_loc;
      value = TypeExpr.TypeDef typedef;
    })
    intf_id;

  { T.Declaration.
    intf_visibility;
    intf_name = intf_name_tuple;
    intf_type_vars;
    intf_methods;
  }

let annotate_program env (program: Ast.program) =
  let { Ast. pprogram_declarations; pprogram_top_level = _; pprogram_loc; _; } = program in

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

        | Interface intf -> (
          let _, ty_var = intf.intf_name in
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

        | Enum enum -> (
          let open T.Enum in
          let { name = (_, ty_var); _ } = enum in
          ty_var::acc
        )

        | Import _ -> acc
        
      )
      tprogram_declarations
  in

  let val_ =
    {
      value = TypeExpr.Unknown;
      loc = pprogram_loc;
      deps;
    }
  in
  let ty_var = Type_context.new_id (Env.ctx env) val_ in
  let tree = { T.
    tprogram_declarations;
    tprogram_scope = Env.file_scope env;
    ty_var
  } in
  tree
