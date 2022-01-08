(*
 * 1. annotate parsed tree to typed tree
 * 2. resolve top-level variables internally
 * 
 * deeper variables resolution remains to type-check phase
 *)
open Core_kernel
open Core_type
open Waterlang_parsing

module T = Typedtree

let rec annotate_statement ~(prev_deps: int list) env (stmt: Ast.Statement.t) =
  let open Ast.Statement in
  let { spec; loc; attributes; _ } = stmt in
  let deps, spec =
    match spec with
    | Expr expr -> (
      let expr = annotate_expression ~prev_deps env expr in 
      let ty_var = T.Expression.(expr.ty_var) in

      [ty_var], (T.Statement.Expr expr)
    )

    | Semi expr -> (
      let expr = annotate_expression ~prev_deps env expr in 
      let ty_var = T.Expression.(expr.ty_var) in

      let node = {
        value = TypeValue.Unit;
        loc;
        deps = [ty_var];
        check = none;  (* TODO: check expr is empty *)
      } in

      let id = Type_context.new_id (Env.ctx env) node in
      [id], (T.Statement.Semi expr)
    )

    | While _ -> prev_deps, failwith "not implment"

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
      Scope.insert_var_symbol scope name sym_id;

      let binding_init = annotate_expression ~prev_deps env binding_init in

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
          Option.value_exn (Scope.find_type_symbol root_scope "i32")

        | Char _ ->
          Option.value_exn (Scope.find_type_symbol root_scope "char")

        (* 'c' *)
        | String _ ->
          Option.value_exn (Scope.find_type_symbol root_scope "string")

        | Float _ ->
          Option.value_exn (Scope.find_type_symbol root_scope "f32")

        | Boolean _ -> 
          Option.value_exn (Scope.find_type_symbol root_scope "boolean")

      in

      ty_var, (T.Expression.Constant cnst)
    )

    | Identifier id -> (
      let ty_var_opt = Scope.find_var_symbol (Env.peek_scope env) id.pident_name in
      match ty_var_opt with
      | Some ty_var ->
        ty_var, (T.Expression.Identifier ty_var)
      | _ ->
        let err_spec = Type_error.CannotFindName id.pident_name in
        let err = Type_error.make_error id.pident_loc err_spec in
        raise (Type_error.Error err)
    )

    | Lambda _
    | If _
    | Array _  ->
      -1, failwith "not implemented"

    | Call call -> (
      let rec cast_expressions_into_callee acc expr : T.Expression.callee =
        let { Ast.Expression. spec; loc; _ } = expr in
        match spec with
        | Identifier id -> (
          let name = id.pident_name in
          let var_opt = Scope.find_var_symbol (Env.peek_scope env) name in
          match var_opt with
          | Some id -> (
            { T.Expression.
              callee_spec = ((name, id), List.rev acc);
              callee_loc = loc;
              callee_ty_var = id;
            }
          )

          | None -> (
            let err_spec = Type_error.CannotFindName id.pident_name in
            let err = Type_error.make_error id.pident_loc err_spec in
            raise (Type_error.Error err)
          )

        )

        | Member (expr, name) -> (
          let name = name.pident_name in
          cast_expressions_into_callee ((`Property name)::acc) expr
        )

        | _ -> failwith "unrechable"

      in

      let { callee; call_params; call_loc } = call in

      let callee = cast_expressions_into_callee [] callee in
      let call_params = List.map ~f:(annotate_expression ~prev_deps env) call_params in

      let ty_var = Type_context.new_id (Env.ctx env) {
        value = TypeValue.Unknown;
        loc;
        deps = [ T.Expression.(callee.callee_ty_var) ];
        check = none;
      } in

      ty_var, (T.Expression.Call { callee; call_params; call_loc })
    )

    | Member _
    | Unary _ -> -1, failwith "not implemented"

    | Binary (op, left, right) -> (
      let left = annotate_expression ~prev_deps env left in
      let right = annotate_expression ~prev_deps env right in
      let open T.Expression in
      let node = {
        value = TypeValue.Unknown;
        deps = [left.ty_var; right.ty_var];
        loc;
        check = (fun id ->
          let ctx = Env.ctx env in
          let left_def_opt = Check_helper.find_to_typedef ctx left.ty_var in
          let right_def_opt = Check_helper.find_to_typedef ctx right.ty_var in
          Format.printf "bin: %d, left: %d, right: %d\n" id left.ty_var right.ty_var;
          match (left_def_opt, right_def_opt) with
          | (Some (left_sym, left_id), Some (right_sym, _)) -> (
            if not (Check_helper.type_addable left_sym right_sym) then (
              let err = Type_error.(make_error loc (NotAddable (left_sym, right_sym))) in
              raise (Type_error.Error err)
            );
            Type_context.update_node_type ctx id (TypeValue.Ctor(left_id, []))
          )
          | _ -> (
            let err = Type_error.(make_error loc CannotResolveTypeOfExpression) in
            raise (Type_error.Error err)
          )
        );
      } in
      let id = Type_context.new_id (Env.ctx env) node in

      id, (T.Expression.Binary(op, left, right))
    )

    | Update _
    | Assign _ -> -1, failwith "not implemented"
    | Block block -> (
      let block = annotate_block ~prev_deps env block in
      T.Block.(block.return_ty), (T.Expression.Block block)
    )
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
    value = TypeValue.Unknown;
    deps = body_dep;
    loc;
    check = none;
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
  let spec =
    match spec with
    | Class _class -> (
      T.Declaration.Class (annotate_class env decl)
    )

    | Function_ _fun -> T.Declaration.Function_ (annotate_function env _fun)

    | Declare declare -> (
      let { decl_spec; decl_loc } = declare in
      match decl_spec with
      | DeclFunction declare_fun -> (
        let { Ast.Function. id; params; _ } = declare_fun in

        let params, params_types = annoate_function_params env params in

        let node = {
          value = TypeValue.Unknown;
          deps = params_types;
          loc = decl_loc;
          check = none;
        } in

        let ty_id = Type_context.new_id (Env.ctx env) node in

        let header = {
          T.Function.
          id = ty_id;
          name = Identifier.(id.pident_name);
          params;
        } in

        T.Declaration.Declare {
          T.Declaration.
          decl_ty_var = ty_id;
          decl_spec = T.Declaration.DeclFunction header;
          decl_loc;
        }
      )
    )

    | Enum enum -> T.Declaration.Enum enum

  in
  { T.Declaration. spec; loc; attributes }

and annotate_class _env _cls =
  failwith "not implement"

and annotate_pattern env pat =
  let open Ast.Pattern in
  let { spec; loc } = pat in
  let id, spec =
    match spec with
    | Identifier ident -> (
      let node = {
        Core_type.
        loc;
        value = TypeValue.Unknown;
        check = none;
        deps = [];
      } in
      let id = Type_context.new_id (Env.ctx env) node in
      id, (T.Pattern.Symbol (ident.pident_name, id))
    )
  in
  { T.Pattern. spec; loc }, id

(* only collect deps, construct value in type check *)
and annotate_type env ty : (TypeValue.t * int list) =
  let open Ast.Type in
  let { spec; _ } = ty in
  let deps = ref [] in
  match spec with
  | Ty_any -> TypeValue.Any, []
  | Ty_ctor(ctor, params) -> (
    let { Identifier. pident_name; pident_loc } = ctor in
    let ty_var_opt = Scope.find_type_symbol (Env.peek_scope env) pident_name in
    match ty_var_opt with
    | Some ty_var -> (
      (* TODO: find ctor in the scope *)
      let params, params_deps = List.map ~f:(annotate_type env) params |> List.unzip in
      deps := List.concat (!deps::params_deps);
      TypeValue.Ctor (ty_var, params), [ty_var]
    )

    | None -> (
      let err_spec = Type_error.CannotFindName pident_name in
      let err = Type_error.make_error pident_loc err_spec in
      raise (Type_error.Error err)
    )

  )
  | Ty_arrow (params, result) -> (
    let params, params_types_deps = List.map ~f:(annotate_type env) params |> List.unzip in
    let return_type, return_type_deps = annotate_type env result in
    deps := List.concat ((!deps)::return_type_deps::params_types_deps);
    TypeValue.Function(params, return_type), !deps
  )
  (* !deps *)

and annoate_function_params env params = 
  let open Ast.Function in
  let annoate_param param =
    let { param_pat; param_ty; param_init = _init; param_loc; param_rest } = param in
    let param_pat, param_id = annotate_pattern env param_pat in
    let deps = ref [] in
    let value = ref TypeValue.Unknown in
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
    Format.printf "param %d: %s\n" param_id (Format.asprintf "%a" TypeValue.pp !value);
    { T.Function.
      param_pat;
      param_ty = param_id;
      param_init = None;  (* TODO *)
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
  let fun_scope = Scope.create ~prev:prev_scope () in

  Env.with_new_scope env fun_scope (fun env ->
    let { visibility = _visibility; header; body; loc; comments; } = fun_ in

    let fun_id_opt = Scope.find_var_symbol prev_scope header.id.pident_name in
    if Option.is_none fun_id_opt then (
      failwith (Format.sprintf "unexpected: function id %s is not added in parsing stage" header.id.pident_name)
    );
    let fun_id = Option.value_exn fun_id_opt in
    let name_node = Type_context.get_node (Env.ctx env) fun_id in

    let name_node = {
      name_node with
      loc;
    } in
    Type_context.update_node (Env.ctx env) fun_id name_node;

    let params, params_types = annoate_function_params env header.params in

    (* add all params into scope *)
    List.iter
      ~f:(fun param -> 
        let pat = param.param_pat in
        let name = 
          match pat.spec with
          | T.Pattern.Symbol (name, _) -> name
        in
        Scope.insert_var_symbol fun_scope name param.param_ty;
      )
      params.params_content;

    let body = annotate_block ~prev_deps:params_types env body in

    let return_node = {
      loc;
      value = TypeValue.Unknown;
      deps = [body.return_ty];
      check = none;
    } in
    let return_id = Type_context.new_id (Env.ctx env) return_node in

    Type_context.update_node (Env.ctx env) fun_id {
      name_node with
      value = TypeValue.Unknown;  (* it's an typedef *)
      deps = return_id::params_types;
      check = none;
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
  let { Ast. pprogram_declarations; pprogram_top_level; pprogram_loc; _; } = program in

  Hashtbl.iter_keys
    ~f:(fun key ->
      let node = {
        value = TypeValue.Unknown;
        loc = Waterlang_lex.Loc.none;
        deps = [];
        check = none;
      } in
      let new_id = Type_context.new_id (Env.ctx env) node in
      Scope.insert_var_symbol (Env.peek_scope env) key new_id
    )
    pprogram_top_level.names
    ;

  let tprogram_declarations = List.map ~f:(annotate_declaration env) pprogram_declarations in

  let deps =
    List.fold
      ~init:[]
      ~f:(fun acc decl -> 
        let open T.Declaration in
        let { spec; _ } = decl in
        match spec with
        | Class cls -> (
          let { cls_id; _} = cls in
          cls_id::acc
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

        | Enum _ -> failwith "not implement"
        
      )
      tprogram_declarations
  in

  let val_ =
    {
      value = TypeValue.Unknown;
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
