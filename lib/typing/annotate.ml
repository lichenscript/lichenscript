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

      let id = Type_env.new_id node in
      [id], (T.Statement.Semi expr)
    )

    | While _
    | Binding _
    | Block _
    | Break _
    | Continue _
    | Debugger
    | Return _
    | Empty -> prev_deps, failwith "not implment"
  in
  deps, { T.Statement. spec; loc; attributes }

and annotate_expression ~prev_deps env expr : T.Expression.t =
  let open Ast.Expression in
  let { spec; loc; attributes; } = expr in
  let ty_var, spec = 
    match spec with
    | Constant cnst -> (
      let open Ast.Literal in
      let ty_var =
        match cnst with
        | Integer _ ->
          Type_env.new_id {
            value = TypeValue.Ctor("i32", []);
            loc;
            deps = [];
            check = none;
          }

        | Char _ ->
          Type_env.new_id {
            value = TypeValue.Ctor("char", []);
            loc;
            deps = [];
            check = none;
          }

        (* 'c' *)
        | String _ ->
          Type_env.new_id {
            value = TypeValue.Ctor("string", []);
            loc;
            deps = [];
            check = none;
          }

        | Float _ ->
          Type_env.new_id {
            value = TypeValue.Ctor("f32", []);
            loc;
            deps = [];
            check = none;
          }

        | Boolean _ -> 
          Type_env.new_id {
            value = TypeValue.Ctor("boolean", []);
            loc;
            deps = [];
            check = none;
          }

      in

      ty_var, (T.Expression.Constant cnst)
    )

    | Identifier id -> (
      let name = id.pident_name in
      let ty_var = Type_env.new_id {
        value = TypeValue.Ctor(name, []);
        loc;
        deps = [];
        check = none;
      } in
      ty_var, (T.Expression.Identifier ty_var)
    )

    | Lambda _
    | If _
    | Array _ 
    | Call _ ->

    (* | Call call -> (
      let { callee; call_params; call_loc; } = call in

      let callee = annotate_expression ~prev_deps env callee in
      let call_params = List.map ~f:(annotate_expression ~prev_deps env) call_params in

      let ty_var = Type_env.new_id {
        value = TypeValue.Unknown;
        loc;
        deps = [ T.Expression.(callee.ty_var) ];
        check = none;
      } in

      ty_var, (T.Expression.Call { callee; call_params; call_loc })
    ) *)
      0, failwith "not implemented"

    | Member _
    | Unary _
    | Binary _
    | Update _
    | Assign _ -> 0, failwith "not implemented"
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

and annotate_block ~prev_deps env block =
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
  let return_ty = Type_env.new_id node in
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
    | Declare _ -> failwith "not implementation"
    | Enum enum -> T.Declaration.Enum enum

  in
  { T.Declaration. spec; loc; attributes }

and annotate_class _env _cls =
  failwith "not implement"

and annoate_pattern _env pat =
  let open Ast.Pattern in
  let { spec; loc } = pat in
  let id, spec =
    match spec with
    | Identifier _ident -> (
      let node = {
        Core_type.
        loc;
        value = TypeValue.Unknown;
        check = none;
        deps = [];
      } in
      let id = Type_env.new_id node in
      id, (T.Pattern.Symbol id)
    )
  in
  { T.Pattern. spec; loc }, id

and annotate_type env ty =
  let open Ast.Type in
  let { spec; loc } = ty in
  let deps = ref [] in
  let value =
    match spec with
    | Ty_any -> TypeValue.Any
    | Ty_ctor(ctor, params) -> (
      let { Identifier. pident_name; _ } = ctor in
      (* TODO: find ctor in the scope *)
      let params = List.map ~f:(annotate_type env) params in
      deps := List.concat [ !deps; params ];
      TypeValue.Ctor (pident_name, params)
    )
    | Ty_arrow (params, result) -> (
      let params_types = List.map ~f:(annotate_type env) params in
      let return_type = annotate_type env result in
      deps := List.concat [ !deps; params_types; [ return_type ] ];
      let ctor_type = Type_env.new_id {
        deps = params_types;
        value = TypeValue.Unknown;
        loc;
        check = none;
      } in
      TypeValue.Function(ctor_type, return_type)
    )
  in
  let node = {
    loc;
    value;
    deps = !deps;
    check = none;
  } in
  Type_env.new_id node

and annotate_function env fun_ =
  let open Ast.Function in

  let annoate_param param =
    let { param_pat; param_ty; param_init = _init; param_loc; param_rest } = param in
    let param_pat, param_ty_id = annoate_pattern env param_pat in
    let deps = ref [ param_ty_id ] in
    Option.iter
      ~f:(fun ty ->
        let param_ty = annotate_type env ty in
        deps := param_ty::!deps
      )
      param_ty
    ;
    let node = {
      loc = param_loc;
      value = TypeValue.Unknown;
      deps = !deps;
      check = none;  (* check init *)
    } in
    let node_id = Type_env.new_id node in
    { T.Function.
      param_pat;
      param_ty = param_ty_id;
      param_init = None;  (* TODO *)
      param_loc;
      param_rest;
    }, node_id
  in

  let annoate_params params = 
    let { params_content; params_loc } = params in
    let params, params_types = List.map ~f:annoate_param params_content |> List.unzip in
    let node = {
      loc = params_loc;
      value = TypeValue.Unknown;
      deps = params_types;
      check = none;  (* check init *)
    } in
    params, (Type_env.new_id node)
  in

  let { visibility = _visibility; header; body; loc; comments; } = fun_ in

  let name_node = {
    loc = header.id.pident_loc;
    value = TypeValue.Unknown;
    deps = [];
    check = none;
  } in
  let name_id = Type_env.new_id name_node in

  let params, params_type = annoate_params header.params in

  let body = annotate_block ~prev_deps:[params_type] env body in

  let return_node = {
    loc;
    value = TypeValue.Unknown;
    deps = [body.return_ty];
    check = none;
  } in
  let return_id = Type_env.new_id return_node in

  Type_env.update_node name_id {
    name_node with
    value = TypeValue.Function (params_type, return_id);
    deps = [ params_type; return_id ];
    check = none;
  };
  { T.Function.
    header = {
      id = params_type;
      params = {
        params_content = params;
        params_loc = header.params.params_loc;
      };
    };
    body;
    ty_var = name_id;
    comments;
  }

let annotate_program env (program: Ast.program) =
  let { Ast. pprogram_declarations; pprogram_loc; _; } = program in
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
          let { declare_ty_var; _ } = declare in
          declare_ty_var::acc
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
      check = (fun () -> ());
    }
  in
  let ty_var = Type_env.new_id val_ in
  let tree = { T.
    tprogram_declarations;
    tprogram_scope = Env.root_scope env;
    ty_var
  } in
  tree
