(*
 * Convert TypedTree to C-op
 *)
open Core_kernel
open Lichenscript_parsing.Ast
open Lichenscript_typing
open Lichenscript_typing.Typedtree

type t = {
  ctx: Type_context.t;
  name_map: (int, string) Hashtbl.t;
  mutable tmp_vars_count: int;
  mutable main_function_name: string option
}

type expr_result = {
  prepend_stmts: C_op.Stmt.t list;
  expr: C_op.Expr.t;
  append_stmts: C_op.Stmt.t list;
}

let create ctx = {
  ctx;
  name_map = Hashtbl.create (module Int);
  tmp_vars_count = 0;
  main_function_name = None;
}

let preserved_name = [| "ret"; "rt"; "this"; "argc"; "argv"; "t" |]

let get_local_var_name realname ty_int =
  if Array.mem preserved_name ~equal:String.equal realname then
    realname ^ "_" ^ (Int.to_string ty_int)
  else
    realname

let rec transform_declaration env decl  =
  let open Declaration in
  let { spec; loc; _ } = decl in
  let specs = match spec with
  | Class cls -> transform_class env cls
  | Function_ _fun -> transform_function env _fun
  | Enum enum -> transform_enum env enum
  | Declare _
  | Import _ -> []
in
List.map
  ~f:(fun spec -> { C_op.Decl. spec; loc })
  specs

(*
 * distribute variable for params
 *)
and transform_function env _fun =
  let open Function in
  let { body; comments; header; _ } = _fun in

  env.tmp_vars_count <- 0;

  let fun_name = "LCC_" ^ header.name in
  Hashtbl.set env.name_map ~key:header.id ~data:fun_name;

  if String.equal header.name "main" then (
    env.main_function_name <- Some fun_name
  );

  List.iter
    ~f:(fun { param_pat; param_ty; _ } ->
      let param_name =
        match param_pat.spec with
        | Pattern.Symbol (name, _) -> name
        | _ -> failwith "not implemented"
      in
      Hashtbl.set env.name_map ~key:param_ty ~data:(get_local_var_name param_name param_ty)
    )
    header.params.params_content;

  let stmts = List.map ~f:(transform_statement env) body.body |> List.concat in
  let new_body = {
    C_op.Block.
    body = stmts;
    loc = body.loc;
  } in

  let t_fun = {
    C_op.Func.
    name = fun_name;
    body = new_body;
    tmp_vars_count = env.tmp_vars_count;
    comments;
  } in

  [
    C_op.Decl.Func t_fun;
  ]

and transform_statement env stmt =
  let open Statement in
  let { spec; loc; _ } = stmt in
  let transform_return_expr expr =
    let tmp = transform_expression env expr in
    let assign = {
      C_op.Expr.
      spec = Assign("ret", tmp.expr);
      loc;
    } in
    let expr = {
      C_op.Stmt.
      spec = Expr assign;
      loc;
    } in
    List.concat [ tmp.prepend_stmts; [ expr ]; tmp.append_stmts ]
  in
  match spec with
  | Expr expr -> transform_return_expr expr

  | Semi expr -> (
    let tmp = transform_expression env expr in
    let expr = {
      C_op.Stmt.
      spec = Expr tmp.expr;
      loc;
    } in
    List.concat [ tmp.prepend_stmts; [ expr ]; tmp.append_stmts ]
  )
  | While _
  | Binding _
  | Block _
  | Break _ -> [
    { C_op.Stmt.
      spec = Break;
      loc;
    }
  ]
  | Continue _ -> [
    { C_op.Stmt.
      spec = Continue;
      loc;
    }
  ]
  | Debugger -> []

  | Return ret_opt ->
    Option.map ~f:transform_return_expr ret_opt
    |> Option.value ~default:[]

  | Empty -> []

and gen_release_temp id =
  { C_op.Stmt.
    spec = Release {
      C_op.Expr.
      spec = Temp id;
      loc = Loc.none;
    };
    loc = Loc.none;
  }

and transform_expression env expr =
  let open Expression in
  let { spec; loc; _ } = expr in
  let prepend_stmts = ref [] in
  let append_stmts = ref [] in
  let expr_spec =
    match spec with
    | Constant literal -> (
      let open Literal in
      match literal with
      | String(content, _, _) -> (
        let tmp_id = env.tmp_vars_count in
        env.tmp_vars_count <- env.tmp_vars_count + 1;

        let assign_expr = {
          C_op.Expr.
          spec = Assign(("t[" ^ (Int.to_string tmp_id) ^ "]"), {
            spec = NewString content;
            loc = Loc.none;
          });
          loc = Loc.none;
        } in

        let prepend_stmt = {
          C_op.Stmt.
          spec = Expr assign_expr;
          loc = Loc.none;
        } in

        prepend_stmts := prepend_stmt::!prepend_stmts;

        let result = C_op.Expr.Temp tmp_id in

        append_stmts := (gen_release_temp tmp_id)::!append_stmts;

        result
      )

      | Integer(content, _) ->
        C_op.Expr.NewInt content

      | _ ->
        failwith "unimplemented"
    )

    | Identifier (_, id) -> (
      let name = Hashtbl.find_exn env.name_map id in
      C_op.Expr.Ident name
    )

    | Lambda
    | If _
    | Array _ -> failwith "n1"

    | Call call -> (
      let open Expression in
      let { callee; call_params; _ } = call in
      match callee with
      | { spec = Identifier (_, id); _ } -> (
        match Type_context.find_external_symbol env.ctx id with
        | Some ext_name -> (
          let params_struct = List.map ~f:(transform_expression env) call_params in

          let prepend, params, append = List.map ~f:(fun expr -> expr.prepend_stmts, expr.expr, expr.append_stmts) params_struct |> List.unzip3 in

          prepend_stmts := List.append !prepend_stmts (List.concat prepend);
          append_stmts := List.append !append_stmts (List.concat append);

          (* external method *)
          C_op.Expr.ExternalCall(ext_name, params)
        )

        | None ->
          failwith "not impelmented"

      )

      | _ -> failwith "not impement"
    )

    | Member _
    | Unary _

    | Binary _
    | Update _
    | Assign _
    | Block _
    | Init _
    | Match _ -> failwith "n"
  in
  {
    prepend_stmts = List.rev !prepend_stmts;
    expr = {
      spec = expr_spec;
      loc;
    };
    append_stmts = List.rev !append_stmts;
  }

and transform_class _env _cls =
  []

and transform_enum _env _enum =
  []


type result = {
  main_function_name: string option;
  declarations: C_op.Decl.t list;
}

let transform_declarations ctx declarations =
  let env = create ctx in
  {
    main_function_name = env.main_function_name;
    declarations =
      List.map ~f:(transform_declaration env) declarations
      |> List.concat;
  }
