open Core_kernel
open Typedtree
open Waterlang_parsing

let rec annotate_statement (env: Env.t) stmt =
  let { Ast. pstmt_loc; pstmt_desc; _; } = stmt in
  let tstmt_desc =
    match pstmt_desc with
    | Pstmt_class _ -> Tstmt_class

    | Pstmt_expr expr ->
      Tstmt_expr (annotate_expression env expr)

    | Pstmt_semi expr ->
      Tstmt_semi (annotate_expression env expr)

    | Pstmt_function _ -> Tstmt_function
    | Pstmt_while {Ast. pwhile_test; pwhile_block; pwhile_loc; } ->
      let twhile_test = annotate_expression env pwhile_test in
      let twhile_block = annotate_block env pwhile_block in
      Tstmt_while {
        twhile_test;
        twhile_block;
        twhile_loc = pwhile_loc;
      }

    | Pstmt_binding binding ->
      Tstmt_binding (annotate_binding env binding)

    | Pstmt_block block ->
      Tstmt_block (annotate_block env block)

    | Pstmt_break id -> Tstmt_break id
    | Pstmt_contintue id -> Tstmt_continue id
    | Pstmt_debugger -> Tstmt_debugger
    | Pstmt_return expr_opt ->
      Tstmt_return (Option.map ~f:(annotate_expression env) expr_opt)

    | Pstmt_empty -> Tstmt_empty

  in

  {
    tstmt_desc;
    tstmt_loc = pstmt_loc;
  }

and annotate_block env block =
  let { Ast. pblk_body; pblk_loc } = block in
  let tblk_body = List.map ~f:(annotate_statement env) pblk_body in
  {
    tblk_body;
    tblk_loc = pblk_loc;
  }

and annotate_binding env binding =
  let { Ast. pbinding_kind; pbinding_loc; pbinding_ty; pbinding_pat; pbinding_init } = binding in
  let tbinding_init = annotate_expression env pbinding_init in
  let tbinding_ty = Option.map ~f:(annotate_type env) pbinding_ty in
  let tbinding_pat = annotate_pattern env pbinding_pat in
  {
    tbinding_kind = pbinding_kind;
    tbinding_loc = pbinding_loc;
    tbinding_ty;
    tbinding_pat;
    tbinding_init;
  }

and annotate_pattern env pat =
  let {Ast. ppat_desc; ppat_loc } = pat in
  let tpat_desc =
    match ppat_desc with
    | Ast.Ppat_identifier id ->
      Tpat_symbol (Env.find_or_create_var_symbol env id.pident_name)
  in
  {
    tpat_desc;
    tpat_loc = ppat_loc;
  }

and annotate_type env ty =
  let {Ast. pty_desc; pty_loc } = ty in
  let tty_desc =
    match pty_desc with
    | Ast.Pty_any -> Tty_any
    | Ast.Pty_var str -> Tty_var str
    | Ast.Pty_ctor (id, types) ->
      let sym = Env.find_or_create_type_symbol env id.pident_name in
      let types = List.map ~f:(annotate_type env) types in
      Tty_ctor (sym, types)
    | Ast.Pty_arrow (params, ret) ->
      let t_params = List.map ~f:(annotate_type env) params in
      let t_ret = annotate_type env ret in
      Tty_arrow (t_params, t_ret)

  in
  {
    tty_desc;
    tty_loc = pty_loc;
  }

and annotate_expression (env: Env.t) expr =
  let { Ast. pexp_desc; pexp_loc; _; } = expr in
  let texp_desc =
    match pexp_desc with
    | Pexp_constant cnst -> Texp_constant cnst
    | Pexp_identifier id ->
      let sym = Env.find_or_create_var_symbol env id.pident_name in
      Texp_identifier sym

    | Pexp_throw expr ->
      Texp_throw (annotate_expression env expr)

    | Pexp_lambda _ -> Texp_lambda

    | Pexp_if { Ast. pif_test; pif_consequent; pif_alternative; pif_loc; } ->
      let tif_test = annotate_expression env pif_test in
      let tif_consequent = annotate_statement env pif_consequent in
      let tif_alternative = Option.map ~f:(annotate_statement env) pif_alternative in
      Texp_if {
        tif_test;
        tif_consequent;
        tif_alternative;
        tif_loc = pif_loc;
      }

    | Pexp_array arr ->
      Texp_array (List.map ~f:(annotate_expression env) arr)

  in
  {
    texp_desc;
    texp_loc = pexp_loc;
  }

let annotate (program: Ast.program) =
  let env = Env.create () in
  let { Ast. pprogram_statements; _; } = program in
  let tprogram_statements =
    List.map ~f:(annotate_statement env) pprogram_statements
  in
  {
    tprogram_statements;
  }
