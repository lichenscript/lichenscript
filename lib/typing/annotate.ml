open Core_kernel
open Typedtree
open Waterlang_parsing

let rec annotate_statement (env: Env.t) stmt =
  let { Ast. pstmt_loc; pstmt_desc; _; } = stmt in
  let tstmt_desc =
    match pstmt_desc with
    | Pstmt_class _ -> Tstmt_class

    | Pstmt_expr expr ->
      Tstmt_expr (anotate_expression env expr)

    | Pstmt_semi expr ->
      Tstmt_semi (anotate_expression env expr)

    | Pstmt_function _ -> Tstmt_function
    | Pstmt_while _ -> Tstmt_while
    | Pstmt_binding _ -> Tstmt_binding
    | Pstmt_block _ -> Tstmt_block
    | Pstmt_break _ -> Tstmt_break
    | Pstmt_contintue _ -> Tstmt_continue
    | Pstmt_debugger -> Tstmt_debugger
    | Pstmt_return expr_opt ->
      Tstmt_return (Option.map ~f:(anotate_expression env) expr_opt)

    | Pstmt_empty -> Tstmt_empty

  in

  {
    tstmt_desc;
    tstmt_loc = pstmt_loc;
  }

and anotate_expression (_env: Env.t) expr =
  let { Ast. pexp_loc; _; } = expr in
  {
    tecp_desc = Texp_constant;
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
