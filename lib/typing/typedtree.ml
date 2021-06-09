open Waterlang_lex

type expression = {
  tecp_desc: expression_desc;
  texp_loc: Loc.t;
}

and expression_desc =
  | Texp_constant
  | Texp_identifier of Symbol.t
  | Texp_lambda
  | Texp_throw
  | Texp_if

and statement = {
  tstmt_desc: statement_desc;
  tstmt_loc: Loc.t;
}

and statement_desc =
  | Tstmt_class
  | Tstmt_expr of expression
  | Tstmt_semi of expression
  | Tstmt_function
  | Tstmt_while
  | Tstmt_binding
  | Tstmt_block
  | Tstmt_break
  | Tstmt_continue
  | Tstmt_debugger
  | Tstmt_return of expression option
  | Tstmt_empty

and program = {
  tprogram_statements: statement list;
}

