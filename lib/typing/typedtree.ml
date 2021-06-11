open Waterlang_lex

type expression = {
  texp_desc: expression_desc;
  texp_loc: Loc.t;
}

and expression_desc =
  | Texp_constant of Waterlang_parsing.Ast.constant
  | Texp_identifier of Core_type.VarSym.t
  | Texp_lambda
  | Texp_throw of expression
  | Texp_if of if_desc
  | Texp_array of expression list

and if_desc = {
  tif_test: expression;
  tif_consequent: statement;
  tif_alternative: statement option;
  tif_loc: Loc.t;
}

and statement = {
  tstmt_desc: statement_desc;
  tstmt_loc: Loc.t;
}

and statement_desc =
  | Tstmt_class
  | Tstmt_expr of expression
  | Tstmt_semi of expression
  | Tstmt_function
  | Tstmt_while of while_desc
  | Tstmt_binding of var_binding
  | Tstmt_block of block
  | Tstmt_break of Waterlang_parsing.Identifier.t option
  | Tstmt_continue of Waterlang_parsing.Identifier.t option
  | Tstmt_debugger
  | Tstmt_return of expression option
  | Tstmt_empty

and var_binding = {
  tbinding_kind: Waterlang_parsing.Ast.var_kind;
  tbinding_loc: Loc.t;
  tbinding_ty: _type option;
  tbinding_pat: pattern;
  tbinding_init: expression;
}

and while_desc = {
  twhile_test: expression;
  twhile_block: block;
  twhile_loc: Loc.t;
}

and block = {
  tblk_body: statement list;
  tblk_loc: Loc.t;
}

and pattern = {
  tpat_desc: pattern_desc;
  tpat_loc: Loc.t
}

and pattern_desc =
 | Tpat_symbol of Core_type.VarSym.t

and program = {
  tprogram_statements: statement list;
}

and _type = {
  tty_desc: type_desc;
  tty_loc: Loc.t;
}

and type_desc =
  | Tty_any
  | Tty_var of string
  | Tty_ctor of Core_type.TypeSym.t * _type list
  | Tty_arrow of
    _type list *
    _type
