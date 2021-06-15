open Waterlang_lex
open Core_type

type expression = {
  texp_desc: expression_desc;
  texp_loc: Loc.t;
  mutable texp_val: TypeValue.t;
}

and expression_desc =
  | Texp_constant of Waterlang_parsing.Ast.constant
  | Texp_identifier of Core_type.VarSym.t
  | Texp_lambda
  | Texp_throw of expression
  | Texp_if of if_desc
  | Texp_array of expression list
  | Texp_call of call
  | Texp_member of expression * Waterlang_parsing.Identifier.t
  | Texp_unary of
    Waterlang_parsing.Asttypes.unary_op *
    expression

  | Texp_binary of
    Waterlang_parsing.Asttypes.binary_op *
    expression *
    expression

and call = {
  tcallee: expression;
  tcall_params: expression list;
  tcall_loc: Loc.t;
}

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
  | Tstmt_class of _class
  | Tstmt_expr of expression
  | Tstmt_semi of expression
  | Tstmt_function of _function
  | Tstmt_while of while_desc
  | Tstmt_binding of var_binding
  | Tstmt_block of block
  | Tstmt_break of Waterlang_parsing.Identifier.t option
  | Tstmt_continue of Waterlang_parsing.Identifier.t option
  | Tstmt_debugger
  | Tstmt_return of expression option
  | Tstmt_empty

and _class = {
  tcls_id: Core_type.TypeSym.t;
  tcls_loc: Loc.t;
  tcls_body: class_body;
}

and class_body = {
  tcls_body_elements: class_body_element list;
  tcls_body_loc: Loc.t;
}

and class_property = {
  tcls_property_visibility: Waterlang_parsing.Ast.visibility;
  tcls_property_loc: Loc.t;
  tcls_property_name: Waterlang_parsing.Identifier.t;
  tcls_property_init: expression option;
}

and class_method = {
  tcls_method_visibility: Waterlang_parsing.Ast.visibility;
  tcls_method_loc: Loc.t;
}

and class_body_element =
  | Tcls_method of class_method
  | Tcls_property of class_property

and var_binding = {
  tbinding_kind: Waterlang_parsing.Ast.var_kind;
  tbinding_loc: Loc.t;
  tbinding_ty: _type option;
  tbinding_pat: pattern;
  tbinding_init: expression;
}

and _function = {
  tfun_id: Core_type.VarSym.t;
  tfun_params: params;
  tfun_body: function_body;
  tfun_loc: Loc.t;
}

and function_body =
  | Tfun_block_body of block
  | Tfun_expression_body of expression

and params = {
  tparams_content: param list;
  tparams_loc: Loc.t;
}

and param = {
  tparam_pat: pattern;
  tparam_init: expression option;
  tparam_loc: Loc.t;
  tparam_rest: bool;
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
