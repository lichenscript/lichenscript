module Loc = Waterlang_lex.Loc

type constant =
  |  Pconst_integer of string * char option
  | Pconst_char of char
  (* 'c' *)
  | Pconst_string of string * Loc.t * string option
  | Pconst_float of string * char option
  | Pconst_boolean of bool
  [@@deriving show]

type location_stack = Loc.t list
[@@deriving show]

type visibility =
  | Pvisibility_public
  | Pvisibility_protected
  | Pvisibility_private
  [@@deriving show]

(** {1 Extension points} *)

type attribute = {
  attr_name : string Asttypes.loc;
  attr_payload : string; (* modified *)
  attr_loc : Loc.t;
}
[@@deriving show]

and attributes = attribute list

and expression = {
  pexp_desc: expression_desc;
  pexp_loc: Loc.t;
  pexp_loc_stack: location_stack;
  pexp_attributes: attributes;
}

and expression_desc =
  | Pexp_constant of constant
  | Pexp_identifier of Identifier.t
  | Pexp_lambda of _function

and statement = {
  pstmt_desc: statement_desc;
  pstmt_loc: Loc.t;
  pstmt_loc_stack: location_stack;
  pstmt_attributes: attributes;
}

and statement_desc =
  | Pstmt_class of _class
  | Pstmt_expression of expression
  | Pstmt_function of _function
  | Pstmt_if of
    expression *  (* test *)
    statement * (* consequent *)
    statement option *  (* alternatives *)
    Loc.t Waterlang_lex.Comment.t list (* comments *)
  | Pstmt_let of let_binding
  | Pstmt_block of block
  | Pstmt_break of Identifier.t option
  | Pstmt_contintue of Identifier.t option
  | Pstmt_debugger
  | Pstmt_return of
    expression option
  | Pstmt_throw of expression
  | Pstmt_empty

and let_binding = {
  plet_loc: Loc.t;
  plet_ty: _type option;
  plet_pat: pattern;
  plet_init: expression;
}

and block = {
  pblk_body: statement list;
  pblk_loc: Loc.t;
}

and pattern = {
  ppat_desc: pattern_desc;
  ppat_loc: Loc.t;
}

and pattern_desc =
  | Ppat_identifier of Identifier.t

and _function = {
  pfun_id: Identifier.t option;
  pfun_params: params;
  pfun_body: function_body;
  pfun_loc: Loc.t;
  pfun_comments: Loc.t Waterlang_lex.Comment.t list;
}

and params = {
  pparams_content: param list;
  pparams_loc: Loc.t
}

and param =  {
  pparam_pat: pattern;
  pparam_ty: _type option;
  pparam_init: expression option;
  pparam_loc: Loc.t;
  pparam_rest: bool;
}

and function_body =
  | Pfun_block_body of block
  | Pfun_expression_body of expression

and _class = {
  pcls_id:       Identifier.t option;
  pcls_loc:      Loc.t;
  pcls_body:     class_body;
  pcls_comments: Loc.t Waterlang_lex.Comment.t list;
}

and class_body = {
  pcls_body_elements: class_body_element list;
  pcls_body_loc: Loc.t;
}

and class_property = {
  pcls_property_visiblity: visibility option;
  pcls_property_loc: Loc.t;
  pcls_property_name: Identifier.t;
  pcls_property_type: _type option;
  pcls_property_init: expression option;
}

and class_method = {
  pcls_method_visiblity: visibility option;
  pcls_method_name: Identifier.t;
  pcls_method_loc: Loc.t;
}

and class_body_element =
  | Pcls_method of class_method
  | Pcls_property of class_property

and _type = {
  pty_desc: type_desc;
  pty_loc: Loc.t;
}

and type_desc =
  | Pty_any
  | Pty_var of string
  | Pty_ctor of Identifier.t * type_desc list
    (* List<int> *)

  | Pty_arrow of
    _type list *  (* params*)
    _type         (* result *)

and program = {
  pprogram_statements: statement list;
  pprogram_comments: Loc.t Waterlang_lex.Comment.t list;
}
[@@deriving show]
