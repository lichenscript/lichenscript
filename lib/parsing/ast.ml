module Loc = Waterlang_lex.Loc

type constant =
  | Pconst_integer of string * char option
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

type var_kind =
  | Pvar_let
  | Pvar_const
[@@deriving show]

(** {1 Extension points} *)

type attribute = {
  attr_name : string Asttypes.loc;
  attr_payload : string list; (* modified *)
  attr_loc : Loc.t;
}
[@@deriving show]

and attributes = attribute list

module rec Expression : sig

  type if_desc = {
    if_test: t;
    if_consequent: Statement.t;
    if_alternative: Statement.t option;
    if_loc: Loc.t;
  }

  and call = {
    callee: t;
    call_params: t list;
    call_loc: Loc.t;
  }

  and spec =
    | Constant of constant
    | Identifier of Identifier.t
    | Lambda of Function.t
    | Throw of t
    | If of if_desc
    | Array of t list
    | Call of call
    | Member of t * Identifier.t
    | Unary of Asttypes.UnaryOp.t * t
    | Binary of Asttypes.BinaryOp.t * t * t
    | Update of Asttypes.UpdateOp.t * t * bool (* prefix *)

  and t = {
    spec: spec;
    loc: Loc.t;
    loc_stack: location_stack;
    attributes: attributes;
  }
  [@@deriving show]
  
end
  = Expression
and Statement : sig

  type _class = {
    cls_id:       Identifier.t option;
    cls_loc:      Loc.t;
    cls_body:     class_body;
    cls_comments: Loc.t Waterlang_lex.Comment.t list;
  }

  and class_body = {
    cls_body_elements: class_body_element list;
    cls_body_loc: Loc.t;
  }

  and class_property = {
    cls_property_visiblity: visibility option;
    cls_property_loc: Loc.t;
    cls_property_name: Identifier.t;
    cls_property_type: Type.t option;
    cls_property_init: Expression.t option;
  }

  and class_method = {
    cls_method_visiblity: visibility option;
    cls_method_name: Identifier.t;
    cls_method_loc: Loc.t;
  }

  and class_body_element =
    | Cls_method of class_method
    | Cls_property of class_property


  and while_desc = {
    while_test: Expression.t;
    while_block: Block.t;
    while_loc: Loc.t;
  }

  and var_binding = {
    binding_kind: var_kind;
    binding_loc: Loc.t;
    binding_ty: Type.t option;
    binding_pat: Pattern.t;
    binding_init: Expression.t;
  }

  and spec =
    | Class of _class
    | Expr of Expression.t (* Expr without trailing semi-colon. *)
    | Semi of Expression.t (* Expr with a trailing semi-colon. *)
    | Function_ of Function.t
    | While of while_desc
    | Binding of var_binding
    | Block of Block.t
    | Break of Identifier.t option
    | Contintue of Identifier.t option
    | Debugger
    | Return of Expression.t option
    | Empty

  and t = {
    spec: spec;
    loc: Loc.t;
    loc_stack: location_stack;
    attributes: attributes;
  }
  [@@deriving show]

end
  = Statement

and Block : sig

  type t = {
    body: Statement.t list;
    loc: Loc.t;
  }
  [@@deriving show]

end
  = Block

and Pattern : sig

  type spec =
    | Identifier of Identifier.t

  and t = {
    spec: spec;
    loc: Loc.t;
  }
  [@@deriving show]

end
  = Pattern

and Function : sig

  type t = {
    id: Identifier.t option;
    params: params;
    return_ty: Type.t option;
    body: function_body;
    loc: Loc.t;
    comments: Loc.t Waterlang_lex.Comment.t list;
  }

  and params = {
    params_content: param list;
    params_loc: Loc.t
  }

  and param =  {
    param_pat: Pattern.t;
    param_ty: Type.t option;
    param_init: Expression.t option;
    param_loc: Loc.t;
    param_rest: bool;
  }

  and function_body =
    | Fun_block_body of Block.t
    | Fun_expression_body of Expression.t
  [@@deriving show]

end
  = Function

and Type : sig
  type t = {
    spec: spec;
    loc: Loc.t;
  }

  and spec =
    | Ty_any
    | Ty_var of string
    | Ty_ctor of Identifier.t * t list
      (* List<int> *)

    | Ty_arrow of
      t list *  (* params*)
      t         (* result *)

end
  = Type


type program = {
  pprogram_statements: Statement.t list;
  pprogram_comments: Loc.t Waterlang_lex.Comment.t list;
}
[@@deriving show]
