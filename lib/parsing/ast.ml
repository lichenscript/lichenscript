module Loc = Waterlang_lex.Loc

type location_stack = Loc.t list
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

module%gen rec Literal : sig

  type t =
    | Integer of string * char option
    | Char of char
    (* 'c' *)
    | String of string * Loc.t * string option
    | Float of string * char option
    | Boolean of bool
    [@@deriving show]

end
 = Literal

and Expression : sig

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
    | Constant of Literal.t
    | Identifier of Identifier.t
    | Lambda of Function.t
    | If of if_desc
    | Array of t list
    | Call of call
    | Member of t * Identifier.t
    | Unary of Asttypes.UnaryOp.t * t
    | Binary of Asttypes.BinaryOp.t * t * t
    | Update of Asttypes.UpdateOp.t * t * bool (* prefix *)
    | Assign of Pattern.t * t
    | Block of Block.t

  and t = {
    spec: spec;
    loc: Loc.t;
    attributes: attributes;
  }
  [@@deriving show]
  
end
  = Expression
and Statement : sig

  type while_desc = {
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
    | Expr of Expression.t (* Expr without trailing semi-colon. *)
    | Semi of Expression.t (* Expr with a trailing semi-colon. *)
    | While of while_desc
    | Binding of var_binding
    | Block of Block.t
    | Break of Identifier.t option
    | Continue of Identifier.t option
    | Debugger
    | Return of Expression.t option
    | Empty

  and t = {
    spec: spec;
    loc: Loc.t;
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
    visibility: Asttypes.visibility option;
    header: header;
    body: Block.t;
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

  and header = {
    id: Identifier.t;
    params: params;
    return_ty: Type.t option;
    header_loc: Loc.t;
  }
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
    | Ty_ctor of Identifier.t * t list
      (* List<int> *)

    | Ty_arrow of
      t list *  (* params*)
      t         (* result *)
  [@@deriving show]

end
  = Type

and Enum : sig
  type member = {
    member_name: Identifier.t;
    fields: Type.t list;
  }
  [@@deriving show]

  type t = {
    name: Identifier.t;
    type_vars: Identifier.t list;
    members: member list;
    loc: Loc.t
  }
  [@@deriving show]

end
  = Enum

(* represeng top-level defintion *)
and Declaration : sig

  type declare_spec =
  | DeclFunction of Function.header

  and declare = {
    decl_spec: declare_spec;
    decl_loc: Loc.t;
  }

  and _class = {
    cls_id:        Identifier.t;
    cls_type_vars: Identifier.t list;
    cls_loc:       Loc.t;
    cls_body:      class_body;
    cls_comments:  Loc.t Waterlang_lex.Comment.t list;
  }

  and class_body = {
    cls_body_elements: class_body_element list;
    cls_body_loc: Loc.t;
  }

  and class_property = {
    cls_property_attributes: attributes;
    cls_property_visiblity: Asttypes.visibility option;
    cls_property_loc: Loc.t;
    cls_property_name: Identifier.t;
    cls_property_type: Type.t option;
    cls_property_init: Expression.t option;
  }

  and class_method = {
    cls_method_attributes: attributes;
    cls_method_static: bool;
    cls_method_visiblity: Asttypes.visibility option;
    cls_method_name: Identifier.t;
    cls_method_params: Function.params;
    cls_method_body: Block.t option;
    cls_method_loc: Loc.t;
    cls_method_return_ty: Type.t option;
  }

  and class_body_element =
    | Cls_method of class_method
    | Cls_property of class_property
  [@@deriving show]

  type spec =
    | Class of _class
    | Function_ of Function.t
    | Declare of declare
    | Enum of Enum.t
    [@@deriving show]

  type t = {
    spec: spec;
    loc: Loc.t;
    attributes: attributes;
  }
  [@@deriving show]

end
  = Declaration

type program = {
  pprogram_top_level: Top_level.t;
  pprogram_declarations: Declaration.t list;
  pprogram_comments: Loc.t Waterlang_lex.Comment.t list;
  pprogram_loc: Loc.t;
}
[@@deriving show]
