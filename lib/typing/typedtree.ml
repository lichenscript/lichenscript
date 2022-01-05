open Waterlang_lex
open Waterlang_parsing
open Core_type

module Pattern = struct

  type t = {
    spec: spec;
    loc: Loc.t
  }

  and spec =
  | Symbol of Core_type.VarSym.t
  [@@deriving show]

end

module%gen rec Expression : sig
  type t = {
    spec: spec;
    loc: Loc.t;
    mutable val_: TypeValue.t;
  }

  and spec =
    | Constant of Ast.Literal.t
    | Identifier of Core_type.VarSym.t
    | UnresolvedIdentifier of Identifier.t
    | Lambda
    | If of if_desc
    | Array of t list
    | Call of call
    | Member of t * Identifier.t
    | Unary of
      Asttypes.UnaryOp.t *
      t

    | Binary of
      Asttypes.BinaryOp.t * t * t

    | Update of
      Asttypes.UpdateOp.t * t * bool

    | Assign of Pattern.t * t

    | Block of Block.t

  and callee = {
    callee_spec: Core_type.VarSym.t * ([ `Property of string | `Expr of t ] list);
    callee_loc: Loc.t;
    callee_ty: Core_type.TypeValue.t
  }

  and call = {
    callee: callee;
    call_params: t list;
    call_loc: Loc.t;
  }

  and if_desc = {
    if_test: t;
    if_consequent: Statement.t;
    if_alternative: Statement.t option;
    if_loc: Loc.t;
  }
  [@@deriving show]
  
end
  = Expression

and Statement : sig

  type t = {
    spec: spec;
    loc: Loc.t;
    attributes: Ast.attributes;
  }

  and spec =
    | Class of _class
    | Module of _module
    | Expr of Expression.t
    | Semi of Expression.t
    | Function_ of Function.t
    | While of while_desc
    | Binding of var_binding
    | Block of Block.t
    | Break of Identifier.t option
    | Continue of Identifier.t option
    | Debugger
    | Return of Expression.t option
    | EnumDecl of Ast.Enum.t
    | Decl of Declare.t
    | Empty

  and _class = {
    cls_id: Core_type.TypeSym.t;
    cls_loc: Loc.t;
    cls_body: class_body;
  }

  and _module = {
    mod_visibility: Ast.visibility option;
    mod_name: Identifier.t;
  }

  and class_body = {
    cls_body_elements: class_body_element list;
    cls_body_loc: Loc.t;
  }

  and class_property = {
    cls_property_visibility: Ast.visibility;
    cls_property_loc: Loc.t;
    cls_property_name: Identifier.t;
    cls_property_init: Expression.t option;
  }

  and class_method = {
    cls_method_visibility: Ast.visibility;
    cls_method_loc: Loc.t;
  }

  and class_body_element =
    | Cls_method of class_method
    | Cls_property of class_property

  and var_binding = {
    binding_kind: Ast.var_kind;
    binding_loc: Loc.t;
    binding_ty: TypeValue.t option;
    binding_pat: Pattern.t;
    binding_init: Expression.t;
  }

  and while_desc = {
    while_test: Expression.t;
    while_block: Block.t;
    while_loc: Loc.t;
  }
  [@@deriving show]

end
  = Statement

and Function : sig
  type t = {
    header: header;
    body: function_body;
    assoc_scope: Scope.t;
    loc: Loc.t;
  }

  and function_body =
    | Fun_block_body of Block.t
    | Fun_expression_body of Expression.t

  and params = {
    params_content: param list;
    params_loc: Loc.t;
  }

  and header = {
    id: Core_type.VarSym.t;
    params: params;
  }

  and param = {
    param_pat: Pattern.t;
    param_ty: Core_type.TypeValue.t;
    param_init: Expression.t option;
    param_loc: Loc.t;
    param_rest: bool;
  }
  [@@deriving show]

end
  = Function

and Block : sig

  type t = {
    body: Statement.t list;
    loc: Loc.t;
    val_: TypeValue.t;
  }
  [@@deriving show]

end
  = Block

and Declare : sig
  type spec =
  | Function_ of Function.header

  and t = {
    spec: spec;
    loc: Loc.t;
  }
  [@@deriving show]

end
  = Declare

type program = {
  root_scope: Scope.t;
  tprogram_statements: Statement.t list;
}
[@@deriving show]

(* let pp_pattern formatter pat =
  match pat.tpat_desc with
  | Tpat_symbol sym ->
    Format.pp_print_string formatter sym.name *)
