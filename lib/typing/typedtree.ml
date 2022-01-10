open Lichenscript_lex
open Lichenscript_parsing
open Scope

module Pattern = struct

  type t = {
    spec: spec;
    loc: Loc.t
  }

  and spec =
  | Symbol of (string * int)
  [@@deriving show]

end

module%gen rec Expression : sig
  type t = {
    spec: spec;
    loc: Loc.t;
    ty_var: int;
    attributes: Ast.attributes;
  }

  and spec =
    | Constant of Ast.Literal.t
    | Identifier of (string * int)
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

    | Init of Ast.Expression.init

  and callee = {
    callee_spec: (string * int) * ([ `Property of string | `Expr of t ] list);
    callee_loc: Loc.t;
    callee_ty_var: int;
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
    | Expr of Expression.t
    | Semi of Expression.t
    | While of while_desc
    | Binding of var_binding
    | Block of Block.t
    | Break of Identifier.t option
    | Continue of Identifier.t option
    | Debugger
    | Return of Expression.t option
    | Empty

  and var_binding = {
    binding_kind: Ast.var_kind;
    binding_loc: Loc.t;
    binding_ty_var: int; 
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
    body: Block.t;
    comments: Loc.t Lichenscript_lex.Comment.t list;
    scope: scope;
    ty_var: int;
  }

  and params = {
    params_content: param list;
    params_loc: Loc.t;
  }

  and header = {
    id: int;
    name: string;
    params: params;
  }

  and param = {
    param_pat: Pattern.t;
    param_ty: int;
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
    return_ty: int;
  }
  [@@deriving show]

end
  = Block

and Declaration : sig

  type declare_spec =
  | DeclFunction of Function.header

  and declare = {
    decl_visibility: Asttypes.visibility option;
    decl_spec: declare_spec;
    decl_loc: Loc.t;
    decl_ty_var: int;
  }

  and _class = {
    cls_id: (string * int);
    cls_visibility: Asttypes.visibility option;
    cls_loc: Loc.t;
    cls_body: class_body;
    cls_comments:  Loc.t Lichenscript_lex.Comment.t list;
  }

  and class_body = {
    cls_body_elements: class_body_element list;
    cls_body_loc: Loc.t;
  }

  and class_property = {
    cls_property_visibility: Asttypes.visibility;
    cls_property_loc: Loc.t;
    cls_property_name: Identifier.t;
    cls_property_init: Expression.t option;
  }

  and class_method = {
    cls_method_visibility: Asttypes.visibility;
    cls_method_modifier: Ast.Declaration.class_modifier option;
    cls_method_loc: Loc.t;
  }

  and class_body_element =
    | Cls_method of class_method
    | Cls_property of class_property
    [@@deriving show]

  type spec =
    | Class of _class
    | Function_ of Function.t
    | Declare of declare
    | Enum of Ast.Enum.t
    | Import of Ast.Declaration.import
    [@@deriving show]

  type t =
    {
      spec: spec;
      loc: Loc.t;
      attributes: Ast.attributes;
    }
    [@@deriving show]

end
 = Declaration

type program = {
  tprogram_declarations: Declaration.t list;
  tprogram_scope: scope;
  ty_var: int;
}
[@@deriving show]

(* let pp_pattern formatter pat =
  match pat.tpat_desc with
  | Tpat_symbol sym ->
    Format.pp_print_string formatter sym.name *)
