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
  | EnumCtor of ((string * int) * t)
  [@@deriving show]

end

module%gen rec Expression : sig

  type if_desc = {
    if_test: t;
    if_consequent: Block.t;
    if_alternative: if_alt option;
    if_loc: Loc.t;
  }

  and if_alt =
    | If_alt_if of if_desc
    | If_alt_block of Block.t

  and init_entry = {
    init_entry_loc: Loc.t;
    init_entry_key: Identifier.t;
    init_entry_value: t option;
  }

  and init_element =
  | InitSpread of Expression.t
  | InitEntry of init_entry

  and init = {
    init_loc: Loc.t;
    init_name: (string * int);
    init_elements: init_element list;
  }

  and _match = {
    match_expr: t;
    match_clauses: match_clause list;
    match_loc: Loc.t;
  }

  and match_clause = {
    clause_pat: Pattern.t;
    clause_consequent: t;
    clause_loc: Loc.t;
  }

  and lambda = {
    lambda_params: Function.params;
    lambda_return_ty: Core_type.TypeExpr.t;
    lambda_scope: scope;
    lambda_body: t;
  }

  and spec =
    | Constant of Ast.Literal.t
    | Identifier of (string * int)
    | Lambda of lambda
    | If of if_desc
    | Array of t list
    | Call of call
    | Member of t * Identifier.t
    | Index of t * t
    | Unary of
      Asttypes.UnaryOp.t *
      t

    | Binary of
      Asttypes.BinaryOp.t * t * t

    | Update of
      Asttypes.UpdateOp.t * t * bool

    | Assign of Asttypes.AssignOp.t option * (string * int) * t
    | Block of Block.t
    | Init of init
    | Match of _match
    | This
    | Super

  and t = {
    spec: spec;
    loc: Loc.t;
    ty_var: int;
    attributes: Ast.attributes;
  }

  and call = {
    callee: t;
    call_params: t list;
    call_loc: Loc.t;
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
    name: (string * int);
    name_loc: Loc.t;  (* used for sourcemap *)
    params: params;
  }

  and param = {
    param_name: (string * int);
    param_ty: int;
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

and Enum : sig

  type case  = {
    case_name: (string * int);
    case_fields: Core_type.TypeExpr.t list;
    case_loc: Loc.t;
  }
  [@@deriving show]

  type t = {
    visibility: Asttypes.visibility option;
    name: (string * int);
    type_vars: string list;  (* generic vars *)
    cases: case list;
    loc: Loc.t;
  }
  [@@deriving show]

end
  = Enum

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
    cls_property_visibility: Asttypes.visibility option;
    cls_property_loc: Loc.t;
    cls_property_name: Identifier.t;
  }

  and class_method = {
    cls_method_attributes: Ast.attributes;
    cls_method_visibility: Asttypes.visibility option;
    cls_method_modifier: Ast.Declaration.class_modifier option;
    cls_method_name: (string * int);
    cls_method_params: Function.params;
    cls_method_body: Block.t option;
    cls_method_scope: scope option;
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
    | Enum of Enum.t
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
