(*
 * Definition of C op
 * used for data generation
 *
 * Primarily, flatten the expressions
 * to declarations.
 *)

open Lichenscript_lex
open Lichenscript_parsing
open Lichenscript_typing

type symbol =
  | SymLocal of string
  | SymParam of int
  | SymLambda of int
  | SymThis
  [@@deriving show]

let map_symbol ~f s =
  match s with
  | SymLocal name -> SymLocal (f name)
  | SymParam _
  | SymLambda _
  | SymThis
    -> s

module%gen rec Decl : sig
  type class_method_tuple = {
    class_method_name: string;
    class_method_gen_name: string;
  }
  [@@deriving show]

  type class_finalizer = {
    finalizer_name: string;
    finalizer_content: Stmt.t list;
  }
  [@@deriving show]

  type _class = {
    name: string;
    original_name: string;
    finalizer: class_finalizer option;
    properties: string list;
  }
  [@@deriving show]

  type class_init = {
    class_name: string;
    class_id_name:  string;
    class_def_name: string;
    class_methods:  class_method_tuple list;
  }
  [@@deriving show]

  type enum_ctor = {
    enum_ctor_name: string;
    enum_ctor_tag_id: int;
    enum_cotr_params_size: int;
  }
  [@@deriving show]

  type lambda_def = {
    lambda_content: Typedtree.Expression.lambda;
    lambda_gen_name: string;
    lambda_ty: int;
  }
  [@@deriving show]

  type spec =
  | Func of Func.t
  | FuncDecl of symbol 
  | LambdaDef of lambda_def
  | Class of _class
  | EnumCtor of enum_ctor
  | GlobalClassInit of string * class_init list
  [@@deriving show]

  type t = {
    spec: spec;
    loc: Loc.t;
  }
  [@@deriving show]

end
 = Decl

and Stmt : sig

  type if_spec = {
    if_test: Expr.t;
    if_consequent: Stmt.t list;
    if_alternate: if_alt option;
  }

  and if_alt =
    | If_alt_if of if_spec
    | If_alt_block of Stmt.t list
  [@@deriving show]

  type spec =
  | If of if_spec
  | While of Expr.t * Block.t
  | Expr of Expr.t
  | VarDecl of string list
  | Continue
  | Break
  | Retain of Expr.t
  | Release of Expr.t
  | Label of string
  | Goto of string
  [@@deriving show]

  type t = {
    spec: spec;
    loc: Loc.t;
  }
  [@@deriving show]

end
  = Stmt

and Expr : sig

  type t =
  | Null
  | NewString of string
  | NewInt of string
  | NewFloat of string
  | NewChar of char 
  | NewLambda of (string * symbol array)
  | NewBoolean of bool
  | NewRef of t
  | GetRef of symbol
  | NewArray of int
  | NewMap of int
  | Not of t
  | ArrayGetValue of (t * t)
  | I32Binary of Asttypes.BinaryOp.t * t * t
  | CallLambda of t * t list
  | Call of int * t list
  | Invoke of t * string * t list
  | Assign of t * t
  | Update of Asttypes.AssignOp.t * t * t
  | ExternalCall of symbol * t option * t list
  | InitCall of symbol
  | Ident of symbol
  | TagEqual of t * int
  | UnionGet of t * int
  | Temp of int
  | IntValue of t
  | GetField of t * string * string (* expr classname fieldname *)
  | StringCmp of Asttypes.BinaryOp.t * t * t
  | Retaining of Expr.t
  [@@deriving show]

end
  = Expr

and Func : sig

  type t = {
    name: string;
    tmp_vars_count: int;
    body: Block.t;
    comments: Loc.t Lichenscript_lex.Comment.t list;
  }
  [@@deriving show]
  
end
  = Func

and Block : sig

  type t = {
    loc: Loc.t;
    body: Stmt.t list;
  }
  [@@deriving show]

end
  = Block
