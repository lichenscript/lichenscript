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
  [@@deriving show]

let map_symbol ~f s =
  match s with
  | SymLocal name -> SymLocal (f name)
  | SymParam _
  | SymLambda _ -> s

module%gen rec Decl : sig

  type _class = {
    name: string;
    original_name: string;
    finalizer_name: string;
    properties: string list;
  }
  [@@deriving show]

  type class_init = {
    class_id_name: string;
    class_def_name: string;
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

  type spec =
  | If of Expr.t * Block.t
  | While of Expr.t * Block.t
  | Expr of Expr.t
  | VarDecl of string list
  | Continue
  | Break
  | Retain of Expr.t
  | Release of Expr.t
  [@@deriving show]

  type t = {
    spec: spec;
    loc: Loc.t;
  }
  [@@deriving show]

end
  = Stmt

and Expr : sig

  type spec =
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
  | ArraySetValue of (symbol * int * t)
  | I32Binary of Asttypes.BinaryOp.t * t * t
  | CallLambda of t * t list
  | Call of int * t list
  | Assign of symbol * t
  | ExternalCall of symbol * t list
  | Ident of symbol
  | TagEqual of t * int
  | Temp of int

  and t = {
    loc: Loc.t;
    spec: spec;
  }
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
