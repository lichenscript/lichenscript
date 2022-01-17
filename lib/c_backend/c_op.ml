(*
 * Definition of C op
 * used for data generation
 *
 * Primarily, flatten the expressions
 * to declarations.
 *)

open Lichenscript_lex

module%gen rec Decl : sig

  type spec =
  | Func of Func.t
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
  | If
  | While
  | Expr of Expr.t
  | VarDecl
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
  | NewString of string
  | NewInt of string
  | NewFloat of string
  | NewChar of char 
  | NewBoolean of bool
  | Call of int * t list
  | Assign of string * t
  | ExternalCall of string * t list
  | Ident of string 
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
