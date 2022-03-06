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
  | SymTemp of int
  | SymParam of int
  | SymLambda of (int * string)
  | SymRet
  | SymThis
  | SymLambdaThis
  [@@deriving show]

let map_symbol ~f s =
  match s with
  | SymLocal name -> SymLocal (f name)
  | SymTemp _
  | SymParam _
  | SymLambda _
  | SymRet
  | SymThis
  | SymLambdaThis
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

  type gc_marker = {
    gc_marker_name: string;
    gc_marker_field_names: string list;
  }
  [@@deriving show]

  type class_init = {
    class_name: string;
    class_id_name: string;
    class_def_name: string;
    class_methods: class_method_tuple list;
  }
  [@@deriving show]

  type _class = {
    name: string;
    original_name: string;
    finalizer: class_finalizer option;
    gc_marker: gc_marker option;
    properties: (string * int) list;
    init: class_init;
  }
  [@@deriving show]

  type enum_ctor = {
    enum_ctor_name: string;
    enum_ctor_tag_id: int;
    enum_ctor_params_size: int;
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
  | WithLabel of string * t list
  | Goto of string
  | Return of Expr.t option
  [@@deriving show]

  and t = {
    spec: spec;
    loc: Loc.t;
  }
  [@@deriving show]

end
  = Stmt

and Expr : sig

  type lambda_spec = {
    lambda_name: string;
    lambda_this: t;
    lambda_capture_symbols: symbol array;
    lambda_decl: Decl.t;
  }
  [@@deriving show]

  and t =
  | Null
  | NewString of string
  | NewInt of string
  | NewFloat of string
  | NewChar of int
  | NewLambda of lambda_spec
  | NewBoolean of bool
  | NewRef of t
  | GetRef of (symbol * string)  (* deref symbol, original_name *)
  | NewArray of int
  | NewTuple of t list
  | NewMap of int
  | Not of t
  | TupleGetValue of (t * int)
  | ArrayGetValue of (t * t)
  | ArraySetValue of (t * t * t)
  | TypeCast of t * Primitives.PrimType.t * Primitives.PrimType.t
  | I32Binary of Asttypes.BinaryOp.t * t * t
  | F32Binary of Asttypes.BinaryOp.t * t * t
  | I64Binary of Asttypes.BinaryOp.t * t * t
  | F64Binary of Asttypes.BinaryOp.t * t * t
  | CallLambda of t * t list
  | Invoke of t * string * t list
  | Assign of t * t
  | Call of symbol * t option * t list
  | InitCall of (symbol * symbol)  (* init call function, meta name *)
  | Ident of symbol
  | TagEqual of t * int
  | UnionGet of t * int
  | Temp of int
  | IntValue of t
  | GetField of t * string * string (* expr classname fieldname *)
  | RawGetField of string * string
  | StringCmp of Asttypes.BinaryOp.t * t * t
  | StringEqUtf8 of t * string
  | Retaining of Expr.t
  [@@deriving show]

end
  = Expr

and Func : sig

  type t = {
    name: (string * Loc.t);
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
