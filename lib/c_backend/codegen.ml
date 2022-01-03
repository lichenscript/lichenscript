open Core_kernel
open Waterlang_typing
open Waterlang_typing.Typedtree

type t = {
  indent: string;
  indent_level: int;
  buffer: Buffer.t;
}

let create ?(indent="    ") () =
  {
    indent;
    indent_level = 0;
    buffer = Buffer.create 1024;
  }

let rec codegen_statement env stmt =
  let open Statement in
  let { spec; _ } = stmt in
  match spec with
  | Class _ -> ()
  | Expr expr -> codegen_expression env expr
  | Semi _
  | Function_ _
  | While _
  | Binding _
  | Block _
  | Break _
  | Continue _
  | Debugger
  | Return _
  | EnumDecl _
  | Decl _
  | Empty -> ()

and codegen_program env (program: Typedtree.program) =
  let { tprogram_statements } = program in
  List.iter ~f:(codegen_statement env) tprogram_statements

and codegen_expression _env (expr: Typedtree.Expression.t) =
  let open Expression in
  let { spec; _ } = expr in
  match spec with
  | Constant _
  | Identifier _
  | Lambda
  | Throw _
  | If _
  | Array _
  | Call _
  | Member _
  | Unary _
  | Binary _
  | Update _
  | Assign _
  | Block  _ -> ()

let contents env = Buffer.contents env.buffer
