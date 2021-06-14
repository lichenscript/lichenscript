open Core_kernel
open Typedtree

let check_statement _env statement =
  let { tstmt_desc; _; } = statement in
  match tstmt_desc with
  | Tstmt_class
  | Tstmt_expr _
  | Tstmt_semi _
  | Tstmt_function
  | Tstmt_while _
  | Tstmt_binding _
  | Tstmt_block _
  | Tstmt_break _
  | Tstmt_continue _
  | Tstmt_debugger
  | Tstmt_return _
  | Tstmt_empty -> ()

let type_check env program =
  let { tprogram_statements; _; } = program in
  List.iter ~f:(check_statement env) tprogram_statements
