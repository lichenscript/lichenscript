
open Ast

let rec parse_string source content = 
  let env = Parser_env.init_env source content in
  parse_program env

and parse_program _env =
  {
    pprogram_statements = [];
    pprogram_comments = [];
  }
