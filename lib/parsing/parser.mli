
type parse_result = {
  tree: Ast.program;
  include_module_ids: string list;
}

val parse_string: Waterlang_lex.File_key.t option -> string ->
  (parse_result, Parse_error.t list) Result.t
