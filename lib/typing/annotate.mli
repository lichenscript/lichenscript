
val is_name_enum_or_class: string -> bool

val annotate_program:
  Env.t ->
  Lichenscript_parsing.Ast.program ->
  Typedtree.program
