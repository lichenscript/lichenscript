open Lichenscript_parsing

type import_checker = Ast.Declaration.import -> unit

val typecheck_module: Type_context.t -> import_checker:import_checker -> verbose:bool -> Typedtree.program -> unit
