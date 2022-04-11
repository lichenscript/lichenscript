open Lichenscript_parsing

type import_checker = Ast.Import.t -> unit

val typecheck_module:
	Program.t ->
	import_checker:import_checker ->
	verbose:bool ->
	Typedtree.program ->
	(unit, Diagnosis.t list) result
