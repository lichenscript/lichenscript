open Lichenscript_parsing
open Lichenscript_typing

exception ParseError of Parse_error.t list
exception TypeCheckError of Type_error.t list

val compile_file_path:
	std_dir: string -> build_dir:string option -> runtime_dir:string ->
		mode:string -> verbose:bool -> string -> (string * string * string option) list
