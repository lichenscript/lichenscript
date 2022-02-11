open Lichenscript_parsing
open Lichenscript_typing

exception ParseError of Parse_error.t list
exception TypeCheckError of Type_error.t list

type profile = {
  profile_name: string;
  profile_dir: string;
  profile_exe_path: string;
}

val compile_file_path:
	std_dir: string -> build_dir:string option -> runtime_dir:string ->
		platform:string -> verbose:bool -> string -> profile list
