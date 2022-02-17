open Lichenscript_lex

type env

val init_env: Lichenscript_lex.File_key.t option -> string -> env

val add_top_level: env -> name:string -> loc:Loc.t -> visibility:Asttypes.visibility option -> unit

val get_top_level: env -> Top_level.t

val include_module_ids: env -> string list

val add_include_module_id: env -> string -> unit

val with_scope: env -> Parse_scope.t -> (unit -> 'a) -> 'a

val with_allow_init: env -> bool -> (env -> 'a) -> 'a

val with_allow_arrow: env -> bool -> (env -> 'a) -> 'a

val allow_init: env -> bool

val allow_arrow: env -> bool

val scope: env -> Parse_scope.t

val in_function: env -> bool

val source: env -> Lichenscript_lex.File_key.t option

val errors: env -> Parse_error.t list

val error : env -> Parse_error.spec -> unit

val get_unexpected_error : ?expected:string -> Lichenscript_lex.Token.t -> Parse_error.spec

val error_unexpected : ?expected:string -> env -> unit

val last_loc : env -> Lichenscript_lex.Loc.t option

module Peek : sig

  val token : env -> Lichenscript_lex.Token.t

  val loc : env -> Lichenscript_lex.Loc.t

  val errors : env -> Parse_error.t list
  
end

module Eat : sig

  val token : env -> unit

  val maybe : env -> Lichenscript_lex.Token.t -> bool
  
end

module Expect : sig

  val error : env -> Lichenscript_lex.Token.t -> unit

  val token : env -> Lichenscript_lex.Token.t -> unit

  val token_opt : env -> Lichenscript_lex.Token.t -> unit

  val identifier : env -> string -> unit

end
