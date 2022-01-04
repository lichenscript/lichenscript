
type env

val init_env: Waterlang_lex.File_key.t option -> string -> env

val include_module_ids: env -> string list

val add_include_module_id: env -> string -> unit

val with_scope: env -> Parse_scope.t -> (unit -> 'a) -> 'a

val scope: env -> Parse_scope.t

val in_function: env -> bool

val source: env -> Waterlang_lex.File_key.t option

val errors: env -> Parse_error.t list

val error : env -> Parse_error.spec -> unit

val error_unexpected : ?expected:string -> env -> unit

val last_loc : env -> Waterlang_lex.Loc.t option

module Peek : sig

  val token : env -> Waterlang_lex.Token.t

  val loc : env -> Waterlang_lex.Loc.t

  val errors : env -> Parse_error.t list
  
end

module Eat : sig

  val token : env -> unit

  val maybe : env -> Waterlang_lex.Token.t -> bool
  
end

module Expect : sig

  val error : env -> Waterlang_lex.Token.t -> unit

  val token : env -> Waterlang_lex.Token.t -> unit

  val token_opt : env -> Waterlang_lex.Token.t -> unit

  val identifier : env -> string -> unit

end
