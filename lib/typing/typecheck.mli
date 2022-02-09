
(* val type_check:
	?type_provider:Type_provider.provider ->
	?open_domains:(string array list) ->
	Typedtree.program -> (Typedtree.program * Type_error.t list) *)

(* val type_check: Type_context.t -> ?debug:bool -> unit -> Type_error.t list *)

val typecheck_module: Type_context.t -> verbose:bool -> Typedtree.program -> unit
