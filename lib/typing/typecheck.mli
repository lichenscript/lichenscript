
val type_check:
	?type_provider:Type_provider.provider ->
	?open_domains:(string array list) ->
	Typedtree.program -> (Typedtree.program * Type_error.t list)
