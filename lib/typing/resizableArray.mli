
(* An array that automatically expands when needed *)

type t

exception Out_of_bounds_set of string

(* `make x` creates a ResizableArray.t where the underlying array has the initial size of `x`.
 * However, this is purely for the purposes of optimization:
 * `ResizableArray.size (ResizableArray.make 5)` still * evaluates to `0`. *)
val make : int -> t

(* `set arr i x` raises `Out_of_bounds_set` if `i >= ResizableArray.size arr`, or if `i < 0` *)
val set : t -> int -> Core_type.node -> unit

(* Expands the underlying array if necessary *)
val push : t -> Core_type.node -> unit

(* Shrinks the representation to match the number of elements stored *)
val shrink : t -> unit

(* Returns None if the index is out of bounds. *)
val get : t -> int -> Core_type.node

val size : t -> int

(* Exposed only for white box testing. Do not use this. Really. *)
val underlying_array_size_do_not_use : t -> int

(* val to_hashtbl : 'a t -> ('a, int) Hashtbl.t *)
