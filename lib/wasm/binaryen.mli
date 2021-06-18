open Ctypes

module Literal : sig

  type t
  type u

  val t: t structure typ

  val u: u union typ

  val u_i32: (Int32.t, u union) field

  val u_i64: (Int64.t, u union) field

  val u_f32: (Float.t, u union) field

  val u_f64: (Float.t, u union) field

  val u_v128: (Unsigned.uint8 carray, u union) field

  val u_func: (char ptr, u union) field

  val lit_type: (Uintptr.t, t Ctypes_static.structure) field

  val union_field: (u Ctypes_static.union, t Ctypes_static.structure) field

  val mk_int32: Int32.t -> t structure

  val mk_int64: Int64.t -> t structure

  val mk_f32: Float.t -> t structure

  val mk_f64: Float.t -> t structure
  
end

module Module : sig

  type t

  val create: unit -> t

  val dispose: t -> unit

  val with_module: (t -> unit) -> unit

  val emit_test: t -> string

end

module Type : sig

  type t

  val none: unit -> t

  val int32: unit -> t

  val int64: unit -> t

  val f32: unit -> t

  val f64: unit -> t

  val any_ref: unit -> t

  val unreachable: unit -> t

end

module Op : sig
  type t = int32

  val tval : t typ

  val add_i32: unit -> t

  val sub_i32: unit -> t

  val mul_i32: unit -> t
  
end

module Expr : sig
  type t

  val if': Module.t -> condition:t -> t:t -> f:t -> t
  
  val const: Module.t -> Literal.t structure -> t

  val binary: Module.t -> Op.t -> left:t -> right:t -> t

end

module Function : sig

  type t

  val add: Module.t ->
          name:string ->
          params:Type.t ->
          result:Type.t ->
          varTypes:(Type.t list) ->
          body:Expr.t ->
          t

end

