open Waterlang_typing

val codegen: Program.t -> Config.t -> string

val codegen_binary: Program.t -> Config.t -> string -> unit
