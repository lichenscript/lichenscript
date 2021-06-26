open Waterlang_typing

val codegen: Program.t -> Config.t -> string

val codegen_binary: Program.t -> Codegen_env.t -> string -> unit
