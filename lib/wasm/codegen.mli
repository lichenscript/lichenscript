open Waterlang_typing

(**
  * Object in waterlang are refcounting
  *
  * object layout: 16 bytes
  * |----------------|-----|
  * | total bytes    | i32 |
  * | flag           | i32 |
  * | strong counter | i32 |
  * | weak counter   | i32 |
  * |----------------|-----|
  *
  * string layout
  * |----------------|----------|
  * | object layout  | 16 bytes |
  * | length         | i32      |
  * | bytes          | u16[]    |
  * |----------------|----------|
  *
  * array layout
  * |----------------|----------|
  * | object layout  | 16 bytes |
  * | length         | i32      |
  * | bytes          | ptr[]    |
  * |----------------|----------|
  * 
  * user defined object
  * |----------------|----------|
  * | object layout  | 16 bytes |
  * | properties     |          |
  * | methods        |          |
  *
  *)
val codegen: Program.t -> Config.t -> string

val codegen_binary: Program.t -> Codegen_env.t -> string -> unit
