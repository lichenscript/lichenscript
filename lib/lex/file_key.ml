
type t =
  | LibFile of string
  | SourceFile of string
  | JsonFile of string
  (* A resource that might get required, like .css, .jpg, etc. We don't parse
     these, just check that they exist *)
  | ResourceFile of string
  | Builtins
  [@@deriving show]

let pp formatter v =
   match v with
   | LibFile str
   | SourceFile str
   | JsonFile str
   (* A resource that might get required, like .css, .jpg, etc. We don't parse
      these, just check that they exist *)
   | ResourceFile str ->
      Format.fprintf formatter "%s" str

   | Builtins ->
      Format.fprintf formatter "<builtin>"
