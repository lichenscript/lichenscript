open Waterlang_lex
open Core_type

type t = {
  loc: Loc.t;
  spec: spec;
}

and spec =
  | NotAssignable of TypeValue.t * TypeValue.t
  | CannotFindName of string

module PP = struct

  let error err =
    let { spec; _ } = err in
    match spec with
    | NotAssignable (be_assigned, assign) ->
      TypeValue.pp Format.str_formatter be_assigned;
      let str1 = Format.flush_str_formatter() in
      TypeValue.pp Format.str_formatter assign;
      let str2 = Format.flush_str_formatter() in
      Format.sprintf "Type '%s' is not assignable to type '%s'" str1 str2

    | CannotFindName name ->
      Format.sprintf "Can not find name '%s'" name
  
end
