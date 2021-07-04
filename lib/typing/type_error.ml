open Waterlang_lex
open Core_type

type t = {
  loc: Loc.t;
  spec: spec;
}

and spec =
  | NotAssignable of TypeValue.t * TypeValue.t
  | CannotFindName of string
  | Redefinition of string
  | NotCallable of TypeValue.t

let make_error loc spec =
  { loc; spec }

exception Error of t

module PP = struct

  let error_spec spec =
    match spec with
    | NotAssignable (be_assigned, assign) ->
      TypeValue.pp Format.str_formatter be_assigned;
      let str1 = Format.flush_str_formatter() in
      TypeValue.pp Format.str_formatter assign;
      let str2 = Format.flush_str_formatter() in
      Format.sprintf "Type '%s' is not assignable to type '%s'" str1 str2

    | CannotFindName name ->
      Format.sprintf "Can not find name '%s'" name

    | Redefinition name ->
      Format.sprintf "Redefinition of '%s'" name

    | NotCallable ty ->
      TypeValue.pp Format.str_formatter ty;
      let str = Format.flush_str_formatter() in
      Format.sprintf "Type '%s' is not callable" str

  let error err =
    let { spec; loc } = err in
    let open Loc in
    let spec_err = error_spec spec in
    Format.sprintf "Error: %d:%d %s" loc.start.line loc.start.column spec_err
  
end
