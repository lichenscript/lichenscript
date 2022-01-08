open Waterlang_lex
open Core_type

type t = {
  loc: Loc.t;
  spec: spec;
}

and spec =
  | NotAssignable of TypeValue.t * TypeValue.t
  | CannotReturn of TypeValue.t * TypeValue.t
  | CannotFindName of string
  | Redefinition of string
  | NotCallable of TypeValue.t
  | ParamsMismatch of TypeValue.t
  | CannotPassParam of string * TypeValue.t * TypeValue.t
  | CannotReadMember of string * TypeValue.t
  | NotAddable of TypeSym.t * TypeSym.t
  | CannotResolveTypeOfExpression

let make_error loc spec =
  { loc; spec }

exception Error of t

module PP = struct

  let error_spec formatter spec =
    match spec with
    | NotAssignable (be_assigned, assign) ->
      Format.fprintf formatter "Type '%a' is not assignable to type '%a'" TypeValue.pp be_assigned TypeValue.pp assign

    | CannotReturn (expected, actual) ->
      Format.fprintf formatter "Type '%a' can not be returned because '%a' is expected" TypeValue.pp actual TypeValue.pp expected

    | CannotFindName name ->
      Format.fprintf formatter "Can not find name '%s'" name

    | Redefinition name ->
      Format.fprintf formatter "Redefinition of '%s'" name

    | NotCallable ty ->
      Format.fprintf formatter "Type '%a' is not callable" TypeValue.pp ty 

    | ParamsMismatch ty ->
      Format.fprintf formatter "Params provided are mismatched with definition '%a'" TypeValue.pp ty 

    | CannotPassParam(name, def_param, actual_param) ->
      Format.fprintf formatter "Can not pass '%a' as param '%s', because '%a' is expected"
        TypeValue.pp actual_param name TypeValue.pp def_param

    | CannotReadMember(name, ty) ->
      Format.fprintf formatter "Can not read member '%s' of type '%a'"
        name TypeValue.pp ty

    | NotAddable (left ,right) ->
      Format.fprintf formatter "Type '%a' can not add '%a'"
        TypeSym.pp left TypeSym.pp right

    | CannotResolveTypeOfExpression ->
      Format.fprintf formatter "Can not resolve type of expression"

  let error formatter err =
    let { spec; loc } = err in
    let open Loc in
    Format.fprintf formatter "Error: %d:%d %a" loc.start.line loc.start.column error_spec spec 
  
end
