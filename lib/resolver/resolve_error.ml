open Lichenscript_lex

type spec =
  | Redeclared of string
  | CannotResolve of string
  | CannotfindExtOfUnivertialImport of string

type t = {
  spec: spec;
  loc: Loc.t;
}

let pp_spec formatter = function
  | Redeclared name ->
    Format.fprintf formatter "'%s' redecalred in this module" name

  | CannotResolve name ->
    Format.fprintf formatter "Can not resolve symbol '%s'" name

  | CannotfindExtOfUnivertialImport path ->
    Format.fprintf formatter "Can get the extension of a universal import: '%s'" path
