open Core_kernel
open Lichenscript_lex

type top_level_item = {
  item_name: string;
  item_loc: Loc.t;
  item_visibility: Asttypes.visibility option;
}

(*
 * Collect all top-level definition while parsing
 * to avoid scan again in annotation stage.
 *)
type t = {
  names: (string, top_level_item) Hashtbl.t;
}

let create () = {
  names = Hashtbl.create(module String);
}

let pp formatter _ = Format.fprintf formatter "top_level"
