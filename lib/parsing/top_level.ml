open Core_kernel

(*
 * Collect all top-level definition while parsing
 * to avoid scan again in annotation stage.
 *)
type t = {
  names: (string, Asttypes.visibility) Hashtbl.t;
}

let create () = {
  names = Hashtbl.create(module String);
}

let pp formatter _ = Format.fprintf formatter "top_level"
