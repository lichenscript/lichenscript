
type kind =
  | Local
  | Global

let counter = ref 0

type t = {
  id:       int;
  name:     string;
  kind:     kind;
  scope_id: int;
  builtin:  bool;
}

let mk_builtin_global ~scope_id name =
  let id = !counter in
  counter := id + 1;
  {
    id;
    name;
    kind = Global;
    scope_id;
    builtin = true;
  }

let mk_local ~scope_id name =
  let id = !counter in
  counter := id + 1;
  {
    id;
    name;
    kind = Local;
    scope_id;
    builtin = false;
  }
