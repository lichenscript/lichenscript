
type kind =
  | Local
  | Global

type t = {
  name:     string;
  kind:     kind;
  scope_id: int;
}

let mk_local scope_id name =
  {
    name;
    kind = Local;
    scope_id;
  }
