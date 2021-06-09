
type kind =
  | Local
  | Global

type t = {
  name:     string;
  kind:     kind;
  scope_id: int;
}
