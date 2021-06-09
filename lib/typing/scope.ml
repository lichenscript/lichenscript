
type t = {
  id: int;
  symbols: (string, Symbol.t) Hashtbl.t;
}

let create id =
  {
    id;
    symbols = Hashtbl.create 8;
  }
