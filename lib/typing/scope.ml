
type t = {
  id: int;
  symbols: (string, Symbol.t) Hashtbl.t;
}

let create id =
  {
    id;
    symbols = Hashtbl.create 8;
  }

let find_or_create_symbol scope name =
  let opt = Hashtbl.find_opt scope.symbols name in
  match opt with
  | Some sym -> sym
  | None ->
    let sym = Symbol.mk_local scope.id name in
    Hashtbl.add scope.symbols name sym;
    sym
