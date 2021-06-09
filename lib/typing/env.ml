
type t = {
  root_scope: Scope.t;
  mutable scope_counter: int;
}

let create () =
  let root_scope = Scope.create 0 in
  {
    root_scope;
    scope_counter = 1;
  }
