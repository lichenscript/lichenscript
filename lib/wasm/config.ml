
type t = {
  stack_size: int;
  release: bool
}

let debug_default () =
  {
    stack_size = 4 * 1024 * 1024;  (* 4M *)
    release = false;
  }
