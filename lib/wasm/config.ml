
type arch =
  | ARCH_WASM32
  | ARCH_WASM64

type t = {
  arch: arch;
  stack_size: int;
  release: bool;
  data_segment_offset: int;
}

let debug_default () =
  {
    arch = ARCH_WASM32;
    stack_size = 4 * 1024 * 1024;  (* 4M *)
    release = false;
    data_segment_offset = 1024;
  }
