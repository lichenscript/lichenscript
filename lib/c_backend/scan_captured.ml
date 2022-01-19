(*
 * Thid file is used to scan the
 * lambda capturing in a function
 *
 * function fun1() {
 *
 * }
 *
 * function fun2() {
 *   const a1 = 0;
 *   let a2 = 0;
 *   return () => {
 *     fun1();    // external call, nothing is captured
 *     print(a1); // capture immutable value, only capture the value
 *     a2 = 3;    // capture mutable value, turn a2 into RefCell
 *   }
 * }
 *
 *)

open Core_kernel
open Lichenscript_typing

type capture =
  | Cap_immutable
  | Cap_mutable

type t = {
  ctx: Type_context.t;
  data: (capture option) Array.t;
}

let create ctx =
  let open Type_context in
  let ctx_len = ResizableArray.size ctx.ty_map in
  let data = Array.create ~len:ctx_len None in
  {
    ctx;
    data;
  }

let scan_function _env _fun =
  ()
