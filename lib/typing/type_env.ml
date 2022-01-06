(*
 * record the dependency of types
 **)

(* num -> [] *)
let deps: int list ResizableArray.t = ResizableArray.make 1024

(* cached the reuslt *)
let cached = ResizableArray.make 1024

let new_id ?(ty=Core_type.TypeValue.Unknown) deps_int () =
  let id = ResizableArray.size deps in
  ResizableArray.push deps deps_int;
  ResizableArray.push cached ty;
  id

