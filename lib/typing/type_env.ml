(*
 * record the dependency of types
 **)

(* num -> [] *)
let type_env: ResizableArray.t = ResizableArray.make 1024

let new_id ty =
  let id = ResizableArray.size type_env in
  ResizableArray.push type_env ty;
  id

let update_node id node =
  ResizableArray.set type_env id node

let size () = ResizableArray.size type_env

let get_node id =
  ResizableArray.get type_env id
