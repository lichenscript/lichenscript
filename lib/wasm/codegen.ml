
let codegen () =
  let m = C_bindings.make_module () in
  let none_ty = C_bindings.make_ty_none () in
  let lit = C_bindings.make_literal_i32 (Int32.of_int 100) in
  let exp = C_bindings.make_exp_const m lit in
  let _fun = C_bindings.add_function m "main" none_ty none_ty [| |] exp in
  let str = C_bindings.module_emit m in
  Format.printf "hello %s" str
  (* Binaryen.Module.with_module (fun _ ->
    let _ = C_bindings.make_literal_i32 100 in
    ()
    (* let str = Binaryen.Module.emit_test m in *)
  ) *)
