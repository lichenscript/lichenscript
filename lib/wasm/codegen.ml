
let codegen () =
  Binaryen.Module.with_module (fun m ->
    let _ = Binaryen.Literal.mk_int32 (Int32.of_int 100) in
    let str = Binaryen.Module.emit_test m in
    Format.printf "hello %s" str
  )
