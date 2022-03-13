

let codegen ~prog ~preclude ~init_calls tree =
  let env = Transpile.transpile_program ~prog ~preclude ~init_calls tree in
  Transpile.contents env
