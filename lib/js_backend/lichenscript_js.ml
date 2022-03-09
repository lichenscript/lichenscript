

let codegen ~prog ~preclude tree =
  let env = Transpile.transpile_program ~prog ~preclude tree in
  Transpile.contents env
