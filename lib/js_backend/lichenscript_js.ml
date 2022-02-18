

let codegen ~ctx ~preclude tree =
  let env = Transpile.transpile_program ~ctx ~preclude tree in
  Transpile.contents env
