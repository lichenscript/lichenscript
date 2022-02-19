

let codegen ~ctx ~preclude tree =
  let tree' = Normalize.normalize tree in
  let env = Transpile.transpile_program ~ctx ~preclude tree' in
  Transpile.contents env
