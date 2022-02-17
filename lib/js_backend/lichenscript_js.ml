

let codegen ~ctx tree =
  let env = Transpile.transpile_program ~ctx tree in
  Transpile.contents env
