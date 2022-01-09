

let codegen ~ctx tree =
  let env = Codegen.codegen_program ~ctx tree in
  Codegen.contents env
