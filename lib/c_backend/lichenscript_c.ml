

let codegen ~prog ~includes ~init_calls tree =
  let env = Codegen.codegen_program ~prog ~includes ~init_calls tree in
  Codegen.contents env
