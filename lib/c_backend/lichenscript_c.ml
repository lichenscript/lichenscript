

let codegen ~prog ~includes tree =
  let env = Codegen.codegen_program ~prog ~includes tree in
  Codegen.contents env
