

let codegen ~prog tree =
  let env = Codegen.codegen_program ~prog tree in
  Codegen.contents env
