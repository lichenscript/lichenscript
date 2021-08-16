open Core

let compile filename =
  printf "Compiling %s\n" filename

let command =
  Command.basic
    ~summary:"Compiling Waterlang"
    ~readme:(fun () -> "More detailed information")
    Command.Param.(
     map (anon ("filename" %: string))
       ~f:(fun filename -> (fun () ->compile filename)))

let () = 
  Command.run ~version:"1.0" ~build_info:"RWO" command
