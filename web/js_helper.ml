open Js_of_ocaml
open Lichenscript_lex

type severity =
| Error
| Warning
| Information
| Hint

let severity_to_code = function
| Error -> 1
| Warning -> 2
| Information -> 3
| Hint -> 4

let mk_position (pos: Loc.position) =
  object%js

    val line = pos.line - 1

    val character = pos.column

  end

let mk_range (loc: Loc.t) =
  object%js

    val start = mk_position loc.start

    val _end = mk_position loc._end

  end

let mk_diagnostic ~loc severity message =
  object%js

    val range = mk_range loc

    val source =
      match loc.source with
      | Some path ->
        Js.def (Js.string (File_key.show path))
      | None -> Js.undefined

    val message = Js.string message

    val severity = severity_to_code severity

  end
