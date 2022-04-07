open Js_of_ocaml
open Lichenscript_lex
open Lichenscript_typing

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
        let path_str = Format.asprintf "%a" File_key.pp path in
        Js.def (Js.string path_str)
      | None -> Js.undefined

    val message = Js.string message

    val severity = severity_to_code severity

  end

  let completion_item_to_js (_val: Auto_complete.CompletionItem.t) =
    let open Js_of_ocaml in
    object%js
      val label = Js.string _val.label
      val kind = Auto_complete.CompletionItemKind.to_num _val.kind

    end
  
