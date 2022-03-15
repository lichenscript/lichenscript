open Core_kernel
open Js_of_ocaml

let parse_errors_to_js_error raw_errors =
  let error_list = Js.array [||] in

  List.iteri
    ~f:(fun index err ->
      let err_content = Format.asprintf "%a" Lichenscript_parsing.Parse_error.PP.error err in

      let start = Js.array [||] in
      Js.array_set start 0 err.perr_loc.start.line;
      Js.array_set start 1 err.perr_loc.start.column;

      let _end = Js.array [||] in
      Js.array_set _end 0 err.perr_loc._end.line;
      Js.array_set _end 1 err.perr_loc._end.column;

      let err_obj = object%js
        val start = start
        val _end = _end
        val line = err.perr_loc.start.line
        val column = err.perr_loc.start.column
        val source =
          match err.perr_loc.source with
          | Some source ->
            let source_str = Format.asprintf "%a" Lichenscript_lex.File_key.pp source in
            Js.string source_str
          | None -> Js.string ""
        val content = Js.string err_content

      end in
      Js.array_set error_list index err_obj
    )
    raw_errors;

  let js_err = new%js Js.error_constr (Js.string "ParseError") in
  Js.Unsafe.set js_err (Js.string "errors") error_list;
  Js_error.of_error js_err
