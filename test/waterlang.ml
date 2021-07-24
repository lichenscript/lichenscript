
open OUnit2
open Waterlang_lex
open Waterlang_parsing

let temp_dir_name = "/tmp/test_waterlang/"

let test_parser _ =
  let result =  Parser.parse_string None "

  @external(\"env\", \"console_log\")
  declare function print(content: string);

  class Array {
    private __name: sring;
  }

  enum Result {
    Ok,
    Error,
  }

  @export
  function main(args, args2: string, ...rest) {
    let name = 3;
    name = 4;
  }
  " in
  match result with
  | Result.Ok _program ->
    (* Ast.pp_program Format.std_formatter program; *)
    let _env = Waterlang_typing.Env.create () in
    (* let program = Waterlang_typing.Annotate.annotate env program in
    Waterlang_typing.Typecheck.type_check env program; *)
    assert true

  | Result.Error errs ->
    errs
    |> List.rev
    |> List.iter
       (fun error ->
         let str = Parse_error.PP.error error in
         let { Loc. line; column; } = error.perr_loc.start in
         Format.printf "%d:%d %s\n" line column str;
        );
    assert false

let test_codegen _ =
  let source = "
    function main(a: i32, b: i32): i32 {
      return a + b;
    }
    "
  in
  let result = Utils.parse_string_and_codegen source in
  Format.printf "%s" result

let test_codegen_binary _ =
  let source = "
    @export
    function main(a: i32, b: i32): i32 {
      return a + b;
    }
    "
  in
  Utils.parse_string_and_codegen_to_path source "test.wasm"

let test_type_checking _ =
  let source = "
    function main(a: i32, b: i32): f32 {
      return a + b;
    }
    "
  in
  assert_raises
    (Utils.ExpectedError "Error: 3:6 Type 'i32' can not be returned because 'f32' is expected\n")
    (fun _ ->
      Utils.parse_string_and_codegen_to_path source "type_check.wasm"
    )

let test_function_call _ =
  let source = "
    function add(a: i32, b: i32): i32 {
      return a + b;
    }

    function main(a: i32, b: i32): i32 {
      return add(a, b);
    }
    "
  in
  let _result = Utils.parse_string_and_codegen source in
  ()
  (* Format.printf "%s" result *)

let test_string _ =
  let source = "
    @external(\"env\", \"console_log\")
    declare function print(content: string)

    function main() {
      let a: string = \"Hello World!\";
      print(a);
    }
  "
  in
  Core.Unix.mkdir_p temp_dir_name;
  let test_output_name = temp_dir_name ^ "test_wtl" in
  Format.printf "output name: %s" test_output_name;
  Utils.parse_string_and_codegen_to_path source test_output_name;
  let in_chan = Core.Unix.open_process_in ("node " ^ test_output_name ^ ".js" ) in
  let r = Core.In_channel.input_all in_chan in
  Core.In_channel.close in_chan;
  assert_equal r "Hello World!\n"

let test_assignment _ =
  let source = "
    function main() {
      let a: i32 = 3;
      a = 4;
    }
  "
  in
  let result = Utils.parse_string_and_codegen source in
  Format.printf "%s" result

let suite =
  "TestParser" >::: [
    "test_parser" >:: test_parser;
    "test_codegen" >:: test_codegen;
    "test_codegen_binary" >:: test_codegen_binary;
    "test_type_checking" >:: test_type_checking;
    "test_function_call" >:: test_function_call;
    "test_string" >:: test_string;
    "test_assignment" >:: test_assignment;
  ]

let () =
  run_test_tt_main suite
