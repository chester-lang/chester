open Compiler_lib.Compiler
open Chester_frontend

let string_of_char_list chars =
  let buf = Buffer.create (List.length chars) in
  List.iter (Buffer.add_char buf) chars;
  Buffer.contents buf

let rec repo_root dir =
  let fixture = Filename.concat dir "tests/macro_hygiene.chester" in
  if Sys.file_exists fixture then dir
  else
    let parent = Filename.dirname dir in
    if parent = dir then failwith "could not locate repository root"
    else repo_root parent

let fixture_path filename = Filename.concat (repo_root (Sys.getcwd ())) filename

let read_file filename =
  let ch = open_in filename in
  let len = in_channel_length ch in
  let buf = Bytes.create len in
  really_input ch buf 0 len;
  close_in ch;
  Bytes.to_string buf

let check_fixture filename =
  let source = read_file (fixture_path filename) in
  let tokens = Lexer.tokenize filename source in
  let cst = parse tokens in
  let expanded_cst = expand_cst cst in
  match elaborate [] expanded_cst None init_elab_state with
  | Inr (msg, _) -> failwith ("Type Error: " ^ string_of_char_list msg)
  | Inl ((ast, _), _) ->
      let ts_ast = emit_ts ast in
      let ts_code = string_of_char_list (stringify_ts_stmt ts_ast) in
      if ts_code = "" then failwith "empty TypeScript output";
      print_endline (filename ^ " ok")

let%expect_test "parse block" =
  let source = "let x = 5;" in
  let tokens = Lexer.tokenize "test.chester" source in
  let cst = parse tokens in
  print_endline (string_of_char_list (format_cst 100 0 cst));
  [%expect {|
    {
      let x = 5;
      Unit
    }
    |}]

let%expect_test "parse shape recovery" =
  let source = "def foo(a, b) = { a + b; @@; Unit }; [1, 2, 3];" in
  let tokens = Lexer.tokenize "test.chester" source in
  let cst = parse tokens in
  print_endline (string_of_char_list (format_cst 100 0 cst));
  [%expect
    {|
    {
      def foo (a, b) = {
        a + b;
        @ @;
        Unit;
        Unit
      };
      [1, 2, 3];
      Unit
    }
    |}]

let%expect_test "fixture macro hygiene" =
  check_fixture "tests/macro_hygiene.chester";
  [%expect {| tests/macro_hygiene.chester ok |}]

let%expect_test "fixture operators" =
  check_fixture "tests/operators.chester";
  [%expect {| tests/operators.chester ok |}]

let%expect_test "fixture extension method" =
  check_fixture "tests/test_ext.chester";
  [%expect {| tests/test_ext.chester ok |}]
