open Compiler_lib.Compiler
open Chester_frontend

let string_of_char_list chars =
  let buf = Buffer.create (List.length chars) in
  List.iter (Buffer.add_char buf) chars;
  Buffer.contents buf

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
