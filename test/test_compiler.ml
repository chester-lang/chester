open Compiler_lib.Compiler
open Chester_frontend
open Effects_runtime

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
  let expanded_cst = expand_cst_top cst in
  match elaborate_top [] expanded_cst None init_elab_state with
  | Inr (msg, _) -> failwith ("Type Error: " ^ string_of_char_list msg)
  | Inl ((ast, _), _) ->
      let ts_ast = emit_ts ast in
      let ts_code = string_of_char_list (stringify_ts_stmt ts_ast) in
      if ts_code = "" then failwith "empty TypeScript output";
      let go_ast = emit_go ast in
      let go_code = string_of_char_list (stringify_go_stmt go_ast) in
      if go_code = "" then failwith "empty Go output";
      let rocq_ast = emit_rocq_top ast in
      let rocq_code = string_of_char_list (stringify_rocq_stmt rocq_ast) in
      if rocq_code = "" then failwith "empty Rocq output";
      print_endline (filename ^ " ok")

let expect_type_error filename =
  let source = read_file (fixture_path filename) in
  let tokens = Lexer.tokenize filename source in
  let cst = parse tokens in
  let expanded_cst = expand_cst_top cst in
  match elaborate_top [] expanded_cst None init_elab_state with
  | Inr (msg, _) -> print_endline (string_of_char_list msg)
  | Inl _ -> failwith ("expected type error for " ^ filename)

let compile_fixture_ast filename =
  let source = read_file (fixture_path filename) in
  let tokens = Lexer.tokenize filename source in
  let cst = parse tokens in
  let expanded_cst = expand_cst_top cst in
  match elaborate_top [] expanded_cst None init_elab_state with
  | Inr (msg, _) -> failwith ("Type Error: " ^ string_of_char_list msg)
  | Inl ((ast, _), _) -> ast

let list_selfhosted_sources () =
  let dir = fixture_path "self-hosted" in
  Sys.readdir dir |> Array.to_list
  |> List.filter (fun f -> Filename.check_suffix f ".chester")
  |> List.sort compare

let check_selfhosted_sources () =
  List.iter
    (fun filename ->
      let path = Filename.concat "self-hosted" filename in
      compile_fixture_ast path |> ignore;
      print_endline (path ^ " ok"))
    (list_selfhosted_sources ())

let run_fixture_main filename =
  let ast = compile_fixture_ast filename in
  let ts_code = string_of_char_list (stringify_ts_stmt (emit_ts ast)) in
  let js = ts_test_preamble ^ ts_code ^ "\n;console.log(main());" in
  let tmp = Filename.temp_file "chester_fx" ".mjs" in
  let out = Filename.temp_file "chester_fx_out" ".txt" in
  let oc = open_out tmp in
  output_string oc js;
  close_out oc;
  let st =
    Sys.command
      (Printf.sprintf "node %s > %s 2>&1" (Filename.quote tmp) (Filename.quote out))
  in
  let line =
    let ic = open_in out in
    let l = try input_line ic with End_of_file -> "" in
    close_in ic;
    l
  in
  Sys.remove tmp;
  Sys.remove out;
  if st <> 0 then failwith ("node failed: " ^ line);
  print_endline line

let has_substr hay needle =
  let n = String.length needle in
  let rec loop i =
    if i + n > String.length hay then false
    else if String.sub hay i n = needle then true
    else loop (i + 1)
  in
  loop 0

let rename_chester_main go_code =
  let needle = "func main(" in
  let repl = "func chester_main(" in
  let n = String.length needle in
  let rec find i =
    if i + n > String.length go_code then None
    else if String.sub go_code i n = needle then Some i
    else find (i + 1)
  in
  match find 0 with
  | None -> go_code
  | Some i ->
      String.sub go_code 0 i ^ repl
      ^ String.sub go_code (i + n) (String.length go_code - i - n)

let assemble_go_program ast =
  let body =
    rename_chester_main (string_of_char_list (stringify_go_stmt (emit_go_top ast)))
  in
  go_effects_preamble ^ "\n" ^ body ^ "\nfunc main() {\n\tfmt.Println(chester_main())\n}\n"

let run_fixture_go filename =
  let ast = compile_fixture_ast filename in
  let dir = Filename.temp_file "chester_go" "" in
  Sys.remove dir;
  Sys.mkdir dir 0o755;
  let path = Filename.concat dir "main.go" in
  let oc = open_out path in
  output_string oc (assemble_go_program ast);
  close_out oc;
  let out = Filename.temp_file "chester_go_out" ".txt" in
  let st =
    Sys.command
      (Printf.sprintf "cd %s && go run main.go > %s 2>&1" (Filename.quote dir)
         (Filename.quote out))
  in
  let output = read_file out in
  Sys.remove out;
  let _ = Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote dir)) in
  if st <> 0 then failwith ("go failed:\n" ^ output);
  let line =
    try
      let idx = String.index output '\n' in
      String.sub output 0 idx
    with Not_found -> output
  in
  print_endline line

let format_source source =
  let tokens = Lexer.tokenize "test.chester" source in
  let cst = parse tokens in
  string_of_char_list (format_program 100 cst)

let check_format source =
  let formatted = format_source source in
  if format_source formatted <> formatted then
    failwith "formatter output is not idempotent";
  print_endline formatted

let%expect_test "parse block" =
  let source = "let x = 5;" in
  let tokens = Lexer.tokenize "test.chester" source in
  let cst = parse tokens in
  print_endline (string_of_char_list (format_cst 100 0 cst));
  [%expect {|
    {
      let x = 5;
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
      def foo(a, b) = {
        a + b;
        @ @;
        Unit
      };
      [1, 2, 3];
    }
    |}]

let%expect_test "format program from parser cst" =
  check_format
    "extension ListExt[T] on List T { def get(self: List T, idx: Integer): T = \
     list_get(self, idx); };\n\
     let xs=[1,2,3];\n\
     xs.get(0);\n";
  [%expect
    {|
    extension ListExt[T] on List T {
      def get(self: List T, idx: Integer): T = list_get(self, idx);
    };
    let xs = [1, 2, 3];
    xs.get(0);
    |}]

let%expect_test "format comments from parser cst" =
  check_format
    "// file comment\n\
     let tmp=1;// inline\n\
     def f(x: Integer): Integer = if x then { 1 } else { 0 };\n";
  [%expect
    {|
    // file comment
    let tmp = 1;
    // inline
    def f(x: Integer): Integer = if x then {
      1
    } else {
      0
    };
    |}]

let%expect_test "format nested match blocks without arm semicolons" =
  check_format
    "def f(x: Integer) = {\n\
     match x {\n\
     case A => {\n\
     let y = 1;\n\
     }\n\
     case B => Unit\n\
     }\n\
     };\n";
  [%expect
    {|
    def f(x: Integer) = {
      match x {
        case A => {
          let y = 1;
        }
        case B => Unit
      }
    };
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

let%expect_test "fixture effects" =
  check_fixture "tests/effects.chester";
  [%expect {| tests/effects.chester ok |}]

let%expect_test "fixture effects state" =
  check_fixture "tests/effects_state.chester";
  [%expect {| tests/effects_state.chester ok |}]

let%expect_test "fixture effects nested" =
  check_fixture "tests/effects_nested.chester";
  [%expect {| tests/effects_nested.chester ok |}]

let%expect_test "fixture effects multishot" =
  check_fixture "tests/effects_multishot.chester";
  [%expect {| tests/effects_multishot.chester ok |}]

let%expect_test "runtime effects multishot fork" =
  run_fixture_main "tests/effects_multishot.chester";
  [%expect {| 11 |}]

let%expect_test "fixture effects box" =
  check_fixture "tests/effects_box.chester";
  [%expect {| tests/effects_box.chester ok |}]

let%expect_test "fixture effects evidence" =
  check_fixture "tests/effects_evidence.chester";
  [%expect {| tests/effects_evidence.chester ok |}]

let%expect_test "runtime effects evidence" =
  run_fixture_main "tests/effects_evidence.chester";
  [%expect {| 7 |}]

let%expect_test "fixture effects rows" =
  check_fixture "tests/effects_rows.chester";
  [%expect {| tests/effects_rows.chester ok |}]

let%expect_test "fixture react mini" =
  check_fixture "tests/react_mini.chester";
  [%expect {| tests/react_mini.chester ok |}]

let%expect_test "runtime effects rows" =
  run_fixture_main "tests/effects_rows.chester";
  [%expect {| 15 |}]

let%expect_test "unbox without handler is rejected" =
  expect_type_error "tests/effects_unbox_unhandled.chester";
  [%expect {| Unhandled effect: State |}]

let%expect_test "go emit effects handle" =
  let prog = assemble_go_program (compile_fixture_ast "tests/effects.chester") in
  print_endline
    (if has_substr prog "__chester_handle"
        && has_substr prog "__chester_perform"
        && has_substr prog "chester_main"
        && has_substr prog "package main"
        && has_substr prog "func main()"
     then "go effects ok"
     else "go effects missing");
  [%expect {| go effects ok |}]

let assemble_rocq_program ast =
  rocq_effects_preamble ^ "\n"
  ^ string_of_char_list (stringify_rocq_stmt (emit_rocq_top ast))
  ^ "\nDefinition chester_run := chester_main.\n"

let%expect_test "rocq emit effects handle" =
  let prog = assemble_rocq_program (compile_fixture_ast "tests/effects.chester") in
  print_endline
    (if has_substr prog "__chester_handle"
        && has_substr prog "__chester_perform"
        && has_substr prog "chester_main"
        && has_substr prog "chester_dyn"
     then "rocq effects ok"
     else "rocq effects missing");
  [%expect {| rocq effects ok |}]

let%expect_test "runtime go effects" =
  run_fixture_go "tests/effects.chester";
  [%expect {| 42 |}]

let%expect_test "runtime go effects multishot fork" =
  run_fixture_go "tests/effects_multishot.chester";
  [%expect {| 11 |}]

let%expect_test "runtime go effects evidence" =
  run_fixture_go "tests/effects_evidence.chester";
  [%expect {| 7 |}]

let%expect_test "runtime go effects rows" =
  run_fixture_go "tests/effects_rows.chester";
  [%expect {| 15 |}]

let%expect_test "runtime go effects box" =
  run_fixture_go "tests/effects_box.chester";
  [%expect {| 5 |}]

let%expect_test "runtime go effects nested" =
  run_fixture_go "tests/effects_nested.chester";
  [%expect {| 10 |}]

let%expect_test "runtime go effects state" =
  run_fixture_go "tests/effects_state.chester";
  [%expect {| 2 |}]

let%expect_test "example go simple" =
  run_fixture_go "examples/go/simple.chester";
  [%expect {| 42 |}]

let%expect_test "example go hello" =
  run_fixture_go "examples/go/hello.chester";
  [%expect {| Hello from Chester with Go FFI |}]

let%expect_test "example ts simple" =
  run_fixture_main "examples/ts/simple.chester";
  [%expect {| 42 |}]

let%expect_test "example ts hello" =
  run_fixture_main "examples/ts/hello.chester";
  [%expect {| Hello from Chester on Node! |}]

let%expect_test "example ts greet" =
  run_fixture_main "examples/ts/greet.chester";
  [%expect {| Chester |}]

let%expect_test "example ts wordcount" =
  check_fixture "examples/ts/wordcount.chester";
  [%expect {| examples/ts/wordcount.chester ok |}]

let%expect_test "example ts path" =
  check_fixture "examples/ts/path.chester";
  [%expect {| examples/ts/path.chester ok |}]

let%expect_test "infix precedence" =
  run_fixture_main "tests/infix_precedence.chester";
  [%expect {| 10 |}]

let%expect_test "go signatures json loads" =
  let sigs = Go_signatures.load (fixture_path "data/go-signatures.json") in
  if not (Go_signatures.has_function sigs "fmt" "Println") then
    failwith "missing fmt.Println in go-signatures.json";
  print_endline
    (Printf.sprintf "go-sigs ok (%d functions)"
       (Go_signatures.function_count sigs));
  [%expect {| go-sigs ok (24 functions) |}]

let%expect_test "example go wordcount" =
  check_fixture "examples/go/wordcount.chester";
  [%expect {| examples/go/wordcount.chester ok |}]

let%expect_test "stdlib elaborates" =
  check_fixture "stdlib/std.chester";
  [%expect {| stdlib/std.chester ok |}]

let%expect_test "cli prelude chains definitions" =
  let root = repo_root (Sys.getcwd ()) in
  let main_bin = Filename.concat root "_build/default/bin/main.exe" in
  let prelude = Filename.concat root "tests/prelude_mini.chester" in
  let src = Filename.concat root "tests/use_prelude_mini.chester" in
  let out = Filename.temp_file "chester_prelude_go" ".go" in
  let st =
    Sys.command
      (Printf.sprintf
         "%s --go --prelude %s -o %s %s > /dev/null 2>&1"
         (Filename.quote main_bin) (Filename.quote prelude) (Filename.quote out)
         (Filename.quote src))
  in
  let code = read_file out in
  Sys.remove out;
  if st <> 0 then failwith "prelude compile failed";
  print_endline (if has_substr code "int_add(forty(), 2)" then "prelude ok" else "prelude missing");
  [%expect {| prelude ok |}]

let%expect_test "self-hosted sources elaborate" =
  check_selfhosted_sources ();
  [%expect {|
    self-hosted/ast.chester ok
    self-hosted/cst.chester ok
    self-hosted/elaborator.chester ok
    self-hosted/expander.chester ok
    self-hosted/formatter.chester ok
    self-hosted/lexer.chester ok
    self-hosted/parser.chester ok
    |}]

let%expect_test "react mini ts emit" =
  let ast = compile_fixture_ast "tests/react_mini.chester" in
  let ts_code = string_of_char_list (stringify_ts_stmt (emit_ts_top ast)) in
  print_endline
    (if has_substr ts_code "import { createElement } from \"react\""
        && has_substr ts_code "export function Counter"
        && has_substr ts_code "export function main"
     then "react mini ts ok"
     else "react mini ts missing");
  [%expect {| react mini ts ok |}]

let check_ts_typecheck emitted_ts =
  let ts_dir = fixture_path "test/ts" in
  let out = Filename.temp_file "chester_tsc_out" ".txt" in
  let gen = Filename.concat ts_dir "generated.ts" in
  let oc = open_out gen in
  output_string oc emitted_ts;
  close_out oc;
  let st =
    Sys.command
      (Printf.sprintf
         "cd %s && (test -d node_modules || npm install --silent) && npx tsc --noEmit > %s 2>&1"
         (Filename.quote ts_dir) (Filename.quote out))
  in
  let msg = read_file out in
  Sys.remove out;
  if st <> 0 then failwith ("tsc failed:\n" ^ msg);
  print_endline "tsc ok"

let%expect_test "react mini tsc smoke" =
  let ast = compile_fixture_ast "tests/react_mini.chester" in
  let ts_code = string_of_char_list (stringify_ts_stmt (emit_ts_top ast)) in
  check_ts_typecheck ts_code;
  [%expect {| tsc ok |}]

let%expect_test "bindgen react mini d.ts" =
  let root = repo_root (Sys.getcwd ()) in
  let scripts_dir = Filename.concat root "scripts" in
  let fixture = Filename.concat root "test/bindgen/fixtures/react_mini.d.ts" in
  let expected_path = Filename.concat root "test/bindgen/expected/react_mini.chester" in
  let out = Filename.temp_file "chester_bg" ".chester" in
  let script = Filename.concat root "scripts/dts2chester.mjs" in
  let st =
    Sys.command
      (Printf.sprintf
         "cd %s && (test -d node_modules || npm install --silent) && node %s \
          --package react --input %s --output %s --filter 'createElement|cloneElement'"
         (Filename.quote scripts_dir) (Filename.quote script) (Filename.quote fixture)
         (Filename.quote out))
  in
  if st <> 0 then failwith "bindgen failed";
  let got = read_file out in
  let expected = read_file expected_path in
  Sys.remove out;
  if got <> expected then
    failwith ("bindgen mismatch:\n" ^ got ^ "\nexpected:\n" ^ expected);
  print_endline "bindgen ok";
  [%expect {| bindgen ok |}]

let%expect_test "counter example vite build" =
  let root = repo_root (Sys.getcwd ()) in
  let main_bin = Filename.concat root "_build/default/bin/main.exe" in
  let out = Filename.temp_file "chester_counter_build" ".txt" in
  let st =
    Sys.command
      (Printf.sprintf
         "cd %s && CHESTER_MAIN=%s bash examples/counter/build-chester.sh > %s 2>&1 && \
          cd examples/counter && (test -d node_modules || npm install --silent) && \
          npm run build:app >> %s 2>&1"
         (Filename.quote root) (Filename.quote main_bin) (Filename.quote out)
         (Filename.quote out))
  in
  let msg = read_file out in
  Sys.remove out;
  if st <> 0 then failwith ("counter example build failed:\n" ^ msg);
  print_endline "counter build ok";
  [%expect {| counter build ok |}]
