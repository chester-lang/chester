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
      print_endline (filename ^ " ok")

let run_fixture_main filename =
  let source = read_file (fixture_path filename) in
  let tokens = Lexer.tokenize filename source in
  let cst = parse tokens in
  let expanded_cst = expand_cst_top cst in
  match elaborate_top [] expanded_cst None init_elab_state with
  | Inr (msg, _) -> failwith ("Type Error: " ^ string_of_char_list msg)
  | Inl ((ast, _), _) ->
      let ts_ast = emit_ts ast in
      let ts_code = string_of_char_list (stringify_ts_stmt ts_ast) in
      let preamble =
        "const Unit = {};\n\
         const prim__string_eq = (a, b) => a === b;\n\
         const prim__int_add = (a, b) => a + b;\n\
         const int_add = prim__int_add;\n\
         let __chester_caps = [];\n\
         const __chester_handle = (label, bodyFn, handlers) => {\n\
           const run = (answers) => {\n\
             let ai = 0;\n\
             const frame = {\n\
               label, handlers,\n\
               take: () => {\n\
                 if (ai < answers.length) return { ok: true, v: answers[ai++] };\n\
                 return { ok: false };\n\
               },\n\
               fork: (v) => run(answers.slice(0, ai).concat([v]))\n\
             };\n\
             __chester_caps.push(frame);\n\
             try { return bodyFn(); }\n\
             catch (e) {\n\
               if (e && e.__chester_handled === frame) return e.result;\n\
               throw e;\n\
             }\n\
             finally { __chester_caps.pop(); }\n\
           };\n\
           return run([]);\n\
         };\n\
         const __chester_perform = (op, args) => {\n\
           for (let i = __chester_caps.length - 1; i >= 0; i--) {\n\
             const frame = __chester_caps[i];\n\
             const h = frame.handlers[op];\n\
             if (!h) continue;\n\
             const got = frame.take();\n\
             if (got.ok) return got.v;\n\
             const resume = (v) => frame.fork(v);\n\
             let fn = h;\n\
             for (let j = 0; j < args.length; j++) fn = fn(args[j]);\n\
             const result = fn(resume);\n\
             throw { __chester_handled: frame, result };\n\
           }\n\
           throw new Error(\"Unhandled effect operation: \" + op);\n\
         };\n\
         const __chester_evidence = (labels) => {\n\
           const ev = [];\n\
           for (let li = 0; li < labels.length; li++) {\n\
             const lab = labels[li];\n\
             for (let i = __chester_caps.length - 1; i >= 0; i--) {\n\
               if (__chester_caps[i].label === lab) {\n\
                 ev.push({ label: lab, handlers: __chester_caps[i].handlers });\n\
                 break;\n\
               }\n\
             }\n\
           }\n\
           return ev;\n\
         };\n\
         const __chester_with_evidence = (ev, bodyFn) => {\n\
           let i = 0;\n\
           const go = () => {\n\
             if (i >= ev.length) return bodyFn();\n\
             const { label, handlers } = ev[i++];\n\
             return __chester_handle(label, go, handlers);\n\
           };\n\
           return go();\n\
         };\n\
         const __chester_box = (labels, bodyFn) => {\n\
           const ev = __chester_evidence(labels);\n\
           return () => __chester_with_evidence(ev, bodyFn);\n\
         };\n"
      in
      let js = preamble ^ ts_code ^ "\n;console.log(main());" in
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

let%expect_test "runtime effects rows" =
  run_fixture_main "tests/effects_rows.chester";
  [%expect {| 15 |}]

let%expect_test "go emit effects handle" =
  let source = read_file (fixture_path "tests/effects.chester") in
  let tokens = Lexer.tokenize "tests/effects.chester" source in
  let cst = parse tokens in
  let expanded_cst = expand_cst_top cst in
  match elaborate_top [] expanded_cst None init_elab_state with
  | Inr (msg, _) -> failwith ("Type Error: " ^ string_of_char_list msg)
  | Inl ((ast, _), _) ->
      let go_code = string_of_char_list (stringify_go_stmt (emit_go ast)) in
      let has s =
        let n = String.length s in
        let rec loop i =
          if i + n > String.length go_code then false
          else if String.sub go_code i n = s then true
          else loop (i + 1)
        in
        loop 0
      in
      print_endline
        (if String.length go_code > 0 && has "__chester_handle" && has "__chester_perform"
         then "go effects ok"
         else "go effects missing");
      [%expect {| go effects ok |}]
