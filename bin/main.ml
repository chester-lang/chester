open Compiler_lib.Compiler
open Chester_frontend

let string_of_char_list chars =
  let buf = Buffer.create (List.length chars) in
  List.iter (Buffer.add_char buf) chars;
  Buffer.contents buf

let preamble =
  "const Unit = {};\n\
   const prim__string_eq = (a, b) => a === b;\n\
   const prim__list_length = (l) => l.length;\n\
   const prim__int_eq = (a, b) => a === b;\n\
   const prim__list_make = (len, f) => Array.from({length: len}, (_, i) => \
   f(i));\n\
   const prim__int_sub = (a, b) => a - b;\n\
   const prim__list_get = (l, i) => l[i];\n\
   const prim__int_add = (a, b) => a + b;\n\
   const prim__int_lt = (a, b) => a < b;\n\
   const prim__string_length = (s) => s.length;\n\
   const prim__string_substring = (s, start, end) => s.substring(start, end);\n\
   const prim__string_concat = (s1, s2) => s1 + s2;\n\
   const prim__list_empty = () => [];\n\
   const prim__int_mul = (a, b) => a * b;\n\
   const prim__int_div = (a, b) => Math.floor(a / b);\n\
   const prim__int_mod = (a, b) => ((a % b) + b) % b;\n\
   const prim__int_gt = (a, b) => a > b;\n\
   const prim__int_ge = (a, b) => a >= b;\n\
   const prim__int_le = (a, b) => a <= b;\n\
   const prim__int_neg = (a) => -a;\n\
   const prim__int_to_string = (n) => String(n);\n\
   let _elab_state = null;\n\
   const prim__get_elab_state = () => _elab_state;\n\
   const prim__put_elab_state = (s) => { _elab_state = s; return Unit; };\n\
   const ParseResult = (result, rest) => ({result, rest});\n\
   const Span = (start, end) => ({start, end});\n\
   const lex = (s) => [{kind: \"Whitespace\"}, {kind: \"Id\", text: \"let\"}];\n"

let process_file filename oc =
  let ch = open_in filename in
  let len = in_channel_length ch in
  let buf = Bytes.create len in
  really_input ch buf 0 len;
  close_in ch;
  let source = Bytes.to_string buf in

  print_endline ("\n[Parsing " ^ filename ^ "]");
  let tokens = Lexer.tokenize filename source in
  let cst = parse tokens in

  print_endline ("\n[Expanding " ^ filename ^ "]");
  let expanded_cst = expand_cst_top cst in

  print_endline (string_of_char_list (format_cst 100 0 expanded_cst));
  print_endline ("\n[Elaborating & TypeChecking " ^ filename ^ "]");
  match elaborate_top [] expanded_cst None init_elab_state with
  | Inr (msg, _) ->
      print_endline ("Type Error: " ^ string_of_char_list msg);
      print_endline (string_of_char_list (format_cst 100 0 expanded_cst));
      exit 1
  | Inl ((ast, _), _) ->
      print_endline ("\n[Emitting TypeScript for " ^ filename ^ "]");
      let ts_ast = emit_ts ast in
      let ts_code = string_of_char_list (stringify_ts_stmt ts_ast) in
      output_string oc ts_code

let () =
  print_endline "Chester Bootstrapper";
  if Array.length Sys.argv > 1 then begin
    let out_dir = "out" in
    if not (Sys.file_exists out_dir) then Sys.mkdir out_dir 0o755;
    let out_file = Filename.concat out_dir "compiler.ts" in
    let oc = open_out out_file in
    output_string oc preamble;
    for i = 1 to Array.length Sys.argv - 1 do
      process_file Sys.argv.(i) oc
    done;
    close_out oc;
    print_endline ("\nSuccessfully emitted to " ^ out_file)
  end
  else print_endline "Usage: main.exe <file.chester> [file2.chester ...]"
