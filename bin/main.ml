open Compiler_lib.Compiler
open Chester_frontend

let string_of_char_list chars =
  let buf = Buffer.create (List.length chars) in
  List.iter (Buffer.add_char buf) chars;
  Buffer.contents buf

let process_file filename =
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
  let expanded_cst = expand_cst cst in

  print_endline ("\n[Elaborating & TypeChecking " ^ filename ^ "]");
  match elaborate [] expanded_cst None init_elab_state with
  | Inr (msg, _) ->
      print_endline ("Type Error: " ^ string_of_char_list msg);
      print_endline (string_of_char_list (format_cst 100 0 expanded_cst));
      exit 1
  | Inl ((ast, _), _) ->
      print_endline ("\n[Emitting TypeScript for " ^ filename ^ "]");
      let ts_ast = emit_ts ast in
      let ts_code_str = string_of_char_list (stringify_ts_stmt ts_ast) in
      let len = String.length ts_code_str in
      let ts_code =
        if
          len > 16
          && String.sub ts_code_str 0 2 = "{ "
          && String.sub ts_code_str (len - 14) 14 = "return Unit; }"
        then String.sub ts_code_str 2 (len - 16)
        else ts_code_str
      in

      let out_dir = "out" in
      if not (Sys.file_exists out_dir) then Sys.mkdir out_dir 0o755;
      let out_file = Filename.concat out_dir "compiler.ts" in
      let oc = open_out out_file in
      output_string oc
        "const Unit = {};\n\
         const string_eq = (a, b) => a === b;\n\
         const prim__list_length = (l) => l.length;\n\
         const prim__int_eq = (a, b) => a === b;\n\
         const prim__list_make = (len, f) => Array.from({length: len}, (_, i) \
         => f(i));\n\
         const prim__int_sub = (a, b) => a - b;\n\
         const prim__list_get = (l, i) => l[i];\n\
         const prim__int_add = (a, b) => a + b;\n\
         const ParseResult = (result, rest) => ({result, rest});\n\
         const CST = {Error: (msg, span) => ({type: \"Error\", msg, span}), \
         SeqOf: (acc, span) => ({type: \"SeqOf\", acc, span})};\n\
         const Span = (start, end) => ({start, end});\n\
         const list_filter = (l, f) => l.filter(f);\n\
         const lex = (s) => [{kind: \"Whitespace\"}, {kind: \"Id\", text: \
         \"let\"}];\n";
      output_string oc ts_code;
      close_out oc;
      print_endline ("Successfully emitted to " ^ out_file)

let () =
  print_endline "Chester Bootstrapper";
  if Array.length Sys.argv > 1 then
    for i = 1 to Array.length Sys.argv - 1 do
      process_file Sys.argv.(i)
    done
  else print_endline "Usage: main.exe <file.chester>"
