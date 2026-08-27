open Compiler_lib.Compiler
open Chester_frontend
open Effects_runtime

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
   const prim__int_to_string = (n) => String(n);\n"
  ^ ts_effects_runtime
  ^ "const int_add = prim__int_add;\n\
   const int_eq = prim__int_eq;\n\
   let _elab_state = null;\n\
   const prim__get_elab_state = () => _elab_state;\n\
   const prim__put_elab_state = (s) => { _elab_state = s; return Unit; };\n\
   const ParseResult = (result, rest) => ({result, rest});\n\
   const Span = (start, end) => ({start, end});\n\
   const lex = (s) => [{kind: \"Whitespace\"}, {kind: \"Id\", text: \"let\"}];\n"

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

let process_file ~target_go filename oc state =
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
  match elaborate_top [] expanded_cst None state with
  | Inr (msg, _) ->
      print_endline ("Type Error: " ^ string_of_char_list msg);
      print_endline (string_of_char_list (format_cst 100 0 expanded_cst));
      exit 1
  | Inl ((ast, _), state') ->
      if target_go then begin
        print_endline ("\n[Emitting Go for " ^ filename ^ "]");
        let go_code =
          rename_chester_main (string_of_char_list (stringify_go_stmt (emit_go_top ast)))
        in
        output_string oc go_code
      end
      else begin
        print_endline ("\n[Emitting TypeScript for " ^ filename ^ "]");
        let ts_code = string_of_char_list (stringify_ts_stmt (emit_ts ast)) in
        output_string oc ts_code
      end;
      state'

let () =
  print_endline "Chester Bootstrapper";
  let args = List.tl (Array.to_list Sys.argv) in
  let target_go, files =
    let rec split go acc = function
      | [] -> (go, List.rev acc)
      | "--go" :: rest -> split true acc rest
      | f :: rest -> split go (f :: acc) rest
    in
    split false [] args
  in
  match files with
  | [] ->
      print_endline "Usage: main.exe [--go] <file.chester> [file2.chester ...]"
  | _ ->
      let out_dir = "out" in
      if not (Sys.file_exists out_dir) then Sys.mkdir out_dir 0o755;
      let out_file =
        Filename.concat out_dir (if target_go then "compiler.go" else "compiler.ts")
      in
      let oc = open_out out_file in
      if target_go then begin
        output_string oc go_effects_preamble;
        output_string oc "\n"
      end
      else output_string oc preamble;
      let state = ref init_elab_state in
      List.iter (fun f -> state := process_file ~target_go f oc !state) files;
      if target_go then
        output_string oc
          "\nfunc main() {\n\tfmt.Println(chester_main())\n}\n";
      close_out oc;
      print_endline ("\nSuccessfully emitted to " ^ out_file)
