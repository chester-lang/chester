open Compiler_lib.Compiler
open Chester_frontend
open Effects_runtime

let string_of_char_list chars =
  let buf = Buffer.create (List.length chars) in
  List.iter (Buffer.add_char buf) chars;
  Buffer.contents buf

type emit_target = EmitTS | EmitGo | EmitRocq

type ts_emit_mode =
  | TsEmitScript
  | TsEmitModule

let preamble =
  ts_primitives
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

let process_file ~target filename oc state =
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
      (match target with
      | EmitGo ->
          print_endline ("\n[Emitting Go for " ^ filename ^ "]");
          let go_code =
            rename_chester_main
              (string_of_char_list (stringify_go_stmt (emit_go_top ast)))
          in
          output_string oc go_code
      | EmitRocq ->
          print_endline ("\n[Emitting Rocq for " ^ filename ^ "]");
          let rocq_code = string_of_char_list (stringify_rocq_stmt (emit_rocq_top ast)) in
          output_string oc rocq_code
      | EmitTS ->
          print_endline ("\n[Emitting TypeScript for " ^ filename ^ "]");
          let ts_code = string_of_char_list (stringify_ts_stmt (emit_ts_top ast)) in
          output_string oc ts_code);
      state'

let () =
  print_endline "Chester Bootstrapper";
  let args = Array.to_list Sys.argv |> fun lst ->
    match lst with
    | _ :: rest -> rest
    | [] -> []
  in
  let rec parse_opts target ts_mode out_file runtime_only acc = function
    | [] -> (target, ts_mode, out_file, runtime_only, List.rev acc)
    | "--go" :: rest -> parse_opts EmitGo ts_mode out_file runtime_only acc rest
    | "--rocq" :: rest -> parse_opts EmitRocq ts_mode out_file runtime_only acc rest
    | "--ts-module" :: rest -> parse_opts target TsEmitModule out_file runtime_only acc rest
    | "--emit-ts-runtime" :: path :: rest ->
        let dir = Filename.dirname path in
        if dir <> "" && not (Sys.file_exists dir) then Sys.mkdir dir 0o755;
        let oc = open_out path in
        output_string oc ts_runtime_file;
        close_out oc;
        print_endline ("Wrote TypeScript runtime to " ^ path);
        parse_opts target ts_mode out_file true acc rest
    | ("-o" | "--output") :: path :: rest ->
        parse_opts target ts_mode (Some path) runtime_only acc rest
    | f :: rest -> parse_opts target ts_mode out_file runtime_only (f :: acc) rest
  in
  let target, ts_mode, out_file, runtime_only, files =
    parse_opts EmitTS TsEmitScript None false [] args
  in
  match files with
  | [] when not runtime_only ->
      print_endline
        "Usage: main.exe [--go | --rocq | --ts-module | --emit-ts-runtime PATH] \
         [-o OUT] <file.chester> [file2.chester ...]";
      exit 1
  | [] -> exit 0
  | _ ->
      let out_dir = "out" in
      if not (Sys.file_exists out_dir) then Sys.mkdir out_dir 0o755;
      let out_file =
        match out_file with
        | Some path -> path
        | None ->
            Filename.concat out_dir
              (match target with
              | EmitGo -> "compiler.go"
              | EmitRocq -> "compiler.v"
              | EmitTS -> "compiler.ts")
      in
      let out_dirname = Filename.dirname out_file in
      if out_dirname <> "" && not (Sys.file_exists out_dirname) then
        Sys.mkdir out_dirname 0o755;
      let oc = open_out out_file in
      (match target with
      | EmitGo ->
          output_string oc go_effects_preamble;
          output_string oc "\n"
      | EmitRocq ->
          output_string oc rocq_effects_preamble;
          output_string oc "\n"
      | EmitTS ->
          if ts_mode = TsEmitScript then output_string oc preamble);
      let state = ref init_elab_state in
      List.iter (fun f -> state := process_file ~target f oc !state) files;
      if target = EmitGo then
        output_string oc
          "\nfunc main() {\n\tfmt.Println(chester_main())\n}\n";
      if target = EmitRocq then
        output_string oc "\nDefinition chester_run := chester_main.\n";
      close_out oc;
      print_endline ("\nSuccessfully emitted to " ^ out_file)
