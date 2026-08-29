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

type cli_options = {
  target : emit_target;
  ts_mode : ts_emit_mode;
  out_file : string option;
  runtime_only : bool;
  module_paths : string list;
  prelude_paths : string list;
  go_sigs_path : string option;
  list_go_sigs : bool;
  files : string list;
}

let preamble =
  ts_primitives
  ^ ts_effects_runtime
  ^ "   const int_add = prim__int_add;\n\
   const int_mul = prim__int_mul;\n\
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

let read_file filename =
  let ch = open_in filename in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ch)
    (fun () ->
      let len = in_channel_length ch in
      let buf = Bytes.create len in
      really_input ch buf 0 len;
      Bytes.to_string buf)

let compile_file ~verbose filename state op_env =
  let source = read_file filename in
  if verbose then print_endline ("\n[Parsing " ^ filename ^ "]");
  let tokens = Lexer.tokenize filename source in
  let cst = parse tokens in
  if verbose then print_endline ("\n[Expanding " ^ filename ^ "]");
  let expanded_cst, op_env' = expand_cst_top_env !op_env cst in
  op_env := op_env';
  if verbose then (
    print_endline (string_of_char_list (format_cst 100 0 expanded_cst));
    print_endline ("\n[Elaborating & TypeChecking " ^ filename ^ "]"));
  match elaborate_top [] expanded_cst None state with
  | Inr (msg, _) ->
      print_endline ("Type Error: " ^ string_of_char_list msg);
      print_endline (string_of_char_list (format_cst 100 0 expanded_cst));
      exit 1
  | Inl ((ast, _), state') -> (ast, state')

let emit_ast ~target ~verbose filename oc ast =
  match target with
  | EmitGo ->
      if verbose then print_endline ("\n[Emitting Go for " ^ filename ^ "]");
      let go_code =
        rename_chester_main (string_of_char_list (stringify_go_stmt (emit_go_top ast)))
      in
      output_string oc go_code
  | EmitRocq ->
      if verbose then print_endline ("\n[Emitting Rocq for " ^ filename ^ "]");
      output_string oc (string_of_char_list (stringify_rocq_stmt (emit_rocq_top ast)))
  | EmitTS ->
      if verbose then print_endline ("\n[Emitting TypeScript for " ^ filename ^ "]");
      output_string oc (string_of_char_list (stringify_ts_stmt (emit_ts_top ast)))

let process_file ~target ~verbose ~emit oc filename state op_env =
  let ast, state' = compile_file ~verbose filename state op_env in
  if emit then emit_ast ~target ~verbose filename oc ast;
  state'

let rec parse_opts acc = function
  | [] -> acc
  | "--go" :: rest -> parse_opts { acc with target = EmitGo } rest
  | "--rocq" :: rest -> parse_opts { acc with target = EmitRocq } rest
  | "--ts-module" :: rest -> parse_opts { acc with ts_mode = TsEmitModule } rest
  | "--module-path" :: path :: rest ->
      parse_opts { acc with module_paths = path :: acc.module_paths } rest
  | "--module-path" :: [] ->
      print_endline "Error: --module-path requires a directory argument";
      exit 1
  | "--prelude" :: path :: rest ->
      parse_opts { acc with prelude_paths = path :: acc.prelude_paths } rest
  | "--prelude" :: [] ->
      print_endline "Error: --prelude requires a file argument";
      exit 1
  | "--go-sigs" :: path :: rest ->
      parse_opts { acc with go_sigs_path = Some path } rest
  | "--go-sigs" :: [] ->
      print_endline "Error: --go-sigs requires a file argument";
      exit 1
  | "--list-go-sigs" :: rest -> parse_opts { acc with list_go_sigs = true } rest
  | "--emit-ts-runtime" :: path :: rest ->
      let dir = Filename.dirname path in
      if dir <> "" && not (Sys.file_exists dir) then Sys.mkdir dir 0o755;
      let oc = open_out path in
      output_string oc ts_runtime_file;
      close_out oc;
      print_endline ("Wrote TypeScript runtime to " ^ path);
      parse_opts { acc with runtime_only = true } rest
  | ("-o" | "--output") :: path :: rest ->
      parse_opts { acc with out_file = Some path } rest
  | ("-o" | "--output") :: [] ->
      print_endline "Error: --output requires a path argument";
      exit 1
  | f :: rest -> parse_opts { acc with files = f :: acc.files } rest

let usage () =
  print_endline
    "Usage: main.exe [--go | --rocq | --ts-module | --emit-ts-runtime PATH] \\\n\
     \       [--module-path DIR]... [--prelude FILE]... \\\n\
     \       [--go-sigs FILE] [--list-go-sigs] \\\n\
     \       [-o OUT] <file.chester> [file2.chester ...]"

let default_options =
  {
    target = EmitTS;
    ts_mode = TsEmitScript;
    out_file = None;
    runtime_only = false;
    module_paths = [];
    prelude_paths = [];
    go_sigs_path = None;
    list_go_sigs = false;
    files = [];
  }

let () =
  print_endline "Chester Bootstrapper";
  let opts =
    parse_opts default_options (List.tl (Array.to_list Sys.argv))
  in
  let opts =
    {
      opts with
      module_paths = List.rev opts.module_paths;
      prelude_paths = List.rev opts.prelude_paths;
      files = List.rev opts.files;
    }
  in
  match opts.files with
  | [] when opts.list_go_sigs ->
      let anchor = Sys.getcwd () in
      let search_paths =
        Chester_paths.default_module_paths ~for_file:anchor opts.module_paths
      in
      let repo_root =
        match Chester_paths.repo_root_from_file anchor with
        | Some root -> root
        | None -> anchor
      in
      let path =
        match opts.go_sigs_path with
        | Some p -> Chester_paths.resolve_input ~search_paths p
        | None -> Go_signatures.default_path repo_root
      in
      let sigs = Go_signatures.load path in
      print_endline
        (Printf.sprintf "Loaded %d functions from %s: %s"
           (Go_signatures.function_count sigs) path
           (Go_signatures.summary sigs));
      exit 0
  | [] when not opts.runtime_only ->
      usage ();
      exit 1
  | [] -> exit 0
  | files ->
      let anchor = List.hd files in
      let search_paths =
        Chester_paths.default_module_paths ~for_file:anchor opts.module_paths
      in
      let repo_root =
        match Chester_paths.repo_root_from_file anchor with
        | Some root -> root
        | None -> Sys.getcwd ()
      in
      let resolve file = Chester_paths.resolve_input ~search_paths file in
      let resolved_files = List.map resolve files in
      let prelude_paths = List.map resolve opts.prelude_paths in
      List.iter (Chester_paths.ensure_exists "prelude file") prelude_paths;
      List.iter (Chester_paths.ensure_exists "input file") resolved_files;
      (match opts.target with
      | EmitGo ->
          let go_sigs_path =
            match opts.go_sigs_path with
            | Some p -> Some (resolve p)
            | None ->
                let default = Go_signatures.default_path repo_root in
                if Sys.file_exists default then Some default else None
          in
          (match go_sigs_path with
          | Some path -> (
              try
                let sigs = Go_signatures.load path in
                print_endline
                  (Printf.sprintf "[go-sigs] %s (%s)"
                     path (Go_signatures.summary sigs))
              with Failure msg | Invalid_argument msg ->
                print_endline ("Warning: go-sigs: " ^ msg))
          | None -> ())
      | _ -> ());
      let go_sigs =
        match opts.target with
        | EmitGo -> (
            let go_sigs_path =
              match opts.go_sigs_path with
              | Some p -> Some (resolve p)
              | None ->
                  let default = Go_signatures.default_path repo_root in
                  if Sys.file_exists default then Some default else None
            in
            match go_sigs_path with
            | Some path -> (
                try Some (Go_signatures.load path) with _ -> None)
            | None -> None)
        | _ -> None
      in
      let out_dir = "out" in
      if not (Sys.file_exists out_dir) then Sys.mkdir out_dir 0o755;
      let out_file =
        match opts.out_file with
        | Some path -> path
        | None ->
            Filename.concat out_dir
              (match opts.target with
              | EmitGo -> "compiler.go"
              | EmitRocq -> "compiler.v"
              | EmitTS -> "compiler.ts")
      in
      let out_dirname = Filename.dirname out_file in
      if out_dirname <> "" && not (Sys.file_exists out_dirname) then
        Sys.mkdir out_dirname 0o755;
      let oc = open_out out_file in
      (match opts.target with
      | EmitGo ->
          output_string oc go_effects_preamble;
          output_string oc "\n"
      | EmitRocq ->
          output_string oc rocq_effects_preamble;
          output_string oc "\n"
      | EmitTS ->
          if opts.ts_mode = TsEmitScript then output_string oc preamble);
      let state =
        match go_sigs with
        | Some sigs -> init_elab_with_go (Go_signatures.to_elab_go_input sigs)
        | None -> init_elab_state
      in
      let state = ref state in
      let op_env = ref [] in
      List.iter
        (fun f ->
          state :=
            process_file ~target:opts.target ~verbose:false ~emit:false oc f
              !state op_env)
        prelude_paths;
      List.iter
        (fun f ->
          state :=
            process_file ~target:opts.target ~verbose:true ~emit:true oc f !state
              op_env)
        resolved_files;
      if opts.target = EmitGo then
        output_string oc "\nfunc main() {\n\tfmt.Println(chester_main())\n}\n";
      if opts.target = EmitRocq then
        output_string oc "\nDefinition chester_run := chester_main.\n";
      close_out oc;
      print_endline ("\nSuccessfully emitted to " ^ out_file)
