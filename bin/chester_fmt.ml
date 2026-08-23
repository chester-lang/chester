open Compiler_lib.Compiler
open Chester_frontend

let string_of_char_list chars =
  let buf = Buffer.create (List.length chars) in
  List.iter (Buffer.add_char buf) chars;
  Buffer.contents buf

let read_file filename =
  let ch = open_in filename in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ch)
    (fun () ->
      let len = in_channel_length ch in
      let buf = Bytes.create len in
      really_input ch buf 0 len;
      Bytes.to_string buf)

let write_file filename contents =
  let ch = open_out filename in
  Fun.protect
    ~finally:(fun () -> close_out_noerr ch)
    (fun () -> output_string ch contents)

let format_source filename source =
  let tokens = Lexer.tokenize filename source in
  let cst = parse tokens in
  string_of_char_list (format_cst 1000 0 cst) ^ "\n"

let format_file filename =
  let source = read_file filename in
  let formatted = format_source filename source in
  if formatted <> source then
    let reformatted = format_source filename formatted in
    if reformatted = formatted then write_file filename formatted
    else
      prerr_endline
        ("chester_fmt: skipping " ^ filename
       ^ ": formatter output is not idempotent yet")

let () =
  match Array.to_list Sys.argv with
  | _ :: files when files <> [] -> List.iter format_file files
  | _ ->
      prerr_endline "Usage: chester_fmt.exe <file.chester> [file2.chester ...]";
      exit 2
