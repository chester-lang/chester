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
  print_endline (string_of_char_list (format_cst 100 0 expanded_cst))

let () =
  print_endline "Chester Bootstrapper";
  if Array.length Sys.argv > 1 then
    for i = 1 to Array.length Sys.argv - 1 do
      process_file Sys.argv.(i)
    done
  else print_endline "Usage: main.exe <file.chester>"
