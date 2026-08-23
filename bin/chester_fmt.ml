open Chester_frontend

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

let format_file filename =
  let source = read_file filename in
  let formatted = Source_formatter.format_source source in
  if formatted <> source then
    let reformatted = Source_formatter.format_source formatted in
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
