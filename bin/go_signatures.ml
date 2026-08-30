(** Minimal reader for `data/go-signatures.json` (packages -> functions map). *)

type package = { name : string; functions : string list }
type t = package list

let read_file path =
  let ch = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ch)
    (fun () ->
      let len = in_channel_length ch in
      let buf = Bytes.create len in
      really_input ch buf 0 len;
      Bytes.to_string buf)

let load path : t =
  if not (Sys.file_exists path) then
    invalid_arg ("go-signatures file not found: " ^ path);
  let content = read_file path in
  let lines = String.split_on_char '\n' content in
  
  let pkg_re = Str.regexp "^    \"\\([^\"]+\\)\": {$" in
  let func_re = Str.regexp "^        \"\\([A-Z][^\"]*\\)\": {$" in
  
  let rec parse_lines lines current_pkg pkgs =
    match lines with
    | [] -> 
        (match current_pkg with 
         | Some p when p.functions <> [] -> p :: pkgs 
         | _ -> pkgs)
    | line :: rest ->
        if Str.string_match pkg_re line 0 then
          let name = Str.matched_group 1 line in
          let new_pkgs = 
            match current_pkg with
            | Some p when p.functions <> [] -> p :: pkgs
            | _ -> pkgs
          in
          parse_lines rest (Some { name; functions = [] }) new_pkgs
        else if Str.string_match func_re line 0 then
          let fn_name = Str.matched_group 1 line in
          match current_pkg with
          | Some p -> 
              let p' = { p with functions = fn_name :: p.functions } in
              parse_lines rest (Some p') pkgs
          | None -> parse_lines rest current_pkg pkgs
        else
          parse_lines rest current_pkg pkgs
  in
  let pkgs = parse_lines lines None [] in
  let pkgs = List.map (fun p -> { p with functions = List.rev p.functions }) (List.rev pkgs) in
  if pkgs = [] then invalid_arg "go-signatures file has no functions";
  pkgs

let function_count t =
  List.fold_left (fun acc pkg -> acc + List.length pkg.functions) 0 t

let summary t =
  t
  |> List.map (fun pkg ->
         Printf.sprintf "%s (%d functions)" pkg.name
           (List.length pkg.functions))
  |> String.concat ", "

let char_list_of_string s =
  let len = String.length s in
  List.init len (String.get s)

let to_elab_go_input (t : t) : (char list * char list list) list =
  List.map
    (fun pkg ->
      (char_list_of_string pkg.name, List.map char_list_of_string pkg.functions))
    t

let default_path repo_root = Filename.concat repo_root "data/go-signatures.json"

let has_function t pkg fn =
  List.exists
    (fun p -> p.name = pkg && List.exists ((=) fn) p.functions)
    t
