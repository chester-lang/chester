(** Minimal reader for `data/go-signatures.json` (packages → functions map). *)

type package = { name : string; functions : string list }
type t = package list

let find_sub content needle =
  let n = String.length needle in
  let len = String.length content in
  let i = ref 0 in
  let result = ref None in
  while !i + n <= len && !result = None do
    if String.sub content !i n = needle then result := Some (!i + n);
    incr i
  done;
  !result

let contains_sub haystack needle =
  match find_sub haystack needle with
  | Some _ -> true
  | None -> false

let read_file path =
  let ch = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ch)
    (fun () ->
      let len = in_channel_length ch in
      let buf = Bytes.create len in
      really_input ch buf 0 len;
      Bytes.to_string buf)

let extract_quoted_key line =
  try
    let start = String.index line '"' in
    let stop = String.index_from line (start + 1) '"' in
    Some (String.sub line (start + 1) (stop - start - 1))
  with Not_found -> None

let is_exported_go_name name =
  name <> ""
  && name <> "functions"
  && name <> "packages"
  && name <> "params"
  && name <> "type"
  && name <> "name"
  && name <> "effects"
  && name.[0] >= 'A'

let is_function_decl_line line =
  String.length line >= 7 && String.sub line 0 6 = "      "
  && line.[6] = '"'

let extract_functions block =
  block
  |> String.split_on_char '\n'
  |> List.filter_map (fun line ->
         if is_function_decl_line line then
           match extract_quoted_key (String.trim line) with
           | Some name when is_exported_go_name name -> Some name
           | _ -> None
         else None)

let find_pkg_name header =
  let names =
    header
    |> String.split_on_char '\n'
    |> List.filter_map (fun line ->
           let trimmed = String.trim line in
           if String.contains trimmed ':' && String.contains trimmed '{' then
             extract_quoted_key trimmed
           else None)
  in
  let rec last_pkg = function
    | [] -> "unknown"
    | [ name ] -> if name = "packages" then "unknown" else name
    | name :: rest ->
        if name = "packages" || name = "functions" then last_pkg rest else name
  in
  last_pkg (List.rev names)

exception Functions_done of int

let functions_block_prefix block =
  let len = String.length block in
  let depth = ref 0 in
  try
    for i = 0 to len - 1 do
      match block.[i] with
      | '{' -> incr depth
      | '}' ->
          decr depth;
          if !depth < 0 then raise (Functions_done i)
      | _ -> ()
    done;
    block
  with Functions_done i -> String.sub block 0 i

let skip_to_functions_body content =
  let needle = "\"functions\"" in
  match find_sub content needle with
  | None -> None
  | Some i ->
      let len = String.length content in
      let j = ref i in
      let found = ref None in
      while !j < len && !found = None do
        match content.[!j] with
        | ' ' | '\n' | '\t' | '\r' | ':' -> incr j
        | '{' -> found := Some (!j + 1)
        | _ -> incr j
      done;
      !found

let load path : t =
  if not (Sys.file_exists path) then
    invalid_arg ("go-signatures file not found: " ^ path);
  let content = read_file path in
  if not (contains_sub content "\"packages\"") then
    invalid_arg "go-signatures file missing \"packages\"";
  match skip_to_functions_body content with
  | None -> invalid_arg "go-signatures file has no function blocks"
  | Some start ->
      let header = String.sub content 0 start in
      let block = String.sub content start (String.length content - start) in
      let pkg_name = find_pkg_name header in
      let body = functions_block_prefix block in
      let pkg = { name = pkg_name; functions = extract_functions body } in
      if pkg.functions = [] then invalid_arg "go-signatures file has no functions";
      [ pkg ]

let function_count t =
  List.fold_left (fun acc pkg -> acc + List.length pkg.functions) 0 t

let summary t =
  t
  |> List.map (fun pkg ->
         Printf.sprintf "%s (%d functions)" pkg.name
           (List.length pkg.functions))
  |> String.concat ", "

let default_path repo_root = Filename.concat repo_root "data/go-signatures.json"

let has_function t pkg fn =
  List.exists
    (fun p -> p.name = pkg && List.exists (( = ) fn) p.functions)
    t
