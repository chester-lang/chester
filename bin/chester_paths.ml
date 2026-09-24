(** Search paths and input resolution for the Chester CLI. *)

let split_search_path s =
  if s = "" then []
  else String.split_on_char ':' s |> List.filter (fun part -> part <> "")

let chester_path_env () =
  match Sys.getenv_opt "CHESTER_PATH" with
  | None -> []
  | Some value -> split_search_path value

let repo_root_from_file filename =
  let rec walk dir =
    if
      Sys.file_exists (Filename.concat dir "dune-project")
      || Sys.file_exists (Filename.concat dir "theories")
    then Some dir
    else
      let parent = Filename.dirname dir in
      if parent = dir then None else walk parent
  in
  walk
    (if Sys.is_directory filename then filename else Filename.dirname filename)

let default_module_paths ~for_file extra =
  let cwd = Sys.getcwd () in
  let roots =
    match repo_root_from_file for_file with
    | Some root -> [ root; cwd ]
    | None -> [ cwd ]
  in
  roots @ chester_path_env () @ extra

let resolve_input ~search_paths filename =
  if Filename.is_relative filename && not (Sys.file_exists filename) then
    match
      List.find_opt
        (fun dir -> Sys.file_exists (Filename.concat dir filename))
        search_paths
    with
    | Some dir -> Filename.concat dir filename
    | None -> filename
  else filename

let ensure_exists label path =
  if not (Sys.file_exists path) then (
    print_endline ("Error: " ^ label ^ " not found: " ^ path);
    exit 1)

(** Resolve a Chester module file: [name] with empty path → Name.chester /
    name.chester; otherwise resolve [path] on the search path. *)
let resolve_chester_module ~search_paths name path =
  let candidates =
    if path = "" then
      let cap =
        if name = "" then []
        else
          let c0 = name.[0] in
          let upper =
            String.make 1 (Char.uppercase_ascii c0)
            ^ String.sub name 1 (String.length name - 1)
          in
          [
            upper ^ ".chester";
            name ^ ".chester";
            String.lowercase_ascii name ^ ".chester";
          ]
      in
      cap
    else [ path ]
  in
  let rec try_one = function
    | [] -> None
    | c :: rest ->
        let resolved = resolve_input ~search_paths c in
        if Sys.file_exists resolved then Some resolved else try_one rest
  in
  try_one candidates

let module_binder_from_path path name =
  if name <> "" && not (String.contains name '/' || String.contains name '.')
  then name
  else
    let base = Filename.basename path in
    try Filename.chop_extension base with Invalid_argument _ -> base
