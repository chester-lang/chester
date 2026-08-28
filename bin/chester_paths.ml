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
    if Sys.file_exists (Filename.concat dir "dune-project")
       || Sys.file_exists (Filename.concat dir "theories")
    then Some dir
    else
      let parent = Filename.dirname dir in
      if parent = dir then None else walk parent
  in
  walk (Filename.dirname filename)

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

let find_in_search_paths ~search_paths basename =
  let rec find = function
    | [] -> None
    | dir :: rest ->
        let path = Filename.concat dir basename in
        if Sys.file_exists path then Some path else find rest
  in
  match find search_paths with
  | Some path -> path
  | None -> basename

let default_prelude_path ~search_paths =
  find_in_search_paths ~search_paths "stdlib/std.chester"
