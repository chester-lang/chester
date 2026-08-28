let rec repo_root dir =
  let fixture = Filename.concat dir "tests/macro_hygiene.chester" in
  if Sys.file_exists fixture then dir
  else
    let parent = Filename.dirname dir in
    if parent = dir then failwith "could not locate repository root"
    else repo_root parent

let quote s = "'" ^ String.escaped s ^ "'"

let ensure_scripts_deps root =
  let scripts_dir = Filename.concat root "scripts" in
  let node_modules = Filename.concat scripts_dir "node_modules" in
  if not (Sys.file_exists node_modules) then
    let st =
      Sys.command
        (Printf.sprintf "cd %s && npm install --silent" (Filename.quote scripts_dir))
    in
    if st <> 0 then failwith "npm install failed in scripts/"

let () =
  let args =
    match Array.to_list Sys.argv with
    | _ :: rest -> rest
    | [] -> []
  in
  if args = [] || List.mem "-h" args || List.mem "--help" args then (
    prerr_endline
      "Usage: chester_bindgen.exe --package NAME --input FILE.d.ts \
       [--output FILE.chester] [--filter REGEX]";
    exit (if args = [] then 1 else 0));
  let root = repo_root (Sys.getcwd ()) in
  ensure_scripts_deps root;
  let script = Filename.concat root "scripts/dts2chester.mjs" in
  let cmd =
    Printf.sprintf "node %s %s"
      (Filename.quote script)
      (String.concat " " (List.map quote args))
  in
  exit (Sys.command cmd)
