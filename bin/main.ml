open Compiler_lib.Compiler

let string_of_char_list cl =
  String.concat "" (List.map (fun c -> String.make 1 c) cl)

let char_list_of_string s =
  let rec aux i acc =
    if i < 0 then acc else aux (i - 1) (s.[i] :: acc)
  in
  aux (String.length s - 1) []

let () =
  print_endline "Chester Compiler CLI";
  let mock_input = char_list_of_string "{}" in
  let pos = { index = { unicode = 0; utf16 = 0 }; line = 0; column = { unicode = 0; utf16 = 0 } } in
  let tokens = tokenize 100 mock_input pos in
  let parsed = parse_cst 100 tokens in
  match parsed with
  | ParseErr e -> print_endline ("Parse Error: " ^ string_of_char_list e)
  | ParseOk (cst, _) ->
      (* Mock FFI parsing *)
      let ts_ffi = TsArrow ([char_list_of_string "x"], TsIdentifier (char_list_of_string "number")) in
      let chester_ffi_type = ts_to_chester ts_ffi in
      
      (* Insert FFI type into type environment for the checker/elaborator *)
      let env = [(char_list_of_string "ffi_function", chester_ffi_type)] in
      let expected = None in
      let elab_res = elaborate env cst expected init_elab_state in
      match elab_res with
      | ElabErr (e, _) -> print_endline ("Elab Error: " ^ string_of_char_list e)
      | ElabOk ((ast, _ty), _) ->
          let ts_ast = emit_ts ast in
          let ts_code = stringify_ts ts_ast in
          print_endline ("Compiled TypeScript:\n" ^ string_of_char_list ts_code)
