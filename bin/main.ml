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
  
  (* Construct CST for: def bool_or(a: Bool, b: Bool): Bool = if a then true else b; *)
  let pos = { index = { unicode = 0; utf16 = 0 }; line = 0; column = { unicode = 0; utf16 = 0 } } in
  let span_range = { start_pos = pos; end_pos = pos } in
  let span = { file_name = char_list_of_string "mock.chester"; range = span_range } in
  let bool_type = Symbol (char_list_of_string "Bool", span) in
  let a_var = Symbol (char_list_of_string "a", span) in
  let b_var = Symbol (char_list_of_string "b", span) in
  let true_lit = BoolLiteral (true, span) in
  let if_expr = IfCST (a_var, true_lit, b_var, span) in
  let params = [ (char_list_of_string "a", bool_type); (char_list_of_string "b", bool_type) ] in
  let def_expr = DefCST (char_list_of_string "bool_or", [], params, bool_type, if_expr, span) in
  
  let env = [ (char_list_of_string "Bool", AstRef (char_list_of_string "Type")) ] in
  let expected = None in
  let elab_res = elaborate env def_expr expected init_elab_state in
  match elab_res with
  | ElabErr (e, _) -> print_endline ("Elab Error: " ^ string_of_char_list e)
  | ElabOk ((ast, _ty), _) ->
      let ts_ast = emit_ts ast in
      let ts_code = stringify_ts ts_ast in
      print_endline ("\n[TypeScript Backend]");
      print_endline (string_of_char_list ts_code);
      
      let go_ast = emit_go ast in
      let go_code = stringify_go go_ast in
      print_endline ("\n[Golang Backend]");
      print_endline (string_of_char_list go_code)
