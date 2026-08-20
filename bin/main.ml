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
  
  (* Construct CST for match: def unwrap_or_zero(opt: Option): Int = match opt { case Some(x) => x; case None => 0 } *)
  let pos = { index = { unicode = 0; utf16 = 0 }; line = 0; column = { unicode = 0; utf16 = 0 } } in
  let span_range = { start_pos = pos; end_pos = pos } in
  let span = { file_name = char_list_of_string "mock.chester"; range = span_range } in
  
  let opt_sym = Symbol (char_list_of_string "opt", span) in
  let x_sym = Symbol (char_list_of_string "x", span) in
  let zero = IntegerLiteral (char_list_of_string "0", span) in
  
  let pat_some = PatConstructorCST (char_list_of_string "Some", [char_list_of_string "x"], span) in
  let pat_none = PatConstructorCST (char_list_of_string "None", [], span) in
  
  let match_cases = [ (pat_some, x_sym); (pat_none, zero) ] in
  let match_expr = MatchCST (opt_sym, match_cases, span) in
  
  let params = [ (char_list_of_string "opt", Symbol (char_list_of_string "Option", span)) ] in
  let def_expr = DefCST (char_list_of_string "unwrap_or_zero", [], params, Symbol (char_list_of_string "Int", span), match_expr, span) in
  
  let env = [ (char_list_of_string "Option", AstRef (char_list_of_string "Type")) ;
              (char_list_of_string "Int", AstRef (char_list_of_string "Type")) ] in
  let expected = None in
  let elab_res = elaborate env def_expr expected init_elab_state in
  match elab_res with
  | ElabErr (e, _) -> print_endline ("Elab Error: " ^ string_of_char_list e)
  | ElabOk ((ast, _ty), st) ->
      let zonk_res = zonk 100 ast st in
      match zonk_res with
      | ElabErr (e, _) -> print_endline ("Zonk Error: " ^ string_of_char_list e)
      | ElabOk (zonked_ast, _) ->
          let ts_ast = emit_ts zonked_ast in
          let ts_code = stringify_ts ts_ast in
          print_endline ("\n[TypeScript Backend]");
          print_endline (string_of_char_list ts_code);
          
          let go_ast = emit_go zonked_ast in
          let go_code = stringify_go go_ast in
          print_endline ("\n[Golang Backend]");
          print_endline (string_of_char_list go_code)
