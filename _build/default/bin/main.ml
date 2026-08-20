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
              (char_list_of_string "Int", AstRef (char_list_of_string "Type")) ;
              (char_list_of_string "Token", AstRef (char_list_of_string "Type")) ;
              (char_list_of_string "Unit", AstRef (char_list_of_string "Type")) ] in
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
          print_endline (string_of_char_list go_code);
          
          (* Build Record & Field Access Test:
             record Token(kind: Int)
             def get_kind(tok: Token): Int = tok.kind
          *)
          let tok_sym = Symbol (char_list_of_string "tok", span) in
          let field_acc = FieldAccessCST (tok_sym, char_list_of_string "kind", span) in
          let rec_decl = RecordCST (char_list_of_string "Token", [], [], span) in
          let def_get = DefCST (char_list_of_string "get_kind", [], [(char_list_of_string "tok", Symbol (char_list_of_string "Token", span))], Symbol (char_list_of_string "Int", span), field_acc, span) in
          let blk = Block ([rec_decl; def_get], Symbol (char_list_of_string "Unit", span), span) in
          let def_blk = DefCST (char_list_of_string "main", [], [], Symbol (char_list_of_string "Unit", span), blk, span) in
          
          print_endline "\n[Code Formatter / Pretty Printer (Records)]";
          print_endline (string_of_char_list (format_cst 100 0 def_blk));
          
          match elaborate env def_blk None init_elab_state with
          | ElabErr (e, _) -> print_endline ("Elab Error (Record): " ^ string_of_char_list e)
          | ElabOk ((record_ast, _), _) ->
              print_endline "\n[TypeScript Backend (Records)]";
              print_endline (string_of_char_list (stringify_ts (emit_ts record_ast)));
              
  print_endline "\n[Error Recovery Parser Test]";
  let source = "x; y; @@; z; 42" in
  let tokens = Lexer.tokenize "test_error.chester" source in
  let cst = Parser.parse tokens in
  print_endline (string_of_char_list (format_cst 100 0 cst))
