open Compiler_lib.Compiler
open Lexer

let char_list_of_string s =
  let rec aux i acc =
    if i < 0 then acc else aux (i - 1) (s.[i] :: acc)
  in
  aux (String.length s - 1) []

let dummy_span = Lexer.make_span "dummy" 0 0

let parse_expr tokens =
  match tokens with
  | (TId id, sp) :: rest -> (Symbol (char_list_of_string id, sp), rest)
  | (TInt i, sp) :: rest -> (IntegerLiteral (char_list_of_string i, sp), rest)
  | (TStr s, sp) :: rest -> (StringLiteral (char_list_of_string s, sp), rest)
  | _ :: rest -> (Error (char_list_of_string "Syntax Error: Unexpected token", dummy_span), rest)
  | [] -> (Error (char_list_of_string "Unexpected EOF", dummy_span), [])

let parse tokens =
  let rec parse_stmts toks acc =
    match toks with
    | [] | (TEOF, _) :: _ -> List.rev acc
    | (TComment c, sp) :: rest -> parse_stmts rest (CommentCST (char_list_of_string c, sp) :: acc)
    | (TSym ";", _) :: rest -> parse_stmts rest acc
    | _ ->
      let (expr, rest) = parse_expr toks in
      match expr with
      | Error _ as e ->
          (* Error recovery: skip to next semicolon or EOF *)
          let rec sync t =
            match t with
            | [] | (TEOF, _) :: _ -> []
            | (TSym ";", _) :: rest' -> rest'
            | _ :: rest' -> sync rest'
          in
          parse_stmts (sync rest) (e :: acc)
      | e -> parse_stmts rest (e :: acc)
  in
  let stmts = parse_stmts tokens [] in
  Block (stmts, Symbol (char_list_of_string "Unit", dummy_span), dummy_span)
