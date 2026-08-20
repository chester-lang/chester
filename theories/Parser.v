From Stdlib Require Import Strings.String.
From Stdlib Require Import List.
From Stdlib Require Import Arith.
From Stdlib Require Import Lia.
Import ListNotations.
Open Scope string_scope.
Require Import Chester.CST.

Inductive Token : Type :=
  | TokId : string -> Span -> Token
  | TokInt : string -> Span -> Token
  | TokStr : string -> Span -> Token
  | TokSym : string -> Span -> Token
  | TokComment : string -> Span -> Token
  | TokEOF : Span -> Token.

Definition token_span (t : Token) : Span :=
  match t with
  | TokId _ s => s
  | TokInt _ s => s
  | TokStr _ s => s
  | TokSym _ s => s
  | TokComment _ s => s
  | TokEOF s => s
  end.

Fixpoint sync (toks : list Token) : list Token :=
  match toks with
  | [] => []
  | TokEOF _ :: _ => toks
  | TokSym s _ :: rest =>
      if string_dec s ";" then rest
      else if string_dec s "}" then toks
      else sync rest
  | _ :: rest => sync rest
  end.

Definition parse_expr (toks : list Token) : (CST * list Token) :=
  match toks with
  | TokId name s :: rest => (Symbol name s, rest)
  | TokInt val s :: rest => (IntegerLiteral val s, rest)
  | TokStr val s :: rest => (StringLiteral val s, rest)
  | TokEOF s :: rest => (Error "Unexpected EOF in expr" s, toks)
  | t :: rest => (Error "Unexpected token in expr" (token_span t), rest)
  | [] => (Error "Unexpected EOF in expr" empty_span, [])
  end.

Fixpoint parse_stmts (fuel : nat) (toks : list Token) : (list CST * list Token) :=
  match fuel with
  | 0 => ([], toks)
  | S fuel' =>
      match toks with
      | [] => ([], toks)
      | TokEOF _ :: _ => ([], toks)
      | TokSym s span :: rest =>
          if string_dec s "}" then ([], toks)
          else if string_dec s ";" then parse_stmts fuel' rest
          else 
            let (stmts, rest') := parse_stmts fuel' (sync rest) in
            (Error "Unexpected symbol" span :: stmts, rest')
      | TokId "let" s_let :: TokId name s_name :: TokSym "=" _ :: rest =>
          let (expr_cst, rest_expr) := parse_expr rest in
          let (stmts, rest') := parse_stmts fuel' rest_expr in
          (LetCST name expr_cst (Symbol "Unit" empty_span) (combine_span s_let empty_span) :: stmts, rest')
      | TokId name s :: rest =>
          let (stmts, rest') := parse_stmts fuel' rest in
          (Symbol name s :: stmts, rest')
      | TokInt val s :: rest =>
          let (stmts, rest') := parse_stmts fuel' rest in
          (IntegerLiteral val s :: stmts, rest')
      | TokStr val s :: rest =>
          let (stmts, rest') := parse_stmts fuel' rest in
          (StringLiteral val s :: stmts, rest')
      | TokComment text s :: rest =>
          let (stmts, rest') := parse_stmts fuel' rest in
          (Error "Syntax Error" empty_span :: stmts, rest')
      end
  end.

Definition parse (toks : list Token) : CST :=
  let (stmts, _) := parse_stmts (length toks) toks in
  Block stmts (Symbol "Unit" empty_span) empty_span.
