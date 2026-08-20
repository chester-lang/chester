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
      else if string_dec s ")" then toks
      else if string_dec s "]" then toks
      else sync rest
  | _ :: rest => sync rest
  end.

Lemma sync_length : forall toks, length (sync toks) <= length toks.
Proof.
  induction toks as [| t rest IHrest]; simpl.
  - lia.
  - destruct t; simpl.
    + apply le_S; exact IHrest.
    + apply le_S; exact IHrest.
    + apply le_S; exact IHrest.
    + destruct (string_dec s ";"); [lia |].
      destruct (string_dec s "}"); [apply le_n |].
      destruct (string_dec s ")"); [apply le_n |].
      destruct (string_dec s "]"); [apply le_n |].
      apply le_S; exact IHrest.
    + apply le_S; exact IHrest.
    + apply le_n.
Qed.

Inductive ParseMode : Type :=
  | ModeSeq : string -> ParseMode (* Parsing a single sequence until delimiter or terminator *)
  | ModeStmts : string -> ParseMode (* Parsing statements separated by ; until terminator *)
  | ModeComma : string -> ParseMode. (* Parsing comma separated sequence until terminator *)

(* Fuel-based parser that handles sequences, blocks, and tuples *)
Section ParserLogic.
  Fixpoint parse_loop (fuel : nat) (mode : ParseMode) (toks : list Token) : (list CST * list Token) :=
    match fuel with
    | 0 => ([], toks)
    | S fuel' =>
        match toks with
        | [] => ([], toks)
        | TokEOF s :: _ => ([], toks)
        | TokComment text s :: rest =>
            let (csts, rest') := parse_loop fuel' mode rest in
            (CommentCST text s :: csts, rest')
        | TokSym s span :: rest =>
            let term := match mode with ModeSeq t => t | ModeStmts t => t | ModeComma t => t end in
            if string_dec s term then ([], toks)
            else
              match mode with
              | ModeSeq _ =>
                  if string_dec s ";" then ([], toks)
                  else if string_dec s "," then ([], toks)
                  else if string_dec s "{" then
                    let (block_stmts, rest1) := parse_loop fuel' (ModeStmts "}") rest in
                    let rest2 := match rest1 with TokSym "}" _ :: r => r | _ => rest1 end in
                    let (seq_csts, rest3) := parse_loop fuel' mode rest2 in
                    (Block block_stmts (Symbol "Unit" empty_span) empty_span :: seq_csts, rest3)
                  else if string_dec s "(" then
                    let (tuple_elems, rest1) := parse_loop fuel' (ModeComma ")") rest in
                    let rest2 := match rest1 with TokSym ")" _ :: r => r | _ => rest1 end in
                    let (seq_csts, rest3) := parse_loop fuel' mode rest2 in
                    (Tuple tuple_elems empty_span :: seq_csts, rest3)
                  else if string_dec s "[" then
                    let (list_elems, rest1) := parse_loop fuel' (ModeComma "]") rest in
                    let rest2 := match rest1 with TokSym "]" _ :: r => r | _ => rest1 end in
                    let (seq_csts, rest3) := parse_loop fuel' mode rest2 in
                    (ListLiteral list_elems empty_span :: seq_csts, rest3)
                  else
                    let (seq_csts, rest1) := parse_loop fuel' mode rest in
                    (Symbol s span :: seq_csts, rest1)
              | ModeStmts _ =>
                  if string_dec s ";" then parse_loop fuel' mode rest
                  else
                    let (seq_csts, rest1) := parse_loop fuel' (ModeSeq term) toks in
                    let stmt := match seq_csts with [] => Symbol "Empty" empty_span | [x] => x | _ => SeqOf seq_csts empty_span end in
                    let (stmts, rest2) := parse_loop fuel' mode rest1 in
                    (stmt :: stmts, rest2)
              | ModeComma _ =>
                  if string_dec s "," then parse_loop fuel' mode rest
                  else
                    let (seq_csts, rest1) := parse_loop fuel' (ModeSeq term) toks in
                    let elem := match seq_csts with [] => Symbol "Empty" empty_span | [x] => x | _ => SeqOf seq_csts empty_span end in
                    let (elems, rest2) := parse_loop fuel' mode rest1 in
                    (elem :: elems, rest2)
              end
        | TokId name s :: rest =>
            match mode with
            | ModeSeq _ =>
                let (csts, rest') := parse_loop fuel' mode rest in
                (Symbol name s :: csts, rest')
            | _ =>
                let (seq_csts, rest1) := parse_loop fuel' (ModeSeq (match mode with ModeStmts t => t | ModeComma t => t | ModeSeq t => t end)) toks in
                let stmt := match seq_csts with [] => Symbol "Empty" empty_span | [x] => x | _ => SeqOf seq_csts empty_span end in
                let (stmts, rest2) := parse_loop fuel' mode rest1 in
                (stmt :: stmts, rest2)
            end
        | TokInt val s :: rest =>
            match mode with
            | ModeSeq _ =>
                let (csts, rest') := parse_loop fuel' mode rest in
                (IntegerLiteral val s :: csts, rest')
            | _ =>
                let (seq_csts, rest1) := parse_loop fuel' (ModeSeq (match mode with ModeStmts t => t | ModeComma t => t | ModeSeq t => t end)) toks in
                let stmt := match seq_csts with [] => Symbol "Empty" empty_span | [x] => x | _ => SeqOf seq_csts empty_span end in
                let (stmts, rest2) := parse_loop fuel' mode rest1 in
                (stmt :: stmts, rest2)
            end
        | TokStr val s :: rest =>
            match mode with
            | ModeSeq _ =>
                let (csts, rest') := parse_loop fuel' mode rest in
                (StringLiteral val s :: csts, rest')
            | _ =>
                let (seq_csts, rest1) := parse_loop fuel' (ModeSeq (match mode with ModeStmts t => t | ModeComma t => t | ModeSeq t => t end)) toks in
                let stmt := match seq_csts with [] => Symbol "Empty" empty_span | [x] => x | _ => SeqOf seq_csts empty_span end in
                let (stmts, rest2) := parse_loop fuel' mode rest1 in
                (stmt :: stmts, rest2)
            end
        end
    end.
End ParserLogic.

Definition parse (toks : list Token) : CST :=
  let (stmts, _) := parse_loop (length toks * 4) (ModeStmts "") toks in
  Block stmts (Symbol "Unit" empty_span) empty_span.
