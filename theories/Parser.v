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

(* Synchronization function for error recovery *)
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

(* Formally prove that sync decreases the token list length or keeps it the same *)
Lemma sync_length : forall toks, length (sync toks) <= length toks.
Proof.
  induction toks as [| t rest IHrest]; simpl.
  - lia.
  - destruct t; simpl.
    + apply le_S; exact IHrest. (* TokId *)
    + apply le_S; exact IHrest. (* TokInt *)
    + apply le_S; exact IHrest. (* TokStr *)
    + destruct (string_dec s ";").
      * lia.
      * destruct (string_dec s "}").
        -- apply le_n.
        -- apply le_S; exact IHrest.
    + apply le_S; exact IHrest. (* TokComment *)
    + apply le_n.
Qed.

(* A resilient parser returning CST and remaining tokens *)
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
          (CommentCST text s :: stmts, rest')
      end
  end.

(* Prove that parse_stmts terminates / does not increase tokens *)
Lemma parse_stmts_length : forall fuel toks,
  length (snd (parse_stmts fuel toks)) <= length toks.
Proof.
  induction fuel as [| fuel' IH]; intros toks; simpl.
  - lia.
  - destruct toks as [| t rest]; simpl; [lia |].
    destruct t; simpl.
    + destruct (parse_stmts fuel' rest) as [stmts rest'] eqn:E.
      generalize (IH rest); rewrite E; simpl; intro H; lia.
    + destruct (parse_stmts fuel' rest) as [stmts rest'] eqn:E.
      generalize (IH rest); rewrite E; simpl; intro H; lia.
    + destruct (parse_stmts fuel' rest) as [stmts rest'] eqn:E.
      generalize (IH rest); rewrite E; simpl; intro H; lia.
    + destruct (string_dec s "}").
      * apply le_n.
      * destruct (string_dec s ";").
        -- generalize (IH rest). intro H. lia.
        -- destruct (parse_stmts fuel' (sync rest)) as [stmts rest'] eqn:E.
           generalize (IH (sync rest)); rewrite E; simpl; intro H.
           generalize (sync_length rest); intro Hsync.
           lia.
    + destruct (parse_stmts fuel' rest) as [stmts rest'] eqn:E.
      generalize (IH rest); rewrite E; simpl; intro H; lia.
    + apply le_n.
Qed.

Definition parse (toks : list Token) : CST :=
  let (stmts, _) := parse_stmts (length toks) toks in
  Block stmts (Symbol "Unit" empty_span) empty_span.
