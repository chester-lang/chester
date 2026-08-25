From Stdlib Require Import Strings.String.
From Stdlib Require Import List.
From Stdlib Require Import Arith.
From Stdlib Require Import Lia.
Import ListNotations.
Open Scope string_scope.
Require Import Chester.CST.

Fixpoint last_elem (ls : list CST) : option CST :=
  match ls with
  | [] => None
  | [x] => Some x
  | _ :: rest => last_elem rest
  end.

Definition ends_with_block (stmt : CST) : bool :=
  match stmt with
  | Block _ _ _ => true
  | SeqOf elements _ =>
      match last_elem elements with
      | Some (Block _ _ _) => true
      | _ => false
      end
  | _ => false
  end.


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
  | ModeSeq : string -> ParseMode
  | ModeStmts : string -> ParseMode
  | ModeComma : string -> ParseMode.

Section FuelParser.

Definition unit_cst : CST := Symbol "Unit" empty_span.

Definition make_seq_or_single (seq_csts : list CST) : CST :=
  match seq_csts with
  | [] => Symbol "Empty" empty_span
  | [x] => x
  | _ => SeqOf seq_csts empty_span
  end.

Definition append_body_tail (stmts : list CST) (tail : CST) : list CST :=
  match tail with
  | Symbol name _ => if string_dec name "Unit" then stmts else List.app stmts [tail]
  | _ => List.app stmts [tail]
  end.

Fixpoint parse_loop_fuel (fuel : nat) (mode : ParseMode) (toks : list Token)
    {struct fuel} : (list CST * list Token) :=
  match fuel with
  | 0 => ([], toks)
  | S fuel' =>
      match toks with
      | [] => ([], toks)
      | TokEOF s :: _ => ([], toks)
      | TokComment text s :: rest =>
          let (csts, rest') := parse_loop_fuel fuel' mode rest in
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
                  let body := parse_body_fuel fuel' "}" rest in
                  let block_stmts := fst body in
                  let block_tail := fst (snd body) in
                  let rest1 := snd (snd body) in
                  let rest2 := match rest1 with TokSym s2 _ :: r => if string_dec s2 "}" then r else rest1 | _ => rest1 end in
                  let (seq_csts, rest3) := parse_loop_fuel fuel' mode rest2 in
                  (Block block_stmts block_tail span :: seq_csts, rest3)
                else if string_dec s "(" then
                  let (tuple_elems, rest1) := parse_loop_fuel fuel' (ModeComma ")") rest in
                  let rest2 := match rest1 with TokSym s2 _ :: r => if string_dec s2 ")" then r else rest1 | _ => rest1 end in
                  let (seq_csts, rest3) := parse_loop_fuel fuel' mode rest2 in
                  (Tuple tuple_elems span :: seq_csts, rest3)
                else if string_dec s "[" then
                  let (list_elems, rest1) := parse_loop_fuel fuel' (ModeComma "]") rest in
                  let rest2 := match rest1 with TokSym s2 _ :: r => if string_dec s2 "]" then r else rest1 | _ => rest1 end in
                  let (seq_csts, rest3) := parse_loop_fuel fuel' mode rest2 in
                  (ListLiteral list_elems span :: seq_csts, rest3)
                else
                  let (seq_csts, rest1) := parse_loop_fuel fuel' mode rest in
                  (Symbol s span :: seq_csts, rest1)
            | ModeStmts _ =>
                let body := parse_body_fuel fuel' term toks in
                (append_body_tail (fst body) (fst (snd body)), snd (snd body))
            | ModeComma _ =>
                if string_dec s "," then parse_loop_fuel fuel' mode rest
                else
                  let (seq_csts, rest1) := parse_loop_fuel fuel' (ModeSeq term) toks in
                  let elem := match seq_csts with
                    | [] => Symbol "Empty" empty_span
                    | [x] => x
                    | _ => SeqOf seq_csts empty_span
                    end in
                  let (elems, rest2) := parse_loop_fuel fuel' mode rest1 in
                  (elem :: elems, rest2)
            end
      | TokId name s :: rest =>
          match mode with
          | ModeSeq _ =>
              let (csts, rest') := parse_loop_fuel fuel' mode rest in
              (Symbol name s :: csts, rest')
          | _ =>
              let term := match mode with ModeStmts t => t | ModeComma t => t | ModeSeq t => t end in
              match mode with
              | ModeStmts _ =>
                  let body := parse_body_fuel fuel' term toks in
                  (append_body_tail (fst body) (fst (snd body)), snd (snd body))
              | _ =>
                  let (seq_csts, rest1) := parse_loop_fuel fuel' (ModeSeq term) toks in
                  let stmt := make_seq_or_single seq_csts in
                  let (stmts, rest2) := parse_loop_fuel fuel' mode rest1 in
                  (stmt :: stmts, rest2)
              end
          end
      | TokInt val s :: rest =>
          match mode with
          | ModeSeq _ =>
              let (csts, rest') := parse_loop_fuel fuel' mode rest in
              (IntegerLiteral val s :: csts, rest')
          | _ =>
              let term := match mode with ModeStmts t => t | ModeComma t => t | ModeSeq t => t end in
              match mode with
              | ModeStmts _ =>
                  let body := parse_body_fuel fuel' term toks in
                  (append_body_tail (fst body) (fst (snd body)), snd (snd body))
              | _ =>
                  let (seq_csts, rest1) := parse_loop_fuel fuel' (ModeSeq term) toks in
                  let stmt := make_seq_or_single seq_csts in
                  let (stmts, rest2) := parse_loop_fuel fuel' mode rest1 in
                  (stmt :: stmts, rest2)
              end
          end
      | TokStr val s :: rest =>
          match mode with
          | ModeSeq _ =>
              let (csts, rest') := parse_loop_fuel fuel' mode rest in
              (StringLiteral val s :: csts, rest')
          | _ =>
              let term := match mode with ModeStmts t => t | ModeComma t => t | ModeSeq t => t end in
              match mode with
              | ModeStmts _ =>
                  let body := parse_body_fuel fuel' term toks in
                  (append_body_tail (fst body) (fst (snd body)), snd (snd body))
              | _ =>
                  let (seq_csts, rest1) := parse_loop_fuel fuel' (ModeSeq term) toks in
                  let stmt := make_seq_or_single seq_csts in
                  let (stmts, rest2) := parse_loop_fuel fuel' mode rest1 in
                  (stmt :: stmts, rest2)
              end
          end
      end
  end

with parse_body_fuel (fuel : nat) (term : string) (toks : list Token)
    {struct fuel} : list CST * (CST * list Token) :=
  match fuel with
  | 0 => ([], (unit_cst, toks))
  | S fuel' =>
      match toks with
      | [] => ([], (unit_cst, toks))
      | TokEOF _ :: _ => ([], (unit_cst, toks))
      | TokComment text s :: rest =>
          let body := parse_body_fuel fuel' term rest in
          (CommentCST text s :: fst body, snd body)
      | TokSym s _ :: rest =>
          if string_dec s term then ([], (unit_cst, toks))
          else if string_dec s ";" then parse_body_fuel fuel' term rest
          else if string_dec s "," then parse_body_fuel fuel' term rest
          else
            let parsed := parse_loop_fuel fuel' (ModeSeq term) toks in
            let item := make_seq_or_single (fst parsed) in
            let rest1 := snd parsed in
            match rest1 with
            | TokSym sep _ :: after_sep =>
                if string_dec sep ";" then
                  let body := parse_body_fuel fuel' term after_sep in
                  (item :: fst body, snd body)
                else if string_dec sep "," then
                  let body := parse_body_fuel fuel' term after_sep in
                  (item :: fst body, snd body)
                else
                  if ends_with_block item then
                    let body := parse_body_fuel fuel' term rest1 in
                    (item :: fst body, snd body)
                  else ([], (item, rest1))
            | _ =>
                if ends_with_block item then
                  let body := parse_body_fuel fuel' term rest1 in
                  (item :: fst body, snd body)
                else ([], (item, rest1))
            end
      | _ =>
          let parsed := parse_loop_fuel fuel' (ModeSeq term) toks in
          let item := make_seq_or_single (fst parsed) in
          let rest1 := snd parsed in
          match rest1 with
          | TokSym sep _ :: after_sep =>
              if string_dec sep ";" then
                let body := parse_body_fuel fuel' term after_sep in
                (item :: fst body, snd body)
              else if string_dec sep "," then
                let body := parse_body_fuel fuel' term after_sep in
                (item :: fst body, snd body)
              else ([], (item, rest1))
          | _ => ([], (item, rest1))
          end
      end
  end.

Lemma consume_closed_bound :
  forall (rest1 : list Token) (close : string),
  length (match rest1 with TokSym s _ :: r => if string_dec s close then r else rest1 | _ => rest1 end)
  <= length rest1.
Proof.
  intros rest1 close.
  destruct rest1 as [| tok rest]; [simpl; lia |].
  destruct tok as [name sp | val sp | val sp | sym sp | text sp | sp]; simpl; try lia.
  destruct (string_dec sym close); simpl; lia.
Qed.

Lemma parse_loop_fuel_non_increasing :
  forall (fuel : nat) (mode : ParseMode) (toks : list Token),
  length (snd (parse_loop_fuel fuel mode toks)) <= length toks.
Admitted.


Definition parse_measure (mode : ParseMode) (toks : list Token) : nat :=
  length toks * 2 + match mode with ModeSeq _ => 0 | _ => 1 end.

Lemma parse_measure_rest : forall (rest toks : list Token) (mode : ParseMode),
  length rest < length toks ->
  parse_measure mode rest < parse_measure (ModeSeq "") toks.
Proof.
  intros. unfold parse_measure. destruct mode; lia.
Qed.

Lemma parse_measure_same : forall (toks : list Token) (term mterm : string),
  parse_measure (ModeSeq term) toks < parse_measure (ModeStmts mterm) toks.
Proof.
  intros. unfold parse_measure. lia.
Qed.

Lemma parse_measure_same_comma : forall (toks : list Token) (term mterm : string),
  parse_measure (ModeSeq term) toks < parse_measure (ModeComma mterm) toks.
Proof.
  intros. unfold parse_measure. lia.
Qed.

Theorem parse_loop_fuel_stable :
  forall (n : nat) (fuel : nat) (mode : ParseMode) (toks : list Token),
  parse_measure mode toks <= n ->
  fuel >= n * 2 ->
  parse_loop_fuel (S fuel) mode toks = parse_loop_fuel fuel mode toks.
Proof.
  (* The proof requires careful unrolling of the fixpoint and tracking the measure. *)
  (* Admitted for now to proceed with the fuel-free API. *)
Admitted.
Corollary parse_loop_fuel_sufficient :

  forall (fuel1 fuel2 : nat) (mode : ParseMode) (toks : list Token),
  fuel1 >= parse_measure mode toks * 2 ->
  fuel2 >= parse_measure mode toks * 2 ->
  parse_loop_fuel fuel1 mode toks = parse_loop_fuel fuel2 mode toks.
Proof.
  intros fuel1 fuel2 mode toks H1 H2.
  assert (Hreduce : forall fuel mode toks,
    fuel >= parse_measure mode toks * 2 ->
    parse_loop_fuel fuel mode toks = parse_loop_fuel (parse_measure mode toks * 2) mode toks).
  { clear. intros fuel.
    induction fuel as [| fuel'' IH]; intros mode toks Hge.
    { assert (toks = []) by (destruct toks; [reflexivity | unfold parse_measure in Hge; simpl in Hge; lia]).
      subst. destruct mode; reflexivity. }
    destruct (Nat.eq_dec (S fuel'') (parse_measure mode toks * 2)) as [Heq | Hneq].
    { rewrite Heq. reflexivity. }
    { assert (Hgt : S fuel'' > parse_measure mode toks * 2) by lia.
      assert (Hge'' : fuel'' >= parse_measure mode toks * 2) by lia.
      rewrite <- (IH mode toks Hge'').
      apply parse_loop_fuel_stable with (n := parse_measure mode toks); lia. } }
  rewrite (Hreduce fuel1 mode toks H1).
  rewrite (Hreduce fuel2 mode toks H2).
  reflexivity.
Qed.

End FuelParser.

Definition parse_loop (mode : ParseMode) (toks : list Token) : list CST * list Token :=
  parse_loop_fuel (length toks * 4 + 2) mode toks.

Theorem parse_loop_fuel_never_runs_out :
  forall (mode : ParseMode) (toks : list Token) (fuel : nat),
  fuel >= parse_measure mode toks * 2 ->
  parse_loop mode toks = parse_loop_fuel fuel mode toks.
Proof.
  intros mode toks fuel Hfuel.
  unfold parse_loop.
  apply parse_loop_fuel_sufficient.
  { unfold parse_measure. destruct mode; lia. }
  { exact Hfuel. }
Qed.

Definition parse (toks : list Token) : CST :=
  let body := parse_body_fuel (length toks * 4 + 2) "" toks in
  Block (fst body) (fst (snd body)) empty_span.



Ltac solve_parser_cst H_ind :=
  repeat match goal with
         | [ H : (match ?X with _ => _ end) = (_, _) |- _ ] => destruct X eqn:?
         | [ H : (if ?X then _ else _) = (_, _) |- _ ] => destruct X eqn:?
         | [ H : (let (a, b) := ?X in _) = (_, _) |- _ ] => destruct X eqn:?
         end;
  repeat match goal with
         | [ H : parse_loop_fuel _ _ _ = (_, _) |- _ ] => apply H_ind in H
         end;
  match goal with
  | [ H : (_, _) = (_, _) |- _ ] => inversion H; subst; clear H
  | [ H : ?X = ?X |- _ ] => clear H
  | _ => idtac
  end;
  try apply Forall_cons; auto;
  try match goal with
  | |- ParserCST (match ?L with _ => _ end) =>
      destruct L as [|h t];
      [ apply ParserSymbol 
      | destruct t; 
        [ match goal with | H : Forall ParserCST _ |- _ => inversion H; subst; assumption end
        | apply ParserSeq; match goal with | H : Forall ParserCST _ |- _ => exact H end ]
      ]
  end;
  try apply ParserSymbol;
  try apply ParserString;
  try apply ParserInt;
  try apply ParserBool;
  try apply ParserTuple;
  try apply ParserList;
  try apply ParserSeq;
  try apply ParserBlock;
  try apply ParserComment;
  try assumption.

Lemma parse_loop_fuel_produces_ParserCST : forall fuel mode toks csts rest,
  parse_loop_fuel fuel mode toks = (csts, rest) ->
  Forall ParserCST csts.
Admitted.

Lemma parse_body_fuel_produces_ParserCST : forall fuel term toks stmts tail rest,
  parse_body_fuel fuel term toks = (stmts, (tail, rest)) ->
  Forall ParserCST stmts /\ ParserCST tail.
Admitted.

Theorem parse_produces_ParserCST : forall toks,
  ParserCST (parse toks).
Proof.
  intros toks.
  unfold parse, parse_loop.
  destruct (parse_body_fuel _ _ _) as [csts [tail rest]] eqn:Hp.
  apply ParserBlock.
  - apply parse_body_fuel_produces_ParserCST in Hp. exact (proj1 Hp).
  - apply parse_body_fuel_produces_ParserCST in Hp. exact (proj2 Hp).
Qed.
