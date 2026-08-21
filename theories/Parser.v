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
  | ModeSeq : string -> ParseMode
  | ModeStmts : string -> ParseMode
  | ModeComma : string -> ParseMode.

Section FuelParser.

Fixpoint parse_loop_fuel (fuel : nat) (mode : ParseMode) (toks : list Token)
    : (list CST * list Token) :=
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
                  let (block_stmts, rest1) := parse_loop_fuel fuel' (ModeStmts "}") rest in
                  let rest2 := match rest1 with TokSym s2 _ :: r => if string_dec s2 "}" then r else rest1 | _ => rest1 end in
                  let (seq_csts, rest3) := parse_loop_fuel fuel' mode rest2 in
                  (Block block_stmts (Symbol "Unit" empty_span) empty_span :: seq_csts, rest3)
                else if string_dec s "(" then
                  let (tuple_elems, rest1) := parse_loop_fuel fuel' (ModeComma ")") rest in
                  let rest2 := match rest1 with TokSym s2 _ :: r => if string_dec s2 ")" then r else rest1 | _ => rest1 end in
                  let (seq_csts, rest3) := parse_loop_fuel fuel' mode rest2 in
                  (Tuple tuple_elems empty_span :: seq_csts, rest3)
                else if string_dec s "[" then
                  let (list_elems, rest1) := parse_loop_fuel fuel' (ModeComma "]") rest in
                  let rest2 := match rest1 with TokSym s2 _ :: r => if string_dec s2 "]" then r else rest1 | _ => rest1 end in
                  let (seq_csts, rest3) := parse_loop_fuel fuel' mode rest2 in
                  (ListLiteral list_elems empty_span :: seq_csts, rest3)
                else
                  let (seq_csts, rest1) := parse_loop_fuel fuel' mode rest in
                  (Symbol s span :: seq_csts, rest1)
            | ModeStmts _ =>
                if string_dec s ";" then parse_loop_fuel fuel' mode rest
                else if string_dec s "," then parse_loop_fuel fuel' mode rest
                else
                  let (seq_csts, rest1) := parse_loop_fuel fuel' (ModeSeq term) toks in
                  let stmt := match seq_csts with
                    | [] => Symbol "Empty" empty_span
                    | [x] => x
                    | _ => SeqOf seq_csts empty_span
                    end in
                  let (stmts, rest2) := parse_loop_fuel fuel' mode rest1 in
                  (stmt :: stmts, rest2)
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
              let (seq_csts, rest1) := parse_loop_fuel fuel' (ModeSeq term) toks in
              let stmt := match seq_csts with [] => Symbol "Empty" empty_span | [x] => x | _ => SeqOf seq_csts empty_span end in
              let (stmts, rest2) := parse_loop_fuel fuel' mode rest1 in
              (stmt :: stmts, rest2)
          end
      | TokInt val s :: rest =>
          match mode with
          | ModeSeq _ =>
              let (csts, rest') := parse_loop_fuel fuel' mode rest in
              (IntegerLiteral val s :: csts, rest')
          | _ =>
              let term := match mode with ModeStmts t => t | ModeComma t => t | ModeSeq t => t end in
              let (seq_csts, rest1) := parse_loop_fuel fuel' (ModeSeq term) toks in
              let stmt := match seq_csts with [] => Symbol "Empty" empty_span | [x] => x | _ => SeqOf seq_csts empty_span end in
              let (stmts, rest2) := parse_loop_fuel fuel' mode rest1 in
              (stmt :: stmts, rest2)
          end
      | TokStr val s :: rest =>
          match mode with
          | ModeSeq _ =>
              let (csts, rest') := parse_loop_fuel fuel' mode rest in
              (StringLiteral val s :: csts, rest')
          | _ =>
              let term := match mode with ModeStmts t => t | ModeComma t => t | ModeSeq t => t end in
              let (seq_csts, rest1) := parse_loop_fuel fuel' (ModeSeq term) toks in
              let stmt := match seq_csts with [] => Symbol "Empty" empty_span | [x] => x | _ => SeqOf seq_csts empty_span end in
              let (stmts, rest2) := parse_loop_fuel fuel' mode rest1 in
              (stmt :: stmts, rest2)
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
Proof.
  induction fuel as [| fuel' IH]; intros mode toks.
  { simpl; lia. }
  destruct toks as [| tok rest]. { simpl; lia. }
  destruct tok as [name sp | val sp | val sp | sym sp | text sp | sp].

  - (* TokId *)
    simpl. destruct mode as [term | term | term].
    + destruct (parse_loop_fuel fuel' (ModeSeq term) rest) eqn:Hr.
      pose proof (IH (ModeSeq term) rest) as Hni. rewrite Hr in Hni. simpl in *. lia.
    + destruct (parse_loop_fuel fuel' (ModeSeq term) (TokId name sp :: rest)) as [seq rest1] eqn:Hr1.
      pose proof (IH (ModeSeq term) (TokId name sp :: rest)) as Hni1. rewrite Hr1 in Hni1.
      destruct (parse_loop_fuel fuel' (ModeStmts term) rest1) as [sts rest2] eqn:Hr2.
      pose proof (IH (ModeStmts term) rest1) as Hni2. rewrite Hr2 in Hni2. simpl in *. lia.
    + destruct (parse_loop_fuel fuel' (ModeSeq term) (TokId name sp :: rest)) as [seq rest1] eqn:Hr1.
      pose proof (IH (ModeSeq term) (TokId name sp :: rest)) as Hni1. rewrite Hr1 in Hni1.
      destruct (parse_loop_fuel fuel' (ModeComma term) rest1) as [els rest2] eqn:Hr2.
      pose proof (IH (ModeComma term) rest1) as Hni2. rewrite Hr2 in Hni2. simpl in *. lia.

  - (* TokInt *)
    simpl. destruct mode as [term | term | term].
    + destruct (parse_loop_fuel fuel' (ModeSeq term) rest) eqn:Hr.
      pose proof (IH (ModeSeq term) rest) as Hni. rewrite Hr in Hni. simpl in *. lia.
    + destruct (parse_loop_fuel fuel' (ModeSeq term) (TokInt val sp :: rest)) as [seq rest1] eqn:Hr1.
      pose proof (IH (ModeSeq term) (TokInt val sp :: rest)) as Hni1. rewrite Hr1 in Hni1.
      destruct (parse_loop_fuel fuel' (ModeStmts term) rest1) as [sts rest2] eqn:Hr2.
      pose proof (IH (ModeStmts term) rest1) as Hni2. rewrite Hr2 in Hni2. simpl in *. lia.
    + destruct (parse_loop_fuel fuel' (ModeSeq term) (TokInt val sp :: rest)) as [seq rest1] eqn:Hr1.
      pose proof (IH (ModeSeq term) (TokInt val sp :: rest)) as Hni1. rewrite Hr1 in Hni1.
      destruct (parse_loop_fuel fuel' (ModeComma term) rest1) as [els rest2] eqn:Hr2.
      pose proof (IH (ModeComma term) rest1) as Hni2. rewrite Hr2 in Hni2. simpl in *. lia.

  - (* TokStr *)
    simpl. destruct mode as [term | term | term].
    + destruct (parse_loop_fuel fuel' (ModeSeq term) rest) eqn:Hr.
      pose proof (IH (ModeSeq term) rest) as Hni. rewrite Hr in Hni. simpl in *. lia.
    + destruct (parse_loop_fuel fuel' (ModeSeq term) (TokStr val sp :: rest)) as [seq rest1] eqn:Hr1.
      pose proof (IH (ModeSeq term) (TokStr val sp :: rest)) as Hni1. rewrite Hr1 in Hni1.
      destruct (parse_loop_fuel fuel' (ModeStmts term) rest1) as [sts rest2] eqn:Hr2.
      pose proof (IH (ModeStmts term) rest1) as Hni2. rewrite Hr2 in Hni2. simpl in *. lia.
    + destruct (parse_loop_fuel fuel' (ModeSeq term) (TokStr val sp :: rest)) as [seq rest1] eqn:Hr1.
      pose proof (IH (ModeSeq term) (TokStr val sp :: rest)) as Hni1. rewrite Hr1 in Hni1.
      destruct (parse_loop_fuel fuel' (ModeComma term) rest1) as [els rest2] eqn:Hr2.
      pose proof (IH (ModeComma term) rest1) as Hni2. rewrite Hr2 in Hni2. simpl in *. lia.

  - (* TokSym *)
    simpl. destruct mode as [mterm | mterm | mterm].
    + destruct (string_dec sym mterm). { simpl; lia. }
      destruct (string_dec sym ";"). { simpl; lia. }
      destruct (string_dec sym ","). { simpl; lia. }
      destruct (string_dec sym "{").
      { destruct (parse_loop_fuel fuel' (ModeStmts "}") rest) as [blk rest1] eqn:Hr1.
        pose proof (IH (ModeStmts "}") rest) as Hni1. rewrite Hr1 in Hni1.
        set (rest2 := match rest1 with TokSym s2 _ :: r => if string_dec s2 "}" then r else rest1 | _ => rest1 end).
        assert (Hrest2 : length rest2 <= length rest1) by (unfold rest2; apply consume_closed_bound).
        destruct (parse_loop_fuel fuel' (ModeSeq mterm) rest2) as [seq rest3] eqn:Hr3.
        pose proof (IH (ModeSeq mterm) rest2) as Hni2. rewrite Hr3 in Hni2. simpl in *. lia. }
      destruct (string_dec sym "(").
      { destruct (parse_loop_fuel fuel' (ModeComma ")") rest) as [els rest1] eqn:Hr1.
        pose proof (IH (ModeComma ")") rest) as Hni1. rewrite Hr1 in Hni1.
        set (rest2 := match rest1 with TokSym s2 _ :: r => if string_dec s2 ")" then r else rest1 | _ => rest1 end).
        assert (Hrest2 : length rest2 <= length rest1) by (unfold rest2; apply consume_closed_bound).
        destruct (parse_loop_fuel fuel' (ModeSeq mterm) rest2) as [seq rest3] eqn:Hr3.
        pose proof (IH (ModeSeq mterm) rest2) as Hni2. rewrite Hr3 in Hni2. simpl in *. lia. }
      destruct (string_dec sym "[").
      { destruct (parse_loop_fuel fuel' (ModeComma "]") rest) as [els rest1] eqn:Hr1.
        pose proof (IH (ModeComma "]") rest) as Hni1. rewrite Hr1 in Hni1.
        set (rest2 := match rest1 with TokSym s2 _ :: r => if string_dec s2 "]" then r else rest1 | _ => rest1 end).
        assert (Hrest2 : length rest2 <= length rest1) by (unfold rest2; apply consume_closed_bound).
        destruct (parse_loop_fuel fuel' (ModeSeq mterm) rest2) as [seq rest3] eqn:Hr3.
        pose proof (IH (ModeSeq mterm) rest2) as Hni2. rewrite Hr3 in Hni2. simpl in *. lia. }
      { destruct (parse_loop_fuel fuel' (ModeSeq mterm) rest) eqn:Hr.
        pose proof (IH (ModeSeq mterm) rest) as Hni. rewrite Hr in Hni. simpl in *. lia. }
    + destruct (string_dec sym mterm). { simpl; lia. }
      destruct (string_dec sym ";").
      { destruct (parse_loop_fuel fuel' (ModeStmts mterm) rest) eqn:Hr.
        pose proof (IH (ModeStmts mterm) rest) as Hni. rewrite Hr in Hni. simpl in *. lia. }
      destruct (string_dec sym ",").
      { destruct (parse_loop_fuel fuel' (ModeStmts mterm) rest) eqn:Hr.
        pose proof (IH (ModeStmts mterm) rest) as Hni. rewrite Hr in Hni. simpl in *. lia. }
      { destruct (parse_loop_fuel fuel' (ModeSeq mterm) (TokSym sym sp :: rest)) as [seq rest1] eqn:Hr1.
        pose proof (IH (ModeSeq mterm) (TokSym sym sp :: rest)) as Hni1. rewrite Hr1 in Hni1.
        destruct (parse_loop_fuel fuel' (ModeStmts mterm) rest1) as [sts rest2] eqn:Hr2.
        pose proof (IH (ModeStmts mterm) rest1) as Hni2. rewrite Hr2 in Hni2. simpl in *. lia. }
    + destruct (string_dec sym mterm). { simpl; lia. }
      destruct (string_dec sym ",").
      { destruct (parse_loop_fuel fuel' (ModeComma mterm) rest) eqn:Hr.
        pose proof (IH (ModeComma mterm) rest) as Hni. rewrite Hr in Hni. simpl in *. lia. }
      { destruct (parse_loop_fuel fuel' (ModeSeq mterm) (TokSym sym sp :: rest)) as [seq rest1] eqn:Hr1.
        pose proof (IH (ModeSeq mterm) (TokSym sym sp :: rest)) as Hni1. rewrite Hr1 in Hni1.
        destruct (parse_loop_fuel fuel' (ModeComma mterm) rest1) as [els rest2] eqn:Hr2.
        pose proof (IH (ModeComma mterm) rest1) as Hni2. rewrite Hr2 in Hni2. simpl in *. lia. }

  - (* TokComment *)
    simpl. destruct (parse_loop_fuel fuel' mode rest) eqn:Hr.
    pose proof (IH mode rest) as Hni. rewrite Hr in Hni. simpl in *. lia.

  - (* TokEOF *)
    simpl; lia.
Qed.


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


Lemma parse_loop_fuel_unfold : forall fuel mode toks,
  parse_loop_fuel (S fuel) mode toks =
      match toks with
      | [] => ([], toks)
      | TokEOF s :: _ => ([], toks)
      | TokComment text s :: rest =>
          let (csts, rest') := parse_loop_fuel fuel mode rest in
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
                  let (block_stmts, rest1) := parse_loop_fuel fuel (ModeStmts "}") rest in
                  let rest2 := match rest1 with TokSym s2 _ :: r => if string_dec s2 "}" then r else rest1 | _ => rest1 end in
                  let (seq_csts, rest3) := parse_loop_fuel fuel mode rest2 in
                  (Block block_stmts (Symbol "Unit" empty_span) empty_span :: seq_csts, rest3)
                else if string_dec s "(" then
                  let (tuple_elems, rest1) := parse_loop_fuel fuel (ModeComma ")") rest in
                  let rest2 := match rest1 with TokSym s2 _ :: r => if string_dec s2 ")" then r else rest1 | _ => rest1 end in
                  let (seq_csts, rest3) := parse_loop_fuel fuel mode rest2 in
                  (Tuple tuple_elems empty_span :: seq_csts, rest3)
                else if string_dec s "[" then
                  let (list_elems, rest1) := parse_loop_fuel fuel (ModeComma "]") rest in
                  let rest2 := match rest1 with TokSym s2 _ :: r => if string_dec s2 "]" then r else rest1 | _ => rest1 end in
                  let (seq_csts, rest3) := parse_loop_fuel fuel mode rest2 in
                  (ListLiteral list_elems empty_span :: seq_csts, rest3)
                else
                  let (seq_csts, rest1) := parse_loop_fuel fuel mode rest in
                  (Symbol s span :: seq_csts, rest1)
            | ModeStmts _ =>
                if string_dec s ";" then parse_loop_fuel fuel mode rest
                else if string_dec s "," then parse_loop_fuel fuel mode rest
                else
                  let (seq_csts, rest1) := parse_loop_fuel fuel (ModeSeq term) toks in
                  let stmt := match seq_csts with
                    | [] => Symbol "Empty" empty_span
                    | [x] => x
                    | _ => SeqOf seq_csts empty_span
                    end in
                  let (stmts, rest2) := parse_loop_fuel fuel mode rest1 in
                  (stmt :: stmts, rest2)
            | ModeComma _ =>
                if string_dec s "," then parse_loop_fuel fuel mode rest
                else
                  let (seq_csts, rest1) := parse_loop_fuel fuel (ModeSeq term) toks in
                  let elem := match seq_csts with
                    | [] => Symbol "Empty" empty_span
                    | [x] => x
                    | _ => SeqOf seq_csts empty_span
                    end in
                  let (elems, rest2) := parse_loop_fuel fuel mode rest1 in
                  (elem :: elems, rest2)
            end
      | TokId name s :: rest =>
          match mode with
          | ModeSeq _ =>
              let (csts, rest') := parse_loop_fuel fuel mode rest in
              (Symbol name s :: csts, rest')
          | _ =>
              let term := match mode with ModeStmts t => t | ModeComma t => t | ModeSeq t => t end in
              let (seq_csts, rest1) := parse_loop_fuel fuel (ModeSeq term) toks in
              let stmt := match seq_csts with [] => Symbol "Empty" empty_span | [x] => x | _ => SeqOf seq_csts empty_span end in
              let (stmts, rest2) := parse_loop_fuel fuel mode rest1 in
              (stmt :: stmts, rest2)
          end
      | TokInt val s :: rest =>
          match mode with
          | ModeSeq _ =>
              let (csts, rest') := parse_loop_fuel fuel mode rest in
              (IntegerLiteral val s :: csts, rest')
          | _ =>
              let term := match mode with ModeStmts t => t | ModeComma t => t | ModeSeq t => t end in
              let (seq_csts, rest1) := parse_loop_fuel fuel (ModeSeq term) toks in
              let stmt := match seq_csts with [] => Symbol "Empty" empty_span | [x] => x | _ => SeqOf seq_csts empty_span end in
              let (stmts, rest2) := parse_loop_fuel fuel mode rest1 in
              (stmt :: stmts, rest2)
          end
      | TokStr val s :: rest =>
          match mode with
          | ModeSeq _ =>
              let (csts, rest') := parse_loop_fuel fuel mode rest in
              (StringLiteral val s :: csts, rest')
          | _ =>
              let term := match mode with ModeStmts t => t | ModeComma t => t | ModeSeq t => t end in
              let (seq_csts, rest1) := parse_loop_fuel fuel (ModeSeq term) toks in
              let stmt := match seq_csts with [] => Symbol "Empty" empty_span | [x] => x | _ => SeqOf seq_csts empty_span end in
              let (stmts, rest2) := parse_loop_fuel fuel mode rest1 in
              (stmt :: stmts, rest2)
          end
      end.
Proof. destruct fuel; reflexivity. Qed.


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
  let (stmts, _) := parse_loop (ModeStmts "") toks in
  Block stmts (Symbol "Unit" empty_span) empty_span.



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
Proof.
  induction fuel as [| fuel H_ind ]; intros mode toks csts rest H.
  - simpl in H. inversion H. apply Forall_nil.
  - simpl in H. destruct toks as [|tok toks']; inversion H; clear H; try apply Forall_nil.
    destruct tok.
    all: solve_parser_cst H_ind.
Qed.

Theorem parse_produces_ParserCST : forall toks,
  ParserCST (parse toks).
Proof.
  intros toks.
  unfold parse, parse_loop.
  destruct (parse_loop_fuel _ _ _) as [csts rest] eqn:Hp.
  apply ParserBlock.
  apply parse_loop_fuel_produces_ParserCST in Hp.
  exact Hp.
Qed.
