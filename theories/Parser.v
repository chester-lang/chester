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

Definition is_block_continue (toks : list Token) : bool :=
  match toks with
  | TokId s _ :: _ =>
      if string_dec s "else" then true
      else if string_dec s "catch" then true
      else if string_dec s "same_as" then true
      else if string_dec s "tighter_than" then true
      else if string_dec s "for" then true
      else if string_dec s "with" then true
      else false
  | _ => false
  end.

Definition parse_loop_step
    (parse_loop_rec : ParseMode -> list Token -> list CST * list Token)
    (parse_body_rec : string -> list Token -> list CST * (CST * list Token))
    (mode : ParseMode) (toks : list Token) : (list CST * list Token) :=
      match toks with
      | [] => ([], toks)
      | TokEOF s :: _ => ([], toks)
      | TokComment text s :: rest =>
          let (csts, rest') := parse_loop_rec mode rest in
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
                  let body := parse_body_rec "}" rest in
                  let block_stmts := fst body in
                  let block_tail := fst (snd body) in
                  let rest1 := snd (snd body) in
                  let rest2 := match rest1 with TokSym s2 _ :: r => if string_dec s2 "}" then r else rest1 | _ => rest1 end in
                  let block_cst := Block block_stmts block_tail span in
                  if is_block_continue rest2 then
                    let (seq_csts, rest3) := parse_loop_rec mode rest2 in
                    (block_cst :: seq_csts, rest3)
                  else ([block_cst], rest2)
                else if string_dec s "(" then
                  let (tuple_elems, rest1) := parse_loop_rec (ModeComma ")") rest in
                  let rest2 := match rest1 with TokSym s2 _ :: r => if string_dec s2 ")" then r else rest1 | _ => rest1 end in
                  let (seq_csts, rest3) := parse_loop_rec mode rest2 in
                  (Tuple tuple_elems span :: seq_csts, rest3)
                else if string_dec s "[" then
                  let (list_elems, rest1) := parse_loop_rec (ModeComma "]") rest in
                  let rest2 := match rest1 with TokSym s2 _ :: r => if string_dec s2 "]" then r else rest1 | _ => rest1 end in
                  let (seq_csts, rest3) := parse_loop_rec mode rest2 in
                  (ListLiteral list_elems span :: seq_csts, rest3)
                else
                  let (seq_csts, rest1) := parse_loop_rec mode rest in
                  (Symbol s span :: seq_csts, rest1)
            | ModeStmts _ =>
                let body := parse_body_rec term toks in
                (append_body_tail (fst body) (fst (snd body)), snd (snd body))
            | ModeComma _ =>
                if string_dec s "," then parse_loop_rec mode rest
                else if string_dec s ";" then parse_loop_rec mode rest
                else
                  let (seq_csts, rest1) := parse_loop_rec (ModeSeq term) toks in
                  let elem := match seq_csts with
                    | [] => Symbol "Empty" empty_span
                    | [x] => x
                    | _ => SeqOf seq_csts empty_span
                    end in
                  let (elems, rest2) := parse_loop_rec mode rest1 in
                  (elem :: elems, rest2)
            end
      | TokId name s :: rest =>
          match mode with
          | ModeSeq _ =>
              let (csts, rest') := parse_loop_rec mode rest in
              (Symbol name s :: csts, rest')
          | _ =>
              let term := match mode with ModeStmts t => t | ModeComma t => t | ModeSeq t => t end in
              match mode with
              | ModeStmts _ =>
                  let body := parse_body_rec term toks in
                  (append_body_tail (fst body) (fst (snd body)), snd (snd body))
              | _ =>
                  let (seq_csts, rest1) := parse_loop_rec (ModeSeq term) toks in
                  let stmt := make_seq_or_single seq_csts in
                  let (stmts, rest2) := parse_loop_rec mode rest1 in
                  (stmt :: stmts, rest2)
              end
          end
      | TokInt val s :: rest =>
          match mode with
          | ModeSeq _ =>
              let (csts, rest') := parse_loop_rec mode rest in
              (IntegerLiteral val s :: csts, rest')
          | _ =>
              let term := match mode with ModeStmts t => t | ModeComma t => t | ModeSeq t => t end in
              match mode with
              | ModeStmts _ =>
                  let body := parse_body_rec term toks in
                  (append_body_tail (fst body) (fst (snd body)), snd (snd body))
              | _ =>
                  let (seq_csts, rest1) := parse_loop_rec (ModeSeq term) toks in
                  let stmt := make_seq_or_single seq_csts in
                  let (stmts, rest2) := parse_loop_rec mode rest1 in
                  (stmt :: stmts, rest2)
              end
          end
      | TokStr val s :: rest =>
          match mode with
          | ModeSeq _ =>
              let (csts, rest') := parse_loop_rec mode rest in
              (StringLiteral val s :: csts, rest')
          | _ =>
              let term := match mode with ModeStmts t => t | ModeComma t => t | ModeSeq t => t end in
              match mode with
              | ModeStmts _ =>
                  let body := parse_body_rec term toks in
                  (append_body_tail (fst body) (fst (snd body)), snd (snd body))
              | _ =>
                  let (seq_csts, rest1) := parse_loop_rec (ModeSeq term) toks in
                  let stmt := make_seq_or_single seq_csts in
                  let (stmts, rest2) := parse_loop_rec mode rest1 in
                  (stmt :: stmts, rest2)
              end
          end
      end.

Definition parse_body_step
    (parse_loop_rec : ParseMode -> list Token -> list CST * list Token)
    (parse_body_rec : string -> list Token -> list CST * (CST * list Token))
    (term : string) (toks : list Token) : list CST * (CST * list Token) :=
      match toks with
      | [] => ([], (unit_cst, toks))
      | TokEOF _ :: _ => ([], (unit_cst, toks))
      | TokComment text s :: rest =>
          let body := parse_body_rec term rest in
          (CommentCST text s :: fst body, snd body)
      | _ =>
          match toks with
          | TokSym s _ :: rest =>
              if string_dec s term then ([], (unit_cst, toks))
              else if string_dec s ";" then parse_body_rec term rest
              else if string_dec s "," then parse_body_rec term rest
              else
                let parsed := parse_loop_rec (ModeSeq term) toks in
                let item := make_seq_or_single (fst parsed) in
                let rest1 := snd parsed in
                match rest1 with
                | TokSym sep _ :: after_sep =>
                    if string_dec sep ";" then
                      let body := parse_body_rec term after_sep in
                      (item :: fst body, snd body)
                    else if string_dec sep "," then
                      let body := parse_body_rec term after_sep in
                      (item :: fst body, snd body)
                    else if ends_with_block item then
                      if string_dec sep term then
                        (item :: [], (unit_cst, rest1))
                      else
                        let body := parse_body_rec term rest1 in
                        (item :: fst body, snd body)
                    else ([], (item, rest1))
                | [] =>
                    if ends_with_block item then (item :: [], (unit_cst, rest1))
                    else ([], (item, rest1))
                | TokEOF _ :: _ =>
                    if ends_with_block item then (item :: [], (unit_cst, rest1))
                    else ([], (item, rest1))
                | _ =>
                    if ends_with_block item then
                      let body := parse_body_rec term rest1 in
                      (item :: fst body, snd body)
                    else ([], (item, rest1))
                end
          | _ =>
              let parsed := parse_loop_rec (ModeSeq term) toks in
              let item := make_seq_or_single (fst parsed) in
              let rest1 := snd parsed in
              match rest1 with
              | TokSym sep _ :: after_sep =>
                  if string_dec sep ";" then
                    let body := parse_body_rec term after_sep in
                    (item :: fst body, snd body)
                  else if string_dec sep "," then
                    let body := parse_body_rec term after_sep in
                    (item :: fst body, snd body)
                  else if ends_with_block item then
                    if string_dec sep term then
                      ([], (item, rest1))
                    else
                      let body := parse_body_rec term rest1 in
                      (item :: fst body, snd body)
                  else ([], (item, rest1))
              | [] =>
                  if ends_with_block item then (item :: [], (unit_cst, rest1))
                  else ([], (item, rest1))
              | TokEOF _ :: _ =>
                  if ends_with_block item then (item :: [], (unit_cst, rest1))
                  else ([], (item, rest1))
              | _ =>
                  if ends_with_block item then
                    let body := parse_body_rec term rest1 in
                    (item :: fst body, snd body)
                  else ([], (item, rest1))
              end
          end
      end.

Fixpoint parse_loop_fuel (fuel : nat) (mode : ParseMode) (toks : list Token)
    {struct fuel} : (list CST * list Token) :=
  match fuel with
  | 0 => ([], toks)
  | S fuel' => parse_loop_step (parse_loop_fuel fuel') (parse_body_fuel fuel') mode toks
  end

with parse_body_fuel (fuel : nat) (term : string) (toks : list Token)
    {struct fuel} : list CST * (CST * list Token) :=
  match fuel with
  | 0 => ([], (unit_cst, toks))
  | S fuel' => parse_body_step (parse_loop_fuel fuel') (parse_body_fuel fuel') term toks
  end.

Lemma parse_loop_fuel_0 : forall mode toks,
  parse_loop_fuel 0 mode toks = ([], toks).
Proof. reflexivity. Qed.

Lemma parse_loop_fuel_S : forall fuel mode toks,
  parse_loop_fuel (S fuel) mode toks =
  parse_loop_step (parse_loop_fuel fuel) (parse_body_fuel fuel) mode toks.
Proof. reflexivity. Qed.

Lemma parse_body_fuel_0 : forall term toks,
  parse_body_fuel 0 term toks = ([], (unit_cst, toks)).
Proof. reflexivity. Qed.

Lemma parse_body_fuel_S : forall fuel term toks,
  parse_body_fuel (S fuel) term toks =
  parse_body_step (parse_loop_fuel fuel) (parse_body_fuel fuel) term toks.
Proof. reflexivity. Qed.

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

Ltac solve_body_case sc1 r1 fuel' term IHbody :=
  destruct r1 as [| t1 rest1];
  [ simpl; destruct (ends_with_block (make_seq_or_single sc1)); simpl; lia
  | destruct t1 as [t1name t1sp | t1val t1sp | t1val t1sp | t1sym t1sp | t1text t1sp | t1sp];
    [ (* TokId *)
      simpl; destruct (ends_with_block (make_seq_or_single sc1));
      [ pose proof (IHbody term (TokId t1name t1sp :: rest1));
        destruct (parse_body_fuel fuel' term (TokId t1name t1sp :: rest1)) as [b2 [tail2 r2]];
        simpl in *; lia
      | simpl in *; lia ]
    | (* TokInt *)
      simpl; destruct (ends_with_block (make_seq_or_single sc1));
      [ pose proof (IHbody term (TokInt t1val t1sp :: rest1));
        destruct (parse_body_fuel fuel' term (TokInt t1val t1sp :: rest1)) as [b2 [tail2 r2]];
        simpl in *; lia
      | simpl in *; lia ]
    | (* TokStr *)
      simpl; destruct (ends_with_block (make_seq_or_single sc1));
      [ pose proof (IHbody term (TokStr t1val t1sp :: rest1));
        destruct (parse_body_fuel fuel' term (TokStr t1val t1sp :: rest1)) as [b2 [tail2 r2]];
        simpl in *; lia
      | simpl in *; lia ]
    | (* TokSym *)
      simpl; destruct (string_dec t1sym ";");
      [ pose proof (IHbody term rest1);
        destruct (parse_body_fuel fuel' term rest1) as [b2 [tail2 r2]];
        simpl in *; lia
      | destruct (string_dec t1sym ",");
        [ pose proof (IHbody term rest1);
          destruct (parse_body_fuel fuel' term rest1) as [b2 [tail2 r2]];
          simpl in *; lia
        | destruct (ends_with_block (make_seq_or_single sc1));
          [ destruct (string_dec t1sym term);
            [ simpl in *; lia
            | pose proof (IHbody term (TokSym t1sym t1sp :: rest1));
              destruct (parse_body_fuel fuel' term (TokSym t1sym t1sp :: rest1)) as [b2 [tail2 r2]];
              simpl in *; lia ]
          | simpl in *; lia ] ] ]
    | (* TokComment *)
      simpl; destruct (ends_with_block (make_seq_or_single sc1));
      [ pose proof (IHbody term (TokComment t1text t1sp :: rest1));
        destruct (parse_body_fuel fuel' term (TokComment t1text t1sp :: rest1)) as [b2 [tail2 r2]];
        simpl in *; lia
      | simpl in *; lia ]
    | (* TokEOF *)
      simpl; destruct (ends_with_block (make_seq_or_single sc1)); simpl in *; lia
    ]
  ].

Lemma parse_fuel_non_increasing :
  forall (fuel : nat),
    (forall (mode : ParseMode) (toks : list Token),
      length (snd (parse_loop_fuel fuel mode toks)) <= length toks) /\
    (forall (term : string) (toks : list Token),
      length (snd (snd (parse_body_fuel fuel term toks))) <= length toks).
Proof.
  induction fuel as [| fuel' [IHloop IHbody]].
  - split; intros; simpl; lia.
  - split.
    + intros mode toks.
      destruct toks as [| tok rest]; [simpl; lia |].
      destruct tok as [name sp | val sp | val sp | sym sp | text sp | sp].
      * (* TokId *)
        destruct mode as [term | term | term]; simpl.
        -- pose proof (IHloop (ModeSeq term) rest).
           destruct (parse_loop_fuel fuel' (ModeSeq term) rest) as [c1 r1].
           simpl in *. lia.
        -- pose proof (IHbody term (TokId name sp :: rest)).
           destruct (parse_body_fuel fuel' term (TokId name sp :: rest)) as [b1 [t1 r1]].
           simpl in *. lia.
        -- pose proof (IHloop (ModeSeq term) (TokId name sp :: rest)).
           destruct (parse_loop_fuel fuel' (ModeSeq term) (TokId name sp :: rest)) as [sc1 r1].
           pose proof (IHloop (ModeComma term) r1).
           destruct (parse_loop_fuel fuel' (ModeComma term) r1) as [sc2 r2].
           simpl in *. lia.
      * (* TokInt *)
        destruct mode as [term | term | term]; simpl.
        -- pose proof (IHloop (ModeSeq term) rest).
           destruct (parse_loop_fuel fuel' (ModeSeq term) rest) as [c1 r1].
           simpl in *. lia.
        -- pose proof (IHbody term (TokInt val sp :: rest)).
           destruct (parse_body_fuel fuel' term (TokInt val sp :: rest)) as [b1 [t1 r1]].
           simpl in *. lia.
        -- pose proof (IHloop (ModeSeq term) (TokInt val sp :: rest)).
           destruct (parse_loop_fuel fuel' (ModeSeq term) (TokInt val sp :: rest)) as [sc1 r1].
           pose proof (IHloop (ModeComma term) r1).
           destruct (parse_loop_fuel fuel' (ModeComma term) r1) as [sc2 r2].
           simpl in *. lia.
      * (* TokStr *)
        destruct mode as [term | term | term]; simpl.
        -- pose proof (IHloop (ModeSeq term) rest).
           destruct (parse_loop_fuel fuel' (ModeSeq term) rest) as [c1 r1].
           simpl in *. lia.
        -- pose proof (IHbody term (TokStr val sp :: rest)).
           destruct (parse_body_fuel fuel' term (TokStr val sp :: rest)) as [b1 [t1 r1]].
           simpl in *. lia.
        -- pose proof (IHloop (ModeSeq term) (TokStr val sp :: rest)).
           destruct (parse_loop_fuel fuel' (ModeSeq term) (TokStr val sp :: rest)) as [sc1 r1].
           pose proof (IHloop (ModeComma term) r1).
           destruct (parse_loop_fuel fuel' (ModeComma term) r1) as [sc2 r2].
           simpl in *. lia.
      * (* TokSym *)
        destruct mode as [term | term | term]; simpl.
        -- destruct (string_dec sym term); [simpl; lia |].
           destruct (string_dec sym ";"); [simpl; lia |].
           destruct (string_dec sym ","); [simpl; lia |].
           destruct (string_dec sym "{").
           ++ pose proof (IHbody "}"%string rest).
              destruct (parse_body_fuel fuel' "}"%string rest) as [b1 [tail1 r1]].
              simpl in *.
              pose proof (consume_closed_bound r1 "}"%string) as Hc.
              remember (match r1 with | TokSym s2 _ :: r => if string_dec s2 "}"%string then r else r1 | _ => r1 end) as r2 eqn:Heqr2.
              destruct (is_block_continue r2) eqn:Ebc.
              ** pose proof (IHloop (ModeSeq term) r2).
                 destruct (parse_loop_fuel fuel' (ModeSeq term) r2) as [sc3 r3].
                 simpl in *. lia.
              ** simpl. lia.
           ++ destruct (string_dec sym "(").
              ** pose proof (IHloop (ModeComma ")"%string) rest).
                 destruct (parse_loop_fuel fuel' (ModeComma ")"%string) rest) as [t1 r1].
                 simpl in *.
                 pose proof (consume_closed_bound r1 ")"%string) as Hc.
                 remember (match r1 with | TokSym s2 _ :: r => if string_dec s2 ")"%string then r else r1 | _ => r1 end) as r2 eqn:Heqr2.
                 pose proof (IHloop (ModeSeq term) r2).
                 destruct (parse_loop_fuel fuel' (ModeSeq term) r2) as [sc2 r3].
                 simpl in *. lia.
              ** destruct (string_dec sym "[").
                 --- pose proof (IHloop (ModeComma "]"%string) rest).
                     destruct (parse_loop_fuel fuel' (ModeComma "]"%string) rest) as [t1 r1].
                     simpl in *.
                     pose proof (consume_closed_bound r1 "]"%string) as Hc.
                     remember (match r1 with | TokSym s2 _ :: r => if string_dec s2 "]"%string then r else r1 | _ => r1 end) as r2 eqn:Heqr2.
                     pose proof (IHloop (ModeSeq term) r2).
                     destruct (parse_loop_fuel fuel' (ModeSeq term) r2) as [sc2 r3].
                     simpl in *. lia.
                 --- pose proof (IHloop (ModeSeq term) rest).
                     destruct (parse_loop_fuel fuel' (ModeSeq term) rest) as [sc1 r1].
                     simpl in *. lia.
        -- destruct (string_dec sym term); [simpl; lia |].
           pose proof (IHbody term (TokSym sym sp :: rest)).
           destruct (parse_body_fuel fuel' term (TokSym sym sp :: rest)) as [b1 [t1 r1]].
           simpl in *. lia.
        -- destruct (string_dec sym term); [simpl; lia |].
           destruct (string_dec sym ",").
           ++ pose proof (IHloop (ModeComma term) rest). lia.
           ++ destruct (string_dec sym ";").
              ** pose proof (IHloop (ModeComma term) rest). lia.
              ** pose proof (IHloop (ModeSeq term) (TokSym sym sp :: rest)).
                 destruct (parse_loop_fuel fuel' (ModeSeq term) (TokSym sym sp :: rest)) as [sc1 r1].
                 pose proof (IHloop (ModeComma term) r1).
                 destruct (parse_loop_fuel fuel' (ModeComma term) r1) as [sc2 r2].
                 simpl in *. lia.
      * (* TokComment *)
        simpl.
        pose proof (IHloop mode rest).
        destruct (parse_loop_fuel fuel' mode rest) as [c1 r1].
        simpl in *. lia.
      * (* TokEOF *)
        simpl. lia.
    + intros term toks.
      destruct toks as [| tok rest]; [simpl; lia |].
      destruct tok as [name sp | val sp | val sp | sym sp | text sp | sp].
      * (* TokId in parse_body_fuel *)
        simpl.
        pose proof (IHloop (ModeSeq term) (TokId name sp :: rest)).
        destruct (parse_loop_fuel fuel' (ModeSeq term) (TokId name sp :: rest)) as [sc1 r1].
        solve_body_case sc1 r1 fuel' term IHbody.
      * (* TokInt in parse_body_fuel *)
        simpl.
        pose proof (IHloop (ModeSeq term) (TokInt val sp :: rest)).
        destruct (parse_loop_fuel fuel' (ModeSeq term) (TokInt val sp :: rest)) as [sc1 r1].
        solve_body_case sc1 r1 fuel' term IHbody.
      * (* TokStr in parse_body_fuel *)
        simpl.
        pose proof (IHloop (ModeSeq term) (TokStr val sp :: rest)).
        destruct (parse_loop_fuel fuel' (ModeSeq term) (TokStr val sp :: rest)) as [sc1 r1].
        solve_body_case sc1 r1 fuel' term IHbody.
      * (* TokSym in parse_body_fuel *)
        simpl.
        destruct (string_dec sym term); [simpl; lia |].
        destruct (string_dec sym ";").
        { pose proof (IHbody term rest).
          destruct (parse_body_fuel fuel' term rest) as [b2 [tail2 r2]].
          simpl in *. lia. }
        destruct (string_dec sym ",").
        { pose proof (IHbody term rest).
          destruct (parse_body_fuel fuel' term rest) as [b2 [tail2 r2]].
          simpl in *. lia. }
        pose proof (IHloop (ModeSeq term) (TokSym sym sp :: rest)).
        destruct (parse_loop_fuel fuel' (ModeSeq term) (TokSym sym sp :: rest)) as [sc1 r1].
        solve_body_case sc1 r1 fuel' term IHbody.
      * (* TokComment in parse_body_fuel *)
        simpl.
        pose proof (IHbody term rest).
        destruct (parse_body_fuel fuel' term rest) as [b1 [t1 r1]].
        simpl in *. lia.
      * (* TokEOF in parse_body_fuel *)
        simpl. lia.
Qed.

Lemma parse_loop_fuel_non_increasing :
  forall (fuel : nat) (mode : ParseMode) (toks : list Token),
  length (snd (parse_loop_fuel fuel mode toks)) <= length toks.
Proof.
  intros. apply (proj1 (parse_fuel_non_increasing fuel)).
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

Lemma parse_seq_progress_id : forall fuel term name sp rest,
  length (snd (parse_loop_fuel (S fuel) (ModeSeq term) (TokId name sp :: rest))) <= length rest.
Proof.
  intros fuel term name sp rest. simpl.
  pose proof (parse_loop_fuel_non_increasing fuel (ModeSeq term) rest).
  destruct (parse_loop_fuel fuel (ModeSeq term) rest) as [c1 r1]. simpl in *. lia.
Qed.

Lemma parse_seq_progress_int : forall fuel term val sp rest,
  length (snd (parse_loop_fuel (S fuel) (ModeSeq term) (TokInt val sp :: rest))) <= length rest.
Proof.
  intros fuel term val sp rest. simpl.
  pose proof (parse_loop_fuel_non_increasing fuel (ModeSeq term) rest).
  destruct (parse_loop_fuel fuel (ModeSeq term) rest) as [c1 r1]. simpl in *. lia.
Qed.

Lemma parse_seq_progress_str : forall fuel term val sp rest,
  length (snd (parse_loop_fuel (S fuel) (ModeSeq term) (TokStr val sp :: rest))) <= length rest.
Proof.
  intros fuel term val sp rest. simpl.
  pose proof (parse_loop_fuel_non_increasing fuel (ModeSeq term) rest).
  destruct (parse_loop_fuel fuel (ModeSeq term) rest) as [c1 r1]. simpl in *. lia.
Qed.

Lemma parse_seq_progress_sym : forall fuel term sym sp rest,
  sym <> term -> sym <> ";" -> sym <> "," ->
  length (snd (parse_loop_fuel (S fuel) (ModeSeq term) (TokSym sym sp :: rest))) <= length rest.
Proof.
  intros fuel term sym sp rest Hterm Hsemi Hcomma. simpl.
  destruct (string_dec sym term); [congruence |].
  destruct (string_dec sym ";"); [congruence |].
  destruct (string_dec sym ","); [congruence |].
  destruct (string_dec sym "{").
  - pose proof (proj2 (parse_fuel_non_increasing fuel) "}"%string rest) as Hb.
    destruct (parse_body_fuel fuel "}"%string rest) as [b1 [tail1 r1]]. simpl in *.
    pose proof (consume_closed_bound r1 "}"%string) as Hc.
    remember (match r1 with TokSym s2 _ :: r => if string_dec s2 "}"%string then r else r1 | _ => r1 end) as r2.
    destruct (is_block_continue r2).
    + pose proof (parse_loop_fuel_non_increasing fuel (ModeSeq term) r2) as Hl.
      destruct (parse_loop_fuel fuel (ModeSeq term) r2) as [sc3 r3]. simpl in *. lia.
    + simpl. lia.
  - destruct (string_dec sym "(").
    + pose proof (parse_loop_fuel_non_increasing fuel (ModeComma ")"%string) rest) as H1.
      destruct (parse_loop_fuel fuel (ModeComma ")"%string) rest) as [t1 r1]. simpl in *.
      pose proof (consume_closed_bound r1 ")"%string) as Hc.
      remember (match r1 with TokSym s2 _ :: r => if string_dec s2 ")"%string then r else r1 | _ => r1 end) as r2.
      pose proof (parse_loop_fuel_non_increasing fuel (ModeSeq term) r2) as H2.
      destruct (parse_loop_fuel fuel (ModeSeq term) r2) as [sc2 r3]. simpl in *. lia.
    + destruct (string_dec sym "[").
      * pose proof (parse_loop_fuel_non_increasing fuel (ModeComma "]"%string) rest) as H1.
        destruct (parse_loop_fuel fuel (ModeComma "]"%string) rest) as [t1 r1]. simpl in *.
        pose proof (consume_closed_bound r1 "]"%string) as Hc.
        remember (match r1 with TokSym s2 _ :: r => if string_dec s2 "]"%string then r else r1 | _ => r1 end) as r2.
        pose proof (parse_loop_fuel_non_increasing fuel (ModeSeq term) r2) as H2.
        destruct (parse_loop_fuel fuel (ModeSeq term) r2) as [sc2 r3]. simpl in *. lia.
      * pose proof (parse_loop_fuel_non_increasing fuel (ModeSeq term) rest) as H1.
        destruct (parse_loop_fuel fuel (ModeSeq term) rest) as [sc1 r1]. simpl in *. lia.
Qed.

Lemma body_step_eq :
  forall (item : CST) (b1 b2 : list CST * (CST * list Token)),
  b1 = b2 ->
  (item :: fst b1, snd b1) = (item :: fst b2, snd b2).
Proof.
  intros item b1 b2 H. rewrite H. reflexivity.
Qed.

Lemma comma_step_eq :
  forall (item : CST) (c1 c2 : list CST * list Token),
  c1 = c2 ->
  (let (stmts, rest2) := c1 in (item :: stmts, rest2)) =
  (let (stmts, rest2) := c2 in (item :: stmts, rest2)).
Proof.
  intros item c1 c2 H. rewrite H. reflexivity.
Qed.

Lemma stmts_step_eq :
  forall (b1 b2 : list CST * (CST * list Token)),
  b1 = b2 ->
  (append_body_tail (fst b1) (fst (snd b1)), snd (snd b1)) =
  (append_body_tail (fst b2) (fst (snd b2)), snd (snd b2)).
Proof.
  intros b1 b2 H. rewrite H. reflexivity.
Qed.

Ltac solve_stable_body_cont_ext Hbody sc1 r1 term Hprog :=
  cbn [fst snd];
  rewrite (Hbody r1 ltac:(cbn [length]; lia));
  destruct r1 as [| [t1name t1sp | t1val t1sp | t1val t1sp | t1sym t1sp | t1text t1sp | t1sp] rest1];
  try reflexivity;
  rewrite (Hbody rest1 ltac:(cbn [length] in *; lia));
  reflexivity.

Lemma parse_seq_step_ext :
  forall (loop1 loop2 : ParseMode -> list Token -> list CST * list Token)
         (body1 body2 : string -> list Token -> list CST * (CST * list Token))
         (toks : list Token) (term : string),
  (forall r, length r < length toks -> loop1 (ModeSeq term) r = loop2 (ModeSeq term) r) ->
  (forall close r, length r < length toks -> loop1 (ModeComma close) r = loop2 (ModeComma close) r) ->
  (forall close r, length r < length toks -> body1 close r = body2 close r) ->
  (forall close r, length (snd (loop2 (ModeComma close) r)) <= length r) ->
  (forall close r, length (snd (snd (body2 close r))) <= length r) ->
  parse_loop_step loop1 body1 (ModeSeq term) toks =
  parse_loop_step loop2 body2 (ModeSeq term) toks.
Proof.
  intros loop1 loop2 body1 body2 [| [name sp | val sp | val sp | sym sp | text sp | sp] rest] term
    Hseq Hcomma Hbody Hlen_comma Hlen_body;
    cbn [parse_loop_step].
  - reflexivity.
  - rewrite (Hseq rest ltac:(cbn [length]; lia)). reflexivity.
  - rewrite (Hseq rest ltac:(cbn [length]; lia)). reflexivity.
  - rewrite (Hseq rest ltac:(cbn [length]; lia)). reflexivity.
  - destruct (string_dec sym term); [reflexivity |].
    destruct (string_dec sym ";"); [reflexivity |].
    destruct (string_dec sym ","); [reflexivity |].
    destruct (string_dec sym "{").
    + rewrite (Hbody "}"%string rest ltac:(cbn [length]; lia)).
      pose proof (Hlen_body "}"%string rest) as Hb.
      destruct (body2 "}"%string rest) as [b1 [tail1 r1]]. cbn [fst snd] in *.
      pose proof (consume_closed_bound r1 "}"%string) as Hc.
      remember (match r1 with TokSym s2 _ :: r => if string_dec s2 "}"%string then r else r1 | _ => r1 end) as r2.
      destruct (is_block_continue r2); [| reflexivity].
      rewrite (Hseq r2 ltac:(cbn [length]; lia)). reflexivity.
    + destruct (string_dec sym "(").
      * rewrite (Hcomma ")"%string rest ltac:(cbn [length]; lia)).
        pose proof (Hlen_comma ")"%string rest) as H1.
        destruct (loop2 (ModeComma ")"%string) rest) as [t1 r1]. cbn [fst snd] in *.
        pose proof (consume_closed_bound r1 ")"%string) as Hc.
        remember (match r1 with TokSym s2 _ :: r => if string_dec s2 ")"%string then r else r1 | _ => r1 end) as r2.
        rewrite (Hseq r2 ltac:(cbn [length]; lia)). reflexivity.
      * destruct (string_dec sym "[").
        -- rewrite (Hcomma "]"%string rest ltac:(cbn [length]; lia)).
           pose proof (Hlen_comma "]"%string rest) as H1.
           destruct (loop2 (ModeComma "]"%string) rest) as [t1 r1]. cbn [fst snd] in *.
           pose proof (consume_closed_bound r1 "]"%string) as Hc.
           remember (match r1 with TokSym s2 _ :: r => if string_dec s2 "]"%string then r else r1 | _ => r1 end) as r2.
           rewrite (Hseq r2 ltac:(cbn [length]; lia)). reflexivity.
        -- rewrite (Hseq rest ltac:(cbn [length]; lia)). reflexivity.
  - rewrite (Hseq rest ltac:(cbn [length]; lia)). reflexivity.
  - reflexivity.
Qed.

Lemma parse_comma_step_ext :
  forall (loop1 loop2 : ParseMode -> list Token -> list CST * list Token)
         (body1 body2 : string -> list Token -> list CST * (CST * list Token))
         (toks : list Token) (term : string),
  (forall r, length r <= length toks -> loop1 (ModeSeq term) r = loop2 (ModeSeq term) r) ->
  (forall r, length r < length toks -> loop1 (ModeComma term) r = loop2 (ModeComma term) r) ->
  (forall name sp rest, length (snd (loop2 (ModeSeq term) (TokId name sp :: rest))) <= length rest) ->
  (forall val sp rest, length (snd (loop2 (ModeSeq term) (TokInt val sp :: rest))) <= length rest) ->
  (forall val sp rest, length (snd (loop2 (ModeSeq term) (TokStr val sp :: rest))) <= length rest) ->
  (forall sym sp rest, sym <> term -> sym <> ";" -> sym <> "," ->
    length (snd (loop2 (ModeSeq term) (TokSym sym sp :: rest))) <= length rest) ->
  parse_loop_step loop1 body1 (ModeComma term) toks =
  parse_loop_step loop2 body2 (ModeComma term) toks.
Proof.
  intros loop1 loop2 body1 body2 [| [name sp | val sp | val sp | sym sp | text sp | sp] rest] term
    Hseq Hcomma Hprog_id Hprog_int Hprog_str Hprog_sym;
    cbn [parse_loop_step].
  - reflexivity.
  - pose proof (Hprog_id name sp rest) as Hprog.
    rewrite (Hseq (TokId name sp :: rest) ltac:(cbn [length]; lia)).
    destruct (loop2 (ModeSeq term) (TokId name sp :: rest)) as [sc1 r1]. cbn [snd] in Hprog.
    apply comma_step_eq. apply Hcomma. cbn [length]. lia.
  - pose proof (Hprog_int val sp rest) as Hprog.
    rewrite (Hseq (TokInt val sp :: rest) ltac:(cbn [length]; lia)).
    destruct (loop2 (ModeSeq term) (TokInt val sp :: rest)) as [sc1 r1]. cbn [snd] in Hprog.
    apply comma_step_eq. apply Hcomma. cbn [length]. lia.
  - pose proof (Hprog_str val sp rest) as Hprog.
    rewrite (Hseq (TokStr val sp :: rest) ltac:(cbn [length]; lia)).
    destruct (loop2 (ModeSeq term) (TokStr val sp :: rest)) as [sc1 r1]. cbn [snd] in Hprog.
    apply comma_step_eq. apply Hcomma. cbn [length]. lia.
  - destruct (string_dec sym term); [reflexivity |].
    destruct (string_dec sym ","); [apply Hcomma; cbn [length]; lia |].
    destruct (string_dec sym ";"); [apply Hcomma; cbn [length]; lia |].
    pose proof (Hprog_sym sym sp rest n n1 n0) as Hprog.
    rewrite (Hseq (TokSym sym sp :: rest) ltac:(cbn [length]; lia)).
    destruct (loop2 (ModeSeq term) (TokSym sym sp :: rest)) as [sc1 r1]. cbn [snd] in Hprog.
    apply comma_step_eq. apply Hcomma. cbn [length]. lia.
  - apply comma_step_eq. apply Hcomma. cbn [length]. lia.
  - reflexivity.
Qed.

Lemma parse_body_step_ext :
  forall (loop1 loop2 : ParseMode -> list Token -> list CST * list Token)
         (body1 body2 : string -> list Token -> list CST * (CST * list Token))
         (toks : list Token) (term : string),
  (forall r, length r <= length toks -> loop1 (ModeSeq term) r = loop2 (ModeSeq term) r) ->
  (forall r, length r < length toks -> body1 term r = body2 term r) ->
  (forall name sp rest, length (snd (loop2 (ModeSeq term) (TokId name sp :: rest))) <= length rest) ->
  (forall val sp rest, length (snd (loop2 (ModeSeq term) (TokInt val sp :: rest))) <= length rest) ->
  (forall val sp rest, length (snd (loop2 (ModeSeq term) (TokStr val sp :: rest))) <= length rest) ->
  (forall sym sp rest, sym <> term -> sym <> ";" -> sym <> "," ->
    length (snd (loop2 (ModeSeq term) (TokSym sym sp :: rest))) <= length rest) ->
  parse_body_step loop1 body1 term toks =
  parse_body_step loop2 body2 term toks.
Proof.
  intros loop1 loop2 body1 body2 [| [name sp | val sp | val sp | sym sp | text sp | sp] rest] term
    Hseq Hbody Hprog_id Hprog_int Hprog_str Hprog_sym;
    cbn [parse_body_step].
  - reflexivity.
  - pose proof (Hprog_id name sp rest) as Hprog.
    rewrite (Hseq (TokId name sp :: rest) ltac:(cbn [length]; lia)).
    destruct (loop2 (ModeSeq term) (TokId name sp :: rest)) as [sc1 r1]. cbn [snd] in Hprog.
    solve_stable_body_cont_ext Hbody sc1 r1 term Hprog.
  - pose proof (Hprog_int val sp rest) as Hprog.
    rewrite (Hseq (TokInt val sp :: rest) ltac:(cbn [length]; lia)).
    destruct (loop2 (ModeSeq term) (TokInt val sp :: rest)) as [sc1 r1]. cbn [snd] in Hprog.
    solve_stable_body_cont_ext Hbody sc1 r1 term Hprog.
  - pose proof (Hprog_str val sp rest) as Hprog.
    rewrite (Hseq (TokStr val sp :: rest) ltac:(cbn [length]; lia)).
    destruct (loop2 (ModeSeq term) (TokStr val sp :: rest)) as [sc1 r1]. cbn [snd] in Hprog.
    solve_stable_body_cont_ext Hbody sc1 r1 term Hprog.
  - destruct (string_dec sym term); [reflexivity |].
    destruct (string_dec sym ";"); [apply Hbody; cbn [length]; lia |].
    destruct (string_dec sym ","); [apply Hbody; cbn [length]; lia |].
    pose proof (Hprog_sym sym sp rest n n0 n1) as Hprog.
    rewrite (Hseq (TokSym sym sp :: rest) ltac:(cbn [length]; lia)).
    destruct (loop2 (ModeSeq term) (TokSym sym sp :: rest)) as [sc1 r1]. cbn [snd] in Hprog.
    solve_stable_body_cont_ext Hbody sc1 r1 term Hprog.
  - apply body_step_eq. apply Hbody. cbn [length]. lia.
  - reflexivity.
Qed.

Lemma parse_stmts_step_ext :
  forall (loop1 loop2 : ParseMode -> list Token -> list CST * list Token)
         (body1 body2 : string -> list Token -> list CST * (CST * list Token))
         (toks : list Token) (term : string),
  (forall r, length r <= length toks -> body1 term r = body2 term r) ->
  (forall r, length r < length toks -> loop1 (ModeStmts term) r = loop2 (ModeStmts term) r) ->
  parse_loop_step loop1 body1 (ModeStmts term) toks =
  parse_loop_step loop2 body2 (ModeStmts term) toks.
Proof.
  intros loop1 loop2 body1 body2 [| [name sp | val sp | val sp | sym sp | text sp | sp] rest] term
    Hbody Hstmts;
    cbn [parse_loop_step].
  - reflexivity.
  - apply stmts_step_eq. apply Hbody. lia.
  - apply stmts_step_eq. apply Hbody. lia.
  - apply stmts_step_eq. apply Hbody. lia.
  - destruct (string_dec sym term); [reflexivity |].
    apply stmts_step_eq. apply Hbody. lia.
  - apply comma_step_eq. apply Hstmts. cbn [length]. lia.
  - reflexivity.
Qed.

Opaque parse_loop_fuel parse_body_fuel parse_loop_step parse_body_step.

Lemma parse_fuel_stable_by_len :
  forall (k : nat),
    (forall toks term fuel,
      length toks <= k ->
      fuel >= length toks * 4 ->
      parse_loop_fuel (S fuel) (ModeSeq term) toks = parse_loop_fuel fuel (ModeSeq term) toks) /\
    (forall toks term fuel,
      length toks <= k ->
      fuel >= length toks * 4 + 1 ->
      parse_loop_fuel (S fuel) (ModeComma term) toks = parse_loop_fuel fuel (ModeComma term) toks) /\
    (forall toks term fuel,
      length toks <= k ->
      fuel >= length toks * 4 + 1 ->
      parse_body_fuel (S fuel) term toks = parse_body_fuel fuel term toks) /\
    (forall toks term fuel,
      length toks <= k ->
      fuel >= length toks * 4 + 2 ->
      parse_loop_fuel (S fuel) (ModeStmts term) toks = parse_loop_fuel fuel (ModeStmts term) toks).
Proof.
  induction k as [| k' [IHseq [IHcomma [IHbody IHstmts]]]].
  - repeat split; intros toks term fuel Hlen Hfuel;
      destruct toks; try (simpl in Hlen; lia);
      destruct fuel as [| fuel'];
      try (rewrite parse_loop_fuel_S, parse_loop_fuel_0; reflexivity);
      try (rewrite (parse_loop_fuel_S (S fuel')), (parse_loop_fuel_S fuel'); reflexivity);
      try (rewrite parse_body_fuel_S, parse_body_fuel_0; reflexivity);
      try (rewrite (parse_body_fuel_S (S fuel')), (parse_body_fuel_S fuel'); reflexivity).
  - assert (Hseq : forall toks term fuel,
      length toks <= S k' ->
      fuel >= length toks * 4 ->
      parse_loop_fuel (S fuel) (ModeSeq term) toks = parse_loop_fuel fuel (ModeSeq term) toks).
    { intros [| tok rest] term fuel Hlen Hfuel.
      - destruct fuel as [| fuel'];
          [ rewrite parse_loop_fuel_S, parse_loop_fuel_0; reflexivity
          | rewrite (parse_loop_fuel_S (S fuel')), (parse_loop_fuel_S fuel'); reflexivity ].
      - destruct fuel as [| fuel']; [cbn [length] in Hfuel; lia |].
        rewrite (parse_loop_fuel_S (S fuel')), (parse_loop_fuel_S fuel').
        apply parse_seq_step_ext.
        + intros r Hr. apply IHseq; cbn [length] in *; lia.
        + intros close r Hr. apply IHcomma; cbn [length] in *; lia.
        + intros close r Hr. apply IHbody; cbn [length] in *; lia.
        + intros close r. apply parse_loop_fuel_non_increasing.
        + intros close r. apply (proj2 (parse_fuel_non_increasing fuel')). }
    assert (Hcomma : forall toks term fuel,
      length toks <= S k' ->
      fuel >= length toks * 4 + 1 ->
      parse_loop_fuel (S fuel) (ModeComma term) toks = parse_loop_fuel fuel (ModeComma term) toks).
    { intros [| tok rest] term fuel Hlen Hfuel.
      - destruct fuel as [| fuel'];
          [ rewrite parse_loop_fuel_S, parse_loop_fuel_0; reflexivity
          | rewrite (parse_loop_fuel_S (S fuel')), (parse_loop_fuel_S fuel'); reflexivity ].
      - destruct fuel as [| fuel']; [cbn [length] in Hfuel; lia |].
        destruct fuel' as [| fuel'']; [cbn [length] in Hfuel; lia |].
        rewrite (parse_loop_fuel_S (S (S fuel''))), (parse_loop_fuel_S (S fuel'')).
        apply parse_comma_step_ext.
        + intros r Hr. apply Hseq; cbn [length] in *; lia.
        + intros r Hr. apply IHcomma; cbn [length] in *; lia.
        + intros name sp r. apply parse_seq_progress_id.
        + intros val sp r. apply parse_seq_progress_int.
        + intros val sp r. apply parse_seq_progress_str.
        + intros sym sp r. apply parse_seq_progress_sym. }
    assert (Hbody : forall toks term fuel,
      length toks <= S k' ->
      fuel >= length toks * 4 + 1 ->
      parse_body_fuel (S fuel) term toks = parse_body_fuel fuel term toks).
    { intros [| tok rest] term fuel Hlen Hfuel.
      - destruct fuel as [| fuel'];
          [ rewrite parse_body_fuel_S, parse_body_fuel_0; reflexivity
          | rewrite (parse_body_fuel_S (S fuel')), (parse_body_fuel_S fuel'); reflexivity ].
      - destruct fuel as [| fuel']; [cbn [length] in Hfuel; lia |].
        destruct fuel' as [| fuel'']; [cbn [length] in Hfuel; lia |].
        rewrite (parse_body_fuel_S (S (S fuel''))), (parse_body_fuel_S (S fuel'')).
        apply parse_body_step_ext.
        + intros r Hr. apply Hseq; cbn [length] in *; lia.
        + intros r Hr. apply IHbody; cbn [length] in *; lia.
        + intros name sp r. apply parse_seq_progress_id.
        + intros val sp r. apply parse_seq_progress_int.
        + intros val sp r. apply parse_seq_progress_str.
        + intros sym sp r. apply parse_seq_progress_sym. }
    assert (Hstmts : forall toks term fuel,
      length toks <= S k' ->
      fuel >= length toks * 4 + 2 ->
      parse_loop_fuel (S fuel) (ModeStmts term) toks = parse_loop_fuel fuel (ModeStmts term) toks).
    { intros [| tok rest] term fuel Hlen Hfuel.
      - destruct fuel as [| fuel'];
          [ rewrite parse_loop_fuel_S, parse_loop_fuel_0; reflexivity
          | rewrite (parse_loop_fuel_S (S fuel')), (parse_loop_fuel_S fuel'); reflexivity ].
      - destruct fuel as [| fuel']; [cbn [length] in Hfuel; lia |].
        rewrite (parse_loop_fuel_S (S fuel')), (parse_loop_fuel_S fuel').
        apply parse_stmts_step_ext.
        + intros r Hr. apply Hbody; cbn [length] in *; lia.
        + intros r Hr. apply IHstmts; cbn [length] in *; lia. }
    repeat split; assumption.
Qed.

Transparent parse_loop_fuel parse_body_fuel parse_loop_step parse_body_step.

Theorem parse_loop_fuel_stable :
  forall (n : nat) (fuel : nat) (mode : ParseMode) (toks : list Token),
  parse_measure mode toks <= n ->
  fuel >= n * 2 ->
  parse_loop_fuel (S fuel) mode toks = parse_loop_fuel fuel mode toks.
Proof.
  intros n fuel mode toks Hmeas Hfuel.
  pose proof (parse_fuel_stable_by_len (length toks)) as [Hseq [Hcomma [_ Hstmts]]].
  unfold parse_measure in Hmeas.
  destruct mode as [term | term | term].
  - apply Hseq; lia.
  - apply Hstmts; lia.
  - apply Hcomma; lia.
Qed.
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



Lemma parser_cst_unit : ParserCST unit_cst.
Proof.
  unfold unit_cst. apply ParserSymbol.
Qed.

Lemma parser_cst_make_seq : forall csts,
  Forall ParserCST csts ->
  ParserCST (make_seq_or_single csts).
Proof.
  intros csts H. unfold make_seq_or_single.
  destruct csts as [| x [| y rest]].
  - apply ParserSymbol.
  - inversion H; subst; assumption.
  - apply ParserSeq. assumption.
Qed.

Lemma parser_cst_append_tail : forall stmts tail,
  Forall ParserCST stmts ->
  ParserCST tail ->
  Forall ParserCST (append_body_tail stmts tail).
Proof.
  intros stmts tail Hstmts Htail.
  unfold append_body_tail.
  destruct tail; try (apply Forall_app; split; [assumption | constructor; [assumption | constructor]]).
  destruct (string_dec s "Unit"); [assumption |].
  apply Forall_app. split; [assumption | constructor; [assumption | constructor]].
Qed.

Ltac solve_body_cst_branch IHloop IHbody sc1 r1 fuel' term H :=
  destruct r1 as [| t1 rest1];
  [ simpl in H;
    destruct (ends_with_block (make_seq_or_single sc1));
    inversion H; subst;
    try (split; [ constructor; [ apply parser_cst_make_seq; eapply IHloop; eassumption | constructor ]
                | apply parser_cst_unit ]);
    try (split; [ constructor | apply parser_cst_make_seq; eapply IHloop; eassumption ])
  | destruct t1 as [t1name t1sp | t1val t1sp | t1val t1sp | t1sym t1sp | t1text t1sp | t1sp];
    simpl in H;
    repeat match type of H with
           | context [if string_dec ?s ?t then _ else _] => destruct (string_dec s t)
           | context [ends_with_block (make_seq_or_single sc1)] => destruct (ends_with_block (make_seq_or_single sc1))
           | context [parse_body_fuel fuel' term ?toks] =>
               let Hrec := fresh "Hrec" in
               pose proof (IHbody term toks) as Hrec;
               let b2 := fresh "b" in
               let tail2 := fresh "tail" in
               let r2 := fresh "r" in
               destruct (parse_body_fuel fuel' term toks) as [b2 [tail2 r2]];
               destruct (Hrec b2 tail2 r2 eq_refl)
           end;
    inversion H; subst;
    try (split; [ constructor; [ apply parser_cst_make_seq; eapply IHloop; eassumption | constructor ]
                | apply parser_cst_unit ]);
    try (split; [ constructor | apply parser_cst_make_seq; eapply IHloop; eassumption ]);
    try (split; [ constructor; [ apply parser_cst_make_seq; eapply IHloop; eassumption | assumption ]
                | assumption ])
  ].

Lemma parse_fuel_produces_ParserCST :
  forall fuel,
    (forall mode toks csts rest,
      parse_loop_fuel fuel mode toks = (csts, rest) ->
      Forall ParserCST csts) /\
    (forall term toks stmts tail rest,
      parse_body_fuel fuel term toks = (stmts, (tail, rest)) ->
      Forall ParserCST stmts /\ ParserCST tail).
Proof.
  induction fuel as [| fuel' [IHloop IHbody]].
  - split.
    + intros mode toks csts rest H.
      simpl in H. inversion H; subst. constructor.
    + intros term toks stmts tail rest H.
      simpl in H. inversion H; subst.
      split; [constructor | apply parser_cst_unit].
  - split.
    + intros mode toks.
      destruct toks as [| tok rest_toks].
      * intros csts rest H. simpl in H. inversion H; subst. constructor.
      * destruct tok as [name sp | val sp | val sp | sym sp | text sp | sp].
        -- (* TokId *)
           intros csts rest H.
           destruct mode as [term | term | term]; simpl in H.
           ++ destruct (parse_loop_fuel fuel' (ModeSeq term) rest_toks) as [c1 r1] eqn:E1.
              inversion H; subst.
              constructor; [apply ParserSymbol | eapply IHloop; eassumption].
           ++ pose proof (IHbody term (TokId name sp :: rest_toks)) as Hrec.
              destruct (parse_body_fuel fuel' term (TokId name sp :: rest_toks)) as [b1 [t1 r1]].
              destruct (Hrec b1 t1 r1 eq_refl) as [Hb1 Ht1].
              inversion H; subst.
              apply parser_cst_append_tail; assumption.
           ++ destruct (parse_loop_fuel fuel' (ModeSeq term) (TokId name sp :: rest_toks)) as [sc1 r1] eqn:E1.
              destruct (parse_loop_fuel fuel' (ModeComma term) r1) as [sc2 r2] eqn:E2.
              inversion H; subst.
              constructor.
              ** apply parser_cst_make_seq. eapply IHloop; eassumption.
              ** eapply IHloop; eassumption.
        -- (* TokInt *)
           intros csts rest H.
           destruct mode as [term | term | term]; simpl in H.
           ++ destruct (parse_loop_fuel fuel' (ModeSeq term) rest_toks) as [c1 r1] eqn:E1.
              inversion H; subst.
              constructor; [apply ParserInt | eapply IHloop; eassumption].
           ++ pose proof (IHbody term (TokInt val sp :: rest_toks)) as Hrec.
              destruct (parse_body_fuel fuel' term (TokInt val sp :: rest_toks)) as [b1 [t1 r1]].
              destruct (Hrec b1 t1 r1 eq_refl) as [Hb1 Ht1].
              inversion H; subst.
              apply parser_cst_append_tail; assumption.
           ++ destruct (parse_loop_fuel fuel' (ModeSeq term) (TokInt val sp :: rest_toks)) as [sc1 r1] eqn:E1.
              destruct (parse_loop_fuel fuel' (ModeComma term) r1) as [sc2 r2] eqn:E2.
              inversion H; subst.
              constructor.
              ** apply parser_cst_make_seq. eapply IHloop; eassumption.
              ** eapply IHloop; eassumption.
        -- (* TokStr *)
           intros csts rest H.
           destruct mode as [term | term | term]; simpl in H.
           ++ destruct (parse_loop_fuel fuel' (ModeSeq term) rest_toks) as [c1 r1] eqn:E1.
              inversion H; subst.
              constructor; [apply ParserString | eapply IHloop; eassumption].
           ++ pose proof (IHbody term (TokStr val sp :: rest_toks)) as Hrec.
              destruct (parse_body_fuel fuel' term (TokStr val sp :: rest_toks)) as [b1 [t1 r1]].
              destruct (Hrec b1 t1 r1 eq_refl) as [Hb1 Ht1].
              inversion H; subst.
              apply parser_cst_append_tail; assumption.
           ++ destruct (parse_loop_fuel fuel' (ModeSeq term) (TokStr val sp :: rest_toks)) as [sc1 r1] eqn:E1.
              destruct (parse_loop_fuel fuel' (ModeComma term) r1) as [sc2 r2] eqn:E2.
              inversion H; subst.
              constructor.
              ** apply parser_cst_make_seq. eapply IHloop; eassumption.
              ** eapply IHloop; eassumption.
        -- (* TokSym *)
           intros csts rest H.
           destruct mode as [term | term | term]; simpl in H.
           ++ destruct (string_dec sym term); [inversion H; subst; constructor |].
              destruct (string_dec sym ";"); [inversion H; subst; constructor |].
              destruct (string_dec sym ","); [inversion H; subst; constructor |].
              destruct (string_dec sym "{").
              ** pose proof (IHbody "}"%string rest_toks) as Hrec.
                 destruct (parse_body_fuel fuel' "}"%string rest_toks) as [b1 [tail1 r1]].
                 destruct (Hrec b1 tail1 r1 eq_refl) as [Hb1 Htail1].
                 destruct (is_block_continue _) eqn:Ebc;
                 [ destruct (parse_loop_fuel fuel' (ModeSeq term) _) as [sc3 r3] eqn:Esc3;
                   inversion H; subst;
                   constructor; [apply ParserBlock; assumption | eapply IHloop; eassumption]
                 | inversion H; subst;
                   constructor; [apply ParserBlock; assumption | constructor] ].
              ** destruct (string_dec sym "(").
                 --- destruct (parse_loop_fuel fuel' (ModeComma ")"%string) rest_toks) as [t1 r1] eqn:Et1.
                     pose proof (IHloop (ModeComma ")"%string) rest_toks t1 r1 Et1) as Ht1.
                     destruct (parse_loop_fuel fuel' (ModeSeq term) _) as [sc2 r3] eqn:Esc2.
                     inversion H; subst.
                     constructor; [apply ParserTuple; assumption | eapply IHloop; eassumption].
                 --- destruct (string_dec sym "[").
                     +++ destruct (parse_loop_fuel fuel' (ModeComma "]"%string) rest_toks) as [t1 r1] eqn:Et1.
                         pose proof (IHloop (ModeComma "]"%string) rest_toks t1 r1 Et1) as Ht1.
                         destruct (parse_loop_fuel fuel' (ModeSeq term) _) as [sc2 r3] eqn:Esc2.
                         inversion H; subst.
                         constructor; [apply ParserList; assumption | eapply IHloop; eassumption].
                     +++ destruct (parse_loop_fuel fuel' (ModeSeq term) rest_toks) as [sc1 r1] eqn:Esc1.
                         inversion H; subst.
                         constructor; [apply ParserSymbol | eapply IHloop; eassumption].
           ++ destruct (string_dec sym term); [inversion H; subst; constructor |].
              pose proof (IHbody term (TokSym sym sp :: rest_toks)) as Hrec.
              destruct (parse_body_fuel fuel' term (TokSym sym sp :: rest_toks)) as [b1 [t1 r1]].
              destruct (Hrec b1 t1 r1 eq_refl) as [Hb1 Ht1].
              inversion H; subst.
              apply parser_cst_append_tail; assumption.
           ++ destruct (string_dec sym term); [inversion H; subst; constructor |].
              destruct (string_dec sym ",").
              ** eapply IHloop. eassumption.
              ** destruct (string_dec sym ";").
                 --- eapply IHloop. eassumption.
                 --- destruct (parse_loop_fuel fuel' (ModeSeq term) (TokSym sym sp :: rest_toks)) as [sc1 r1] eqn:Esc1.
                     destruct (parse_loop_fuel fuel' (ModeComma term) r1) as [sc2 r2] eqn:Esc2.
                     inversion H; subst.
                     constructor.
                     +++ apply parser_cst_make_seq. eapply IHloop; eassumption.
                     +++ eapply IHloop; eassumption.
        -- (* TokComment *)
           intros csts rest H.
           simpl in H.
           destruct (parse_loop_fuel fuel' mode rest_toks) as [c1 r1] eqn:E1.
           inversion H; subst.
           constructor; [apply ParserComment | eapply IHloop; eassumption].
        -- (* TokEOF *)
           intros csts rest H.
           simpl in H. inversion H; subst. constructor.
    + intros term toks stmts tail rest H.
      destruct toks as [| tok rest_toks].
      * simpl in H. inversion H; subst.
        split; [constructor | apply parser_cst_unit].
      * destruct tok as [name sp | val sp | val sp | sym sp | text sp | sp].
        -- (* TokId in body *)
           simpl in H.
           destruct (parse_loop_fuel fuel' (ModeSeq term) (TokId name sp :: rest_toks)) as [sc1 r1] eqn:Esc1.
           solve_body_cst_branch IHloop IHbody sc1 r1 fuel' term H.
        -- (* TokInt in body *)
           simpl in H.
           destruct (parse_loop_fuel fuel' (ModeSeq term) (TokInt val sp :: rest_toks)) as [sc1 r1] eqn:Esc1.
           solve_body_cst_branch IHloop IHbody sc1 r1 fuel' term H.
        -- (* TokStr in body *)
           simpl in H.
           destruct (parse_loop_fuel fuel' (ModeSeq term) (TokStr val sp :: rest_toks)) as [sc1 r1] eqn:Esc1.
           solve_body_cst_branch IHloop IHbody sc1 r1 fuel' term H.
        -- (* TokSym in body *)
           simpl in H.
           destruct (string_dec sym term).
           ++ inversion H; subst. split; [constructor | apply parser_cst_unit].
           ++ destruct (string_dec sym ";").
              ** eapply IHbody; eassumption.
              ** destruct (string_dec sym ",").
                 --- eapply IHbody; eassumption.
                 --- destruct (parse_loop_fuel fuel' (ModeSeq term) (TokSym sym sp :: rest_toks)) as [sc1 r1] eqn:Esc1.
                     solve_body_cst_branch IHloop IHbody sc1 r1 fuel' term H.
        -- (* TokComment in body *)
           simpl in H.
           pose proof (IHbody term rest_toks) as Hrec.
           destruct (parse_body_fuel fuel' term rest_toks) as [b1 [t1 r1]].
           destruct (Hrec b1 t1 r1 eq_refl) as [Hb1 Ht1].
           inversion H; subst.
           split; [constructor; [apply ParserComment | assumption] | assumption].
        -- (* TokEOF in body *)
           simpl in H. inversion H; subst.
           split; [constructor | apply parser_cst_unit].
Qed.

Lemma parse_loop_fuel_produces_ParserCST : forall fuel mode toks csts rest,
  parse_loop_fuel fuel mode toks = (csts, rest) ->
  Forall ParserCST csts.
Proof.
  intros. eapply (proj1 (parse_fuel_produces_ParserCST fuel)). eassumption.
Qed.

Lemma parse_body_fuel_produces_ParserCST : forall fuel term toks stmts tail rest,
  parse_body_fuel fuel term toks = (stmts, (tail, rest)) ->
  Forall ParserCST stmts /\ ParserCST tail.
Proof.
  intros. eapply (proj2 (parse_fuel_produces_ParserCST fuel)). eassumption.
Qed.

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
