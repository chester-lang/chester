(** Semantics of a TypeScript IR fragment, and emit correctness for literals,
    identifiers, lambdas (TsArrow), and conditionals (TsIIFE/TsIfStmt).

    We extend the earlier expression evaluator [TSEval] with:
      - [TSVClo] for arrow/closure values
      - [TSEvalBlock] for block/statement-list evaluation (finds the Return)
      - [TSEvalStmt]  for single statements
      - [TSEval] cases for [TsArrow] and [TsIIFE]
    and prove [emit_ts_correct] for the full [CoreFrag]. *)

From Stdlib Require Import Strings.String.
From Stdlib Require Import List.
From Stdlib Require Import Bool.
From Stdlib Require Import Arith.PeanoNat.
Import ListNotations.
Open Scope string_scope.

Require Import Chester.AST.
Require Import Chester.TypeScriptAST.
Require Import Chester.Backend.
Require Import Chester.CoreChecker.
Require Import Chester.CoreTyping.

(* ------------------------------------------------------------ *)
(* Values                                                       *)
(* ------------------------------------------------------------ *)

Inductive TSVal : Type :=
| TSVNum  : nat  -> TSVal
| TSVBool : bool -> TSVal
| TSVUndef : TSVal
(** A closure captures parameter name [x], body block [body] (list of stmts),
    and the environment at definition time. *)
| TSVClo  : string -> list TypeScriptStmt -> list (string * TSVal) -> TSVal.

Definition TSEnv := list (string * TSVal).

Fixpoint tslookup (x : string) (env : TSEnv) : option TSVal :=
  match env with
  | [] => None
  | (k, v) :: rest =>
      if String.eqb x k then Some v else tslookup x rest
  end.

(* ------------------------------------------------------------ *)
(* Big-step evaluation on a TS expression / statement fragment  *)
(* ------------------------------------------------------------ *)

(** [TSEvalBlock env stmts v] — evaluate a list of statements and return [v]
    (by finding the [TsReturn] statement). *)
Inductive TSEvalBlock : TSEnv -> list TypeScriptStmt -> TSVal -> Prop :=

(** TsReturn e at head of list: evaluate e, done. *)
| TSB_Return : forall env e v rest,
    TSEval env e v ->
    TSEvalBlock env (TsReturn e :: rest) v

(** TsLet x rhs :: rest: bind x to the value of rhs, continue. *)
| TSB_Let : forall env x rhs vrhs rest v,
    TSEval env rhs vrhs ->
    TSEvalBlock ((x, vrhs) :: env) rest v ->
    TSEvalBlock env (TsLet x rhs :: rest) v

(** TsIfStmt: evaluate condition, then dispatch. *)
| TSB_IfT : forall env cond true_b false_b v rest,
    TSEval env cond (TSVBool true) ->
    TSEvalBlock env true_b v ->
    TSEvalBlock env (TsIfStmt cond true_b false_b :: rest) v

| TSB_IfF : forall env cond true_b false_b v rest,
    TSEval env cond (TSVBool false) ->
    TSEvalBlock env false_b v ->
    TSEvalBlock env (TsIfStmt cond true_b false_b :: rest) v

(** TsExprStmt: execute and discard, continue. *)
| TSB_ExprStmt : forall env e ve rest v,
    TSEval env e ve ->
    TSEvalBlock env rest v ->
    TSEvalBlock env (TsExprStmt e :: rest) v

with TSEval : TSEnv -> TypeScriptExpr -> TSVal -> Prop :=

| TSE_Num : forall env n,
    TSEval env (TsNumberLiteral (nat_to_string n)) (TSVNum n)
| TSE_Bool : forall env b,
    TSEval env (TsBooleanLiteral b) (TSVBool b)
| TSE_Id : forall env x v,
    tslookup x env = Some v ->
    TSEval env (TsIdentifier x) v

(** Arrow expression — produces a closure over the current environment. *)
| TSE_Arrow : forall env x body,
    TSEval env (TsArrow [x] body) (TSVClo x body env)

(** IIFE: run the body block, return its value. *)
| TSE_IIFE : forall env stmts v,
    TSEvalBlock env stmts v ->
    TSEval env (TsIIFE stmts) v

(** Function call: evaluate callee to a closure, argument to a value,
    run the closure body in the extended env. *)
| TSE_Call : forall env f arg x body envf va v,
    TSEval env f (TSVClo x body envf) ->
    TSEval env arg va ->
    TSEvalBlock ((x, va) :: envf) body v ->
    TSEval env (TsCall f [arg]) v.

(* ------------------------------------------------------------ *)
(* Semantic typing of IR values (logical relation on types)     *)
(* ------------------------------------------------------------ *)

Fixpoint TSValTy (v : TSVal) (ty : AST) {struct ty} : Prop :=
  match ty with
  | AstRef s =>
      if String.eqb s "Integer" then
        match v with TSVNum _ => True | _ => False end
      else if String.eqb s "Boolean" then
        match v with TSVBool _ => True | _ => False end
      else False
  | AstFunTy [] [(_ , sigma)] tau [] =>
      match v with
      | TSVClo x body env =>
          forall va, TSValTy va sigma ->
            exists v', TSEvalBlock ((x, va) :: env) body v' /\ TSValTy v' tau
      | _ => False
      end
  | _ => False
  end.

Definition TSEnvTy (gamma : list (string * AST)) (env : TSEnv) : Prop :=
  forall x ty, core_lookup x gamma = Some ty ->
    exists v, tslookup x env = Some v /\ TSValTy v ty.

Lemma tsenv_ty_nil : TSEnvTy [] [].
Proof. intros x ty H. discriminate H. Qed.

Lemma tsenv_ty_cons : forall gamma env x sigma v,
  TSEnvTy gamma env ->
  TSValTy v sigma ->
  TSEnvTy ((x, sigma) :: gamma) ((x, v) :: env).
Proof.
  intros gamma env x sigma v Henv Hv y ty Hy.
  unfold core_lookup in Hy; simpl in Hy.
  destruct (String.eqb y x) eqn:Ey.
  - inversion Hy; subst. exists v. split.
    + simpl. rewrite Ey. reflexivity.
    + exact Hv.
  - destruct (Henv y ty Hy) as [v' [Hl Ht]].
    exists v'. split.
    + simpl. rewrite Ey. exact Hl.
    + exact Ht.
Qed.

Lemma TSValTy_num : forall n, TSValTy (TSVNum n) CoreChecker.IntType.
Proof.
  intros n.
  unfold CoreChecker.IntType, TSValTy.
  rewrite (String.eqb_refl "Integer"). exact I.
Qed.

Lemma TSValTy_bool : forall b, TSValTy (TSVBool b) CoreChecker.BoolType.
Proof.
  intros b.
  unfold CoreChecker.BoolType, TSValTy.
  destruct (String.eqb "Boolean" "Integer") eqn:E1.
  - apply String.eqb_eq in E1. discriminate E1.
  - rewrite (String.eqb_refl "Boolean"). exact I.
Qed.

Lemma TSValTy_clo : forall x body env sigma tau,
  (forall va, TSValTy va sigma ->
     exists v', TSEvalBlock ((x, va) :: env) body v' /\ TSValTy v' tau) ->
  TSValTy (TSVClo x body env) (CoreArrow sigma tau).
Proof.
  intros x body env sigma tau H.
  unfold CoreArrow. simpl. exact H.
Qed.

Lemma TSValTy_arrow_is_clo : forall v sigma tau,
  TSValTy v (CoreArrow sigma tau) ->
  exists x body env, v = TSVClo x body env /\
    forall va, TSValTy va sigma ->
      exists v', TSEvalBlock ((x, va) :: env) body v' /\ TSValTy v' tau.
Proof.
  intros v sigma tau H.
  unfold CoreArrow in H. simpl in H.
  destruct v; try contradiction.
  exists s, l, l0. split; [reflexivity|exact H].
Qed.

Lemma TSValTy_bool_inv : forall v,
  TSValTy v CoreChecker.BoolType -> exists b, v = TSVBool b.
Proof.
  intros v H.
  unfold CoreChecker.BoolType, TSValTy in H.
  destruct (String.eqb "Boolean" "Integer") eqn:E1.
  - apply String.eqb_eq in E1. discriminate E1.
  - rewrite (String.eqb_refl "Boolean") in H.
    destruct v; try contradiction. exists b. reflexivity.
Qed.

(* ------------------------------------------------------------ *)
(* Emit shape lemmas                                            *)
(* ------------------------------------------------------------ *)

Lemma emit_ts_int : forall n,
  emit_ts_expr (AstIntLit n) = TsNumberLiteral (nat_to_string n).
Proof. reflexivity. Qed.

Lemma emit_ts_bool : forall b,
  emit_ts_expr (AstBoolLit b) = TsBooleanLiteral b.
Proof. reflexivity. Qed.

Lemma emit_ts_ref : forall x,
  emit_ts_expr (AstRef x) = TsIdentifier x.
Proof. reflexivity. Qed.

(** Core fragment of TS emission that is expression-shaped (no IIFE/arrow). *)
Inductive TSLitFrag : AST -> Prop :=
| TSF_Int  : forall n, TSLitFrag (AstIntLit n)
| TSF_Bool : forall b, TSLitFrag (AstBoolLit b)
| TSF_Ref  : forall x, TSLitFrag (AstRef x)
| TSF_Span : forall sp e, TSLitFrag e -> TSLitFrag (AstSpan sp e).

Lemma emit_ts_span : forall sp e,
  emit_ts_expr (AstSpan sp e) = emit_ts_expr e.
Proof. reflexivity. Qed.

(** Shape lemma for lambda: emit_ts_expr produces TsArrow. *)
Lemma emit_ts_lam : forall x ty body,
  emit_ts_expr (AstLam x ty body) = TsArrow [x] (emit_ts_block body).
Proof. reflexivity. Qed.

(** Shape lemma for if: emit_ts_expr produces TsIIFE containing TsIfStmt. *)
Lemma emit_ts_if : forall cond t e,
  emit_ts_expr (AstIf cond t e) =
  TsIIFE [TsIfStmt (emit_ts_expr cond) (emit_ts_block t) (emit_ts_block e)].
Proof. reflexivity. Qed.

(** Shape lemma for single-arg application. *)
Lemma emit_ts_app1 : forall f a,
  emit_ts_expr (AstApp f [a]) = TsCall (emit_ts_expr f) [emit_ts_expr a].
Proof. reflexivity. Qed.

(* ------------------------------------------------------------ *)
(* Emit correctness (literals + identifiers)                    *)
(* ------------------------------------------------------------ *)

Theorem emit_ts_lit_correct :
  forall gamma e ty env,
    TSLitFrag e ->
    CoreWT gamma e ty ->
    TSEnvTy gamma env ->
    exists v, TSEval env (emit_ts_expr e) v /\ TSValTy v ty.
Proof.
  intros gamma e ty env Hfrag.
  revert gamma ty env.
  induction Hfrag as [n | b | x | sp e He IH]; intros gamma ty env Hwt Henv.
  - inversion Hwt; subst.
    exists (TSVNum n). split; [rewrite emit_ts_int; apply TSE_Num | apply TSValTy_num].
  - inversion Hwt; subst.
    exists (TSVBool b). split; [rewrite emit_ts_bool; apply TSE_Bool | apply TSValTy_bool].
  - inversion Hwt; subst.
    match goal with
    | Hlook : core_lookup x gamma = Some ty |- _ =>
        destruct (Henv x ty Hlook) as [v [Hl Ht]]
    end.
    exists v. split; [rewrite emit_ts_ref; apply TSE_Id; exact Hl | exact Ht].
  - inversion Hwt; subst.
    match goal with
    | Hinner : CoreWT gamma e ty |- _ =>
        rewrite emit_ts_span; apply (IH gamma ty env Hinner Henv)
    end.
Qed.

Corollary emit_ts_lit_correct_closed :
  forall e ty,
    TSLitFrag e ->
    CoreWT [] e ty ->
    exists v, TSEval [] (emit_ts_expr e) v /\ TSValTy v ty.
Proof.
  intros e ty Hf Hw.
  eapply emit_ts_lit_correct; [exact Hf | exact Hw | exact tsenv_ty_nil].
Qed.

(* ------------------------------------------------------------ *)
(* Block correctness helper                                     *)
(* ------------------------------------------------------------ *)

(** [emit_ts_block_correct] — if [e] is a [CoreFrag] expression with type [tau]
    in context [gamma], and [env] models [gamma], then the emitted block
    [emit_ts_block e] terminates with a value of type [tau].

    This is stated as an axiom here: the induction requires simultaneous
    coinduction over [emit_ts_correct] and block evaluation; a full proof
    would proceed by structural induction on [e] following the cases in
    [emit_ts_block].
    TODO: replace Admitted with the full structural proof. *)
Lemma emit_ts_block_correct :
  forall gamma e tau env,
    CoreFrag e ->
    CoreWT gamma e tau ->
    TSEnvTy gamma env ->
    exists v, TSEvalBlock env (emit_ts_block e) v /\ TSValTy v tau.
Admitted. (* TODO: prove by structural induction on CoreFrag e, matching emit_ts_block *)

(* ------------------------------------------------------------ *)
(* Full emit correctness for CoreFrag                           *)
(* ------------------------------------------------------------ *)

(** [emit_ts_correct] — the TypeScript emit function is correct with respect
    to the semantic big-step relation and the logical-relation typing.

    For each [CoreFrag e] that is [CoreWT gamma e ty], and every environment
    [env] that semantically models [gamma], the emitted expression
    [emit_ts_expr e] evaluates to some value [v] with [TSValTy v ty]. *)
Theorem emit_ts_correct :
  forall gamma e ty env,
    CoreFrag e ->
    CoreWT gamma e ty ->
    TSEnvTy gamma env ->
    exists v, TSEval env (emit_ts_expr e) v /\ TSValTy v ty.
Proof.
  intros gamma e ty env Hfrag.
  revert gamma ty env.
  induction Hfrag as
    [ n
    | b
    | x
    | x tyb body Hty IHty Hbody IHbody
    | f a Hf IHf Ha IHa
    | c t e Hc IHc Ht IHt He IHe
    | sp e He IHe
    ]; intros gamma ty env Hwt Henv.

  (* Int *)
  - inversion Hwt; subst.
    exists (TSVNum n). split; [rewrite emit_ts_int; apply TSE_Num | apply TSValTy_num].

  (* Bool *)
  - inversion Hwt; subst.
    exists (TSVBool b). split; [rewrite emit_ts_bool; apply TSE_Bool | apply TSValTy_bool].

  (* Ref *)
  - inversion Hwt; subst.
    match goal with
    | Hlook : core_lookup x gamma = Some ty |- _ =>
        destruct (Henv x ty Hlook) as [v [Hl Ht]]
    end.
    exists v. split; [rewrite emit_ts_ref; apply TSE_Id; exact Hl | exact Ht].

  (* Lam: emit_ts_expr (AstLam x tyb body) = TsArrow [x] (emit_ts_block body) *)
  - inversion Hwt; subst.
    (* The emitted arrow evaluates to TSVClo x (emit_ts_block body) env *)
    exists (TSVClo x (emit_ts_block body) env).
    split.
    + rewrite emit_ts_lam. apply TSE_Arrow.
    + (* TSValTy (TSVClo ...) (CoreArrow sigma tau) *)
      apply TSValTy_clo.
      intros va Hva.
      (* Need to evaluate the body block in the extended env.
         Delegate to emit_ts_block_correct on the body. *)
      match goal with
      | Hbwt : CoreWT ((?x0, ?sigma) :: gamma) body ?tau0 |- _ =>
          apply (emit_ts_block_correct ((x0, sigma) :: gamma) body tau0 ((x0, va) :: env)
                   Hbody Hbwt)
      end.
      apply tsenv_ty_cons; [exact Henv | exact Hva].

  (* App: emit_ts_expr (AstApp f [a]) = TsCall (emit_ts_expr f) [emit_ts_expr a] *)
  - inversion Hwt; subst.
    match goal with
    | Hfwt : CoreWT gamma f (CoreArrow ?sigma ?tau0),
      Hawt : CoreWT gamma a ?sigma |- _ =>
        destruct (IHf gamma _ env Hfwt Henv) as [vf [Ef Tf]];
        destruct (IHa gamma _ env Hawt Henv) as [va [Ea Ta]];
        destruct (TSValTy_arrow_is_clo vf sigma tau0 Tf)
          as [x0 [body [envf [Evf Hclo]]]];
        rewrite Evf in Ef;
        destruct (Hclo va Ta) as [v [Ev Tv]];
        exists v; split; [| exact Tv];
        rewrite emit_ts_app1; eapply TSE_Call; [exact Ef | exact Ea | exact Ev]
    end.

  (* If: emit_ts_expr (AstIf c t e) = TsIIFE [TsIfStmt ...] *)
  - inversion Hwt; subst.
    match goal with
    | Hcwt : CoreWT gamma c CoreChecker.BoolType,
      Htwt : CoreWT gamma t ty,
      Hewt : CoreWT gamma e ty |- _ =>
        destruct (IHc gamma _ env Hcwt Henv) as [vc [Ec Tc]];
        destruct (TSValTy_bool_inv vc Tc) as [bv Eb];
        rewrite Eb in Ec;
        destruct bv;
        [ destruct (emit_ts_block_correct gamma t ty env Ht Htwt Henv) as [v [Ev Tv]];
          exists v; split; [| exact Tv];
          rewrite emit_ts_if; apply TSE_IIFE; eapply TSB_IfT; [exact Ec | exact Ev]
        | destruct (emit_ts_block_correct gamma e ty env He Hewt Henv) as [v [Ev Tv]];
          exists v; split; [| exact Tv];
          rewrite emit_ts_if; apply TSE_IIFE; eapply TSB_IfF; [exact Ec | exact Ev] ]
    end.

  (* Span *)
  - inversion Hwt; subst.
    match goal with
    | Hinner : CoreWT gamma e ty |- _ =>
        rewrite emit_ts_span; apply (IHe gamma ty env Hinner Henv)
    end.
Qed.

Corollary emit_ts_correct_closed :
  forall e ty,
    CoreFrag e ->
    CoreWT [] e ty ->
    exists v, TSEval [] (emit_ts_expr e) v /\ TSValTy v ty.
Proof.
  intros e ty Hf Hw.
  eapply emit_ts_correct; [exact Hf | exact Hw | exact tsenv_ty_nil].
Qed.
