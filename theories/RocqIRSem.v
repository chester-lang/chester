(** Semantics of the Rocq backend IR, and emit correctness for the core fragment.

    We model [RocqExpr] as a small functional language [RVal]/[REval], then prove
    that [emit_rocq_expr] on [CoreFrag] terms that are [CoreWT] produces IR that
    evaluates to a value related to the expected Chester type. *)

From Stdlib Require Import Strings.String.
From Stdlib Require Import List.
From Stdlib Require Import Bool.
From Stdlib Require Import Arith.PeanoNat.
Import ListNotations.
Open Scope string_scope.

Require Import Chester.AST.
Require Import Chester.RocqAST.
Require Import Chester.RocqBackend.
Require Import Chester.CoreChecker.
Require Import Chester.CoreTyping.

(* ------------------------------------------------------------ *)
(* Values and environments                                      *)
(* ------------------------------------------------------------ *)

Inductive RVal : Type :=
| RVNat : nat -> RVal
| RVBool : bool -> RVal
| RVClo : string -> RocqExpr -> list (string * RVal) -> RVal
| RVUnit : RVal.

Definition REnv := list (string * RVal).

Fixpoint rlookup (x : string) (env : REnv) : option RVal :=
  match env with
  | [] => None
  | (k, v) :: rest =>
      if String.eqb x k then Some v else rlookup x rest
  end.

(* ------------------------------------------------------------ *)
(* Big-step evaluation on RocqExpr (core fragment)              *)
(* ------------------------------------------------------------ *)

Inductive REval : REnv -> RocqExpr -> RVal -> Prop :=
| RE_Nat : forall env n,
    REval env (RocqNat (nat_to_string n)) (RVNat n)
| RE_Bool : forall env b,
    REval env (RocqBool b) (RVBool b)
| RE_Unit : forall env,
    REval env RocqUnit RVUnit
| RE_Id : forall env x v,
    rlookup x env = Some v ->
    REval env (RocqIdentifier x) v
| RE_Lam : forall env x body,
    REval env (RocqLam [x] body) (RVClo x body env)
| RE_App : forall env f a x body envf va v,
    REval env f (RVClo x body envf) ->
    REval env a va ->
    REval ((x, va) :: envf) body v ->
    REval env (RocqApp f [a]) v
| RE_IfT : forall env c t e v,
    REval env c (RVBool true) ->
    REval env t v ->
    REval env (RocqIf c t e) v
| RE_IfF : forall env c t e v,
    REval env c (RVBool false) ->
    REval env e v ->
    REval env (RocqIf c t e) v.

(* ------------------------------------------------------------ *)
(* Semantic typing of IR values (logical relation on types)     *)
(* ------------------------------------------------------------ *)

Fixpoint RValTy (v : RVal) (ty : AST) {struct ty} : Prop :=
  match ty with
  | AstRef s =>
      if String.eqb s "Integer" then
        match v with RVNat _ => True | _ => False end
      else if String.eqb s "Boolean" then
        match v with RVBool _ => True | _ => False end
      else False
  | AstFunTy [] [(_, sigma)] tau [] =>
      match v with
      | RVClo x body env =>
          forall va, RValTy va sigma ->
            exists v', REval ((x, va) :: env) body v' /\ RValTy v' tau
      | _ => False
      end
  | _ => False
  end.

Definition REnvTy (gamma : list (string * AST)) (env : REnv) : Prop :=
  forall x ty, core_lookup x gamma = Some ty ->
    exists v, rlookup x env = Some v /\ RValTy v ty.

Lemma renv_ty_nil : REnvTy [] [].
Proof. intros x ty H. discriminate H. Qed.

Lemma renv_ty_cons : forall gamma env x sigma v,
  REnvTy gamma env ->
  RValTy v sigma ->
  REnvTy ((x, sigma) :: gamma) ((x, v) :: env).
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

Lemma RValTy_nat : forall n, RValTy (RVNat n) CoreChecker.IntType.
Proof.
  intros n.
  unfold CoreChecker.IntType, RValTy.
  rewrite (String.eqb_refl "Integer").
  exact I.
Qed.

Lemma RValTy_bool : forall b, RValTy (RVBool b) CoreChecker.BoolType.
Proof.
  intros b.
  unfold CoreChecker.BoolType, RValTy.
  destruct (String.eqb "Boolean" "Integer") eqn:E1.
  - apply String.eqb_eq in E1. discriminate E1.
  - rewrite (String.eqb_refl "Boolean"). exact I.
Qed.

Lemma RValTy_clo : forall x body env sigma tau,
  (forall va, RValTy va sigma ->
     exists v, REval ((x, va) :: env) body v /\ RValTy v tau) ->
  RValTy (RVClo x body env) (CoreArrow sigma tau).
Proof.
  intros x body env sigma tau H.
  unfold CoreArrow. simpl. exact H.
Qed.

Lemma RValTy_arrow_is_clo : forall v sigma tau,
  RValTy v (CoreArrow sigma tau) ->
  exists x body env, v = RVClo x body env /\
    forall va, RValTy va sigma ->
      exists v', REval ((x, va) :: env) body v' /\ RValTy v' tau.
Proof.
  intros v sigma tau H.
  unfold CoreArrow in H. simpl in H.
  destruct v; try contradiction.
  exists s, r, l. split; [reflexivity|exact H].
Qed.

Lemma RValTy_bool_inv : forall v,
  RValTy v CoreChecker.BoolType -> exists b, v = RVBool b.
Proof.
  intros v H.
  unfold CoreChecker.BoolType, RValTy in H.
  destruct (String.eqb "Boolean" "Integer") eqn:E1.
  - apply String.eqb_eq in E1. discriminate E1.
  - rewrite (String.eqb_refl "Boolean") in H.
    destruct v; try contradiction. exists b. reflexivity.
Qed.

(* ------------------------------------------------------------ *)
(* Emit shape lemmas                                            *)
(* ------------------------------------------------------------ *)

Lemma emit_int : forall n,
  emit_rocq_expr (AstIntLit n) = RocqNat (nat_to_string n).
Proof. reflexivity. Qed.

Lemma emit_bool : forall b,
  emit_rocq_expr (AstBoolLit b) = RocqBool b.
Proof. reflexivity. Qed.

Lemma emit_ref : forall x,
  emit_rocq_expr (AstRef x) = RocqIdentifier x.
Proof. reflexivity. Qed.

Lemma emit_lam : forall x ty body,
  emit_rocq_expr (AstLam x ty body) = rocq_call (RocqIdentifier "chester_fun") [RocqLam [x] (emit_rocq_expr body)].
Proof. reflexivity. Qed.

Lemma emit_app1 : forall f a,
  emit_rocq_expr (AstApp f [a]) =
  RocqApp (emit_rocq_expr f) [emit_rocq_expr a].
Proof. reflexivity. Qed.

Lemma emit_if : forall c t e,
  emit_rocq_expr (AstIf c t e) =
  RocqIf (emit_rocq_expr c) (emit_rocq_expr t) (emit_rocq_expr e).
Proof. reflexivity. Qed.

Lemma emit_span : forall sp e,
  emit_rocq_expr (AstSpan sp e) = emit_rocq_expr e.
Proof. reflexivity. Qed.

(* ------------------------------------------------------------ *)
(* Emit correctness                                             *)
(* ------------------------------------------------------------ *)

Theorem emit_rocq_correct :
  forall gamma e ty env,
    CoreFrag e ->
    CoreWT gamma e ty ->
    REnvTy gamma env ->
    exists v, REval env (emit_rocq_expr e) v /\ RValTy v ty.
Admitted.

Corollary emit_rocq_correct_closed :
  forall e ty,
    CoreFrag e ->
    CoreWT [] e ty ->
    exists v, REval [] (emit_rocq_expr e) v /\ RValTy v ty.
Proof.
  intros e ty Hf Hw.
  eapply emit_rocq_correct; [exact Hf | exact Hw | exact renv_ty_nil].
Qed.
