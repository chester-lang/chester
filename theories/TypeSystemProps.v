(** Properties of Chester's executable type-system helpers.

    The elaborator and CoreChecker are algorithmic (boolean / [TyResult]), not a
    declarative [HasType] judgment. This file proves structural properties of the
    predicates those algorithms rely on — a foothold toward larger soundness
    results without inventing a separate typing relation from scratch. *)

From Stdlib Require Import Strings.String.
From Stdlib Require Import List.
From Stdlib Require Import Bool.
From Stdlib Require Import Arith.PeanoNat.
Import ListNotations.
Open Scope string_scope.

Require Import Chester.AST.
Require Import Chester.CoreChecker.
Require Import Chester.Elaborator.

(* ------------------------------------------------------------ *)
(* Effect rows                                                  *)
(* ------------------------------------------------------------ *)

Lemma effect_row_subsumes_nil : forall provided,
  effect_row_subsumes [] provided = true.
Proof. reflexivity. Qed.

Lemma effect_row_has_open : forall n mu ps,
  effect_row_has n (EffectRowVar mu :: ps) = true.
Proof. intros; reflexivity. Qed.

Lemma effect_row_has_head_builtin : forall a ps,
  effect_row_has (BuiltinEffect a) (BuiltinEffect a :: ps) = true.
Proof.
  intros a ps. simpl.
  destruct (string_dec a a) as [|E]; [reflexivity|contradiction E; reflexivity].
Qed.

Lemma effect_row_has_head_user : forall a ps,
  effect_row_has (UserEffect a) (UserEffect a :: ps) = true.
Proof.
  intros a ps. simpl.
  destruct (string_dec a a) as [|E]; [reflexivity|contradiction E; reflexivity].
Qed.

(** Extending the provider row never removes coverage. *)
Lemma effect_row_has_cons_r : forall n p ps,
  effect_row_has n ps = true ->
  effect_row_has n (p :: ps) = true.
Proof.
  intros n p ps H.
  destruct p as [b|u|mu].
  - (* BuiltinEffect head — may or may not match n *)
    destruct n as [nb|nu|nmu]; simpl;
      try (destruct (string_dec nb b); [reflexivity|exact H]);
      try (destruct (string_dec nu b); [reflexivity|exact H]);
      exact H.
  - destruct n as [nb|nu|nmu]; simpl;
      try (destruct (string_dec nb u); [reflexivity|exact H]);
      try (destruct (string_dec nu u); [reflexivity|exact H]);
      exact H.
  - (* open var accepts everything *)
    reflexivity.
Qed.

Lemma effect_row_subsumes_cons_provided : forall needed p provided,
  effect_row_subsumes needed provided = true ->
  effect_row_subsumes needed (p :: provided) = true.
Proof.
  induction needed as [|n needed IH]; intros p provided H;
    [reflexivity|].
  destruct n as [a|a|mu]; simpl in H; simpl.
  - destruct (effect_row_has (BuiltinEffect a) provided) eqn:Has; [|discriminate H].
    destruct p as [b|u|m]; simpl;
      try (destruct (string_dec a b) as [|_]; [apply IH; exact H|]);
      try (destruct (string_dec a u) as [|_]; [apply IH; exact H|]);
      try (rewrite Has; apply IH; exact H);
      apply IH; exact H.
  - destruct (effect_row_has (UserEffect a) provided) eqn:Has; [|discriminate H].
    destruct p as [b|u|m]; simpl;
      try (destruct (string_dec a b) as [|_]; [apply IH; exact H|]);
      try (destruct (string_dec a u) as [|_]; [apply IH; exact H|]);
      try (rewrite Has; apply IH; exact H);
      apply IH; exact H.
  - apply IH. exact H.
Qed.

(** Reflexivity: a row always subsumes itself. *)
Lemma effect_row_subsumes_refl : forall es,
  effect_row_subsumes es es = true.
Proof.
  induction es as [|e es IH]; [reflexivity|].
  destruct e as [a|a|mu]; simpl.
  - destruct (string_dec a a) as [Heq|Hneq]; [|elim Hneq; reflexivity].
    clear Heq.
    change (effect_row_subsumes es (BuiltinEffect a :: es) = true).
    apply effect_row_subsumes_cons_provided. exact IH.
  - destruct (string_dec a a) as [Heq|Hneq]; [|elim Hneq; reflexivity].
    clear Heq.
    change (effect_row_subsumes es (UserEffect a :: es) = true).
    apply effect_row_subsumes_cons_provided. exact IH.
  - change (effect_row_subsumes es (EffectRowVar mu :: es) = true).
    apply effect_row_subsumes_cons_provided. exact IH.
Qed.

(** Empty provider only covers an empty (or open-only) needed row. *)
Lemma effect_row_has_nil : forall n,
  effect_row_has n [] = false.
Proof. intros n; destruct n; reflexivity. Qed.

(* ------------------------------------------------------------ *)
(* Free names / binders                                         *)
(* ------------------------------------------------------------ *)

Lemma free_in_lam_shadow : forall name ty body,
  free_in name (AstLam name ty body) = free_in name ty.
Proof.
  intros name ty body. simpl.
  destruct (String.eqb name name) eqn:Eq.
  - rewrite Bool.orb_false_r. reflexivity.
  - rewrite String.eqb_refl in Eq. discriminate.
Qed.

Lemma free_in_pi_shadow : forall name ty ret effs,
  free_in name (AstPi name ty ret effs) = free_in name ty.
Proof.
  intros name ty ret effs. simpl.
  destruct (String.eqb name name) eqn:Eq.
  - rewrite Bool.orb_false_r. reflexivity.
  - rewrite String.eqb_refl in Eq. discriminate.
Qed.

Lemma free_in_ref : forall name,
  free_in name (AstRef name) = true.
Proof. intros name. simpl. apply String.eqb_refl. Qed.

(* ------------------------------------------------------------ *)
(* Span erasure                                                 *)
(* ------------------------------------------------------------ *)

Lemma strip_span_span : forall sp e,
  strip_span (AstSpan sp e) = strip_span e.
Proof. intros; reflexivity. Qed.

Lemma strip_span_ref : forall n,
  strip_span (AstRef n) = AstRef n.
Proof. intros; reflexivity. Qed.

(** Nested spans collapse. *)
Lemma strip_span_span_span : forall sp1 sp2 e,
  strip_span (AstSpan sp1 (AstSpan sp2 e)) = strip_span e.
Proof. intros; reflexivity. Qed.

(* ------------------------------------------------------------ *)
(* Module sealing                                               *)
(* ------------------------------------------------------------ *)

Lemma seal_exports_check_nil : forall opaque full,
  seal_exports_check opaque full [] = inr [].
Proof. intros; reflexivity. Qed.

(** Successful seal never invents names absent from the full export list —
    every sealed pair is drawn from [full] or rebuilt from a matching type
    component. We record the empty-spec case and a concrete value-spec case. *)
Example seal_requires_member :
  seal_exports_check true [] [AstSigVal "show" [] [] (AstRef "String") []]
  = inl "signature requires missing value: show".
Proof. reflexivity. Qed.

Example seal_accepts_matching_fun :
  let full := [("show"%string,
                AstFunTy [] [] (AstRef "String") [])] in
  seal_exports_check true full
    [AstSigVal "show" [] [] (AstRef "String") []]
  = inr [("show"%string, AstFunTy [] [] (AstRef "String") [])].
Proof. reflexivity. Qed.

Example seal_rejects_effect_overflow :
  let full := [("go"%string,
                AstFunTy [] [] (AstRef "Unit") [UserEffect "IO"])] in
  seal_exports_check true full
    [AstSigVal "go" [] [] (AstRef "Unit") []]
  = inl "effect row too large for signature member: go".
Proof. reflexivity. Qed.

Example seal_accepts_effect_subsumption :
  let full := [("go"%string,
                AstFunTy [] [] (AstRef "Unit") [UserEffect "IO"])] in
  seal_exports_check true full
    [AstSigVal "go" [] [] (AstRef "Unit") [UserEffect "IO"]]
  = inr [("go"%string, AstFunTy [] [] (AstRef "Unit") [UserEffect "IO"])].
Proof. reflexivity. Qed.

(* ------------------------------------------------------------ *)
(* CoreChecker subst examples (re-exported as named theorems)   *)
(* ------------------------------------------------------------ *)

Theorem subst_stops_at_shadowing_thm :
  subst_ast "x" (AstIntLit 1)
    (AstLam "x" (AstRef "Int") (AstRef "x"))
  = AstLam "x" (AstRef "Int") (AstRef "x").
Proof. reflexivity. Qed.

Theorem subst_under_distinct_binder_thm :
  subst_ast "x" (AstIntLit 1)
    (AstLam "y" (AstRef "Int") (AstRef "x"))
  = AstLam "y" (AstRef "Int") (AstIntLit 1).
Proof. reflexivity. Qed.

Theorem subst_avoids_capture_thm :
  subst_ast "x" (AstRef "y")
    (AstLam "y" (AstRef "Int") (AstRef "x"))
  = AstLam "y'" (AstRef "Int") (AstRef "y").
Proof. reflexivity. Qed.
