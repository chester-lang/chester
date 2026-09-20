(** Declarative core typing and seal soundness properties that matter.

    [infer_check] is intentionally soft ([meet_expected] never hard-fails).
    Soundness work therefore uses a small declarative judgment [CoreWT] for a
    pure core fragment, plus theorems about [seal_exports_check] which gate
    module export membership and effect-row subsumption. *)

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
(* find helpers                                                 *)
(* ------------------------------------------------------------ *)

Lemma find_String_eqb_In_eq :
  forall (A : Type) (l : list (string * A)) n n' ty,
    find (fun p => String.eqb (fst p) n) l = Some (n', ty) ->
    n' = n /\ In (n, ty) l.
Proof.
  intros A l n n' ty H.
  induction l as [|p l IH]; simpl in *; [discriminate|].
  destruct p as [k v]; simpl in *.
  destruct (String.eqb k n) eqn:Ek.
  - inversion H; subst. apply String.eqb_eq in Ek. subst.
    split; [reflexivity|left; reflexivity].
  - destruct (IH H) as [-> Hin]. split; [reflexivity|right; exact Hin].
Qed.

Lemma find_Some_In :
  forall (A : Type) (l : list (string * A)) n p,
    find (fun q => String.eqb (fst q) n) l = Some p ->
    In p l /\ fst p = n.
Proof.
  intros A l n p H.
  destruct p as [n' ty].
  destruct (find_String_eqb_In_eq A l n n' ty H) as [-> Hin].
  split; [exact Hin|reflexivity].
Qed.

(* ------------------------------------------------------------ *)
(* Seal soundness                                               *)
(* ------------------------------------------------------------ *)

Theorem seal_exports_check_names_from_full :
  forall (opaque : bool) (full sealed : list (string * AST)) (decls : list AST),
    seal_exports_check opaque full decls = inr sealed ->
    forall n ty, In (n, ty) sealed -> exists ty0, In (n, ty0) full.
Proof.
  intros opaque full sealed decls.
  revert sealed.
  induction decls as [|d decls IH]; intros sealed H n ty Hin.
  - simpl in H. inversion H; subst. contradiction.
  - destruct d; simpl in H; try (exact (IH sealed H n ty Hin)).
    + (* AstDef *)
      destruct (find (fun p => String.eqb (fst p) s) full) as [p|] eqn:F;
        [|discriminate H].
      destruct (seal_exports_check opaque full decls) as [|rest] eqn:Rest;
        [discriminate H|].
      inversion H; subst; clear H.
      destruct Hin as [Heq|Hin'].
      * inversion Heq; subst.
        apply find_Some_In in F. destruct F as [HinP Es].
        simpl in Es. subst.
        exists ty. exact HinP.
      * exact (IH rest eq_refl n ty Hin').
    + (* AstSigVal *)
      destruct (find (fun p => String.eqb (fst p) s) full) as [p|] eqn:F;
        [|discriminate H].
      destruct p as [pn fty].
      destruct (find_String_eqb_In_eq _ full s pn fty F) as [-> HinF].
      destruct fty;
        try (destruct (seal_exports_check opaque full decls) as [|rest] eqn:Rest;
             [discriminate H|];
             inversion H; subst; clear H;
             destruct Hin as [Heq|Hin'];
             [inversion Heq; subst; eexists; exact HinF
             | exact (IH rest eq_refl n ty Hin')]).
      (* AstFunTy: e0 = impl_effs, e = sig_effs *)
      match goal with
      | H : context [effect_row_subsumes ?impl ?sig] |- _ =>
          destruct (effect_row_subsumes impl sig) eqn:Sub; [|discriminate H]
      end.
      destruct (seal_exports_check opaque full decls) as [|rest] eqn:Rest;
        [discriminate H|].
      inversion H; subst; clear H.
      destruct Hin as [Heq|Hin'].
      * inversion Heq; subst. eexists. exact HinF.
      * exact (IH rest eq_refl n ty Hin').
    + (* AstTypeDecl *)
      destruct (find (fun p => String.eqb (fst p) s) full) as [p|] eqn:F;
        [|discriminate H].
      destruct p as [pn fty].
      destruct (find_String_eqb_In_eq _ full s pn fty F) as [-> HinF].
      destruct fty; try discriminate H.
      destruct (seal_exports_check opaque full decls) as [|rest] eqn:Rest;
        [discriminate H|].
      inversion H; subst; clear H.
      destruct Hin as [Heq|Hin'].
      * inversion Heq; subst. eexists. exact HinF.
      * exact (IH rest eq_refl n ty Hin').
Qed.

Theorem seal_sigval_requires_effect_subsumption :
  forall opaque full n tps params ret effs rest sealed,
    seal_exports_check opaque full
      (AstSigVal n tps params ret effs :: rest) = inr sealed ->
    forall impl_tps impl_params impl_ret impl_effs,
      find (fun p => String.eqb (fst p) n) full
        = Some (n, AstFunTy impl_tps impl_params impl_ret impl_effs) ->
      effect_row_subsumes impl_effs effs = true.
Proof.
  intros opaque full n tps params ret effs rest sealed H
         impl_tps impl_params impl_ret impl_effs F.
  simpl in H. rewrite F in H.
  destruct (effect_row_subsumes impl_effs effs) eqn:Sub; [reflexivity|discriminate H].
Qed.

(* ------------------------------------------------------------ *)
(* Core fragment + declarative typing                           *)
(* ------------------------------------------------------------ *)

Inductive CoreFrag : AST -> Prop :=
| CF_Int : forall n, CoreFrag (AstIntLit n)
| CF_Bool : forall b, CoreFrag (AstBoolLit b)
| CF_Ref : forall x, CoreFrag (AstRef x)
| CF_Lam : forall x ty b,
    CoreFrag ty -> CoreFrag b -> CoreFrag (AstLam x ty b)
| CF_App1 : forall f a,
    CoreFrag f -> CoreFrag a -> CoreFrag (AstApp f [a])
| CF_If : forall c t e,
    CoreFrag c -> CoreFrag t -> CoreFrag e -> CoreFrag (AstIf c t e)
| CF_Span : forall sp e, CoreFrag e -> CoreFrag (AstSpan sp e).

Definition CoreArrow (σ τ : AST) : AST :=
  AstFunTy [] [("_"%string, σ)] τ [].

Fixpoint core_lookup (name : string) (env : list (string * AST)) : option AST :=
  match env with
  | [] => None
  | (k, v) :: rest =>
      if String.eqb name k then Some v else core_lookup name rest
  end.

Inductive CoreWT (Γ : list (string * AST)) : AST -> AST -> Prop :=
| CWT_Int : forall n, CoreWT Γ (AstIntLit n) CoreChecker.IntType
| CWT_Bool : forall b, CoreWT Γ (AstBoolLit b) CoreChecker.BoolType
| CWT_Ref : forall x τ,
    core_lookup x Γ = Some τ -> CoreWT Γ (AstRef x) τ
| CWT_Lam : forall x σ e τ,
    CoreWT ((x, σ) :: Γ) e τ ->
    CoreWT Γ (AstLam x σ e) (CoreArrow σ τ)
| CWT_App : forall f σ τ a,
    CoreWT Γ f (CoreArrow σ τ) ->
    CoreWT Γ a σ ->
    CoreWT Γ (AstApp f [a]) τ
| CWT_If : forall c t e τ,
    CoreWT Γ c CoreChecker.BoolType ->
    CoreWT Γ t τ ->
    CoreWT Γ e τ ->
    CoreWT Γ (AstIf c t e) τ
| CWT_Span : forall sp e τ,
    CoreWT Γ e τ -> CoreWT Γ (AstSpan sp e) τ.

Example core_wt_id :
  CoreWT [] (AstLam "x" CoreChecker.IntType (AstRef "x"))
    (CoreArrow CoreChecker.IntType CoreChecker.IntType).
Proof.
  apply CWT_Lam. apply CWT_Ref.
  unfold core_lookup.
  rewrite (String.eqb_refl "x").
  reflexivity.
Qed.

Example core_wt_id_app :
  forall n,
    CoreWT []
      (AstApp (AstLam "x" CoreChecker.IntType (AstRef "x")) [AstIntLit n])
      CoreChecker.IntType.
Proof.
  intros n. eapply CWT_App.
  - exact core_wt_id.
  - apply CWT_Int.
Qed.

Lemma core_wt_span_inv : forall Γ sp e τ,
  CoreWT Γ (AstSpan sp e) τ -> CoreWT Γ e τ.
Proof. intros Γ sp e τ H; inversion H; subst; assumption. Qed.

Lemma core_frag_span_inv : forall sp e,
  CoreFrag (AstSpan sp e) -> CoreFrag e.
Proof. intros sp e H; inversion H; subst; assumption. Qed.
