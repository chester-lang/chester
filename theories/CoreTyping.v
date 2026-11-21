(** Declarative core typing and seal soundness properties that matter.

    [meet_expected] rejects clear type mismatches (soft aliases / structural
    FunTy·App·Pi still accepted). Soundness work uses a small declarative
    judgment [CoreWT] for a pure core fragment (no effects/modules), plus
    theorems about [seal_exports_check] which gate module export membership
    and effect-row subsumption. *)

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
(* Seal soundness (on the flattened checker)                    *)
(* ------------------------------------------------------------ *)

Lemma seal_cons_export_names :
  forall n ty rest_res sealed n' ty',
    seal_cons_export n ty rest_res = inr sealed ->
    In (n', ty') sealed ->
    (n', ty') = (n, ty) \/
    (exists rest, rest_res = inr rest /\ In (n', ty') rest).
Proof.
  intros n ty rest_res sealed n' ty' H Hin.
  unfold seal_cons_export in H.
  destruct rest_res as [|rest]; [discriminate H|].
  inversion H; subst; clear H.
  destruct Hin as [Heq|Hin']; [left; symmetry; exact Heq|].
  right. exists rest. split; [reflexivity|exact Hin'].
Qed.

(* [seal_cons_export] membership is proved above. The full [seal_sigval_member]
   / [seal_exports_check_flat] name theorem is exercised computationally by the
   reflexivity examples in [TypeSystemProps]; a general inductive proof is left
   as follow-up (FunTy/Pi guard peeling vs. opaque string [sum] discrimination). *)

Theorem seal_sigval_requires_effect_subsumption :
  forall opaque full subs n tps params ret effs rest sealed,
    seal_exports_check_flat opaque full subs
      (AstSigVal n tps params ret effs :: rest) = inr sealed ->
    forall impl_tps impl_params impl_ret impl_effs,
      find (fun p => String.eqb (fst p) n) full
        = Some (n, AstFunTy impl_tps impl_params impl_ret impl_effs) ->
      effect_row_subsumes impl_effs effs = true.
Proof.
  intros opaque full subs n tps params ret effs rest sealed H
         impl_tps impl_params impl_ret impl_effs F.
  simpl in H.
  unfold seal_sigval_member in H.
  rewrite F in H.
  destruct (effect_row_subsumes impl_effs effs) eqn:Sub; [reflexivity|].
  (* Effects fail ⇒ [seal_sigval_member] returns [inl] before type matching. *)
  discriminate H.
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
