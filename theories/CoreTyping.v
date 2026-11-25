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

(** A successful flat seal check preserves the checked value member's name at
    the head of the resulting export list.  This is the general membership
    counterpart to [seal_cons_export_names] below; it peels the value-shape
    guards in [seal_sigval_member] without relying on a concrete signature. *)
Lemma inl_inr_neq : forall (A B : Type) (a : A) (b : B),
  (inl a : sum A B) = inr b -> False.
Proof. intros A B a b H. discriminate H. Qed.
Arguments inl_inr_neq {A B a b} _.

Theorem seal_sigval_member_name :
  forall fuel opaque full subs n tps params ret effs rest sealed,
    seal_exports_check_flat_fuel (S fuel) opaque full subs
      (AstSigVal n tps params ret effs :: rest) = inr sealed ->
    exists exported tail,
      sealed = (n, exported) :: tail.
Proof.
  intros fuel opaque full subs n tps params ret effs rest sealed H.
  simpl in H.
  unfold seal_sigval_member in H.
  destruct (find (fun p => String.eqb (fst p) n) full)
    as [[impl_n impl_ty]|] eqn:F.
  - destruct impl_ty as
      [impl_ref
      | impl_level
      | impl_elems
      | impl_str
      | impl_nat
      | impl_bool_lit
      | impl_stmts impl_tail
      | impl_fn impl_args
      | impl_implicit_fn impl_targs
      | impl_lam_arg impl_lam_arg_ty impl_lam_body
      | impl_pi_arg impl_pi_arg_ty impl_pi_ret impl_pi_effs
      | impl_op impl_args
      | impl_handle_body impl_handle_eff impl_handle_handlers
      | impl_let_name impl_let_value
      | impl_var_name impl_var_value
      | impl_assign_name impl_assign_value
      | impl_cond impl_then impl_else
      | impl_def_name impl_def_tps impl_def_params impl_def_ret impl_def_body impl_def_effs
      | impl_fun_tps impl_fun_params impl_fun_ret impl_fun_effs
      | impl_enum_name impl_enum_tps impl_variants
      | impl_match_expr impl_cases
      | impl_record_name impl_record_tps impl_record_fields
      | impl_field_expr impl_field_name
      | impl_ext_name impl_ext_tps impl_ext_target impl_ext_methods
      | impl_box_inner impl_box_caps
      | impl_unbox_inner
      | impl_import_lang impl_import_alias impl_import_path impl_import_syms
      | impl_module_name impl_module_params impl_module_seal impl_module_body
      | impl_signature_name impl_signature_decls
      | impl_functor impl_functor_args
      | impl_modty_exports
      | impl_sigval_name impl_sigval_tps impl_sigval_params impl_sigval_ret impl_sigval_effs
      | impl_type_name impl_type_def
      | impl_with_base impl_with_eqs
      | impl_pack_structure impl_pack_signature
      | impl_unpack_name impl_unpack_signature impl_unpack_packed impl_unpack_body
      | impl_file_name impl_file_path
      | impl_meta
      | impl_span impl_inner
      | impl_error].
    all: try discriminate H.
    + (* AstPi is the first surviving constructor in declaration order. *)
      cbn [seal_sigval_member] in H.
      destruct (effect_row_subsumes impl_pi_effs effs) eqn:Eff; [|discriminate H].
      destruct (apply_seal_subst_params subs params) as [|sig_param sig_params];
        [discriminate H|].
      destruct sig_param as [sig_name sig_arg].
      destruct sig_params as [|sig_param2 sig_rest]; [|discriminate H].
      generalize dependent H.
      destruct (types_match_seal sig_arg
        (apply_seal_subst subs impl_pi_arg_ty)) eqn:ArgMatch.
      * destruct (types_match_seal (apply_seal_subst subs ret)
          (apply_seal_subst subs impl_pi_ret))
          eqn:RetMatch.
        -- destruct (seal_exports_check_flat_fuel fuel opaque full subs rest)
             as [err|tail] eqn:Rest.
           ++ intro H. cbn in H.
              exfalso.
              exact (inl_inr_neq H).
           ++ intro H. cbn in H. inversion H; subst.
              exists (if opaque
                then AstFunTy tps params ret effs
                else AstFunTy tps [(sig_name, sig_arg)]
                  (apply_seal_subst subs ret) effs).
              exists tail. reflexivity.
        -- intro H. cbn in H. exfalso. exact (inl_inr_neq H).
      * destruct (types_match_seal (apply_seal_subst subs ret)
          (apply_seal_subst subs impl_pi_ret))
          eqn:RetMatch.
        -- intro H. cbn in H. exfalso. exact (inl_inr_neq H).
        -- intro H. cbn in H. exfalso. exact (inl_inr_neq H).
    + (* AstFunTy implementation. *)
      cbn [seal_sigval_member] in H.
      destruct (effect_row_subsumes impl_fun_effs effs) eqn:Eff; [|discriminate H].
      generalize dependent H.
      destruct (seal_params_ok (apply_seal_subst_params subs params)
        (apply_seal_subst_params subs impl_fun_params)) eqn:ParamsMatch.
      * destruct (types_match_seal (apply_seal_subst subs ret)
          (apply_seal_subst subs impl_fun_ret)) eqn:RetMatch.
        -- destruct (seal_exports_check_flat_fuel fuel opaque full subs rest)
             as [err|tail] eqn:Rest.
           ++ intro H. cbn in H. exfalso. exact (inl_inr_neq H).
           ++ intro H. cbn in H. inversion H; subst.
              exists (if opaque
                then AstFunTy tps params ret effs
                else AstFunTy tps (apply_seal_subst_params subs params)
                  (apply_seal_subst subs ret) effs).
              exists tail. reflexivity.
        -- intro H. cbn in H. exfalso. exact (inl_inr_neq H).
      * destruct (types_match_seal (apply_seal_subst subs ret)
          (apply_seal_subst subs impl_fun_ret))
          eqn:RetMatch.
        -- intro H. cbn in H. exfalso. exact (inl_inr_neq H).
        -- intro H. cbn in H. exfalso. exact (inl_inr_neq H).
  - discriminate H.
Qed.

Theorem seal_sigval_requires_effect_subsumption :
  forall fuel opaque full subs n tps params ret effs rest sealed,
    seal_exports_check_flat_fuel (S fuel) opaque full subs
      (AstSigVal n tps params ret effs :: rest) = inr sealed ->
    forall impl_tps impl_params impl_ret impl_effs,
      find (fun p => String.eqb (fst p) n) full
        = Some (n, AstFunTy impl_tps impl_params impl_ret impl_effs) ->
      effect_row_subsumes impl_effs effs = true.
Proof.
  intros fuel opaque full subs n tps params ret effs rest sealed H
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
