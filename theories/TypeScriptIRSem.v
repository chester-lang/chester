(** Semantics of a TypeScript IR fragment, and emit correctness for literals
    and identifiers (the stable core of [emit_ts_expr]).

    Full arrow/if correctness is deferred: TS emission wraps lambdas and
    conditionals in statement blocks / IIFEs, which need a heavier statement
    evaluator. Here we model expression values and prove the cases that map
    1-1 onto [TypeScriptExpr] constructors. *)

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
| TSVNum : nat -> TSVal
| TSVBool : bool -> TSVal
| TSVUndef : TSVal.

Definition TSEnv := list (string * TSVal).

Fixpoint tslookup (x : string) (env : TSEnv) : option TSVal :=
  match env with
  | [] => None
  | (k, v) :: rest =>
      if String.eqb x k then Some v else tslookup x rest
  end.

(* ------------------------------------------------------------ *)
(* Big-step evaluation on a tiny TS expression fragment         *)
(* ------------------------------------------------------------ *)

Inductive TSEval : TSEnv -> TypeScriptExpr -> TSVal -> Prop :=
| TSE_Num : forall env n,
    TSEval env (TsNumberLiteral (nat_to_string n)) (TSVNum n)
| TSE_Bool : forall env b,
    TSEval env (TsBooleanLiteral b) (TSVBool b)
| TSE_Id : forall env x v,
    tslookup x env = Some v ->
    TSEval env (TsIdentifier x) v.

Definition TSValTy (v : TSVal) (ty : AST) : Prop :=
  match ty with
  | AstRef s =>
      if String.eqb s "Integer" then
        match v with TSVNum _ => True | _ => False end
      else if String.eqb s "Boolean" then
        match v with TSVBool _ => True | _ => False end
      else False
  | _ => False
  end.

Definition TSEnvTy (gamma : list (string * AST)) (env : TSEnv) : Prop :=
  forall x ty, core_lookup x gamma = Some ty ->
    exists v, tslookup x env = Some v /\ TSValTy v ty.

Lemma tsenv_ty_nil : TSEnvTy [] [].
Proof. intros x ty H. discriminate H. Qed.

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

(* ------------------------------------------------------------ *)
(* Emit shape                                                   *)
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
| TSF_Int : forall n, TSLitFrag (AstIntLit n)
| TSF_Bool : forall b, TSLitFrag (AstBoolLit b)
| TSF_Ref : forall x, TSLitFrag (AstRef x)
| TSF_Span : forall sp e, TSLitFrag e -> TSLitFrag (AstSpan sp e).

Lemma emit_ts_span : forall sp e,
  emit_ts_expr (AstSpan sp e) = emit_ts_expr e.
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
