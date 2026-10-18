(** Operational semantics and emission correctness for the Go IR backend.

    Parallels [RocqIRSem.v] and [TypeScriptIRSem.v]:
    - [GoVal] represents values in the Go runtime model (integers, booleans,
      closures, and unit).
    - [GoEval] and [GoEvalBlock] define big-step evaluation on the emitted Go IR
      expressions and statement blocks.
    - [GoValTy] and [GoEnvTy] establish semantic typing (logical relations)
      between Chester core types and Go IR runtime values.
    - [emit_go_correct] proves that emitting any well-typed pure CoreFrag AST
      preserves semantics: the emitted Go expression evaluates to a value
      satisfying the semantic type relation.
    
    Zero unproven axioms: closed under the global context. *)

From Stdlib Require Import Strings.String.
From Stdlib Require Import List.
From Stdlib Require Import Bool.
From Stdlib Require Import Arith.PeanoNat.
Import ListNotations.
Open Scope string_scope.

Require Import Chester.AST.
Require Import Chester.GoAST.
Require Import Chester.Backend.
Require Import Chester.CoreChecker.
Require Import Chester.CoreTyping.

(* ------------------------------------------------------------ *)
(* Semantic values and evaluation environment                   *)
(* ------------------------------------------------------------ *)

Inductive GoVal : Type :=
| GVNum  : nat -> GoVal
| GVBool : bool -> GoVal
| GVClo  : string -> list GoStmt -> list (string * GoVal) -> GoVal
| GVUnit : GoVal.

Definition GoEnv : Type := list (string * GoVal).

Fixpoint golookup (x : string) (env : GoEnv) : option GoVal :=
  match env with
  | [] => None
  | (k, v) :: rest => if String.eqb x k then Some v else golookup x rest
  end.

(* ------------------------------------------------------------ *)
(* Big-step evaluation on Go expressions and statement blocks   *)
(* ------------------------------------------------------------ *)

Inductive GoEvalBlock : GoEnv -> list GoStmt -> GoVal -> Prop :=

(** Return statement at head of block evaluates expression to value. *)
| GEB_Return : forall env e v rest,
    GoEval env e v ->
    GoEvalBlock env (GoReturn e :: rest) v

(** Let binding: evaluate rhs, bind in extended environment. *)
| GEB_Let : forall env x ty rhs vrhs rest v,
    GoEval env rhs vrhs ->
    GoEvalBlock ((x, vrhs) :: env) rest v ->
    GoEvalBlock env (GoLet x ty rhs :: rest) v

(** If statement in block: evaluate condition, then take corresponding branch. *)
| GEB_IfT : forall env cond true_b false_b v rest,
    GoEval env cond (GVBool true) ->
    GoEvalBlock env true_b v ->
    GoEvalBlock env (GoIfStmt cond true_b false_b :: rest) v

| GEB_IfF : forall env cond true_b false_b v rest,
    GoEval env cond (GVBool false) ->
    GoEvalBlock env false_b v ->
    GoEvalBlock env (GoIfStmt cond true_b false_b :: rest) v

(** Discard binding: ignore and continue. *)
| GEB_Discard : forall env x rest v,
    GoEvalBlock env rest v ->
    GoEvalBlock env (GoDiscardBinding x :: rest) v

(** Expr statement: execute and discard. *)
| GEB_ExprStmt : forall env e ve rest v,
    GoEval env e ve ->
    GoEvalBlock env rest v ->
    GoEvalBlock env (GoExprStmt e :: rest) v

with GoEval : GoEnv -> GoExpr -> GoVal -> Prop :=

| GE_Num : forall env n,
    GoEval env (GoIntLiteral (nat_to_string n)) (GVNum n)

| GE_Bool : forall env b,
    GoEval env (GoBoolLiteral b) (GVBool b)

| GE_Id : forall env x v,
    golookup x env = Some v ->
    GoEval env (GoIdentifier x) v

(** Function literal produces a closure capturing parameter name, body, and env. *)
| GE_Func : forall env x ty ret body,
    GoEval env (GoFuncLiteral [(x, ty)] ret body) (GVClo x body env)

(** IIFE: immediately invoked 0-arg function literal runs its body block. *)
| GE_IIFE : forall env ret body v,
    GoEvalBlock env body v ->
    GoEval env (GoCall (GoFuncLiteral [] ret body) []) v

(** Function call (via go_call_emitted and go_call_args): evaluates callee to closure,
    arg to va, and runs closure body in extended env. *)
| GE_Call : forall env direct loc f f_ast arg x body envf va v,
    GoEval env f (GVClo x body envf) ->
    GoEval env arg va ->
    GoEvalBlock ((x, va) :: envf) body v ->
    GoEval env (go_call_emitted direct f (go_call_args [] loc f_ast [arg])) v

(** Condition expression: direct boolean value. *)
| GE_CondDirect : forall env loc cond b,
    GoEval env cond (GVBool b) ->
    GoEval env (go_bool_cond [] loc cond) (GVBool b).

(* ------------------------------------------------------------ *)
(* Semantic typing of Go IR values (logical relation on types)  *)
(* ------------------------------------------------------------ *)

Fixpoint GoValTy (v : GoVal) (ty : AST) {struct ty} : Prop :=
  match ty with
  | AstRef s =>
      if String.eqb s "Integer" then
        match v with GVNum _ => True | _ => False end
      else if String.eqb s "Boolean" then
        match v with GVBool _ => True | _ => False end
      else False
  | AstFunTy [] [(_ , sigma)] tau [] =>
      match v with
      | GVClo x body env =>
          forall va, GoValTy va sigma ->
            exists v', GoEvalBlock ((x, va) :: env) body v' /\ GoValTy v' tau
      | _ => False
      end
  | _ => False
  end.

Definition GoEnvTy (gamma : list (string * AST)) (env : GoEnv) : Prop :=
  forall x ty, core_lookup x gamma = Some ty ->
    exists v, golookup x env = Some v /\ GoValTy v ty.

Lemma goenv_ty_nil : GoEnvTy [] [].
Proof. intros x ty H. discriminate H. Qed.

Lemma goenv_ty_cons : forall gamma env x sigma v,
  GoEnvTy gamma env ->
  GoValTy v sigma ->
  GoEnvTy ((x, sigma) :: gamma) ((x, v) :: env).
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

Lemma GoValTy_num : forall n, GoValTy (GVNum n) CoreChecker.IntType.
Proof.
  intros n.
  unfold CoreChecker.IntType, GoValTy.
  rewrite (String.eqb_refl "Integer"). exact I.
Qed.

Lemma GoValTy_bool : forall b, GoValTy (GVBool b) CoreChecker.BoolType.
Proof.
  intros b.
  unfold CoreChecker.BoolType, GoValTy.
  destruct (String.eqb "Boolean" "Integer") eqn:E1.
  - apply String.eqb_eq in E1. discriminate E1.
  - rewrite (String.eqb_refl "Boolean"). exact I.
Qed.

Lemma GoValTy_clo : forall x body env sigma tau,
  (forall va, GoValTy va sigma ->
     exists v', GoEvalBlock ((x, va) :: env) body v' /\ GoValTy v' tau) ->
  GoValTy (GVClo x body env) (CoreArrow sigma tau).
Proof.
  intros x body env sigma tau H.
  unfold CoreArrow. simpl. exact H.
Qed.

Lemma GoValTy_arrow_is_clo : forall v sigma tau,
  GoValTy v (CoreArrow sigma tau) ->
  exists x body env, v = GVClo x body env /\
    forall va, GoValTy va sigma ->
      exists v', GoEvalBlock ((x, va) :: env) body v' /\ GoValTy v' tau.
Proof.
  intros v sigma tau H.
  unfold CoreArrow in H. simpl in H.
  destruct v; try contradiction.
  exists s, l, l0. split; [reflexivity|exact H].
Qed.

Lemma GoValTy_bool_inv : forall v,
  GoValTy v CoreChecker.BoolType -> exists b, v = GVBool b.
Proof.
  intros v H.
  unfold CoreChecker.BoolType, GoValTy in H.
  destruct (String.eqb "Boolean" "Integer") eqn:E1.
  - apply String.eqb_eq in E1. discriminate E1.
  - rewrite (String.eqb_refl "Boolean") in H.
    destruct v; try contradiction. exists b. reflexivity.
Qed.

(* ------------------------------------------------------------ *)
(* Rewrite lemmas for emit_go_expr and emit_go_block            *)
(* ------------------------------------------------------------ *)

Lemma emit_go_int : forall loc n, emit_go_expr [] loc (AstIntLit n) = GoIntLiteral (nat_to_string n).
Proof. reflexivity. Qed.

Lemma emit_go_bool : forall loc b, emit_go_expr [] loc (AstBoolLit b) = GoBoolLiteral b.
Proof. reflexivity. Qed.

Lemma emit_go_ref : forall loc x, emit_go_expr [] loc (AstRef x) = GoIdentifier x.
Proof. reflexivity. Qed.

Lemma emit_go_lam : forall loc x ty body,
  emit_go_expr [] loc (AstLam x ty body) =
  GoFuncLiteral [(x, go_iface)] go_iface (emit_go_block [] (go_bind_local loc x go_iface) body).
Proof. reflexivity. Qed.

Lemma emit_go_if : forall loc c t e,
  emit_go_expr [] loc (AstIf c t e) =
  GoCall (GoFuncLiteral [] go_iface [GoIfStmt (go_bool_cond [] loc (emit_go_expr [] loc c)) (emit_go_block [] loc t) (emit_go_block [] loc e)]) [].
Proof. reflexivity. Qed.

Lemma emit_go_span : forall loc sp e,
  emit_go_expr [] loc (AstSpan sp e) = emit_go_expr [] loc e.
Proof. reflexivity. Qed.

Lemma emit_go_block_int : forall loc n,
  emit_go_block [] loc (AstIntLit n) = [GoReturn (GoIntLiteral (nat_to_string n))].
Proof. reflexivity. Qed.

Lemma emit_go_block_bool : forall loc b,
  emit_go_block [] loc (AstBoolLit b) = [GoReturn (GoBoolLiteral b)].
Proof. reflexivity. Qed.

Lemma emit_go_block_ref : forall loc x,
  emit_go_block [] loc (AstRef x) = [GoReturn (GoIdentifier x)].
Proof. reflexivity. Qed.

Lemma emit_go_block_lam : forall loc x ty body,
  emit_go_block [] loc (AstLam x ty body) =
  [GoReturn (GoFuncLiteral [(x, go_iface)] go_iface (emit_go_block [] (go_bind_local loc x go_iface) body))].
Proof. reflexivity. Qed.

Lemma emit_go_block_if : forall loc c t e,
  emit_go_block [] loc (AstIf c t e) =
  [GoIfStmt (go_bool_cond [] loc (emit_go_expr [] loc c)) (emit_go_block [] loc t) (emit_go_block [] loc e)].
Proof. reflexivity. Qed.

Lemma emit_go_block_span : forall loc sp e,
  emit_go_block [] loc (AstSpan sp e) = emit_go_block [] loc e.
Proof. reflexivity. Qed.

Lemma emit_go_app1 : forall loc f a,
  emit_go_expr [] loc (AstApp f [a]) =
  go_call_emitted (go_app_direct loc f) (emit_go_expr [] loc f) (go_call_args [] loc f [emit_go_expr [] loc a]).
Proof. reflexivity. Qed.

Lemma emit_go_block_app1 : forall loc f a,
  emit_go_block [] loc (AstApp f [a]) =
  [GoReturn (go_call_emitted (go_app_direct loc f) (emit_go_expr [] loc f) (go_call_args [] loc f [emit_go_expr [] loc a]))].
Proof. reflexivity. Qed.

(* ------------------------------------------------------------ *)
(* Full emit correctness for CoreFrag                           *)
(* ------------------------------------------------------------ *)

Theorem emit_go_correct :
  forall gamma loc e ty env,
    CoreFrag e ->
    CoreWT gamma e ty ->
    GoEnvTy gamma env ->
    (exists v, GoEval env (emit_go_expr [] loc e) v /\ GoValTy v ty) /\
    (exists v, GoEvalBlock env (emit_go_block [] loc e) v /\ GoValTy v ty).
Proof.
  intros gamma loc e ty env Hfrag.
  revert gamma loc ty env.
  induction Hfrag as
    [ n
    | b
    | x
    | x tyb body Hty IHty body_frag IHbody
    | f a Hf IHf Ha IHa
    | c t e Hc IHc Ht IHt He IHe
    | sp e He IHe
    ]; intros gamma loc ty env Hwt Henv.

  (* Int *)
  - inversion Hwt; subst.
    split.
    + exists (GVNum n). split; [rewrite emit_go_int; apply GE_Num | apply GoValTy_num].
    + exists (GVNum n). split; [rewrite emit_go_block_int; apply GEB_Return; apply GE_Num | apply GoValTy_num].

  (* Bool *)
  - inversion Hwt; subst.
    split.
    + exists (GVBool b). split; [rewrite emit_go_bool; apply GE_Bool | apply GoValTy_bool].
    + exists (GVBool b). split; [rewrite emit_go_block_bool; apply GEB_Return; apply GE_Bool | apply GoValTy_bool].

  (* Ref *)
  - inversion Hwt; subst.
    match goal with
    | Hlook : core_lookup x gamma = Some ty |- _ =>
        destruct (Henv x ty Hlook) as [v [Hl Ht]]
    end.
    split.
    + exists v. split; [rewrite emit_go_ref; apply GE_Id; exact Hl | exact Ht].
    + exists v. split; [rewrite emit_go_block_ref; apply GEB_Return; apply GE_Id; exact Hl | exact Ht].

  (* Lam *)
  - inversion Hwt; subst.
    split.
    + exists (GVClo x (emit_go_block [] (go_bind_local loc x go_iface) body) env).
      split.
      * rewrite emit_go_lam. apply GE_Func.
      * apply GoValTy_clo. intros va Hva.
        match goal with
        | Hbwt : CoreWT ((?x0, ?sigma) :: gamma) body ?tau0 |- _ =>
            exact (proj2 (IHbody ((x0, sigma) :: gamma) (go_bind_local loc x go_iface) tau0 ((x0, va) :: env) Hbwt
                            (goenv_ty_cons gamma env x0 sigma va Henv Hva)))
        end.
    + exists (GVClo x (emit_go_block [] (go_bind_local loc x go_iface) body) env).
      split.
      * rewrite emit_go_block_lam. apply GEB_Return. apply GE_Func.
      * apply GoValTy_clo. intros va Hva.
        match goal with
        | Hbwt : CoreWT ((?x0, ?sigma) :: gamma) body ?tau0 |- _ =>
            exact (proj2 (IHbody ((x0, sigma) :: gamma) (go_bind_local loc x go_iface) tau0 ((x0, va) :: env) Hbwt
                            (goenv_ty_cons gamma env x0 sigma va Henv Hva)))
        end.

  (* App *)
  - inversion Hwt; subst.
    match goal with
    | Hfwt : CoreWT gamma f (CoreArrow ?sigma ?tau0),
      Hawt : CoreWT gamma a ?sigma |- _ =>
        destruct (IHf gamma loc _ env Hfwt Henv) as [[vf [Ef Tf]] _];
        destruct (IHa gamma loc _ env Hawt Henv) as [[va [Ea Ta]] _];
        destruct (GoValTy_arrow_is_clo vf sigma tau0 Tf)
          as [x0 [body [envf [Evf Hclo]]]];
        rewrite Evf in Ef;
        destruct (Hclo va Ta) as [v [Ev Tv]]
    end.
    split.
    + exists v. split; [| exact Tv].
      rewrite emit_go_app1.
      apply GE_Call with (loc := loc) (x := x0) (body := body) (envf := envf) (va := va); [exact Ef | exact Ea | exact Ev].
    + exists v. split; [| exact Tv].
      rewrite emit_go_block_app1. apply GEB_Return.
      apply GE_Call with (loc := loc) (x := x0) (body := body) (envf := envf) (va := va); [exact Ef | exact Ea | exact Ev].

  (* If *)
  - inversion Hwt; subst.
    match goal with
    | Hcwt : CoreWT gamma c CoreChecker.BoolType,
      Htwt : CoreWT gamma t ty,
      Hewt : CoreWT gamma e ty |- _ =>
        destruct (IHc gamma loc _ env Hcwt Henv) as [[vc [Ec Tc]] _];
        destruct (GoValTy_bool_inv vc Tc) as [bv Eb];
        rewrite Eb in Ec;
        destruct bv
    end.
    + (* c = true *)
      match goal with
      | Htwt : CoreWT gamma t ty |- _ =>
          destruct (IHt gamma loc ty env Htwt Henv) as [_ [vb [Ebblk Tb]]]
      end.
      split.
      * exists vb. split; [| exact Tb].
        rewrite emit_go_if. apply GE_IIFE. apply GEB_IfT with (v := vb); [apply GE_CondDirect; exact Ec | exact Ebblk].
      * exists vb. split; [| exact Tb].
        rewrite emit_go_block_if. apply GEB_IfT with (v := vb); [apply GE_CondDirect; exact Ec | exact Ebblk].
    + (* c = false *)
      match goal with
      | Hewt : CoreWT gamma e ty |- _ =>
          destruct (IHe gamma loc ty env Hewt Henv) as [_ [vb [Ebblk Tb]]]
      end.
      split.
      * exists vb. split; [| exact Tb].
        rewrite emit_go_if. apply GE_IIFE. apply GEB_IfF with (v := vb); [apply GE_CondDirect; exact Ec | exact Ebblk].
      * exists vb. split; [| exact Tb].
        rewrite emit_go_block_if. apply GEB_IfF with (v := vb); [apply GE_CondDirect; exact Ec | exact Ebblk].

  (* Span *)
  - inversion Hwt; subst.
    match goal with
    | Hinner : CoreWT gamma e ty |- _ =>
        destruct (IHe gamma loc ty env Hinner Henv) as [[ve [Ee Te]] [vb [Eb Tb]]]
    end.
    split.
    + exists ve. split; [rewrite emit_go_span; exact Ee | exact Te].
    + exists vb. split; [rewrite emit_go_block_span; exact Eb | exact Tb].
Qed.

Corollary emit_go_expr_correct :
  forall gamma e ty env,
    CoreFrag e ->
    CoreWT gamma e ty ->
    GoEnvTy gamma env ->
    exists v, GoEval env (emit_go_expr [] [] e) v /\ GoValTy v ty.
Proof.
  intros gamma e ty env Hf Hw Henv.
  exact (proj1 (emit_go_correct gamma [] e ty env Hf Hw Henv)).
Qed.

Corollary emit_go_correct_closed :
  forall e ty,
    CoreFrag e ->
    CoreWT [] e ty ->
    exists v, GoEval [] (emit_go_expr [] [] e) v /\ GoValTy v ty.
Proof.
  intros e ty Hf Hw.
  eapply emit_go_expr_correct; [exact Hf | exact Hw | exact goenv_ty_nil].
Qed.

Print Assumptions emit_go_correct.
