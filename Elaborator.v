From Stdlib Require Import Strings.String.
From Stdlib Require Import List.
From Stdlib Require Import PeanoNat.
Import ListNotations.

Require Import Chester.CST.
Require Import Chester.AST.
Require Import Chester.CoreChecker.

(* 
  Elaborator State
  The Elaborator transforms a CST into an AST. During this process, 
  it allocates fresh Meta variables and solves constraints.
*)
Record ElabState : Type := mkElabState {
  next_meta : nat;
  solver_st : SolverState
}.

Definition init_elab_state : ElabState := 
  mkElabState 0 empty_state.

(* The State Monad with Error handling *)
Inductive ElabResult (A : Type) :=
  | ElabOk : A -> ElabState -> ElabResult A
  | ElabErr : string -> ElabState -> ElabResult A.

Arguments ElabOk {A}.
Arguments ElabErr {A}.

Definition ElabM (A : Type) := ElabState -> ElabResult A.

Definition ret {A} (a : A) : ElabM A :=
  fun s => ElabOk a s.

Definition bind {A B} (m : ElabM A) (f : A -> ElabM B) : ElabM B :=
  fun s => 
    match m s with
    | ElabOk a s' => f a s'
    | ElabErr e s' => ElabErr e s'
    end.

Definition throw {A} (e : string) : ElabM A :=
  fun s => ElabErr e s.

(* Notations for do-notation *)
Declare Scope elab_scope.
Notation "x <- m1 ; m2" := (bind m1 (fun x => m2)) 
  (right associativity, at level 60) : elab_scope.
Notation "m1 ;; m2" := (bind m1 (fun _ => m2)) 
  (right associativity, at level 60) : elab_scope.
Open Scope elab_scope.

(* Generate a fresh metavariable *)
Definition fresh_meta : ElabM AST :=
  fun s =>
    let id := next_meta s in
    let s' := mkElabState (id + 1) (solver_st s) in
    ElabOk (AstMeta id) s'.

(* Get the current solver state *)
Definition get_solver : ElabM SolverState :=
  fun s => ElabOk (solver_st s) s.

(* Update the solver state *)
Definition put_solver (st : SolverState) : ElabM unit :=
  fun s => ElabOk tt (mkElabState (next_meta s) st).

(* Constrain a metavariable with an effect *)
Definition constrain_effect (id : MetaId) (eff : EffectRef) : ElabM unit :=
  st <- get_solver ;
  put_solver (add_effect_constraint id eff st).

(* 
  Unification / Constraint Generation 
  For simplicity in this mockup, we'll assume exact equality or filling an unsolved metavariable.
*)
Fixpoint unify (t1 t2 : AST) : ElabM unit :=
  match t1, t2 with
  | AstRef n1, AstRef n2 =>
      if String.eqb n1 n2 then ret tt else throw "Unification failed: name mismatch"
  | AstMeta m1, AstMeta m2 =>
      if Nat.eqb m1 m2 then ret tt else throw "Unification of two different metas not fully implemented"
  | _, _ =>
      (* In a real elaborator, if one is a Meta, we'd solve it.
         For this mockup, we just do a simple fallback. *)
      throw "Unification failed or unimplemented"
  end.

(* 
  The Elaborator: Elaborates CST to AST
  Takes a CST expression and expected type (if bidirectional)
*)
Fixpoint elaborate (env : TypeEnv) (expr : CST) (expected : option AST) {struct expr} : ElabM (AST * AST) :=
  match expr with
  | Symbol name _ =>
      match lookup_type name env with
      | Some ty => 
          match expected with
          | Some expTy => 
              unify ty expTy ;;
              ret (AstRef name, ty)
          | None => ret (AstRef name, ty)
          end
      | None => throw ("Unbound variable: " ++ name)
      end
  
  | StringLiteral s _ => 
      ret (AstStringLit s, StringType)
      
  | SeqOf [Symbol "io_print" _; arg] _ =>
      (* Example of a function that has an effect *)
      res <- elaborate env arg (Some StringType) ;
      let (ast_arg, _) := res in
      
      (* Generate a fresh meta for the effect of this expression *)
      eff_meta_ast <- fresh_meta ;
      
      match eff_meta_ast with
      | AstMeta m =>
          (* We constrain this metavariable to include the IoEffect *)
          constrain_effect m (BuiltinEffect "io") ;;
          ret (AstApp (AstRef "io_print") [ast_arg], IntType)
      | _ => throw "Internal error"
      end
      
  | _ => throw "Unsupported CST node for elaboration"
  end.
