From Stdlib Require Import Strings.String.
From Stdlib Require Import List.
From Stdlib Require Import PeanoNat.
Import ListNotations.

Require Import Chester.CST.

(* A unique identifier for meta variables *)
Definition MetaId := nat.

(* 
  Meta variables (holes, schematic variables) can be in one of three states:
  1. Unsolved: No information is known yet.
  2. Constrained (Partial): Some information is known. For example, during effect 
     inference, we might know that a hole MUST contain at least the 'IO' effect, 
     but there might be others discovered later.
  3. Solved: The final, exact information is known.
*)
Inductive MetaState (A : Type) (Partial : Type) :=
  | Unsolved : MetaState A Partial
  | Constrained : Partial -> MetaState A Partial
  | Solved : A -> MetaState A Partial.

Arguments Unsolved {A} {Partial}.
Arguments Constrained {A} {Partial}.
Arguments Solved {A} {Partial}.

(* An effect reference in the Chester language *)
Inductive EffectRef :=
  | BuiltinEffect : string -> EffectRef
  | UserEffect : string -> EffectRef.

(* A set/list of effects *)
Definition EffectSet := list EffectRef.

(* 
  The Abstract Syntax Tree (AST). 
  Notice how it includes Meta variables that point to the solver's state. 
*)
Inductive AST : Type :=
  | AstRef : string -> AST
  | AstTuple : list AST -> AST
  | AstStringLit : string -> AST
  | AstIntLit : nat -> AST
  | AstBlock : list AST -> AST -> AST
  | AstApp : AST -> list AST -> AST
  | AstLam : string -> AST -> AST -> AST (* argName, argTy, body *)
  | AstPi : string -> AST -> AST -> EffectSet -> AST (* argName, argTy, retTy, effects *)
  | AstDo : AST -> list AST -> AST (* perform an effect operation *)
  | AstHandle : AST -> EffectRef -> list (string * AST) -> AST
  
  (* A meta variable or hole, indexed by its unique ID *)
  | AstMeta : MetaId -> AST
  
  | AstError : string -> AST.

(* 
  The Solver Environment 
  Maps a MetaId to its current state. For this example, we specialize it 
  to inferring effect sets, where both Partial and Final information are EffectSets.
*)
Definition SolverState := MetaId -> MetaState EffectSet EffectSet.

Definition empty_state : SolverState :=
  fun _ => Unsolved.

(* 
  Constraint Solving Operations 
*)

(* Update the state of a specific MetaId in the solver environment *)
Definition update_state (id : MetaId) (new_state : MetaState EffectSet EffectSet) (st : SolverState) : SolverState :=
  fun x => if Nat.eqb x id then new_state else st x.

(* 
  Add an effect to a meta variable.
  This models the transition: Unsolved -> Constrained -> Solved 
*)
Definition add_effect_constraint (id : MetaId) (eff : EffectRef) (st : SolverState) : SolverState :=
  match st id with
  | Unsolved => 
      (* Transition from Unsolved to Constrained with a single effect *)
      update_state id (Constrained [eff]) st
  | Constrained effs =>
      (* Add the new effect to the partial knowledge *)
      update_state id (Constrained (eff :: effs)) st
  | Solved effs =>
      (* If it's already fully solved, we might just verify or ignore. 
         For simplicity, we leave it as is, or we could return an error state. *)
      st
  end.

(* Finalize a constrained meta variable, cementing its partial knowledge as the final solved state *)
Definition finalize_meta (id : MetaId) (st : SolverState) : SolverState :=
  match st id with
  | Unsolved => update_state id (Solved []) st
  | Constrained effs => update_state id (Solved effs) st
  | Solved effs => st
  end.
