From Stdlib Require Import Strings.String.
From Stdlib Require Import List.
From Stdlib Require Import Bool.
Import ListNotations.

Require Import Chester.AST.

(* An environment mapping variable names to their inferred types *)
Definition TypeEnv := list (string * AST).

(* Simple lookup function *)
Fixpoint lookup_type (name : string) (env : TypeEnv) : option AST :=
  match env with
  | [] => None
  | (k, v) :: rest => 
      if String.eqb name k then Some v else lookup_type name rest
  end.

(* Result type for type checking *)
Inductive TyResult (A : Type) :=
  | TyOk : A -> TyResult A
  | TyErr : string -> TyResult A.

Arguments TyOk {A}.
Arguments TyErr {A}.

(* A very naive equality checker for AST nodes. 
   In a real dependently typed language, this would be alpha-equivalence + beta-reduction. *)
Fixpoint eq_ast (t1 t2 : AST) : bool :=
  match t1, t2 with
  | AstRef n1, AstRef n2 => String.eqb n1 n2
  | AstStringLit s1, AstStringLit s2 => String.eqb s1 s2
  | AstIntLit n1, AstIntLit n2 => Nat.eqb n1 n2
  | AstPi n1 ty1 ret1 eff1, AstPi n2 ty2 ret2 eff2 =>
      (* naive string eq for binder, realistically needs De Bruijn or alpha equivalence *)
      String.eqb n1 n2 && eq_ast ty1 ty2 && eq_ast ret1 ret2
  | AstMeta m1, AstMeta m2 => Nat.eqb m1 m2
  | _, _ => false (* Simplified for demonstration *)
  end.

(* Since AST types are types themselves (e.g. Type, Int, String), we might want some built-ins. *)
Definition TypeUniverse := AstRef "Type".
Definition IntType := AstRef "Int".
Definition StringType := AstRef "String".

(* 
  Core Bidirectional Type Checker
  Assuming NO metavariables.
  We combine inference and checking into a single function for Coq termination.
  If `expected` is None, we infer the type and return it.
  If `expected` is Some ty, we check against it and return TyOk ty.
*)

Fixpoint infer_check (env : TypeEnv) (expr : AST) (expected : option AST) {struct expr} : TyResult AST :=
  match expr with
  | AstRef name =>
      match lookup_type name env with
      | Some ty => 
          match expected with
          | Some expTy => if eq_ast ty expTy then TyOk ty else TyErr "Type mismatch"
          | None => TyOk ty
          end
      | None => TyErr ("Unbound variable: " ++ name)
      end
      
  | AstIntLit _ => 
      match expected with
      | Some expTy => if eq_ast IntType expTy then TyOk IntType else TyErr "Type mismatch"
      | None => TyOk IntType
      end
  
  | AstStringLit _ => 
      match expected with
      | Some expTy => if eq_ast StringType expTy then TyOk StringType else TyErr "Type mismatch"
      | None => TyOk StringType
      end
  
  | AstLam argName argTy body =>
      match expected with
      | Some (AstPi _ expArgTy expRetTy _) =>
          if eq_ast argTy expArgTy then
            match infer_check ((argName, argTy) :: env) body (Some expRetTy) with
            | TyOk _ => TyOk (AstPi argName argTy expRetTy [])
            | TyErr e => TyErr e
            end
          else TyErr "Lambda argument type does not match expected Pi type"
      | Some _ => TyErr "Expected Pi type for lambda"
      | None =>
          (* Infer mode for lambda *)
          match infer_check ((argName, argTy) :: env) body None with
          | TyOk bodyTy => TyOk (AstPi argName argTy bodyTy [])
          | TyErr e => TyErr e
          end
      end
      
  | AstApp func args =>
      match infer_check env func None with
      | TyOk (AstPi argName argTy retTy effs) =>
          match args with
          | arg :: _ => 
              match infer_check env arg (Some argTy) with
              | TyOk _ => 
                  match expected with
                  | Some expTy => if eq_ast retTy expTy then TyOk retTy else TyErr "Type mismatch"
                  | None => TyOk retTy
                  end
              | TyErr e => TyErr e
              end
          | [] => TyErr "Cannot apply to zero arguments"
          end
      | TyOk _ => TyErr "Cannot apply to non-function"
      | TyErr e => TyErr e
      end
      
  | AstPi argName argTy retTy effs =>
      match infer_check env argTy (Some TypeUniverse) with
      | TyOk _ =>
          match infer_check ((argName, argTy) :: env) retTy (Some TypeUniverse) with
          | TyOk _ => 
              match expected with
              | Some expTy => if eq_ast TypeUniverse expTy then TyOk TypeUniverse else TyErr "Type mismatch"
              | None => TyOk TypeUniverse
              end
          | TyErr e => TyErr e
          end
      | TyErr e => TyErr e
      end
      
  | AstMeta _ => TyErr "Core Checker: Encountered unsolved metavariable"
  | _ => TyErr "Unsupported AST node for checker"
  end.

Definition infer (env : TypeEnv) (expr : AST) : TyResult AST :=
  infer_check env expr None.

Definition check (env : TypeEnv) (expr : AST) (expected : AST) : TyResult unit :=
  match infer_check env expr (Some expected) with
  | TyOk _ => TyOk tt
  | TyErr e => TyErr e
  end.
