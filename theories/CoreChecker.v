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

Fixpoint subst_ast (x : string) (v : AST) (body : AST) : AST :=
  match body with
  | AstRef name => if String.eqb name x then v else body
  | AstUniverse l => AstUniverse l
  | AstTuple elems => AstTuple (map (subst_ast x v) elems)
  | AstStringLit s => AstStringLit s
  | AstIntLit n => AstIntLit n
  | AstBoolLit b => AstBoolLit b
  | AstBlock stmts tail => 
      AstBlock (map (subst_ast x v) stmts) (subst_ast x v tail)
  | AstApp f args => AstApp (subst_ast x v f) (map (subst_ast x v) args)
  | AstTypeApp f args => AstTypeApp (subst_ast x v f) (map (subst_ast x v) args)
  | AstLam argName argTy argBody =>
      let newTy := subst_ast x v argTy in
      if String.eqb argName x then AstLam argName newTy argBody
      else AstLam argName newTy (subst_ast x v argBody)
  | AstPi argName argTy retTy effs =>
      let newTy := subst_ast x v argTy in
      if String.eqb argName x then AstPi argName newTy retTy effs
      else AstPi argName newTy (subst_ast x v retTy) effs
  | AstDo e effs => AstDo (subst_ast x v e) (map (subst_ast x v) effs)
  | AstHandle e eff hs => 
      AstHandle (subst_ast x v e) eff (map (fun p => (fst p, subst_ast x v (snd p))) hs)
  | AstLet n val => AstLet n (subst_ast x v val)
  | AstIf c t e => AstIf (subst_ast x v c) (subst_ast x v t) (subst_ast x v e)
  | AstDef n tp p r b => AstDef n tp p r b (* Not substituting inside defs for now *)
  | AstFunTy tp p r effs => AstFunTy tp p (subst_ast x v r) effs
  | AstEnum n tp vars => AstEnum n tp vars
  | AstMatch expr cases => AstMatch (subst_ast x v expr) cases (* Simplistic *)
  | AstRecord n tp fields => AstRecord n tp fields
  | AstFieldAccess expr f => AstFieldAccess (subst_ast x v expr) f
  | AstMeta m => AstMeta m
  | AstSpan sp inner => AstSpan sp (subst_ast x v inner)
  | AstError msg => AstError msg
  end.

Fixpoint whnf_fuel (fuel : nat) (expr : AST) : AST :=
  match fuel with
  | 0 => expr
  | S fuel' =>
      match expr with
      | AstApp f args =>
          let f' := whnf_fuel fuel' f in
          match f' with
          | AstLam argName argTy body =>
              match args with
              | arg :: rest =>
                  let body' := subst_ast argName arg body in
                  match rest with
                  | [] => whnf_fuel fuel' body'
                  | _ => whnf_fuel fuel' (AstApp body' rest)
                  end
              | [] => f'
              end
          | _ => AstApp f' args
          end
      | AstSpan sp inner => whnf_fuel fuel' inner
      | _ => expr
      end
  end.

Definition whnf (expr : AST) : AST := whnf_fuel 1000 expr.


Fixpoint strip_span (e : AST) : AST :=
  match e with
  | AstSpan _ inner => strip_span inner
  | AstTuple elems => AstTuple (map strip_span elems)
  | AstBlock stmts tail => AstBlock (map strip_span stmts) (strip_span tail)
  | AstApp f args => AstApp (strip_span f) (map strip_span args)
  | AstTypeApp f args => AstTypeApp (strip_span f) (map strip_span args)
  | AstLam n ty b => AstLam n (strip_span ty) (strip_span b)
  | AstPi n ty ret effs => AstPi n (strip_span ty) (strip_span ret) effs
  | AstDo e effs => AstDo (strip_span e) effs
  | AstHandle e eff hs => AstHandle (strip_span e) eff (map (fun p => (fst p, strip_span (snd p))) hs)
  | AstLet n val => AstLet n (strip_span val)
  | AstIf c t e => AstIf (strip_span c) (strip_span t) (strip_span e)
  | AstDef n tp p r b => AstDef n tp p r b
  | AstFunTy tp p r effs => AstFunTy tp p (strip_span r) effs
  | AstMatch expr cases => AstMatch (strip_span expr) cases
  | AstRecord n tp fields => AstRecord n tp fields
  | AstFieldAccess expr f => AstFieldAccess (strip_span expr) f
  | _ => e
  end.

Fixpoint equiv_ast_raw (t1 t2 : AST) : bool :=
  match t1, t2 with
  | AstRef n1, AstRef n2 => String.eqb n1 n2
  | AstUniverse l1, AstUniverse l2 => Nat.eqb l1 l2
  | AstStringLit s1, AstStringLit s2 => String.eqb s1 s2
  | AstIntLit n1, AstIntLit n2 => Nat.eqb n1 n2
  | AstBoolLit b1, AstBoolLit b2 => Bool.eqb b1 b2
  | AstApp f1 a1, AstApp f2 a2 => false (* Simplistic for multiple args *)
  | AstLam n1 t1 b1, AstLam n2 t2 b2 => false
  | AstPi n1 ty1 ret1 eff1, AstPi n2 ty2 ret2 eff2 => 
      String.eqb n1 n2 && equiv_ast_raw ty1 ty2 && equiv_ast_raw ret1 ret2
  | AstMeta m1, AstMeta m2 => Nat.eqb m1 m2
  | _, _ => false
  end.

Definition equiv_ast (t1 t2 : AST) : bool :=
  equiv_ast_raw (strip_span (whnf t1)) (strip_span (whnf t2)).

Definition TypeUniverse := AstUniverse 0.
Definition IntType := AstRef "Int".
Definition StringType := AstRef "String".
Definition BoolType := AstRef "Bool".

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
          | Some expTy => if equiv_ast ty expTy then TyOk ty else TyErr "Type mismatch"
          | None => TyOk ty
          end
      | None => TyErr ("Unbound variable: " ++ name)
      end
      
  | AstIntLit _ => 
      match expected with
      | Some expTy => if equiv_ast IntType expTy then TyOk IntType else TyErr "Type mismatch"
      | None => TyOk IntType
      end
  
  | AstStringLit _ => 
      match expected with
      | Some expTy => if equiv_ast StringType expTy then TyOk StringType else TyErr "Type mismatch"
      | None => TyOk StringType
      end
      
  | AstBoolLit _ =>
      match expected with
      | Some expTy => if equiv_ast BoolType expTy then TyOk BoolType else TyErr "Type mismatch"
      | None => TyOk BoolType
      end
  
  | AstLam argName argTy body =>
      match expected with
      | Some (AstPi _ expArgTy expRetTy _) =>
          if equiv_ast argTy expArgTy then
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
                  let actualRetTy := subst_ast argName arg retTy in
                  match expected with
                  | Some expTy => if equiv_ast actualRetTy expTy then TyOk actualRetTy else TyErr "Type mismatch"
                  | None => TyOk actualRetTy
                  end
              | TyErr e => TyErr e
              end
          | [] => TyErr "Cannot apply to zero arguments"
          end
      | TyOk _ => TyErr "Cannot apply to non-function"
      | TyErr e => TyErr e
      end
      
  | AstPi argName argTy retTy effs =>
      match infer_check env argTy None with
      | TyOk (AstUniverse l1) =>
          match infer_check ((argName, argTy) :: env) retTy None with
          | TyOk (AstUniverse l2) => 
              let outUni := AstUniverse (Nat.max l1 l2) in
              match expected with
              | Some expTy => if equiv_ast outUni expTy then TyOk outUni else TyErr "Type mismatch"
              | None => TyOk outUni
              end
          | TyOk _ => TyErr "Return type of Pi is not a Universe"
          | TyErr e => TyErr e
          end
      | TyOk _ => TyErr "Argument type of Pi is not a Universe"
      | TyErr e => TyErr e
      end
      
  | AstBlock stmts ret_expr =>
      let fix check_stmts (current_env : list (string * AST)) (ls : list AST) : TyResult (list (string * AST)) :=
        match ls with
        | [] => TyOk current_env
        | x :: xs =>
            match x with
            | AstLet name value =>
                match infer_check current_env value None with
                | TyOk valTy => check_stmts ((name, valTy) :: current_env) xs
                | TyErr e => TyErr e
                end
            | _ =>
                match infer_check current_env x None with
                | TyOk _ => check_stmts current_env xs
                | TyErr e => TyErr e
                end
            end
        end
      in
      match check_stmts env stmts with
      | TyOk final_env => infer_check final_env ret_expr expected
      | TyErr e => TyErr e
      end
      
  | AstLet name value => TyOk (AstTuple [])
      
  | AstIf cond thenB elseB =>
      match infer_check env cond (Some BoolType) with
      | TyOk _ =>
          match infer_check env thenB expected with
          | TyOk thenTy =>
              match infer_check env elseB (Some thenTy) with
              | TyOk _ => TyOk thenTy
              | TyErr e => TyErr e
              end
          | TyErr e => TyErr e
          end
      | TyErr e => TyErr e
      end
      
  | AstDef name type_params params ret_ty body =>
      let fix build_env (ps : list (string * AST)) (e : TypeEnv) : TypeEnv :=
        match ps with
        | [] => e
        | (pname, pty) :: rest => build_env rest ((pname, pty) :: e)
        end
      in
      let body_env := build_env params env in
      match infer_check body_env body (Some ret_ty) with
      | TyOk _ => 
          let fix build_pi (ps : list (string * AST)) : AST :=
            match ps with
            | [] => ret_ty
            | (pname, pty) :: rest => AstPi pname pty (build_pi rest) []
            end
          in
          TyOk (build_pi params)
      | TyErr e => TyErr e
      end
      
  | AstMatch expr cases =>
      match infer_check env expr None with
      | TyOk expr_ty =>
          let fix check_cases (cs : list (PatternAST * AST)) : TyResult AST :=
            match cs with
            | [] => TyErr "Empty match"
            | [(pat, body)] => infer_check env body expected
            | (pat, body) :: rest =>
                match infer_check env body expected with
                | TyOk ty_body =>
                    match check_cases rest with
                    | TyOk ty_rest =>
                        if equiv_ast ty_body ty_rest then TyOk ty_body else TyErr "Match branches have mismatching types"
                    | err => err
                    end
                | err => err
                end
            end
          in
          check_cases cases
      | err => err
      end
      
  | AstEnum _ _ _ => TyOk (AstRef "Unit")
  | AstRecord _ _ _ => TyOk (AstRef "Unit")
  | AstFieldAccess expr field =>
      match infer_check env expr None with
      | TyOk expr_ty =>
          (* In a complete checker, we would look up expr_ty's record definition and find the type of `field`. 
             For this minimal verified milestone, we just assume the field access evaluates successfully. *)
          TyOk (AstRef "Unit")
      | err => err
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

(* 
  --- Tests ---
*)
Require Import String.
Definition test_env : TypeEnv := 
  [ ("Bool"%string, AstUniverse 0);
    ("true"%string, AstRef "Bool"%string) ].

Definition test_func : AST := 
  AstLam "b"%string (AstRef "Bool"%string) (AstIf (AstRef "b"%string) (AstRef "Int"%string) (AstRef "String"%string)).

Definition test_app_true : AST := 
  AstApp test_func [AstRef "true"%string].

Definition test_whnf_app : AST := whnf test_app_true.

Eval compute in test_whnf_app.

