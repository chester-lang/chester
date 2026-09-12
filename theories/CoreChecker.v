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

From Stdlib Require Import Arith.PeanoNat.
Open Scope string_scope.

Definition string_in_list (s : string) (xs : list string) : bool :=
  existsb (String.eqb s) xs.


(* Free-name occurrence (stops under binders of the same name). *)
Fixpoint free_in (name : string) (t : AST) {struct t} : bool :=
  match t with
  | AstRef n => String.eqb n name
  | AstUniverse _ | AstStringLit _ | AstIntLit _ | AstBoolLit _
  | AstMeta _ | AstError _ | AstImport _ _ _ _ | AstEnum _ _ _ | AstRecord _ _ _ =>
      false
  | AstTuple elems => existsb (free_in name) elems
  | AstBlock stmts tail => orb (existsb (free_in name) stmts) (free_in name tail)
  | AstApp f args => orb (free_in name f) (existsb (free_in name) args)
  | AstImplicitApp f args => orb (free_in name f) (existsb (free_in name) args)
  | AstLam n ty body =>
      orb (free_in name ty)
        (if String.eqb n name then false else free_in name body)
  | AstPi n ty ret _ =>
      orb (free_in name ty)
        (if String.eqb n name then false else free_in name ret)
  | AstDo e effs => orb (free_in name e) (existsb (free_in name) effs)
  | AstHandle e _ hs =>
      orb (free_in name e) (existsb (fun p => free_in name (snd p)) hs)
  | AstLet n val => free_in name val
  | AstVar n val => free_in name val
  | AstAssign _ val => free_in name val
  | AstBox e _ => free_in name e
  | AstUnbox e => free_in name e
  | AstIf c th el => orb (free_in name c) (orb (free_in name th) (free_in name el))
  | AstDef _ _ params ret body =>
      orb (existsb (fun p => free_in name (snd p)) params)
        (orb (free_in name ret) (free_in name body))
  | AstFunTy _ params ret _ =>
      orb (existsb (fun p => free_in name (snd p)) params) (free_in name ret)
  | AstMatch e cases =>
      orb (free_in name e)
        (existsb
           (fun c =>
              let pat := fst c in
              let body := snd c in
              let bound :=
                match pat with
                | PatVar n => String.eqb n name
                | PatConstructor _ vs => string_in_list name vs
                | PatWildcard => false
                end
              in
              if bound then false else free_in name body)
           cases)
  | AstFieldAccess e _ => free_in name e
  | AstExtension _ _ tgt meths =>
      orb (free_in name tgt) (existsb (free_in name) meths)
  | AstSpan _ inner => free_in name inner
  end.

(* Rename free occurrences of [old] to [new] (capture-aware: stop under binder [old]). *)
Fixpoint rename_free (old new : string) (t : AST) {struct t} : AST :=
  match t with
  | AstRef n => if String.eqb n old then AstRef new else t
  | AstUniverse _ | AstStringLit _ | AstIntLit _ | AstBoolLit _
  | AstMeta _ | AstError _ | AstImport _ _ _ _ => t
  | AstTuple elems => AstTuple (map (rename_free old new) elems)
  | AstBlock stmts tail =>
      AstBlock (map (rename_free old new) stmts) (rename_free old new tail)
  | AstApp f args => AstApp (rename_free old new f) (map (rename_free old new) args)
  | AstImplicitApp f args =>
      AstImplicitApp (rename_free old new f) (map (rename_free old new) args)
  | AstLam n ty body =>
      let ty' := rename_free old new ty in
      if String.eqb n old then AstLam n ty' body
      else AstLam n ty' (rename_free old new body)
  | AstPi n ty ret effs =>
      let ty' := rename_free old new ty in
      if String.eqb n old then AstPi n ty' ret effs
      else AstPi n ty' (rename_free old new ret) effs
  | AstDo e effs => AstDo (rename_free old new e) (map (rename_free old new) effs)
  | AstHandle e eff hs =>
      AstHandle (rename_free old new e) eff
        (map (fun p => (fst p, rename_free old new (snd p))) hs)
  | AstLet n val => AstLet n (rename_free old new val)
  | AstVar n val => AstVar n (rename_free old new val)
  | AstAssign n val => AstAssign n (rename_free old new val)
  | AstBox e caps => AstBox (rename_free old new e) caps
  | AstUnbox e => AstUnbox (rename_free old new e)
  | AstIf c th el =>
      AstIf (rename_free old new c) (rename_free old new th) (rename_free old new el)
  | AstDef n tp params ret body =>
      AstDef n tp
        (map (fun p => (fst p, rename_free old new (snd p))) params)
        (rename_free old new ret) (rename_free old new body)
  | AstFunTy tp params ret effs =>
      AstFunTy tp
        (map (fun p => (fst p, rename_free old new (snd p))) params)
        (rename_free old new ret) effs
  | AstEnum n tp vars => AstEnum n tp vars
  | AstMatch e cases =>
      AstMatch (rename_free old new e)
        (map
           (fun c =>
              let pat := fst c in
              let body := snd c in
              let bound :=
                match pat with
                | PatVar n => String.eqb n old
                | PatConstructor _ vs => string_in_list old vs
                | PatWildcard => false
                end
              in
              if bound then c else (pat, rename_free old new body))
           cases)
  | AstRecord n tp fields => AstRecord n tp fields
  | AstFieldAccess e f => AstFieldAccess (rename_free old new e) f
  | AstExtension n tp tgt meths =>
      AstExtension n tp (rename_free old new tgt) (map (rename_free old new) meths)
  | AstSpan sp inner => AstSpan sp (rename_free old new inner)
  end.

Definition fresh_name (base : string) (avoid : list string) : string :=
  let fix go (cand : string) (fuel : nat) : string :=
    match fuel with
    | 0 => append base "_fresh"
    | S fuel' =>
        if string_in_list cand avoid then go (append cand "'") fuel' else cand
    end
  in
  go (append base "'") 256.

Definition pat_bound_names (p : PatternAST) : list string :=
  match p with
  | PatWildcard => []
  | PatVar n => [n]
  | PatConstructor _ vs => vs
  end.

(* Capture-avoiding substitution: freshen binders that occur free in [v].
   Fuel is derived from [ast_size] (no hardcoded bound). *)
Fixpoint ast_size (t : AST) {struct t} : nat :=
  let fix sizes (xs : list AST) : nat :=
    match xs with
    | [] => 0
    | x :: rest => ast_size x + sizes rest
    end
  in
  let fix sizes_paired (xs : list (string * AST)) : nat :=
    match xs with
    | [] => 0
    | (_, a) :: rest => ast_size a + sizes_paired rest
    end
  in
  let fix sizes_cases (xs : list (PatternAST * AST)) : nat :=
    match xs with
    | [] => 0
    | (_, a) :: rest => ast_size a + sizes_cases rest
    end
  in
  match t with
  | AstRef _ | AstUniverse _ | AstStringLit _ | AstIntLit _ | AstBoolLit _
  | AstMeta _ | AstError _ | AstImport _ _ _ _ | AstEnum _ _ _ | AstRecord _ _ _ =>
      1
  | AstTuple elems => S (sizes elems)
  | AstBlock stmts tail => S (sizes stmts + ast_size tail)
  | AstApp f args => S (ast_size f + sizes args)
  | AstImplicitApp f args => S (ast_size f + sizes args)
  | AstLam _ ty body => S (ast_size ty + ast_size body)
  | AstPi _ ty ret _ => S (ast_size ty + ast_size ret)
  | AstDo e effs => S (ast_size e + sizes effs)
  | AstHandle e _ hs => S (ast_size e + sizes_paired hs)
  | AstLet _ val | AstVar _ val | AstAssign _ val => S (ast_size val)
  | AstBox e _ | AstUnbox e | AstFieldAccess e _ | AstSpan _ e => S (ast_size e)
  | AstIf c th el => S (ast_size c + ast_size th + ast_size el)
  | AstDef _ _ params ret body =>
      S (sizes_paired params + ast_size ret + ast_size body)
  | AstFunTy _ params ret _ => S (sizes_paired params + ast_size ret)
  | AstMatch e cases => S (ast_size e + sizes_cases cases)
  | AstExtension _ _ tgt meths => S (ast_size tgt + sizes meths)
  end.

Fixpoint subst_ast_fuel (fuel : nat) (x : string) (v : AST) (body : AST) {struct fuel} : AST :=
  match fuel with
  | 0 => body
  | S fuel' =>
  match body with
  | AstRef name => if String.eqb name x then v else body
  | AstUniverse l => AstUniverse l
  | AstTuple elems => AstTuple (map (subst_ast_fuel fuel' x v) elems)
  | AstStringLit s => AstStringLit s
  | AstIntLit n => AstIntLit n
  | AstBoolLit b => AstBoolLit b
  | AstBlock stmts tail =>
      AstBlock (map (subst_ast_fuel fuel' x v) stmts) (subst_ast_fuel fuel' x v tail)
  | AstApp f args => AstApp (subst_ast_fuel fuel' x v f) (map (subst_ast_fuel fuel' x v) args)
  | AstImplicitApp f args =>
      AstImplicitApp (subst_ast_fuel fuel' x v f) (map (subst_ast_fuel fuel' x v) args)
  | AstLam argName argTy argBody =>
      let newTy := subst_ast_fuel fuel' x v argTy in
      if String.eqb argName x then AstLam argName newTy argBody
      else if free_in argName v then
        let z := fresh_name argName [x; argName] in
        let body' := rename_free argName z argBody in
        AstLam z newTy (subst_ast_fuel fuel' x v body')
      else AstLam argName newTy (subst_ast_fuel fuel' x v argBody)
  | AstPi argName argTy retTy effs =>
      let newTy := subst_ast_fuel fuel' x v argTy in
      if String.eqb argName x then AstPi argName newTy retTy effs
      else if free_in argName v then
        let z := fresh_name argName [x; argName] in
        let ret' := rename_free argName z retTy in
        AstPi z newTy (subst_ast_fuel fuel' x v ret') effs
      else AstPi argName newTy (subst_ast_fuel fuel' x v retTy) effs
  | AstDo e effs => AstDo (subst_ast_fuel fuel' x v e) (map (subst_ast_fuel fuel' x v) effs)
  | AstHandle e eff hs =>
      AstHandle (subst_ast_fuel fuel' x v e) eff
        (map (fun p => (fst p, subst_ast_fuel fuel' x v (snd p))) hs)
  | AstLet n val => AstLet n (subst_ast_fuel fuel' x v val)
  | AstVar n val => AstVar n (subst_ast_fuel fuel' x v val)
  | AstAssign n val => AstAssign n (subst_ast_fuel fuel' x v val)
  | AstBox e caps => AstBox (subst_ast_fuel fuel' x v e) caps
  | AstUnbox e => AstUnbox (subst_ast_fuel fuel' x v e)
  | AstIf c t e =>
      AstIf (subst_ast_fuel fuel' x v c) (subst_ast_fuel fuel' x v t)
        (subst_ast_fuel fuel' x v e)
  | AstDef n tp params ret b =>
      let params' := map (fun p => (fst p, subst_ast_fuel fuel' x v (snd p))) params in
      let ret' := subst_ast_fuel fuel' x v ret in
      let bound := n :: map fst params in
      if string_in_list x bound then AstDef n tp params' ret' b
      else
        let fix freshen_params (ps : list (string * AST)) (body0 : AST) {struct ps}
          : list (string * AST) * AST :=
          match ps with
          | [] => ([], body0)
          | (pn, pty) :: rest =>
              if free_in pn v then
                let z := fresh_name pn (x :: pn :: map fst rest) in
                let body1 := rename_free pn z body0 in
                let (ps2, body2) := freshen_params rest body1 in
                ((z, pty) :: ps2, body2)
              else
                let (ps2, body2) := freshen_params rest body0 in
                ((pn, pty) :: ps2, body2)
          end
        in
        let (params'', b0) := freshen_params params' b in
        AstDef n tp params'' ret' (subst_ast_fuel fuel' x v b0)
  | AstFunTy tp params ret effs =>
      let params' := map (fun p => (fst p, subst_ast_fuel fuel' x v (snd p))) params in
      let fix go (ps : list (string * AST)) (r : AST) {struct ps}
        : list (string * AST) * AST :=
        match ps with
        | [] => ([], subst_ast_fuel fuel' x v r)
        | (pn, pty) :: rest =>
            if String.eqb pn x then ((pn, pty) :: rest, r)
            else if free_in pn v then
              let z := fresh_name pn [x; pn] in
              let r' := rename_free pn z r in
              let (ps2, r2) := go rest r' in
              ((z, pty) :: ps2, r2)
            else
              let (ps2, r2) := go rest r in
              ((pn, pty) :: ps2, r2)
        end
      in
      let (ps', r') := go params' ret in
      AstFunTy tp ps' r' effs
  | AstEnum n tp vars => AstEnum n tp vars
  | AstMatch expr cases =>
      AstMatch (subst_ast_fuel fuel' x v expr)
        (map
           (fun c =>
              let pat := fst c in
              let bdy := snd c in
              let bounds := pat_bound_names pat in
              if string_in_list x bounds then c
              else
                match pat with
                | PatWildcard => (pat, subst_ast_fuel fuel' x v bdy)
                | PatVar n =>
                    if free_in n v then
                      let z := fresh_name n [x; n] in
                      (PatVar z, subst_ast_fuel fuel' x v (rename_free n z bdy))
                    else (pat, subst_ast_fuel fuel' x v bdy)
                | PatConstructor ctor vs =>
                    let fix freshen_vs (xs : list string) (b0 : AST) {struct xs}
                      : list string * AST :=
                      match xs with
                      | [] => ([], b0)
                      | vn :: rest =>
                          if free_in vn v then
                            let z := fresh_name vn (x :: vn :: rest) in
                            let b1 := rename_free vn z b0 in
                            let (vs2, b2) := freshen_vs rest b1 in
                            (z :: vs2, b2)
                          else
                            let (vs2, b2) := freshen_vs rest b0 in
                            (vn :: vs2, b2)
                      end
                    in
                    let (vs', b1) := freshen_vs vs bdy in
                    (PatConstructor ctor vs', subst_ast_fuel fuel' x v b1)
                end)
           cases)
  | AstRecord n tp fields => AstRecord n tp fields
  | AstFieldAccess expr f => AstFieldAccess (subst_ast_fuel fuel' x v expr) f
  | AstExtension n tp tgt meths =>
      AstExtension n tp (subst_ast_fuel fuel' x v tgt)
        (map (subst_ast_fuel fuel' x v) meths)
  | AstImport lang alias modp syms => AstImport lang alias modp syms
  | AstMeta m => AstMeta m
  | AstSpan sp inner => AstSpan sp (subst_ast_fuel fuel' x v inner)
  | AstError msg => AstError msg
  end
  end.

Definition subst_ast (x : string) (v : AST) (body : AST) : AST :=
  subst_ast_fuel (S (ast_size body)) x v body.

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
  | AstImplicitApp f args => AstImplicitApp (strip_span f) (map strip_span args)
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
  | AstExtension n tp tgt meths => AstExtension n tp (strip_span tgt) meths
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
  | AstExtension _ _ _ _ => TyOk (AstRef "Unit")
  | AstImport _ _ _ _ => TyOk (AstRef "Unit")
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

(* Binder / capture notes for string subst (freshen binders; LN still the long-term plan). *)
Example subst_stops_at_shadowing :
  subst_ast "x" (AstIntLit 1)
    (AstLam "x" (AstRef "Int") (AstRef "x"))
  = AstLam "x" (AstRef "Int") (AstRef "x").
Proof. reflexivity. Qed.

Example subst_under_distinct_binder :
  subst_ast "x" (AstIntLit 1)
    (AstLam "y" (AstRef "Int") (AstRef "x"))
  = AstLam "y" (AstRef "Int") (AstIntLit 1).
Proof. reflexivity. Qed.

(* Classic capture case: freshen binder `y` so free `y` in the substitutend stays free. *)
Example subst_avoids_capture :
  subst_ast "x" (AstRef "y")
    (AstLam "y" (AstRef "Int") (AstRef "x"))
  = AstLam "y'" (AstRef "Int") (AstRef "y").
Proof. reflexivity. Qed.

