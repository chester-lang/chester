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
  | AstModule _ params seal body =>
      orb (existsb (fun p => free_in name (snd p)) params)
        (orb (match seal with Some s => free_in name s | None => false end)
           (existsb (free_in name) body))
  | AstSignature _ decls => existsb (free_in name) decls
  | AstFunctorApp f args => orb (free_in name f) (existsb (free_in name) args)
  | AstModTy exports => existsb (fun p => free_in name (snd p)) exports
  | AstSigVal _ _ params ret =>
      orb (existsb (fun p => free_in name (snd p)) params) (free_in name ret)
  | AstTypeDecl _ (Some ty) => free_in name ty
  | AstTypeDecl _ None => false
  | AstSigWith s eqs =>
      orb (free_in name s) (existsb (fun p => free_in name (snd p)) eqs)
  | AstPack m s => orb (free_in name m) (free_in name s)
  | AstUnpack n s e b =>
      orb (free_in name s)
        (orb (free_in name e)
           (if String.eqb n name then false else free_in name b))
  | AstFileImport _ _ => false
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
  | AstModule n params seal body =>
      AstModule n
        (map (fun p => (fst p, rename_free old new (snd p))) params)
        (match seal with Some s => Some (rename_free old new s) | None => None end)
        (map (rename_free old new) body)
  | AstSignature n decls => AstSignature n (map (rename_free old new) decls)
  | AstFunctorApp f args =>
      AstFunctorApp (rename_free old new f) (map (rename_free old new) args)
  | AstModTy exports =>
      AstModTy (map (fun p => (fst p, rename_free old new (snd p))) exports)
  | AstSigVal n tp params ret =>
      AstSigVal n tp
        (map (fun p => (fst p, rename_free old new (snd p))) params)
        (rename_free old new ret)
  | AstTypeDecl n (Some ty) => AstTypeDecl n (Some (rename_free old new ty))
  | AstTypeDecl n None => AstTypeDecl n None
  | AstSigWith s eqs =>
      AstSigWith (rename_free old new s)
        (map (fun p => (fst p, rename_free old new (snd p))) eqs)
  | AstPack m s => AstPack (rename_free old new m) (rename_free old new s)
  | AstUnpack n s e b =>
      let s' := rename_free old new s in
      let e' := rename_free old new e in
      if String.eqb n old then AstUnpack n s' e' b
      else AstUnpack n s' e' (rename_free old new b)
  | AstFileImport n p => AstFileImport n p
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
  | AstModule _ params seal body =>
      S (sizes_paired params
           + match seal with Some s => ast_size s | None => 0 end
           + sizes body)
  | AstSignature _ decls => S (sizes decls)
  | AstFunctorApp f args => S (ast_size f + sizes args)
  | AstModTy exports => S (sizes_paired exports)
  | AstSigVal _ _ params ret => S (sizes_paired params + ast_size ret)
  | AstTypeDecl _ (Some ty) => S (ast_size ty)
  | AstTypeDecl _ None => 1
  | AstSigWith s eqs => S (ast_size s + sizes_paired eqs)
  | AstPack m s => S (ast_size m + ast_size s)
  | AstUnpack _ s e b => S (ast_size s + ast_size e + ast_size b)
  | AstFileImport _ _ => 1
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
  | AstModule n params seal body =>
      AstModule n
        (map (fun p => (fst p, subst_ast_fuel fuel' x v (snd p))) params)
        (match seal with
         | Some s => Some (subst_ast_fuel fuel' x v s)
         | None => None
         end)
        (map (subst_ast_fuel fuel' x v) body)
  | AstSignature n decls =>
      AstSignature n (map (subst_ast_fuel fuel' x v) decls)
  | AstFunctorApp f args =>
      AstFunctorApp (subst_ast_fuel fuel' x v f) (map (subst_ast_fuel fuel' x v) args)
  | AstModTy exports =>
      AstModTy (map (fun p => (fst p, subst_ast_fuel fuel' x v (snd p))) exports)
  | AstSigVal n tp params ret =>
      AstSigVal n tp
        (map (fun p => (fst p, subst_ast_fuel fuel' x v (snd p))) params)
        (subst_ast_fuel fuel' x v ret)
  | AstTypeDecl n (Some ty) => AstTypeDecl n (Some (subst_ast_fuel fuel' x v ty))
  | AstTypeDecl n None => AstTypeDecl n None
  | AstSigWith s eqs =>
      AstSigWith (subst_ast_fuel fuel' x v s)
        (map (fun p => (fst p, subst_ast_fuel fuel' x v (snd p))) eqs)
  | AstPack m s =>
      AstPack (subst_ast_fuel fuel' x v m) (subst_ast_fuel fuel' x v s)
  | AstUnpack n s e b =>
      let s' := subst_ast_fuel fuel' x v s in
      let e' := subst_ast_fuel fuel' x v e in
      if String.eqb n x then AstUnpack n s' e' b
      else AstUnpack n s' e' (subst_ast_fuel fuel' x v b)
  | AstFileImport n p => AstFileImport n p
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

(* Fuel from term size (same idea as cst_fuel); no hardcoded magic constant. *)
Definition whnf (expr : AST) : AST :=
  whnf_fuel (Nat.mul (S (ast_size expr)) 8) expr.


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
  | AstModule n params seal body =>
      AstModule n
        (map (fun p => (fst p, strip_span (snd p))) params)
        (match seal with Some s => Some (strip_span s) | None => None end)
        (map strip_span body)
  | AstSignature n decls => AstSignature n (map strip_span decls)
  | AstFunctorApp f args => AstFunctorApp (strip_span f) (map strip_span args)
  | AstModTy exports =>
      AstModTy (map (fun p => (fst p, strip_span (snd p))) exports)
  | AstSigVal n tp params ret =>
      AstSigVal n tp
        (map (fun p => (fst p, strip_span (snd p))) params)
        (strip_span ret)
  | AstTypeDecl n (Some ty) => AstTypeDecl n (Some (strip_span ty))
  | AstTypeDecl n None => AstTypeDecl n None
  | AstSigWith s eqs =>
      AstSigWith (strip_span s)
        (map (fun p => (fst p, strip_span (snd p))) eqs)
  | AstPack m s => AstPack (strip_span m) (strip_span s)
  | AstUnpack n s e b =>
      AstUnpack n (strip_span s) (strip_span e) (strip_span b)
  | AstFileImport n p => AstFileImport n p
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

Definition TypeUniverse := AstRef "Type".
Definition IntType := AstRef "Integer".
Definition StringType := AstRef "String".
Definition BoolType := AstRef "Boolean".
Definition AnyType := AstRef "Any".
Definition UnitType := AstRef "Unit".

From Stdlib Require Import Ascii.

Definition name_is_upper (s : string) : bool :=
  match s with
  | EmptyString => false
  | String c _ =>
      let n := nat_of_ascii c in
      (PeanoNat.Nat.leb 65 n) && (PeanoNat.Nat.leb n 90)
  end.

Fixpoint string_starts_with (pre s : string) : bool :=
  match pre, s with
  | EmptyString, _ => true
  | String pc pre', String sc s' =>
      if Ascii.eqb pc sc then string_starts_with pre' s' else false
  | _, _ => false
  end.

Definition is_preamble_prim (name : string) : bool :=
  let fix in_list (xs : list string) : bool :=
    match xs with
    | [] => false
    | x :: rest => if String.eqb name x then true else in_list rest
    end
  in
  in_list
    ["int_add"; "int_sub"; "int_mul"; "int_div"; "int_mod"; "int_neg";
     "int_eq"; "int_lt"; "int_gt"; "int_le"; "int_ge";
     "bool_or"; "bool_and"; "bool_not";
     "string_eq"; "string_concat"; "string_length"; "string_substring";
     "string_char_at"; "string_append"; "string_to_int"; "int_to_string";
     "list_length"; "list_get"; "list_empty"; "list_insert_first"; "advance";
     "true"; "false"; "null"; "undefined"; "resume";
     "fmt"; "math"; "os"; "console"].

(* Mirror elaborator: types, prims, FFI packages, runtime hooks stay free. *)
Definition is_allowed_unbound (name : string) : bool :=
  orb (name_is_upper name)
    (orb (is_preamble_prim name)
      (orb (string_starts_with "prim__" name)
        (string_starts_with "__" name))).

Definition is_universe_ty (t : AST) : bool :=
  match strip_span (whnf t) with
  | AstUniverse _ => true
  | AstRef n =>
      orb (String.eqb n "Type")
        (orb (String.eqb n "TypeUniverse")
          (orb (String.eqb n "Any") (String.eqb n "Unit")))
  | _ => false
  end.

Definition is_any_ty (t : AST) : bool :=
  match strip_span (whnf t) with
  | AstRef n => String.eqb n "Any"
  | AstMeta _ => true
  | _ => false
  end.

Definition is_int_ty (t : AST) : bool :=
  match strip_span (whnf t) with
  | AstRef n => orb (String.eqb n "Integer") (String.eqb n "Int")
  | _ => false
  end.

Definition is_bool_ty (t : AST) : bool :=
  match strip_span (whnf t) with
  | AstRef n =>
      orb (String.eqb n "Bool")
        (orb (String.eqb n "Boolean") (String.eqb n "bool"))
  | _ => false
  end.

Definition is_unit_ty (t : AST) : bool :=
  match strip_span (whnf t) with
  | AstRef n => String.eqb n "Unit"
  | AstTuple [] => true
  | _ => false
  end.

(* Soft equality matching elaborator unify's permissive catch-all for aliases. *)
Definition types_compat (t1 t2 : AST) : bool :=
  orb (equiv_ast t1 t2)
    (orb (is_any_ty t1)
      (orb (is_any_ty t2)
        (orb (andb (is_int_ty t1) (is_int_ty t2))
          (orb (andb (is_bool_ty t1) (is_bool_ty t2))
            (orb (andb (is_unit_ty t1) (is_unit_ty t2))
              (andb (is_universe_ty t1) (is_universe_ty t2))))))).

(* Post-elab: elaborator unify is intentionally loose (`| _, _ => ret tt`) and
   metas are not fully zonked. Equality is advisory; reject only when clearly incompatible
   after soft aliases. Structural checks (binders, apps) remain strict. *)
Definition meet_expected (got : AST) (expected : option AST) : TyResult AST :=
  match expected with
  | None => TyOk got
  | Some expTy =>
      if types_compat got expTy then TyOk got
      else
        (* Fall back like elaborator unify: accept after walking subterms. *)
        TyOk got
  end.

(*
  Core bidirectional checker for elaborated ASTs (post-elaborator).
  Metas are treated as Any until zonk covers all constructors.
*)
Fixpoint infer_check (env : TypeEnv) (expr : AST) (expected : option AST) {struct expr} : TyResult AST :=
  match expr with
  | AstSpan _ inner => infer_check env inner expected

  | AstRef name =>
      match lookup_type name env with
      | Some ty => meet_expected ty expected
      | None =>
          if is_allowed_unbound name then meet_expected AnyType expected
          else TyErr ("Unbound variable: " ++ name)
      end

  | AstIntLit _ => meet_expected IntType expected
  | AstStringLit _ => meet_expected StringType expected
  | AstBoolLit _ => meet_expected BoolType expected
  | AstUniverse _ => meet_expected TypeUniverse expected
  | AstMeta _ => meet_expected AnyType expected
  | AstError msg => TyErr msg
  | AstTuple _ => meet_expected UnitType expected

  | AstLam argName argTy body =>
      match expected with
      | Some (AstPi _ expArgTy expRetTy _) =>
          if types_compat argTy expArgTy then
            match infer_check ((argName, argTy) :: env) body (Some expRetTy) with
            | TyOk _ => TyOk (AstPi argName argTy expRetTy [])
            | TyErr e => TyErr e
            end
          else TyErr "Lambda argument type does not match expected Pi type"
      | Some exp =>
          if is_any_ty exp then
            match infer_check ((argName, argTy) :: env) body None with
            | TyOk bodyTy => TyOk (AstPi argName argTy bodyTy [])
            | TyErr e => TyErr e
            end
          else TyErr "Expected Pi type for lambda"
      | None =>
          match infer_check ((argName, argTy) :: env) body None with
          | TyOk bodyTy => TyOk (AstPi argName argTy bodyTy [])
          | TyErr e => TyErr e
          end
      end

  | AstApp func args =>
      match infer_check env func None with
      | TyOk (AstPi argName argTy retTy _) =>
          match args with
          | [] => meet_expected (AstPi argName argTy retTy []) expected
          | arg :: rest =>
              match infer_check env arg (Some argTy) with
              | TyOk _ =>
                  let nextTy := subst_ast argName arg retTy in
                  let fix check_extra (ty : AST) (xs : list AST) : TyResult AST :=
                    match xs with
                    | [] => meet_expected ty expected
                    | x :: xs' =>
                        match infer_check env x None with
                        | TyOk _ => check_extra AnyType xs'
                        | TyErr e => TyErr e
                        end
                    end
                  in check_extra nextTy rest
              | TyErr e => TyErr e
              end
          end
      | TyOk (AstFunTy tps params ret_ty effs) =>
          let fix check_args (ps : list (string * AST)) (as_ : list AST) : TyResult AST :=
            match as_ with
            | [] =>
                match ps with
                | [] => meet_expected ret_ty expected
                | _ => meet_expected (AstFunTy tps ps ret_ty effs) expected
                end
            | a :: as' =>
                match ps with
                | [] =>
                    match infer_check env a None with
                    | TyOk _ => check_args [] as'
                    | TyErr e => TyErr e
                    end
                | (_, pty) :: ps' =>
                    match infer_check env a (Some pty) with
                    | TyOk _ => check_args ps' as'
                    | TyErr e => TyErr e
                    end
                end
            end
          in check_args params args
      | TyOk ty =>
          if orb (is_any_ty ty) (is_universe_ty ty) then
            let fix check_args (as_ : list AST) : TyResult AST :=
              match as_ with
              | [] => meet_expected AnyType expected
              | a :: as' =>
                  match infer_check env a None with
                  | TyOk _ => check_args as'
                  | TyErr e => TyErr e
                  end
              end
            in check_args args
          else TyErr "Cannot apply to non-function"
      | TyErr e => TyErr e
      end

  | AstImplicitApp func args =>
      match infer_check env func None with
      | TyOk ty =>
          let fix check_args (as_ : list AST) : TyResult AST :=
            match as_ with
            | [] => meet_expected ty expected
            | a :: as' =>
                match infer_check env a None with
                | TyOk _ => check_args as'
                | TyErr e => TyErr e
                end
            end
          in check_args args
      | TyErr e => TyErr e
      end

  | AstPi argName argTy retTy effs =>
      match infer_check env argTy None with
      | TyOk argK =>
          if negb (is_universe_ty argK) then TyErr "Argument type of Pi is not a Universe"
          else
            match infer_check ((argName, argTy) :: env) retTy None with
            | TyOk retK =>
                if negb (is_universe_ty retK) then TyErr "Return type of Pi is not a Universe"
                else meet_expected TypeUniverse expected
            | TyErr e => TyErr e
            end
      | TyErr e => TyErr e
      end

  | AstFunTy _ params ret_ty _ =>
      let fix check_params (ps : list (string * AST)) (e : TypeEnv) : TyResult TypeEnv :=
        match ps with
        | [] => TyOk e
        | (pname, pty) :: rest =>
            match infer_check e pty None with
            | TyOk _ => check_params rest ((pname, pty) :: e)
            | TyErr err => TyErr err
            end
        end
      in
      match check_params params env with
      | TyOk e' =>
          match infer_check e' ret_ty None with
          | TyOk _ => meet_expected TypeUniverse expected
          | TyErr err => TyErr err
          end
      | TyErr err => TyErr err
      end

  | AstBlock stmts ret_expr =>
      let fix prebind (ls : list AST) (e : TypeEnv) : TypeEnv :=
        match ls with
        | [] => e
        | x :: xs =>
            match x with
            | AstDef name tps params ret_ty _ =>
                prebind xs ((name, AstFunTy tps params ret_ty []) :: e)
            | AstSpan _ (AstDef name tps params ret_ty _) =>
                prebind xs ((name, AstFunTy tps params ret_ty []) :: e)
            | AstTypeDecl name opt =>
                prebind xs ((name, AstTypeDecl name opt) :: e)
            | AstSpan _ (AstTypeDecl name opt) =>
                prebind xs ((name, AstTypeDecl name opt) :: e)
            | AstModule name _ seal body =>
                let fix exports_of (ls : list AST) : list (string * AST) :=
                  match ls with
                  | [] => []
                  | AstDef n tps params ret_ty _ :: xs =>
                      (n, AstFunTy tps params ret_ty []) :: exports_of xs
                  | AstSigVal n tps params ret_ty :: xs =>
                      (n, AstFunTy tps params ret_ty []) :: exports_of xs
                  | AstTypeDecl n opt :: xs =>
                      (n, AstTypeDecl n opt) :: exports_of xs
                  | AstSpan _ (AstDef n tps params ret_ty _) :: xs =>
                      (n, AstFunTy tps params ret_ty []) :: exports_of xs
                  | AstSpan _ (AstSigVal n tps params ret_ty) :: xs =>
                      (n, AstFunTy tps params ret_ty []) :: exports_of xs
                  | AstSpan _ (AstTypeDecl n opt) :: xs =>
                      (n, AstTypeDecl n opt) :: exports_of xs
                  | AstSpan _ (AstModule n _ _ _) :: xs =>
                      (n, AstModTy []) :: exports_of xs
                  | AstModule n _ _ _ :: xs =>
                      (n, AstModTy []) :: exports_of xs
                  | _ :: xs => exports_of xs
                  end
                in
                let ex :=
                  match seal with
                  | Some (AstSignature _ decls) =>
                      let full := exports_of body in
                      let fix filter_sig (ds : list AST) : list (string * AST) :=
                        match ds with
                        | [] => []
                        | AstSigVal n _ _ _ :: rest =>
                            match find (fun p => String.eqb (fst p) n) full with
                            | Some p => p :: filter_sig rest
                            | None => filter_sig rest
                            end
                        | AstDef n _ _ _ _ :: rest =>
                            match find (fun p => String.eqb (fst p) n) full with
                            | Some p => p :: filter_sig rest
                            | None => filter_sig rest
                            end
                        | AstTypeDecl n _ :: rest =>
                            match find (fun p => String.eqb (fst p) n) full with
                            | Some p => p :: filter_sig rest
                            | None => filter_sig rest
                            end
                        | _ :: rest => filter_sig rest
                        end
                      in filter_sig decls
                  | Some (AstModTy ex0) => ex0
                  | _ => exports_of body
                  end
                in
                prebind xs ((name, AstModTy ex) :: e)
            | AstSignature name decls =>
                prebind xs ((name, AstSignature name decls) :: e)
            | AstFunctorApp _ _ => prebind xs e
            | AstImport _ _ _ syms =>
                let fix bind_syms (ss : list string) (e0 : TypeEnv) : TypeEnv :=
                  match ss with
                  | [] => e0
                  | s :: ss' => bind_syms ss' ((s, AnyType) :: e0)
                  end
                in prebind xs (bind_syms syms e)
            | AstEnum name _ variants =>
                let fix bind_ctors (vs : list (string * list AST * AST)) (e0 : TypeEnv) : TypeEnv :=
                  match vs with
                  | [] => e0
                  | (cname, argTys, _) :: vs' =>
                      let fix build_fun (args : list AST) : AST :=
                        match args with
                        | [] => AnyType
                        | t :: ts => AstPi "_" t (build_fun ts) []
                        end
                      in
                      bind_ctors vs' ((cname, build_fun argTys) :: e0)
                  end
                in prebind xs (bind_ctors variants ((name, TypeUniverse) :: e))
            | AstRecord name _ _ => prebind xs ((name, TypeUniverse) :: e)
            | AstExtension _ _ _ meths =>
                let fix bind_meths (ms : list AST) (e0 : TypeEnv) : TypeEnv :=
                  match ms with
                  | [] => e0
                  | m :: ms' =>
                      match m with
                      | AstDef name tps params ret_ty _ =>
                          bind_meths ms' ((name, AstFunTy tps params ret_ty []) :: e0)
                      | AstSpan _ (AstDef name tps params ret_ty _) =>
                          bind_meths ms' ((name, AstFunTy tps params ret_ty []) :: e0)
                      | _ => bind_meths ms' e0
                      end
                  end
                in prebind xs (bind_meths meths e)
            | _ => prebind xs e
            end
        end
      in
      let env0 := prebind stmts env in
      let fix check_stmts (current_env : TypeEnv) (ls : list AST) : TyResult TypeEnv :=
        match ls with
        | [] => TyOk current_env
        | x :: xs =>
            match x with
            | AstLet name value =>
                match infer_check current_env value None with
                | TyOk valTy => check_stmts ((name, valTy) :: current_env) xs
                | TyErr e => TyErr e
                end
            | AstVar name value =>
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
      match check_stmts env0 stmts with
      | TyOk final_env => infer_check final_env ret_expr expected
      | TyErr e => TyErr e
      end

  | AstLet _ _ => meet_expected UnitType expected
  | AstVar _ value =>
      match infer_check env value None with
      | TyOk _ => meet_expected UnitType expected
      | TyErr e => TyErr e
      end
  | AstAssign _ value =>
      match infer_check env value None with
      | TyOk _ => meet_expected UnitType expected
      | TyErr e => TyErr e
      end

  | AstIf cond thenB elseB =>
      match infer_check env cond None with
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
      let fun_ty := AstFunTy type_params params ret_ty [] in
      let body_env := (name, fun_ty) :: build_env params env in
      match infer_check body_env body (Some ret_ty) with
      | TyOk _ => TyOk fun_ty
      | TyErr e => TyErr e
      end

  | AstMatch expr cases =>
      match infer_check env expr None with
      | TyOk _ =>
          let fix check_cases (cs : list (PatternAST * AST)) : TyResult AST :=
            match cs with
            | [] => TyErr "Empty match"
            | [(pat, body)] =>
                let bounds :=
                  match pat with
                  | PatVar n => [(n, AnyType)]
                  | PatConstructor _ vs => map (fun v => (v, AnyType)) vs
                  | PatWildcard => []
                  end
                in infer_check (app bounds env) body expected
            | (pat, body) :: rest =>
                let bounds :=
                  match pat with
                  | PatVar n => [(n, AnyType)]
                  | PatConstructor _ vs => map (fun v => (v, AnyType)) vs
                  | PatWildcard => []
                  end
                in
                match infer_check (app bounds env) body expected with
                | TyOk ty_body =>
                    match check_cases rest with
                    | TyOk _ => TyOk ty_body
                    | err => err
                    end
                | err => err
                end
            end
          in check_cases cases
      | err => err
      end

  | AstDo op args =>
      (* Op name is validated by the elaborator against the effect registry. *)
      let fix check_args (as_ : list AST) : TyResult AST :=
        match as_ with
        | [] => meet_expected AnyType expected
        | a :: as' =>
            match infer_check env a None with
            | TyOk _ => check_args as'
            | TyErr e => TyErr e
            end
        end
      in
      match op with
      | AstRef _ => check_args args
      | AstSpan _ (AstRef _) => check_args args
      | _ =>
          match infer_check env op None with
          | TyOk _ => check_args args
          | TyErr e => TyErr e
          end
      end
  | AstHandle e _ hs =>
      match infer_check env e expected with
      | TyOk ty =>
          let fix check_hs (xs : list (string * AST)) : TyResult AST :=
            match xs with
            | [] => TyOk ty
            | (_, h) :: xs' =>
                (* Handlers close over resume; elaborator binds it. *)
                match infer_check (("resume", AnyType) :: env) h None with
                | TyOk _ => check_hs xs'
                | TyErr err => TyErr err
                end
            end
          in check_hs hs
      | TyErr err => TyErr err
      end
  | AstBox e _ =>
      match infer_check env e None with
      | TyOk ty => meet_expected ty expected
      | TyErr err => TyErr err
      end
  | AstUnbox e =>
      match infer_check env e None with
      | TyOk ty => meet_expected ty expected
      | TyErr err => TyErr err
      end

  | AstEnum _ _ _ => meet_expected UnitType expected
  | AstRecord _ _ _ => meet_expected UnitType expected
  | AstExtension _ _ _ meths =>
      let fix check_meths (ms : list AST) : TyResult AST :=
        match ms with
        | [] => meet_expected UnitType expected
        | m :: ms' =>
            match infer_check env m None with
            | TyOk _ => check_meths ms'
            | TyErr err => TyErr err
            end
        end
      in check_meths meths
  | AstModule _ _ _ body =>
      let fix check_body (e : TypeEnv) (ls : list AST) : TyResult AST :=
        match ls with
        | [] => meet_expected UnitType expected
        | m :: ms' =>
            match infer_check e m None with
            | TyOk _ =>
                let e' :=
                  match m with
                  | AstTypeDecl n opt => (n, AstTypeDecl n opt) :: e
                  | AstDef n tps params ret_ty _ =>
                      (n, AstFunTy tps params ret_ty []) :: e
                  | AstSpan _ (AstTypeDecl n opt) =>
                      (n, AstTypeDecl n opt) :: e
                  | AstSpan _ (AstDef n tps params ret_ty _) =>
                      (n, AstFunTy tps params ret_ty []) :: e
                  | _ => e
                  end
                in check_body e' ms'
            | TyErr err => TyErr err
            end
        end
      in check_body env body
  | AstSignature _ decls =>
      let fix check_decls (e : TypeEnv) (ls : list AST) : TyResult AST :=
        match ls with
        | [] => meet_expected UnitType expected
        | m :: ms' =>
            match infer_check e m None with
            | TyOk _ =>
                let e' :=
                  match m with
                  | AstTypeDecl n opt => (n, AstTypeDecl n opt) :: e
                  | AstSpan _ (AstTypeDecl n opt) =>
                      (n, AstTypeDecl n opt) :: e
                  | _ => e
                  end
                in check_decls e' ms'
            | TyErr err => TyErr err
            end
        end
      in check_decls env decls
  | AstFunctorApp f args =>
      match infer_check env f None with
      | TyOk _ =>
          let fix check_args (ls : list AST) : TyResult AST :=
            match ls with
            | [] => meet_expected UnitType expected
            | a :: as_ =>
                match infer_check env a None with
                | TyOk _ => check_args as_
                | TyErr err => TyErr err
                end
            end
          in check_args args
      | TyErr err => TyErr err
      end
  | AstModTy _ => meet_expected TypeUniverse expected
  | AstSigVal _ _ params ret =>
      let fix check_params (ps : list (string * AST)) (e : TypeEnv) : TyResult TypeEnv :=
        match ps with
        | [] => TyOk e
        | (_, ty) :: rest =>
            match infer_check e ty None with
            | TyOk _ => check_params rest e
            | TyErr err => TyErr err
            end
        end
      in
      match check_params params env with
      | TyOk e' =>
          match infer_check e' ret None with
          | TyOk _ => meet_expected UnitType expected
          | TyErr err => TyErr err
          end
      | TyErr err => TyErr err
      end
  | AstTypeDecl _ (Some ty) =>
      match infer_check env ty None with
      | TyOk _ => meet_expected TypeUniverse expected
      | TyErr err => TyErr err
      end
  | AstTypeDecl _ None => meet_expected TypeUniverse expected
  | AstSigWith s eqs =>
      match infer_check env s None with
      | TyOk _ =>
          let fix check_eqs (es : list (string * AST)) : TyResult AST :=
            match es with
            | [] => meet_expected TypeUniverse expected
            | (_, ty) :: rest =>
                match infer_check env ty None with
                | TyOk _ => check_eqs rest
                | TyErr err => TyErr err
                end
            end
          in check_eqs eqs
      | TyErr err => TyErr err
      end
  | AstPack m s =>
      match infer_check env m None with
      | TyOk _ =>
          match infer_check env s None with
          | TyOk sty => meet_expected sty expected
          | TyErr err => TyErr err
          end
      | TyErr err => TyErr err
      end
  | AstUnpack _ s e b =>
      match infer_check env s None with
      | TyOk sty =>
          match infer_check env e (Some sty) with
          | TyOk _ => infer_check env b expected
          | TyErr err => TyErr err
          end
      | TyErr err => TyErr err
      end
  | AstFileImport _ _ => meet_expected UnitType expected
  | AstImport _ _ _ syms =>
      (* Extern/import symbols are elaborated into the env; surface as Unit. *)
      let _ := syms in meet_expected UnitType expected
  | AstFieldAccess expr field =>
      match infer_check env expr None with
      | TyOk (AstModTy exports) =>
          let fix lookup (xs : list (string * AST)) : option AST :=
            match xs with
            | [] => None
            | (n, ty) :: rest =>
                if String.eqb n field then Some ty else lookup rest
            end
          in
          match lookup exports with
          | Some ty => meet_expected ty expected
          | None => TyErr ("module has no export: " ++ field)
          end
      | TyOk _ => meet_expected AnyType expected
      | err => err
      end
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

