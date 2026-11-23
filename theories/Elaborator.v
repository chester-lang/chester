From Stdlib Require Import Ascii.
From Stdlib Require Import Strings.String.
From Stdlib Require Import List.
From Stdlib Require Import PeanoNat.
Import ListNotations.
Open Scope string_scope.
Require Import Chester.CST.
Require Import Chester.CoreChecker.

Require Import String.

Fixpoint string_of_nat (n : nat) : string :=
    match n with
    | 0 => "0" | 1 => "1" | 2 => "2" | 3 => "3" | 4 => "4" | 5 => "5"
    | 6 => "6" | 7 => "7" | 8 => "8" | 9 => "9" | _ => "X"
    end.

Definition digit_value (c : ascii) : option nat :=
  match c with
  | "0"%char => Some 0 | "1"%char => Some 1 | "2"%char => Some 2
  | "3"%char => Some 3 | "4"%char => Some 4 | "5"%char => Some 5
  | "6"%char => Some 6 | "7"%char => Some 7 | "8"%char => Some 8
  | "9"%char => Some 9 | _ => None
  end.

Fixpoint string_to_nat_aux (s : string) (acc : nat) : nat :=
  match s with
  | EmptyString => acc
  | String c rest =>
      match digit_value c with
      | Some d => string_to_nat_aux rest (acc * 10 + d)
      | None => acc
      end
  end.

Definition string_to_nat (s : string) : nat := string_to_nat_aux s 0.

(* Unbound lowercase locals error; types, prims, FFI packages, and runtime hooks stay free.
   Policy lives in CoreChecker so post-elab core checking shares it. *)
Definition is_allowed_unbound (name : string) : bool :=
  Chester.CoreChecker.is_allowed_unbound name.

Definition mangle_name (n : string) (ctx : list nat) : string :=
  let fix join (ls : list nat) : string :=
    match ls with
    | nil => EmptyString
    | cons x xs => append (append "_" (string_of_nat x)) (join xs)
    end
  in
  match ctx with
  | nil => n
  | cons _ _ => append n (join ctx)
  end.

Require Import Chester.AST.
Require Import Chester.Formatter.
Open Scope string_scope.
Import ListNotations.

Inductive ElabState :=
| mkElabState : nat -> SolverState
    -> list (string * AST * list (string * string))
    -> list (string * list (string * list AST * AST))
    -> list string
    -> EffectSet
    -> list (string * list (string * AST))
    -> ElabState.

Definition ElabM (A : Type) := ElabState -> (A * ElabState) + (list ascii * ElabState).

Definition ret {A : Type} (a : A) : ElabM A :=
  fun s => inl (a, s).

Definition bind {A B : Type} (m : ElabM A) (f : A -> ElabM B) : ElabM B :=
  fun s => match m s with
           | inl (a, s') => f a s'
           | inr e => inr e
           end.

Notation "x <- m ; f" := (bind m (fun x => f)) (at level 60, right associativity).
Notation "m ;; f" := (bind m (fun _ => f)) (at level 60, right associativity).

Definition throw {A : Type} (msg : string) : ElabM A :=
  fun s =>
    let fix string_to_list (s : string) : list ascii :=
      match s with
      | EmptyString => []
      | String c s' => c :: string_to_list s'
      end
    in
    inr (string_to_list msg, s).

Definition get_state : ElabM ElabState :=
  fun s => inl (s, s).

Definition set_state (s : ElabState) : ElabM unit :=
  fun _ => inl (tt, s).

Definition fresh_meta : ElabM AST :=
  s <- get_state ;
  match s with
  | mkElabState n sol exts effs caps pending go_pkgs =>
      set_state (mkElabState (S n) sol exts effs caps pending go_pkgs) ;;
      ret (AstMeta n)
  end.

(** Prenex-instantiate [AstFunTy] type parameters with fresh metas (HM-style).
    Call sites must freshen before unifying args so surface type-param letters
    stay rigid inside polymorphic bodies (see LITERATURE §4). *)
Fixpoint freshen_fun_tps (tps : list string) (params : list (string * AST))
    (rty : AST) {struct tps} : ElabM (list (string * AST) * AST) :=
  match tps with
  | [] => ret (params, rty)
  | tp :: tps' =>
      m <- fresh_meta ;
      let params' :=
        map (fun p => (fst p, subst_ast tp m (snd p))) params
      in
      freshen_fun_tps tps' params' (subst_ast tp m rty)
  end.

Definition instantiate_funty (ty : AST) : ElabM AST :=
  match strip_span ty with
  | AstFunTy tps params ret_ty effs =>
      fr <- freshen_fun_tps tps params ret_ty ;
      match fst fr with
      | [] => ret (snd fr)
      | _ => ret (AstFunTy [] (fst fr) (snd fr) effs)
      end
  | other => ret other
  end.

Definition TypeEnv := list ((string * list nat) * AST).
Definition init_elab_state := mkElabState 0 empty_state [] [] [] [] [].

Definition go_variadic_fun_ty : AST :=
  AstFunTy [] [("a", AstRef "Any")] (AstTuple [AstRef "Any"; AstRef "Any"]) [].

Fixpoint go_funcs_to_meths (funcs : list string) : list (string * AST) :=
  match funcs with
  | [] => []
  | f :: rest => (f, go_variadic_fun_ty) :: go_funcs_to_meths rest
  end.

Fixpoint build_go_pkgs (entries : list (string * list string)) : list (string * list (string * AST)) :=
  match entries with
  | [] => []
  | (pkg, funcs) :: rest => (pkg, go_funcs_to_meths funcs) :: build_go_pkgs rest
  end.

Fixpoint lookup_in_go_meths (meth : string) (meths : list (string * AST)) : option AST :=
  match meths with
  | [] => None
  | (n, ty) :: rest =>
      if string_dec n meth then Some ty else lookup_in_go_meths meth rest
  end.

Fixpoint lookup_go_method (pkg meth : string) (pkgs : list (string * list (string * AST))) : option AST :=
  match pkgs with
  | [] => None
  | (p, meths) :: rest =>
      if string_dec p pkg then
        match lookup_in_go_meths meth meths with
        | Some ty => Some ty
        | None => lookup_go_method pkg meth rest
        end
      else lookup_go_method pkg meth rest
  end.

Definition init_elab_with_go (entries : list (string * list string)) : ElabState :=
  mkElabState 0 empty_state [] [] [] [] (build_go_pkgs entries).

Definition effect_eqb (e1 e2 : EffectRef) : bool :=
  match e1, e2 with
  | UserEffect a, UserEffect b => if string_dec a b then true else false
  | BuiltinEffect a, BuiltinEffect b => if string_dec a b then true else false
  | EffectRowVar a, EffectRowVar b => if string_dec a b then true else false
  | _, _ => false
  end.

Fixpoint effect_in (e : EffectRef) (es : EffectSet) : bool :=
  match es with
  | [] => false
  | x :: xs => if effect_eqb e x then true else effect_in e xs
  end.

Definition add_effect (e : EffectRef) (es : EffectSet) : EffectSet :=
  if effect_in e es then es else e :: es.

Fixpoint remove_effect (e : EffectRef) (es : EffectSet) : EffectSet :=
  match es with
  | [] => []
  | x :: xs => if effect_eqb e x then remove_effect e xs else x :: remove_effect e xs
  end.

Fixpoint merge_effects (a b : EffectSet) : EffectSet :=
  match a with
  | [] => b
  | x :: xs => merge_effects xs (add_effect x b)
  end.

(** Map a surface effect name to an EffectRef (`io` is builtin). *)
Definition effect_ref_of_name (n : string) : EffectRef :=
  if string_dec n "io" then BuiltinEffect n else UserEffect n.

Fixpoint effect_set_of_names (ns : list string) : EffectSet :=
  match ns with
  | [] => []
  | n :: rest => effect_ref_of_name n :: effect_set_of_names rest
  end.

(** Peel expander encoding `AppCST #effect_row [ret; ListLiteral names]`. *)
Definition peel_effect_row_cst (ret_ty : CST) : (CST * EffectSet) :=
  match ret_ty with
  | AppCST (Symbol "#effect_row" _) [inner; ListLiteral names _] _ =>
      let fix names_of (es : list CST) : list string :=
        match es with
        | [] => []
        | Symbol n _ :: rest => n :: names_of rest
        | _ :: rest => names_of rest
        end
      in (inner, effect_set_of_names (names_of names))
  | other => (other, [])
  end.

Fixpoint string_in_list (s : string) (xs : list string) : bool :=
  match xs with
  | [] => false
  | x :: rest => if string_dec s x then true else string_in_list s rest
  end.

(* True when every concrete effect in `needed` is installed on the capability stack. *)
Fixpoint effects_covered_by (needed : EffectSet) (active : list string) : bool :=
  match needed with
  | [] => true
  | EffectRowVar _ :: rest => effects_covered_by rest active
  | UserEffect n :: rest =>
      if string_in_list n active then effects_covered_by rest active else false
  | BuiltinEffect n :: rest =>
      if string_in_list n active then effects_covered_by rest active else false
  end.

Definition get_active_caps : ElabM (list string) :=
  s <- get_state ;
  match s with mkElabState _ _ _ _ caps _ _ => ret caps end.

Definition fresh_row_var : ElabM EffectRef :=
  s <- get_state ;
  match s with
  | mkElabState n sol exts effs caps pending go_pkgs =>
      set_state (mkElabState (S n) sol exts effs caps pending go_pkgs) ;;
      ret (EffectRowVar ("μ" ++ string_of_nat n))
  end.

Definition with_open_row (effs : EffectSet) : ElabM EffectSet :=
  rv <- fresh_row_var ;
  ret (app effs [rv]).

Definition get_pending : ElabM EffectSet :=
  s <- get_state ;
  match s with mkElabState _ _ _ _ _ pending _ => ret pending end.

Definition set_pending (pending : EffectSet) : ElabM unit :=
  s <- get_state ;
  match s with
  | mkElabState n sol exts effs caps _ go_pkgs =>
      set_state (mkElabState n sol exts effs caps pending go_pkgs)
  end.

Definition push_capability (eff : string) : ElabM unit :=
  s <- get_state ;
  match s with
  | mkElabState n sol exts effs caps pending go_pkgs =>
      set_state (mkElabState n sol exts effs (eff :: caps) pending go_pkgs)
  end.

Definition pop_capability : ElabM unit :=
  s <- get_state ;
  match s with
  | mkElabState n sol exts effs (_ :: caps) pending go_pkgs =>
      set_state (mkElabState n sol exts effs caps pending go_pkgs)
  | mkElabState n sol exts effs [] pending go_pkgs =>
      set_state (mkElabState n sol exts effs [] pending go_pkgs)
  end.

Definition register_effect (name : string) (ops : list (string * list AST * AST)) : ElabM unit :=
  s <- get_state ;
  match s with
  | mkElabState n sol exts effs caps pending go_pkgs =>
      set_state (mkElabState n sol exts ((name, ops) :: effs) caps pending go_pkgs)
  end.

Fixpoint lookup_effect_ops (name : string) (reg : list (string * list (string * list AST * AST)))
  : option (list (string * list AST * AST)) :=
  match reg with
  | [] => None
  | (n, ops) :: rest =>
      if string_dec n name then Some ops else lookup_effect_ops name rest
  end.

Fixpoint lookup_op (op : string) (ops : list (string * list AST * AST))
  : option (list AST * AST) :=
  match ops with
  | [] => None
  | (n, args, ret) :: rest =>
      if string_dec n op then Some (args, ret) else lookup_op op rest
  end.

(* Find op among active capabilities (innermost first). *)
Fixpoint find_active_op (op : string) (active : list string)
    (reg : list (string * list (string * list AST * AST)))
  : option (string * list AST * AST) :=
  match active with
  | [] => None
  | eff :: rest =>
      match lookup_effect_ops eff reg with
      | Some ops =>
          match lookup_op op ops with
          | Some (args, ret) => Some (eff, args, ret)
          | None => find_active_op op rest reg
          end
      | None => find_active_op op rest reg
      end
  end.

(* Find op in the full registry (for unknown-op errors). *)
Fixpoint find_registered_op (op : string)
    (reg : list (string * list (string * list AST * AST)))
  : option (string * list AST * AST) :=
  match reg with
  | [] => None
  | (eff, ops) :: rest =>
      match lookup_op op ops with
      | Some (args, ret) => Some (eff, args, ret)
      | None => find_registered_op op rest
      end
  end.

Definition extract_arg_cst (a : CST) : (string * CST) :=
  match a with
  | Symbol n _ => (n, Symbol "Unknown" empty_span)
  | SeqOf (Symbol n _ :: Symbol kwd2 _ :: ty :: _) _ =>
      if if string_dec kwd2 ":" then true else false then (n, ty)
      else ("unknown", Symbol "Unknown" empty_span)
  | _ => ("unknown", Symbol "Unknown" empty_span)
  end.

(* Effect method signature: DefCST or SeqOf from expander when no '='. *)
Definition parse_effect_sig (d : CST) : option (string * list (string * CST) * CST) :=
  match d with
  | DefCST name _ params ret_ty _ _ => Some (name, params, ret_ty)
  | SeqOf (Symbol "def" _ :: Symbol name _ :: ListLiteral _ _ :: Tuple args _ :: rest) _ =>
      let params := map extract_arg_cst args in
      let ret_ty := match rest with
                    | Symbol ":" _ :: tys =>
                        match tys with
                        | [t] => t
                        | _ => Symbol "Unknown" empty_span
                        end
                    | _ => Symbol "Unknown" empty_span
                    end in
      Some (name, params, ret_ty)
  | SeqOf (Symbol "def" _ :: AppCST (Symbol name _) args _ :: rest) _ =>
      let params := map extract_arg_cst args in
      let ret_ty := match rest with
                    | Symbol ":" _ :: tys =>
                        match tys with
                        | [t] => t
                        | _ => Symbol "Unknown" empty_span
                        end
                    | _ => Symbol "Unknown" empty_span
                    end in
      Some (name, params, ret_ty)
  | _ => None
  end.

Definition parse_handler_case (h : CST)
  : option (string * list string * CST) :=
  match h with
  | SeqOf (Symbol "case" _ :: AppCST (Symbol op _) args _ :: Symbol "=>" _ :: body) _ =>
      let binders := map (fun a => match a with Symbol n _ => n | _ => "_" end) args in
      let body_cst := match body with [b] => b | _ => SeqOf body empty_span end in
      Some (op, binders, body_cst)
  | SeqOf (Symbol "case" _ :: Symbol op _ :: Symbol "=>" _ :: body) _ =>
      let body_cst := match body with [b] => b | _ => SeqOf body empty_span end in
      Some (op, [], body_cst)
  | _ => None
  end.

Fixpoint build_handler_lam (binders : list string) (tys : list AST)
    (resume_ty : AST) (body : AST) : AST :=
  match binders, tys with
  | [], _ => AstLam "resume" resume_ty body
  | b :: rest, ty :: tys' =>
      AstLam b ty (build_handler_lam rest tys' resume_ty body)
  | b :: rest, [] =>
      AstLam b (AstRef "Any") (build_handler_lam rest [] resume_ty body)
  end.

Fixpoint list_nat_eq (l1 l2 : list nat) : bool :=
  match l1, l2 with
  | [], [] => true
  | x::xs, y::ys => if Nat.eqb x y then list_nat_eq xs ys else false
  | _, _ => false
  end.


Fixpoint lookup_type (name : string) (ctx : list nat) (env : TypeEnv) : option AST :=
  match env with
  | [] => None
  | (k, k_ctx, v) :: rest =>
      if andb (if string_dec name k then true else false) (list_nat_eq ctx k_ctx)
      then Some v
      else lookup_type name ctx rest
  end.

Fixpoint resolve_hygiene (env : TypeEnv) (name : string) (ctx : list nat) : option (AST * list nat) :=
  match lookup_type name ctx env with
  | Some res => Some (res, ctx)
  | None =>
      match ctx with
      | nil => None
      | cons _ ctx' => resolve_hygiene env name ctx'
      end
  end.






Definition StringType := AstRef "String".
Definition IntType := AstRef "Int".
Definition BoolType := AstRef "Bool".
Definition TypeUniverse := AstRef "Type".

Definition is_type_expected (expected : option AST) : bool :=
  match expected with
  | Some (AstRef n) =>
      orb (if string_dec n "Type" then true else false)
        (if string_dec n "TypeUniverse" then true else false)
  | _ => false
  end.

(** True if meta [m] occurs in [t] (occurs-check for unification). *)
Fixpoint occurs_meta (m : nat) (t : AST) {struct t} : bool :=
  match t with
  | AstMeta m' => Nat.eqb m m'
  | AstRef _ | AstUniverse _ | AstStringLit _ | AstIntLit _ | AstBoolLit _
  | AstError _ | AstImport _ _ _ _ | AstFileImport _ _ => false
  | AstTuple elems => existsb (occurs_meta m) elems
  | AstBlock stmts tail =>
      orb (existsb (occurs_meta m) stmts) (occurs_meta m tail)
  | AstApp f args => orb (occurs_meta m f) (existsb (occurs_meta m) args)
  | AstImplicitApp f args =>
      orb (occurs_meta m f) (existsb (occurs_meta m) args)
  | AstLam _ ty body => orb (occurs_meta m ty) (occurs_meta m body)
  | AstPi _ ty ret _ => orb (occurs_meta m ty) (occurs_meta m ret)
  | AstDo e args => orb (occurs_meta m e) (existsb (occurs_meta m) args)
  | AstHandle e _ hs =>
      orb (occurs_meta m e) (existsb (fun p => occurs_meta m (snd p)) hs)
  | AstLet _ val => occurs_meta m val
  | AstVar _ val => occurs_meta m val
  | AstAssign _ val => occurs_meta m val
  | AstIf c th el =>
      orb (occurs_meta m c) (orb (occurs_meta m th) (occurs_meta m el))
  | AstDef _ _ params ret body _ =>
      orb (existsb (fun p => occurs_meta m (snd p)) params)
        (orb (occurs_meta m ret) (occurs_meta m body))
  | AstFunTy _ params ret _ =>
      orb (existsb (fun p => occurs_meta m (snd p)) params) (occurs_meta m ret)
  | AstEnum _ _ variants =>
      existsb
        (fun v =>
           match v with
           | (_, fields, ret) =>
               orb (existsb (occurs_meta m) fields) (occurs_meta m ret)
           end)
        variants
  | AstMatch e cases =>
      orb (occurs_meta m e) (existsb (fun c => occurs_meta m (snd c)) cases)
  | AstRecord _ _ fields =>
      existsb (fun p => occurs_meta m (snd p)) fields
  | AstFieldAccess e _ => occurs_meta m e
  | AstExtension _ _ tgt meths =>
      orb (occurs_meta m tgt) (existsb (occurs_meta m) meths)
  | AstBox e _ => occurs_meta m e
  | AstUnbox e => occurs_meta m e
  | AstModule _ params seal body =>
      orb (existsb (fun p => occurs_meta m (snd p)) params)
        (orb (match seal with Some s => occurs_meta m s | None => false end)
           (existsb (occurs_meta m) body))
  | AstSignature _ decls => existsb (occurs_meta m) decls
  | AstFunctorApp f args =>
      orb (occurs_meta m f) (existsb (occurs_meta m) args)
  | AstModTy exports => existsb (fun p => occurs_meta m (snd p)) exports
  | AstSigVal _ _ params ret _ =>
      orb (existsb (fun p => occurs_meta m (snd p)) params) (occurs_meta m ret)
  | AstTypeDecl _ (Some ty) => occurs_meta m ty
  | AstTypeDecl _ None => false
  | AstSigWith s eqs =>
      orb (occurs_meta m s) (existsb (fun p => occurs_meta m (snd p)) eqs)
  | AstPack m' s => orb (occurs_meta m m') (occurs_meta m s)
  | AstUnpack _ s e b =>
      orb (occurs_meta m s) (orb (occurs_meta m e) (occurs_meta m b))
  | AstSpan _ inner => occurs_meta m inner
  end.

(** Name aliases: exact equality, plus Int/Integer, Bool/Boolean, Type universes.
    Distinct type-param letters ([T] vs [A]) are *not* aliases — rigidity is the
    point of polymorphic binders (LITERATURE §4). *)
Definition refs_compat (n1 n2 : string) : bool :=
  orb (String.eqb n1 n2)
    (orb (andb (is_int_ty (AstRef n1)) (is_int_ty (AstRef n2)))
      (orb (andb (is_bool_ty (AstRef n1)) (is_bool_ty (AstRef n2)))
        (andb (is_universe_ty (AstRef n1)) (is_universe_ty (AstRef n2))))).

(** Expand solved metas in [t] before occurs / assign (fuel from [ast_size]). *)
Fixpoint expand_metas (fuel : nat) (sol : SolverState) (t : AST) {struct fuel} : AST :=
  match fuel with
  | 0 => t
  | S fuel' =>
      match strip_span t with
      | AstMeta m =>
          match type_metas sol m with
          | Solved v => expand_metas fuel' sol v
          | _ => AstMeta m
          end
      | AstApp f args =>
          AstApp (expand_metas fuel' sol f)
            (map (expand_metas fuel' sol) args)
      | AstPi n a r e =>
          AstPi n (expand_metas fuel' sol a) (expand_metas fuel' sol r) e
      | AstLam n a b =>
          AstLam n (expand_metas fuel' sol a) (expand_metas fuel' sol b)
      | AstFunTy tps ps r e =>
          AstFunTy tps
            (map (fun p => (fst p, expand_metas fuel' sol (snd p))) ps)
            (expand_metas fuel' sol r) e
      | AstTuple xs => AstTuple (map (expand_metas fuel' sol) xs)
      | AstDo op args =>
          AstDo (expand_metas fuel' sol op) (map (expand_metas fuel' sol) args)
      | other => other
      end
  end.

Definition assign_meta (m : nat) (t : AST) : ElabM unit :=
  s <- get_state ;
  match s with
  | mkElabState n sol exts effs caps pending go_pkgs =>
      let t' := expand_metas (S (ast_size t)) sol t in
      if occurs_meta m t' then throw "Occurs check failed"
      else
        set_state (mkElabState n (update_type_state m (Solved t') sol)
                     exts effs caps pending go_pkgs)
  end.



(** Collapse multi-arg [AstFunTy] into a curried [AstPi] telescope for unify. *)
Fixpoint funty_as_pi (params : list (string * AST)) (ret : AST) (effs : EffectSet) : AST :=
  match params with
  | [] => ret
  | (n, ty) :: rest =>
      AstPi n ty (funty_as_pi rest ret effs) effs
  end.

Fixpoint unify (fuel : nat) (t1 t2 : AST) {struct fuel} : ElabM unit :=
  match fuel with
  | 0 => throw "Unification fuel exhausted"
  | S fuel' =>
      let fix unify_list (l1 l2 : list AST) : ElabM unit :=
        match l1, l2 with
        | [], [] => ret tt
        | x :: xs, y :: ys => unify fuel' x y ;; unify_list xs ys
        | _, _ => throw "Unification arity mismatch"
        end in
      let t1' := strip_span (whnf t1) in
      let t2' := strip_span (whnf t2) in
      match t1', t2' with
      | AstMeta m1, AstMeta m2 =>
          if Nat.eqb m1 m2 then ret tt else
          s <- get_state ;
          match s with
          | mkElabState n sol exts effs caps pending go_pkgs =>
              match type_metas sol m1 with
              | Solved v1 => unify fuel' v1 t2'
              | _ => match type_metas sol m2 with
                     | Solved v2 => unify fuel' t1' v2
                     | _ => assign_meta m1 t2'
                     end
              end
          end
      | AstMeta m1, _ =>
          s <- get_state ;
          match s with
          | mkElabState _ sol _ _ _ _ _ =>
              match type_metas sol m1 with
              | Solved v1 => unify fuel' v1 t2'
              | _ => assign_meta m1 t2'
              end
          end
      | _, AstMeta m2 =>
          s <- get_state ;
          match s with
          | mkElabState _ sol _ _ _ _ _ =>
              match type_metas sol m2 with
              | Solved v2 => unify fuel' t1' v2
              | _ => assign_meta m2 t1'
              end
          end
      | AstApp f1 a1, AstApp f2 a2 =>
          unify fuel' f1 f2 ;; unify_list a1 a2
      | AstTypeDecl n1 o1, AstTypeDecl n2 o2 =>
          if negb (String.eqb n1 n2) then throw "Type mismatch"
          else
            match o1, o2 with
            | Some a, Some b => unify fuel' a b
            | None, None => ret tt
            | _, _ => throw "Type mismatch"
            end
      | AstPi _n1 ty1 ret1 eff1, AstPi _n2 ty2 ret2 eff2 =>
          unify fuel' ty1 ty2 ;;
          unify fuel' ret1 ret2 ;;
          (* Koka-lite: rows compatible if either subsumes the other (open tails). *)
          if orb (effect_row_subsumes eff1 eff2) (effect_row_subsumes eff2 eff1)
          then ret tt
          else throw "Effect row mismatch"
      | AstFunTy _tp1 p1 r1 e1, AstFunTy _tp2 p2 r2 e2 =>
          let fix unify_params (a b : list (string * AST)) : ElabM unit :=
            match a, b with
            | [], [] => ret tt
            | (_, t) :: xs, (_, u) :: ys => unify fuel' t u ;; unify_params xs ys
            | _, _ => throw "Unification arity mismatch"
            end
          in
          unify_params p1 p2 ;;
          unify fuel' r1 r2 ;;
          if orb (effect_row_subsumes e1 e2) (effect_row_subsumes e2 e1)
          then ret tt
          else throw "Effect row mismatch"
      (* Named defs are FunTy; lambdas / arrow types are Pi — unify via currying. *)
      | AstFunTy _ p1 r1 e1, AstPi _ _ _ _ =>
          unify fuel' (funty_as_pi p1 r1 e1) t2'
      | AstPi _ _ _ _, AstFunTy _ p2 r2 e2 =>
          unify fuel' t1' (funty_as_pi p2 r2 e2)
      | AstTuple xs, AstTuple ys => unify_list xs ys
      | AstUniverse l1, AstUniverse l2 =>
          if Nat.eqb l1 l2 then ret tt else throw "Type mismatch"
      | AstStringLit s1, AstStringLit s2 =>
          if string_dec s1 s2 then ret tt else throw "Type mismatch"
      | AstIntLit n1, AstIntLit n2 =>
          if Nat.eqb n1 n2 then ret tt else throw "Type mismatch"
      | AstBoolLit b1, AstBoolLit b2 =>
          match b1, b2 with
          | true, true | false, false => ret tt
          | _, _ => throw "Type mismatch"
          end
      | AstRef n1, AstRef n2 =>
          (* Explicit [Any]/[Unknown] are wildcards; other refs use aliases. *)
          if orb (refs_compat n1 n2)
               (orb (is_wildcard_ty t1') (is_wildcard_ty t2'))
          then ret tt else throw "Type mismatch"
      | AstFieldAccess e1 f1, AstFieldAccess e2 f2 =>
          if negb (String.eqb f1 f2) then throw "Type mismatch"
          else unify fuel' e1 e2
      | AstFieldAccess _ f, AstRef n =>
          if String.eqb f n then ret tt else throw "Type mismatch"
      | AstRef n, AstFieldAccess _ f =>
          if String.eqb f n then ret tt else throw "Type mismatch"
      | _, _ =>
          if orb (is_wildcard_ty t1') (is_wildcard_ty t2') then ret tt
          else if types_compat t1' t2' then ret tt
          else throw "Type mismatch"
      end
  end.

Fixpoint zonk (fuel : nat) (expr : AST) : ElabM AST :=
  match fuel with
  | 0 => ret expr
  | S fuel' =>
      let fix zonk_list (l : list AST) : ElabM (list AST) :=
        match l with
        | [] => ret []
        | x :: xs => x' <- zonk fuel' x ; xs' <- zonk_list xs ; ret (x' :: xs')
        end in
      let fix zonk_params (ps : list (string * AST))
          : ElabM (list (string * AST)) :=
        match ps with
        | [] => ret []
        | (n, ty) :: rest =>
            ty' <- zonk fuel' ty ;
            rest' <- zonk_params rest ;
            ret ((n, ty') :: rest')
        end in
      match strip_span expr with
      | AstMeta m =>
          s <- get_state ;
          match s with
          | mkElabState _ sol _ _ _ _ _ =>
              match type_metas sol m with
              | Solved v => zonk fuel' v
              | _ => ret (AstMeta m)
              end
          end
      | AstApp f args =>
          f' <- zonk fuel' f ;
          args' <- zonk_list args ;
          ret (AstApp f' args')
      | AstPi n ty retTy effs =>
          ty' <- zonk fuel' ty ;
          retTy' <- zonk fuel' retTy ;
          ret (AstPi n ty' retTy' effs)
      | AstLam n ty body =>
          ty' <- zonk fuel' ty ;
          body' <- zonk fuel' body ;
          ret (AstLam n ty' body')
      | AstFunTy tps params retTy effs =>
          params' <- zonk_params params ;
          retTy' <- zonk fuel' retTy ;
          ret (AstFunTy tps params' retTy' effs)
      | AstTuple xs =>
          xs' <- zonk_list xs ;
          ret (AstTuple xs')
      | AstBlock stmts tail =>
          stmts' <- zonk_list stmts ;
          tail' <- zonk fuel' tail ;
          ret (AstBlock stmts' tail')
      | AstDo op args =>
          op' <- zonk fuel' op ;
          args' <- zonk_list args ;
          ret (AstDo op' args')
      | AstHandle e eff hs =>
          e' <- zonk fuel' e ;
          let fix zonk_hs (xs : list (string * AST))
              : ElabM (list (string * AST)) :=
            match xs with
            | [] => ret []
            | (op, h) :: rest =>
                h' <- zonk fuel' h ;
                rest' <- zonk_hs rest ;
                ret ((op, h') :: rest')
            end in
          hs' <- zonk_hs hs ;
          ret (AstHandle e' eff hs')
      | AstDef n tps params retTy body effs =>
          params' <- zonk_params params ;
          retTy' <- zonk fuel' retTy ;
          body' <- zonk fuel' body ;
          ret (AstDef n tps params' retTy' body' effs)
      | AstIf c th el =>
          c' <- zonk fuel' c ;
          th' <- zonk fuel' th ;
          el' <- zonk fuel' el ;
          ret (AstIf c' th' el')
      | AstLet n v =>
          v' <- zonk fuel' v ;
          ret (AstLet n v')
      | AstMatch e cases =>
          e' <- zonk fuel' e ;
          let fix zonk_cases (cs : list (PatternAST * AST))
              : ElabM (list (PatternAST * AST)) :=
            match cs with
            | [] => ret []
            | (p, b) :: rest =>
                b' <- zonk fuel' b ;
                rest' <- zonk_cases rest ;
                ret ((p, b') :: rest')
            end in
          cases' <- zonk_cases cases ;
          ret (AstMatch e' cases')
      | AstFieldAccess e f =>
          e' <- zonk fuel' e ;
          ret (AstFieldAccess e' f)
      | AstBox e caps =>
          e' <- zonk fuel' e ;
          ret (AstBox e' caps)
      | AstUnbox e =>
          e' <- zonk fuel' e ;
          ret (AstUnbox e')
      | AstModTy ex =>
          let fix zonk_ex (xs : list (string * AST))
              : ElabM (list (string * AST)) :=
            match xs with
            | [] => ret []
            | (n, ty) :: rest =>
                ty' <- zonk fuel' ty ;
                rest' <- zonk_ex rest ;
                ret ((n, ty') :: rest')
            end in
          ex' <- zonk_ex ex ;
          ret (AstModTy ex')
      | AstPack m s =>
          m' <- zonk fuel' m ;
          s' <- zonk fuel' s ;
          ret (AstPack m' s')
      | AstUnpack n s e b =>
          s' <- zonk fuel' s ;
          e' <- zonk fuel' e ;
          b' <- zonk fuel' b ;
          ret (AstUnpack n s' e' b')
      | AstVar n v =>
          v' <- zonk fuel' v ;
          ret (AstVar n v')
      | AstAssign n v =>
          v' <- zonk fuel' v ;
          ret (AstAssign n v')
      | AstTypeDecl n (Some ty) =>
          ty' <- zonk fuel' ty ;
          ret (AstTypeDecl n (Some ty'))
      | AstTypeDecl n None => ret (AstTypeDecl n None)
      | AstSigVal n tps params retTy effs =>
          params' <- zonk_params params ;
          retTy' <- zonk fuel' retTy ;
          ret (AstSigVal n tps params' retTy' effs)
      | AstSignature n decls =>
          decls' <- zonk_list decls ;
          ret (AstSignature n decls')
      | AstModule n params seal0 body =>
          params' <- zonk_params params ;
          seal' <-
            match seal0 with
            | Some s => s' <- zonk fuel' s ; ret (Some s')
            | None => ret None
            end ;
          body' <- zonk_list body ;
          ret (AstModule n params' seal' body')
      | AstEnum n tps vars =>
          let fix zonk_vars (vs : list (string * list AST * AST))
              : ElabM (list (string * list AST * AST)) :=
            match vs with
            | [] => ret []
            | (c, fields, r) :: rest =>
                fields' <- zonk_list fields ;
                r' <- zonk fuel' r ;
                rest' <- zonk_vars rest ;
                ret ((c, fields', r') :: rest')
            end in
          vars' <- zonk_vars vars ;
          ret (AstEnum n tps vars')
      | AstRecord n tps fields =>
          fields' <- zonk_params fields ;
          ret (AstRecord n tps fields')
      | AstExtension n tps tgt meths =>
          tgt' <- zonk fuel' tgt ;
          meths' <- zonk_list meths ;
          ret (AstExtension n tps tgt' meths')
      | AstFunctorApp f args =>
          f' <- zonk fuel' f ;
          args' <- zonk_list args ;
          ret (AstFunctorApp f' args')
      | AstSigWith s eqs =>
          s' <- zonk fuel' s ;
          eqs' <- zonk_params eqs ;
          ret (AstSigWith s' eqs')
      | AstImplicitApp f args =>
          f' <- zonk fuel' f ;
          args' <- zonk_list args ;
          ret (AstImplicitApp f' args')
      | AstSpan sp i =>
          i' <- zonk fuel' i ;
          ret (AstSpan sp i')
      | other => ret other
      end
  end.


Fixpoint get_base_type (t : AST) : AST :=
  match t with
  | AstSpan _ inner => get_base_type inner
  | AstApp f _ => get_base_type f
  | AstImplicitApp f _ => get_base_type f
  | _ => t
  end.

Fixpoint check_type_match (t1 t2 : AST) : bool :=
  match get_base_type t1, get_base_type t2 with
  | AstRef n1, AstRef n2 => if string_dec n1 n2 then true else false
  | _, _ => false
  end.

Fixpoint lookup_ext_method (field : string) (meths : list (string * string)) : string :=
  match meths with
  | [] => EmptyString
  | (n, full) :: rest => if string_dec n field then full else lookup_ext_method field rest
  end.

Fixpoint find_ext_method (ty : AST) (field : string) (exts : list (string * AST * list (string * string))) : string :=
  match exts with
  | [] => EmptyString
  | (ext_name, target_ty, meths) :: rest =>
      if check_type_match target_ty ty then
        let res := lookup_ext_method field meths in
        if string_dec res EmptyString then find_ext_method ty field rest
        else res
      else find_ext_method ty field rest
  end.

Fixpoint extract_ext_methods (meths : list CST) (ext_name : string) : list (string * string) :=
  match meths with
  | [] => []
  | DefCST n tps ps rt b sp :: rest =>
      let full := append (append ext_name "_") n in
      (n, full) :: extract_ext_methods rest ext_name
  | _ :: rest => extract_ext_methods rest ext_name
  end.

Fixpoint elab_extern_decls (env : TypeEnv) (decls : list CST) : list string * TypeEnv :=
  match decls with
  | [] => ([], env)
  | d :: rest =>
      match parse_effect_sig d with
      | Some (op, _params, _ret_ty_cst) =>
          let fun_ty := AstRef "Any" in
          let new_env := ((op, []), fun_ty) :: env in
          let (syms, final_env) := elab_extern_decls new_env rest in
          (op :: syms, final_env)
      | None =>
          elab_extern_decls env rest
      end
  end.

(** Collect structure exports (name, type) from an elaborated module body. *)
Fixpoint module_exports_of (body : list AST) : list (string * AST) :=
  match body with
  | [] => []
  | AstDef n tps params ret _ effs :: xs =>
      (n, AstFunTy tps params ret effs) :: module_exports_of xs
  | AstSigVal n tps params ret effs :: xs =>
      (n, AstFunTy tps params ret effs) :: module_exports_of xs
  | AstTypeDecl n opt :: xs =>
      (n, AstTypeDecl n opt) :: module_exports_of xs
  | AstModule n _ (Some (AstModTy ex)) _ :: xs =>
      (n, AstModTy ex) :: module_exports_of xs
  | AstModule n _ _ _ :: xs =>
      (n, AstModTy []) :: module_exports_of xs
  | AstSignature n decls :: xs =>
      (n, AstSignature n decls) :: module_exports_of xs
  | AstSpan _ (AstDef n tps params ret _ effs) :: xs =>
      (n, AstFunTy tps params ret effs) :: module_exports_of xs
  | AstSpan _ (AstSigVal n tps params ret effs) :: xs =>
      (n, AstFunTy tps params ret effs) :: module_exports_of xs
  | AstSpan _ (AstTypeDecl n opt) :: xs =>
      (n, AstTypeDecl n opt) :: module_exports_of xs
  | AstSpan _ (AstModule n _ (Some (AstModTy ex)) _) :: xs =>
      (n, AstModTy ex) :: module_exports_of xs
  | AstSpan _ (AstModule n _ _ _) :: xs =>
      (n, AstModTy []) :: module_exports_of xs
  | AstSpan _ (AstSignature n decls) :: xs =>
      (n, AstSignature n decls) :: module_exports_of xs
  | _ :: xs => module_exports_of xs
  end.

(** Pure structural type matching for sealing (ElabM unify is unavailable here).
    [types_compat] already treats [Any] and soft aliases; we also accept equal
    [AstRef] names after strip (same component, pre- or post-resolve). *)
Fixpoint types_match_seal (t1 t2 : AST) {struct t1} : bool :=
  match t1 with
  | AstSpan _ i1 => types_match_seal i1 t2
  | _ =>
      let t2' := strip_span t2 in
      if types_compat t1 t2' then true
      else
        match t1, t2' with
        | AstFunTy _ p1 r1 _, AstFunTy _ p2 r2 _ =>
            let fix params_ok (a b : list (string * AST)) : bool :=
              match a, b with
              | [], [] => true
              | (_, t) :: xs, (_, u) :: ys =>
                  andb (types_match_seal t u) (params_ok xs ys)
              | _, _ => false
              end
            in
            andb (params_ok p1 p2) (types_match_seal r1 r2)
        | AstPi _ a1 r1 _, AstPi _ a2 r2 _ =>
            andb (types_match_seal a1 a2) (types_match_seal r1 r2)
        | AstApp f1 a1, AstApp f2 a2 =>
            let fix args_ok (xs ys : list AST) : bool :=
              match xs, ys with
              | [], [] => true
              | x :: xs', y :: ys' =>
                  andb (types_match_seal x y) (args_ok xs' ys')
              | _, _ => false
              end
            in
            andb (types_match_seal f1 f2) (args_ok a1 a2)
        | AstTuple xs, AstTuple ys =>
            let fix elems_ok (a b : list AST) : bool :=
              match a, b with
              | [], [] => true
              | x :: xs', y :: ys' =>
                  andb (types_match_seal x y) (elems_ok xs' ys')
              | _, _ => false
              end
            in
            elems_ok xs ys
        | AstTypeDecl n1 o1, AstTypeDecl n2 o2 =>
            if negb (String.eqb n1 n2) then false
            else
              match o1, o2 with
              | Some a, Some b => types_match_seal a b
              | None, None => true
              | _, _ => false
              end
        | AstRef n1, AstRef n2 => refs_compat n1 n2
        | AstRef n1, _ => String.eqb n1 "Any"
        | _, AstRef n2 => String.eqb n2 "Any"
        | AstUniverse l1, AstUniverse l2 => Nat.eqb l1 l2
        | _, _ => false
        end
  end.

(** Peel leading Pi/FunTy argument types for constructor pattern binders. *)
Fixpoint ctor_arg_tys (fuel : nat) (ty : AST) (n : nat) {struct fuel}
  : list AST :=
  match fuel, n with
  | 0, _ => []
  | _, 0 => []
  | S fuel', S n' =>
      match strip_span (whnf ty) with
      | AstPi _ a r _ => a :: ctor_arg_tys fuel' r n'
      | AstFunTy _ ((_, a) :: ps) r e =>
          a :: ctor_arg_tys fuel' (AstFunTy [] ps r e) n'
      | _ =>
          (* Unknown ctor shape: remaining binders get [Any]. *)
          let fix fill (k : nat) : list AST :=
            match k with
            | 0 => []
            | S k' => AstRef "Any" :: fill k'
            end
          in fill (S n')
      end
  end.

(** Collect impl type-component definitions for seal matching:
    signature [type t] / [type t = _] checked against [full]'s concrete def. *)
Fixpoint seal_type_substs_fuel (fuel : nat) (sig_decls : list AST)
    (full : list (string * AST)) {struct fuel} : list (string * AST) :=
  match fuel with
  | 0 => []
  | S fuel' =>
      match sig_decls with
      | [] => []
      | AstTypeDecl n _ :: rest =>
          match find (fun p => String.eqb (fst p) n) full with
          | Some (_, AstTypeDecl _ (Some def)) =>
              (n, def) :: seal_type_substs_fuel fuel' rest full
          | _ => seal_type_substs_fuel fuel' rest full
          end
      | AstSpan _ inner :: rest =>
          seal_type_substs_fuel fuel' (inner :: rest) full
      | _ :: rest => seal_type_substs_fuel fuel' rest full
      end
  end.

Definition seal_type_substs (sig_decls : list AST) (full : list (string * AST))
  : list (string * AST) :=
  seal_type_substs_fuel
    (Nat.mul (S (fold_left (fun n d => n + ast_size d) sig_decls 0)) 2)
    sig_decls full.

Fixpoint apply_seal_subst_fuel (fuel : nat) (subs : list (string * AST)) (t : AST)
  {struct fuel} : AST :=
  match fuel with
  | 0 => t
  | S fuel' =>
      match t with
      | AstRef n =>
          match find (fun p => String.eqb (fst p) n) subs with
          | Some (_, def) => def
          | None => t
          end
      | AstFunTy tps params ret effs =>
          let fix go (ps : list (string * AST)) (s : list (string * AST))
              : list (string * AST) * AST :=
            match ps with
            | [] => ([], apply_seal_subst_fuel fuel' s ret)
            | (pn, pty) :: rest =>
                let pty' := apply_seal_subst_fuel fuel' s pty in
                let s' :=
                  filter (fun p => negb (String.eqb (fst p) pn)) s
                in
                let (ps2, r2) := go rest s' in
                ((pn, pty') :: ps2, r2)
            end
          in
          let (ps', r') := go params subs in
          AstFunTy tps ps' r' effs
      | AstPi n a r e =>
          let a' := apply_seal_subst_fuel fuel' subs a in
          let subs' :=
            filter (fun p => negb (String.eqb (fst p) n)) subs
          in
          AstPi n a' (apply_seal_subst_fuel fuel' subs' r) e
      | AstApp f args =>
          AstApp (apply_seal_subst_fuel fuel' subs f)
            (map (apply_seal_subst_fuel fuel' subs) args)
      | AstTuple xs => AstTuple (map (apply_seal_subst_fuel fuel' subs) xs)
      | AstSpan sp i => AstSpan sp (apply_seal_subst_fuel fuel' subs i)
      | AstTypeDecl n (Some ty) =>
          AstTypeDecl n (Some (apply_seal_subst_fuel fuel' subs ty))
      | AstModTy ex =>
          AstModTy
            (map (fun p => (fst p, apply_seal_subst_fuel fuel' subs (snd p))) ex)
      | other => other
      end
  end.

Definition apply_seal_subst (subs : list (string * AST)) (t : AST) : AST :=
  apply_seal_subst_fuel (S (ast_size t)) subs t.

Definition apply_seal_subst_params (subs : list (string * AST))
    (params : list (string * AST)) : list (string * AST) :=
  map (fun p => (fst p, apply_seal_subst subs (snd p))) params.

(** Substitute [AstRef n] for sharing equations (with-type enrichment). *)
Fixpoint apply_eq_subst_fuel (fuel : nat) (eqs : list (string * AST)) (t : AST)
  {struct fuel} : AST :=
  match fuel with
  | 0 => t
  | S fuel' =>
      match t with
      | AstRef n =>
          match find (fun p => String.eqb (fst p) n) eqs with
          | Some (_, ty) => ty
          | None => t
          end
      | AstFunTy tps params ret effs =>
          AstFunTy tps
            (map (fun p => (fst p, apply_eq_subst_fuel fuel' eqs (snd p))) params)
            (apply_eq_subst_fuel fuel' eqs ret) effs
      | AstPi n a r e =>
          AstPi n (apply_eq_subst_fuel fuel' eqs a)
            (apply_eq_subst_fuel fuel' eqs r) e
      | AstApp f args =>
          AstApp (apply_eq_subst_fuel fuel' eqs f)
            (map (apply_eq_subst_fuel fuel' eqs) args)
      | AstSpan sp i => AstSpan sp (apply_eq_subst_fuel fuel' eqs i)
      | other => other
      end
  end.

Definition apply_eq_subst (eqs : list (string * AST)) (t : AST) : AST :=
  apply_eq_subst_fuel (S (ast_size t)) eqs t.

(** Apply `with type t = T` sharing equations to signature decls.
    Rewrites type components and substitutes into value member types. *)
Fixpoint apply_type_eqs (decls : list AST) (eqs : list (string * AST))
  : list AST :=
  match decls with
  | [] => []
  | AstTypeDecl n opt :: rest =>
      match find (fun p => String.eqb (fst p) n) eqs with
      | Some (_, ty) => AstTypeDecl n (Some ty) :: apply_type_eqs rest eqs
      | None => AstTypeDecl n opt :: apply_type_eqs rest eqs
      end
  | AstSpan sp (AstTypeDecl n opt) :: rest =>
      match find (fun p => String.eqb (fst p) n) eqs with
      | Some (_, ty) =>
          AstSpan sp (AstTypeDecl n (Some ty)) :: apply_type_eqs rest eqs
      | None => AstSpan sp (AstTypeDecl n opt) :: apply_type_eqs rest eqs
      end
  | AstSigVal n tps params ret effs :: rest =>
      let params' :=
        map (fun p => (fst p, apply_eq_subst eqs (snd p))) params
      in
      AstSigVal n tps params' (apply_eq_subst eqs ret) effs
        :: apply_type_eqs rest eqs
  | x :: rest => x :: apply_type_eqs rest eqs
  end.

Fixpoint apply_sig_with (base : AST) (eqs : list (string * AST)) {struct base}
  : AST :=
  match base with
  | AstSignature name decls => AstSignature name (apply_type_eqs decls eqs)
  | AstSigWith b eqs2 => apply_sig_with b (app eqs2 eqs)
  | AstSpan _ inner => apply_sig_with inner eqs
  | other => AstSigWith other eqs
  end.

(** Flatten spans / `with type` enrichments before sealing (fuel from AST size). *)
Fixpoint flatten_sig_decls (fuel : nat) (ds : list AST) : list AST :=
  match fuel with
  | 0 => ds
  | S fuel' =>
      match ds with
      | [] => []
      | AstSpan _ inner :: rest => flatten_sig_decls fuel' (inner :: rest)
      | AstSigWith base eqs :: rest =>
          match apply_sig_with base eqs with
          | AstSignature _ decls => flatten_sig_decls fuel' (app decls rest)
          | other => other :: flatten_sig_decls fuel' rest
          end
      | x :: rest => x :: flatten_sig_decls fuel' rest
      end
  end.

Definition sig_decls_fuel (ds : list AST) : nat :=
  Nat.mul (S (fold_left (fun n d => n + ast_size d) ds 0)) 4.

Fixpoint seal_params_ok (a b : list (string * AST)) : bool :=
  match a, b with
  | [], [] => true
  | (_, t) :: xs, (_, u) :: ys =>
      andb (types_match_seal t u) (seal_params_ok xs ys)
  | _, _ => false
  end.

Definition seal_cons_export (n : string) (ty : AST)
    (rest_res : sum string (list (string * AST)))
  : sum string (list (string * AST)) :=
  match rest_res with
  | inl err => inl err
  | inr xs => inr ((n, ty) :: xs)
  end.

(** Check one signature value member against [found] export and recursive result.
    [subs] maps signature type-component names to impl definitions from [full].
    Matching uses the substituted view; opaque sealing exports the *signature*
    types (abstract components stay abstract — no representation leak). *)
Definition seal_sigval_member (opaque : bool) (n : string) (tps : list string)
    (params : list (string * AST)) (ret : AST) (effs : EffectSet)
    (subs : list (string * AST))
    (found : option (string * AST))
    (rest_res : sum string (list (string * AST)))
  : sum string (list (string * AST)) :=
  let params' := apply_seal_subst_params subs params in
  let ret' := apply_seal_subst subs ret in
  let exported :=
    if opaque then AstFunTy tps params ret effs
    else AstFunTy tps params' ret' effs
  in
  match found with
  | Some (_, AstFunTy _ impl_params impl_ret impl_effs) =>
      let impl_params' := apply_seal_subst_params subs impl_params in
      let impl_ret' := apply_seal_subst subs impl_ret in
      if effect_row_subsumes impl_effs effs then
        if andb (seal_params_ok params' impl_params') (types_match_seal ret' impl_ret')
        then seal_cons_export n exported rest_res
        else inl ("type mismatch for signature member: " ++ n)
      else inl ("effect row too large for signature member: " ++ n)
  | Some (_, AstPi _ impl_arg impl_ret impl_effs) =>
      let impl_arg' := apply_seal_subst subs impl_arg in
      let impl_ret' := apply_seal_subst subs impl_ret in
      if effect_row_subsumes impl_effs effs then
        match params' with
        | [(_, sig_arg)] =>
            if andb (types_match_seal sig_arg impl_arg')
                    (types_match_seal ret' impl_ret')
            then seal_cons_export n exported rest_res
            else inl ("type mismatch for signature member: " ++ n)
        | _ => inl ("type mismatch for signature member: " ++ n)
        end
      else inl ("effect row too large for signature member: " ++ n)
  | Some _ => inl ("type mismatch for signature member: " ++ n)
  | None => inl ("signature requires missing value: " ++ n)
  end.

(** Seal exports against signature specs. Missing required members are an error
    (returned as inl err); opaque abstracts type components; value specs require
    implementation effects ⊆ declared signature effects and matching param/ret types.
    [subs] is the type-component substitution built from [full] (see [seal_type_substs]). *)
Fixpoint seal_exports_check_flat (opaque : bool) (full : list (string * AST))
    (subs : list (string * AST))
    (sig_decls : list AST) : sum string (list (string * AST)) :=
  match sig_decls with
  | [] => inr []
  | AstSigVal n tps params ret effs :: rest =>
      seal_sigval_member opaque n tps params ret effs subs
        (find (fun p => String.eqb (fst p) n) full)
        (seal_exports_check_flat opaque full subs rest)
  | AstDef n _ _ _ _ _ :: rest =>
      match find (fun p => String.eqb (fst p) n) full with
      | Some p =>
          match seal_exports_check_flat opaque full subs rest with
          | inl err => inl err
          | inr xs => inr (p :: xs)
          end
      | None => inl ("signature requires missing value: " ++ n)
      end
  | AstTypeDecl n opt_sig :: rest =>
      match find (fun p => String.eqb (fst p) n) full with
      | Some (_, AstTypeDecl _ opt_impl) =>
          let manifest_ok :=
            match opt_sig, opt_impl with
            | Some sig_ty, Some impl_ty =>
                types_match_seal (apply_seal_subst subs sig_ty) impl_ty
            | Some _, None => false
            | None, _ => true
            end
          in
          if negb manifest_ok then
            inl ("manifest type mismatch for: " ++ n)
          else
            (* Manifest signature types stay visible under opaque sealing
               (SML-style); only abstract [type t] is hidden. *)
            let exported :=
              match opt_sig with
              | Some sig_ty =>
                  AstTypeDecl n (Some (apply_seal_subst subs sig_ty))
              | None =>
                  if opaque then AstTypeDecl n None
                  else AstTypeDecl n opt_impl
              end
            in
            match seal_exports_check_flat opaque full subs rest with
            | inl err => inl err
            | inr xs => inr ((n, exported) :: xs)
            end
      | Some _ =>
          inl ("signature type component is not a type: " ++ n)
      | None => inl ("signature requires missing type: " ++ n)
      end
  | _ :: _ =>
      (* Spans / with-type should already be flattened; do not re-cons. *)
      inl "unsupported signature declaration under sealing"
  end.

Definition seal_exports_check (opaque : bool) (full : list (string * AST))
    (sig_decls : list AST) : sum string (list (string * AST)) :=
  let flat := flatten_sig_decls (sig_decls_fuel sig_decls) sig_decls in
  let subs := seal_type_substs flat full in
  seal_exports_check_flat opaque full subs flat.

(** Legacy name filter (CoreChecker / callers that ignore errors). *)
Definition seal_exports (full : list (string * AST)) (sig_decls : list AST)
  : list (string * AST) :=
  match seal_exports_check true full sig_decls with
  | inr xs => xs
  | inl _ =>
      let fix go (ds : list AST) : list (string * AST) :=
        match ds with
        | [] => []
        | AstSigVal n _ _ _ _ :: rest =>
            match find (fun p => String.eqb (fst p) n) full with
            | Some p => p :: go rest
            | None => go rest
            end
        | AstDef n _ _ _ _ _ :: rest =>
            match find (fun p => String.eqb (fst p) n) full with
            | Some p => p :: go rest
            | None => go rest
            end
        | AstTypeDecl n _ :: rest =>
            match find (fun p => String.eqb (fst p) n) full with
            | Some p => p :: go rest
            | None => go rest
            end
        | _ :: rest => go rest
        end
      in go sig_decls
  end.

Definition lookup_export (exports : list (string * AST)) (field : string) : option AST :=
  match find (fun p => String.eqb (fst p) field) exports with
  | Some (_, ty) => Some ty
  | None => None
  end.

Fixpoint elaborate (fuel : nat) (env : TypeEnv) (expr : CST) (expected : option AST) {struct fuel}
  : ElabM (AST * AST) :=
  match fuel with
  | 0 => throw "Out of fuel"
  | S fuel' =>
  match expr with
  | Symbol name span =>
      match resolve_hygiene env name (context span) with
      | Some (ty, resolved_ctx) =>
          match ty with
          | AstTypeDecl _ (Some def) =>
              if is_type_expected expected then ret (def, TypeUniverse)
              else throw ("type component used as value: " ++ name)
          | AstTypeDecl _ None =>
              if is_type_expected expected then
                ret (AstRef (mangle_name name resolved_ctx), TypeUniverse)
              else throw ("type component used as value: " ++ name)
          | AstFunTy _ _ _ _ | AstPi _ _ _ _ =>
              (* Ctor / value may share the enum type name; in type position
                 use the name as a type ref (e.g. [sp: Span] after [case Span]). *)
              if is_type_expected expected then
                ret (AstRef name, TypeUniverse)
              else
                tyI <- instantiate_funty ty ;
                match expected with
                | Some exp =>
                    unify fuel' tyI exp ;;
                    ret (AstRef (mangle_name name resolved_ctx), tyI)
                | None => ret (AstRef (mangle_name name resolved_ctx), tyI)
                end
          | _ =>
              match expected with
              | Some exp => unify fuel' ty exp ;; ret (AstRef (mangle_name name resolved_ctx), ty)
              | None => ret (AstRef (mangle_name name resolved_ctx), ty)
              end
          end
      | None =>
          if is_allowed_unbound name then ret (AstRef name, AstRef "Any")
          else if is_type_expected expected then ret (AstRef name, TypeUniverse)
          else throw (append "Unbound variable: " name)
      end
  | StringLiteral s _ => 
      match expected with Some exp => unify fuel' StringType exp | None => ret tt end ;;
      ret (AstStringLit s, StringType)
  | IntegerLiteral s _ => 
      match expected with Some exp => unify fuel' IntType exp | None => ret tt end ;;
      ret (AstIntLit (string_to_nat s), IntType)
  | BoolLiteral b _ => 
      match expected with Some exp => unify fuel' BoolType exp | None => ret tt end ;;
      ret (AstBoolLit b, BoolType)
  | SeqOf exprs span =>
      match exprs with
      | [] => throw "Empty SeqOf"
      | func :: args =>
          funcAst <- elaborate fuel' env func None;
          match fst funcAst with
          | AstRef name => if string_dec name "\\" then throw "Lambda!" else ret tt
          | _ => ret tt
          end ;;
          (* Prenex-instantiate polymorphic FunTy before checking value args.
             Keep raw FunTy only when the first param is a Type (explicit type
             arg / dependent [my_id(T: Type, x: T)] style). *)
          fs0 <-
            match strip_span (snd funcAst) with
            | AstFunTy tps params ret_ty effs =>
                match params with
                | (_, arg_ty) :: _ =>
                    if is_universe_ty arg_ty then
                      ret (AstFunTy tps params ret_ty effs)
                    else
                      fr <- freshen_fun_tps tps params ret_ty ;
                      ret (AstFunTy [] (fst fr) (snd fr) effs)
                | [] =>
                    fr <- freshen_fun_tps tps params ret_ty ;
                    ret (AstFunTy [] (fst fr) (snd fr) effs)
                end
            | other => ret other
            end ;
          let fix check_args (fs : AST) (as_ : list CST) {struct as_} : ElabM (list AST * (AST * EffectSet)) :=
              match as_, fs with
              | [], AstFunTy _ _ ret_ty effs => ret ([], (ret_ty, effs))
              | [], AstPi _ _ ret_ty effs => ret ([], (ret_ty, effs))
              | [], ty => ret ([], (ty, []))
              | a :: rest, AstFunTy tps ((pn, arg_ty) :: params) ret_ty effs =>
                  aAst <- elaborate fuel' env a (Some arg_ty);
                  (* Dependent params (e.g. my_id(T: Type, x: T)): always subst
                     the bound name into remaining param/ret types, like AstPi. *)
                  let params' :=
                    map (fun p => (fst p, subst_ast pn (fst aAst) (snd p))) params
                  in
                  let ret' := subst_ast pn (fst aAst) ret_ty in
                  (* Discharge a leading FunTy type_param when the arg is a type. *)
                  let tps' :=
                    match tps with
                    | tp :: tps0 =>
                        if is_universe_ty arg_ty then tps0 else tps
                    | [] => []
                    end
                  in
                  let params'' :=
                    match tps with
                    | tp :: _ =>
                        if is_universe_ty arg_ty then
                          map (fun p => (fst p, subst_ast tp (fst aAst) (snd p)))
                            params'
                        else params'
                    | [] => params'
                    end
                  in
                  let ret'' :=
                    match tps with
                    | tp :: _ =>
                        if is_universe_ty arg_ty then subst_ast tp (fst aAst) ret'
                        else ret'
                    | [] => ret'
                    end
                  in
                  restAst <- check_args (AstFunTy tps' params'' ret'' effs) rest;
                  ret (fst aAst :: fst restAst, snd restAst)
              | a :: rest, AstFunTy (tp :: tps) [] ret_ty effs =>
                  (* Type argument when value params are exhausted. *)
                  aAst <- elaborate fuel' env a (Some TypeUniverse);
                  let ret' := subst_ast tp (fst aAst) ret_ty in
                  restAst <- check_args (AstFunTy tps [] ret' effs) rest;
                  ret (fst aAst :: fst restAst, snd restAst)
              | a :: rest, AstFunTy [] [] _ret_ty _effs =>
                  throw "too many arguments"
              | a :: rest, AstPi n arg_ty ret_ty effs =>
                  aAst <- elaborate fuel' env a (Some arg_ty);
                  let ret' := subst_ast n (fst aAst) ret_ty in
                  restAst <- check_args ret' rest;
                  match snd restAst with
                  | (rty, e) => ret (fst aAst :: fst restAst, (rty, merge_effects effs e))
                  end
              | a :: rest, AstMeta _ =>
                  argTyM <- fresh_meta;
                  retTyM <- fresh_meta;
                  unify fuel' fs (AstPi "x" argTyM retTyM []);;
                  aAst <- elaborate fuel' env a (Some argTyM);
                  restAst <- check_args retTyM rest;
                  ret (fst aAst :: fst restAst, snd restAst)
              | a :: rest, AstRef n =>
                  if orb (String.eqb n "Any") (String.eqb n "Unknown") then
                    aAst <- elaborate fuel' env a None ;
                    restAst <- check_args (AstRef "Any") rest ;
                    ret (fst aAst :: fst restAst, (AstRef "Any", snd (snd restAst)))
                  else throw "cannot apply non-function"
              | _ :: _, _ => throw "cannot apply non-function"
              end
          in
          argsRes <- check_args fs0 args;
          match snd argsRes with
          | (ret_ty, call_effs) =>
              pending <- get_pending ;
              set_pending (merge_effects pending (effect_labels call_effs)) ;;
              match expected with
              | Some exp => unify fuel' ret_ty exp
              | None => ret tt
              end;; ret (AstApp (fst funcAst) (fst argsRes), ret_ty)
          end
      end
  | Block stmts ret_expr _ => 
      (* Pre-bind every def name so mutual / forward references resolve. *)
      let fix collect_def_binders (ls : list CST) : TypeEnv :=
        match ls with
        | [] => []
        | DefCST name _ _ _ _ span :: rest =>
            ((name, context span), AstRef "Any") :: collect_def_binders rest
        | ModuleCST name _ _ _ span :: rest =>
            ((name, context span), AstModTy []) :: collect_def_binders rest
        | ModuleAliasCST name _ span :: rest =>
            ((name, context span), AstModTy []) :: collect_def_binders rest
        | SignatureCST name _ span :: rest =>
            ((name, context span), AstSignature name []) :: collect_def_binders rest
        | _ :: rest => collect_def_binders rest
        end
      in
      let env_with_defs := app (collect_def_binders stmts) env in
      let fix map_elabs (current_env : TypeEnv) (ls : list CST) : ElabM (list AST * TypeEnv) :=
        match ls with
        | [] => ret ([], current_env)
        | x :: xs => 
            match x with
            | LetCST name value _ span =>
                valueAst <- elaborate fuel' current_env value None ;
                let new_env := ((name, context span), snd valueAst) :: current_env in
                rest <- map_elabs new_env xs ;
                ret (AstLet (mangle_name name (context span)) (fst valueAst) :: fst rest, snd rest)
            | VarCST name value _ span =>
                valueAst <- elaborate fuel' current_env value None ;
                let new_env := ((name, context span), snd valueAst) :: current_env in
                rest <- map_elabs new_env xs ;
                ret (AstVar (mangle_name name (context span)) (fst valueAst) :: fst rest, snd rest)
            | AssignCST name value span =>
                valueAst <- elaborate fuel' current_env value None ;
                rest <- map_elabs current_env xs ;
                ret (AstAssign (mangle_name name (context span)) (fst valueAst) :: fst rest, snd rest)
            | DefCST name _ _ ret_ty _ span =>
                res <- elaborate fuel' current_env x None ;
                let new_env := ((name, context span), snd res) :: current_env in
                rest <- map_elabs new_env xs ;
                ret (fst res :: fst rest, snd rest)
            | ImportCST lang alias modp syms span =>
                rest <- map_elabs current_env xs ;
                ret (AstImport lang alias modp syms :: fst rest, snd rest)
            | ModuleCST name params seal body span =>
                res <- elaborate fuel' current_env x None ;
                let new_env := ((name, context span), snd res) :: current_env in
                rest <- map_elabs new_env xs ;
                ret (fst res :: fst rest, snd rest)
            | ModuleAliasCST name rhs span =>
                res <- elaborate fuel' current_env x None ;
                let new_env := ((name, context span), snd res) :: current_env in
                rest <- map_elabs new_env xs ;
                ret (fst res :: fst rest, snd rest)
            | SignatureCST name body span =>
                res <- elaborate fuel' current_env x None ;
                let new_env := ((name, context span), snd res) :: current_env in
                rest <- map_elabs new_env xs ;
                ret (fst res :: fst rest, snd rest)
            | FunctorAppCST func args span =>
                res <- elaborate fuel' current_env x None ;
                rest <- map_elabs current_env xs ;
                ret (fst res :: fst rest, snd rest)
            | ExternCST lang modp decls span =>
                let (syms, new_env) := elab_extern_decls current_env decls in
                rest <- map_elabs new_env xs ;
                ret (AstImport lang "" modp syms :: fst rest, snd rest)
            | EnumCST name type_params variants span =>
                res <- elaborate fuel' current_env x None ;
                let enum_ret :=
                  match type_params with
                  | [] => AstRef name
                  | _ => AstApp (AstRef name) (map (fun tp => AstRef tp) type_params)
                  end
                in
                let ctors :=
                  match fst res with
                  | AstEnum _ _ vars =>
                      map (fun v =>
                        let '((vname, fields), _) := v in
                        match fields with
                        | [] =>
                            (* Nullary poly ctors stay schemes so lookup
                               prenex-instantiates [Option[T]] → [Option[?m]]. *)
                            ((vname, context span),
                             AstFunTy type_params [] enum_ret [])
                        | _ =>
                            let ps := map (fun ty => ("_", ty)) fields in
                            ((vname, context span),
                             AstFunTy type_params ps enum_ret [])
                        end) vars
                  | _ => []
                  end
                in
                let new_env := (ctors ++ current_env)%list in
                rest <- map_elabs new_env xs ;
                ret (fst res :: fst rest, snd rest)
            | ExtensionCST _ _ _ _ span =>
                res <- elaborate fuel' current_env x None ;
                let meth_binds :=
                  match fst res with
                  | AstExtension _ _ _ meths =>
                      let fix go (ms : list AST) : TypeEnv :=
                        match ms with
                        | [] => []
                        | AstDef n tps ps rt _ effs :: rest =>
                            ((n, context span), AstFunTy tps ps rt effs)
                              :: go rest
                        | AstSpan _ (AstDef n tps ps rt _ effs) :: rest =>
                            ((n, context span), AstFunTy tps ps rt effs)
                              :: go rest
                        | _ :: rest => go rest
                        end
                      in go meths
                  | _ => []
                  end
                in
                let new_env := (meth_binds ++ current_env)%list in
                rest <- map_elabs new_env xs ;
                ret (fst res :: fst rest, snd rest)
            | _ =>
                res <- elaborate fuel' current_env x None ;
                rest <- map_elabs current_env xs ;
                ret (fst res :: fst rest, snd rest)
            end
        end
      in
      stmtsRes <- map_elabs env_with_defs stmts ;
      let stmtsAst := fst stmtsRes in
      let final_env := snd stmtsRes in
      retAst <- elaborate fuel' final_env ret_expr None ;
      ret (AstBlock stmtsAst (fst retAst), snd retAst)
  
  | LetCST name value body span =>
      valueAst <- elaborate fuel' env value None ;
      bodyAst <- elaborate fuel' (((name, context span), snd valueAst) :: env) body expected ;
      ret (AstBlock [AstLet (mangle_name name (context span)) (fst valueAst)] (fst bodyAst), snd bodyAst)

  | VarCST name value body span =>
      valueAst <- elaborate fuel' env value None ;
      bodyAst <- elaborate fuel' (((name, context span), snd valueAst) :: env) body expected ;
      ret (AstBlock [AstVar (mangle_name name (context span)) (fst valueAst)] (fst bodyAst), snd bodyAst)

  | AssignCST name value span =>
      valueAst <- elaborate fuel' env value None ;
      ret (AstAssign (mangle_name name (context span)) (fst valueAst), AstRef "Unit")

  | BoxCST e span =>
      old_pending <- get_pending ;
      set_pending [] ;;
      eAst <- elaborate fuel' env e None ;
      caps <- get_pending ;
      set_pending old_pending ;;
      active <- get_active_caps ;
      (* Evidence capture requires all claimed caps on the lexical stack
         (EaC-lite). Empty capture is not allowed — that silently dropped evidence. *)
      if negb (effects_covered_by caps active) then
        throw "Missing capability for box"
      else
        ret (AstBox (fst eAst) caps,
             AstPi "x" (AstRef "Unit") (snd eAst) [])

  | UnboxCST e span =>
      eAst <- elaborate fuel' env e None ;
      pending <- get_pending ;
      let caps := match snd eAst with
                  | AstPi _ _ _ effs => effect_labels effs
                  | AstFunTy _ _ _ effs => effect_labels effs
                  | _ => []
                  end in
      set_pending (merge_effects pending caps) ;;
      ret (AstUnbox (fst eAst), match snd eAst with
                                | AstPi _ _ ret _ => ret
                                | AstFunTy _ _ ret _ => ret
                                | ty => ty
                                end)
      
  | IfCST cond thenB elseB _ =>
      condAst <- elaborate fuel' env cond None ;
      thenAst <- elaborate fuel' env thenB expected ;
      elseAst <- elaborate fuel' env elseB expected ;
      ret (AstIf (fst condAst) (fst thenAst) (fst elseAst), snd thenAst)
      
  | DefCST name type_params params ret_ty body span =>
      let fix map_params (ps : list (string * CST)) : ElabM (list (string * AST)) :=
        match ps with
        | [] => ret []
        | (pname, pty) :: rest =>
            tyAst <- elaborate fuel' env pty (Some TypeUniverse) ;
            restAst <- map_params rest ;
            ret ((pname, fst tyAst) :: restAst)
        end
      in
      paramsAst <- map_params params ;
      let fix build_env (ps : list (string * AST)) (env0 : TypeEnv) {struct ps} : TypeEnv :=
        match ps with
        | [] => env0
        | (pname, pty) :: rest => build_env rest (((pname, context span), pty) :: env0)
        end
      in
      let (ret_cst, decl_effs) := peel_effect_row_cst ret_ty in
      retAst <- elaborate fuel' env ret_cst (Some TypeUniverse) ;
      (* Bind the def name before the body so recursive calls resolve. *)
      let fun_ty0 := AstFunTy type_params paramsAst (fst retAst) decl_effs in
      let body_env := ((name, context span), fun_ty0) :: build_env paramsAst env in
      old_pending <- get_pending ;
      set_pending [] ;;
      bodyAst <- elaborate fuel' body_env body (Some (fst retAst)) ;
      body_effs <- get_pending ;
      open_effs <- with_open_row body_effs ;
      let body_term := fst bodyAst in
      let needs_div :=
        andb (Chester.CoreChecker.free_in name body_term)
          (negb (Chester.CoreChecker.check_terminates name paramsAst body_term))
      in
      let ensure_div (es : EffectSet) : EffectSet :=
        if needs_div then add_effect (UserEffect "div") es else es
      in
      match decl_effs with
      | [] =>
          (* Unannotated: infer [div] for recursion that is not structurally
             decreasing. Callers still see other body effects via pending. *)
          set_pending (merge_effects old_pending body_effs) ;;
          let effs := ensure_div open_effs in
          let fun_ty := AstFunTy type_params paramsAst (fst retAst) effs in
          ret (AstDef name type_params paramsAst (fst retAst) body_term effs, fun_ty)
      | _ =>
          (* Annotated: effects are latent on the FunTy only — not performed
             at the definition site. A closed row must mention [div] when the
             body is recursive and not structurally decreasing. *)
          set_pending old_pending ;;
          if andb needs_div (negb (Chester.CoreChecker.effect_has_div decl_effs)) then
            throw ("Termination check failed: recursive function '" ++ name ++ "' must declare the 'div' effect")
          else if effect_row_subsumes body_effs decl_effs then
            let fun_ty := AstFunTy type_params paramsAst (fst retAst) decl_effs in
            (* Annotated AstDef nodes keep the declared row (not open_effs). *)
            ret (AstDef name type_params paramsAst (fst retAst) body_term decl_effs, fun_ty)
          else
            throw ("effect row too large for declared annotation on: " ++ name)
      end

  | LamCST arg_name opt_arg_ty body span =>
      argTyAst <- (match opt_arg_ty with
                   | Some ty => elaborate fuel' env ty (Some TypeUniverse)
                   | None => m <- fresh_meta ; ret (m, TypeUniverse)
                   end) ;
      (* In type position, `\x: T => U` / `(x: T) -> U` elaborate to AstPi. *)
      if is_type_expected expected then
        bodyAst <- elaborate fuel' (((arg_name, context span), fst argTyAst) :: env) body (Some TypeUniverse) ;
        let piTy := AstPi arg_name (fst argTyAst) (fst bodyAst) [] in
        ret (piTy, TypeUniverse)
      else
      (* When checking against an expected Pi (e.g. map's [f]), solve the
         binder meta from [expArg] before elaborating the body. *)
      arg_ty <-
        match expected with
        | Some exp =>
            match strip_span (whnf exp) with
            | AstPi _ expArg _ _ =>
                unify fuel' (fst argTyAst) expArg ;;
                zonk (S (ast_size (fst argTyAst))) (fst argTyAst)
            | _ => ret (fst argTyAst)
            end
        | None => ret (fst argTyAst)
        end ;
      old_pending <- get_pending ;
      set_pending [] ;;
      let body_exp :=
        match expected with
        | Some exp =>
            match strip_span (whnf exp) with
            | AstPi _ _ expRet _ => Some expRet
            | _ => None
            end
        | None => None
        end
      in
      bodyAst <- elaborate fuel' (((arg_name, context span), arg_ty) :: env) body body_exp ;
      body_effs <- get_pending ;
      open_effs <- with_open_row body_effs ;
      (* Latent effects live on the Pi only — not performed at lambda creation
         (same rule as annotated [def]). *)
      set_pending old_pending ;;
      (* Latent [open_effs] sit on the Pi; do not [unify] the whole arrow against
         an expected effect-free Pi — that rejected [\x => …] at map call sites. *)
      let arrTy := AstPi arg_name arg_ty (snd bodyAst) open_effs in
      ret (AstLam (mangle_name arg_name (context span)) arg_ty (fst bodyAst), arrTy)

  | AppCST func args span =>
      (* Type application Option(T) / Result(T, E) uses (); [] is implicits only. *)
      if is_type_expected expected then
        funcAst <- elaborate fuel' env func (Some TypeUniverse) ;
        let fix check_targs (as_ : list CST) : ElabM (list AST) :=
          match as_ with
          | [] => ret []
          | a :: rest =>
              aAst <- elaborate fuel' env a (Some TypeUniverse) ;
              restAst <- check_targs rest ;
              ret (fst aAst :: restAst)
          end
        in
        argsRes <- check_targs args ;
        ret (AstApp (fst funcAst) argsRes, TypeUniverse)
      else
      match func with
      | FieldAccessCST expr field fsp =>
          exprAst <- elaborate fuel' env expr None ;
          s <- get_state ;
          match s with mkElabState _ _ exts _ _ _ go_pkgs =>
          let fix elab_args (as_ : list CST) : ElabM (list AST) :=
            match as_ with
            | [] => ret []
            | a :: rest =>
                aAst <- elaborate fuel' env a None ;
                restAst <- elab_args rest ;
                ret (fst aAst :: restAst)
            end
          in
          let fix check_mod_args (fs : AST) (as_ : list CST)
              : ElabM (list AST * (AST * EffectSet)) :=
            match as_, fs with
            | [], AstFunTy _ _ ret_ty e => ret ([], (ret_ty, e))
            | [], AstPi _ _ ret_ty e => ret ([], (ret_ty, e))
            | [], ty => ret ([], (ty, []))
            | a :: rest, AstFunTy tps ((pn, arg_ty) :: params) ret_ty effs =>
                aAst <- elaborate fuel' env a (Some arg_ty) ;
                let params' :=
                  map (fun p => (fst p, subst_ast pn (fst aAst) (snd p))) params
                in
                let ret' := subst_ast pn (fst aAst) ret_ty in
                restAst <- check_mod_args (AstFunTy tps params' ret' effs) rest ;
                ret (fst aAst :: fst restAst, snd restAst)
            | a :: rest, AstPi n arg_ty ret_ty _e =>
                aAst <- elaborate fuel' env a (Some arg_ty) ;
                let ret' := subst_ast n (fst aAst) ret_ty in
                restAst <- check_mod_args ret' rest ;
                ret (fst aAst :: fst restAst, snd restAst)
            | a :: rest, _ =>
                aAst <- elaborate fuel' env a None ;
                restAst <- check_mod_args (AstRef "Any") rest ;
                ret (fst aAst :: fst restAst, (AstRef "Any", []))
            end
          in
          let charge_mod_call (ty0 : AST) (args : list CST)
              : ElabM (list AST * AST) :=
            ty <- instantiate_funty ty0 ;
            let call_effs :=
              match strip_span ty with
              | AstFunTy _ _ _ e => e
              | AstPi _ _ _ e => e
              | _ => []
              end
            in
            argsRes <- check_mod_args ty args ;
            pending <- get_pending ;
            set_pending (merge_effects pending (effect_labels call_effs)) ;;
            ret (fst argsRes, fst (snd argsRes))
          in
          match expr with
          | Symbol pkg_name _ =>
              match lookup_go_method pkg_name field go_pkgs with
              | Some _ =>
                  argsAst <- elab_args args ;
                  ret (AstApp (AstFieldAccess (fst exprAst) field) argsAst, AstRef "Any")
              | None =>
                  match snd exprAst with
                  | AstModTy exports =>
                      match lookup_export exports field with
                      | Some ty0 =>
                          argsRes <- charge_mod_call ty0 args ;
                          ret (AstApp (AstFieldAccess (fst exprAst) field)
                                (fst argsRes), snd argsRes)
                      | None => throw ("module has no export: " ++ field)
                      end
                  | _ =>
                      (* Extension method, else FFI/Go package call (typed Any). *)
                      let full_name := find_ext_method (snd exprAst) field exts in
                      if string_dec full_name EmptyString then
                        argsAst <- elab_args args ;
                        ret (AstApp (AstFieldAccess (fst exprAst) field) argsAst,
                             AstRef "Any")
                      else
                        let new_cst := AppCST (Symbol full_name fsp) (expr :: args) span in
                        elaborate fuel' env new_cst expected
                  end
              end
          | _ =>
              match snd exprAst with
              | AstModTy exports =>
                  match lookup_export exports field with
                  | Some ty0 =>
                      argsRes <- charge_mod_call ty0 args ;
                      ret (AstApp (AstFieldAccess (fst exprAst) field)
                            (fst argsRes), snd argsRes)
                  | None => throw ("module has no export: " ++ field)
                  end
              | _ =>
                  let full_name := find_ext_method (snd exprAst) field exts in
                  if string_dec full_name EmptyString then
                    argsAst <- elab_args args ;
                    ret (AstApp (AstFieldAccess (fst exprAst) field) argsAst,
                         AstRef "Any")
                  else
                    let new_cst := AppCST (Symbol full_name fsp) (expr :: args) span in
                    elaborate fuel' env new_cst expected
              end
          end
          end
      | ImplicitAppCST inner_func _targs _tspan =>
          (* Erase the implicit [A,B] telescope; elaborate fuel' only the inner function and explicit args *)
          funcAst <- elaborate fuel' env inner_func None ;
          let fix check_args (fs : AST) (as_ : list CST) {struct as_} : ElabM (list AST * AST) :=
            match as_ with
            | [] =>
                match fs with
                | AstFunTy _ [] ret_ty _ => ret ([], ret_ty)
                | _ => ret ([], fs)
                end
            | a :: rest =>
                match fs with
                | AstFunTy _params params ret_ty _ =>
                    (* Non-curried: elaborate all explicit args against the param list *)
                    match params with
                    | (pn, arg_ty) :: _rest_params =>
                        aAst <- elaborate fuel' env a (Some arg_ty) ;
                        let rest_params' :=
                          map (fun p => (fst p, subst_ast pn (fst aAst) (snd p)))
                            _rest_params
                        in
                        let ret' := subst_ast pn (fst aAst) ret_ty in
                        let next_fs :=
                          match rest_params' with
                          | [] => ret'
                          | _ => AstFunTy [] rest_params' ret' []
                          end
                        in
                        restAst <- check_args next_fs rest ;
                        ret (fst aAst :: fst restAst, snd restAst)
                    | [] =>
                        aAst <- elaborate fuel' env a None ;
                        restAst <- check_args (AstRef "Any") rest ;
                        ret (fst aAst :: fst restAst, AstRef "Any")
                    end
                | AstPi n arg_ty ret_ty _ =>
                    aAst <- elaborate fuel' env a (Some arg_ty) ;
                    let ret' := subst_ast n (fst aAst) ret_ty in
                    restAst <- check_args ret' rest ;
                    ret (fst aAst :: fst restAst, snd restAst)
                | AstRef _ =>
                    aAst <- elaborate fuel' env a None ;
                    restAst <- check_args (AstRef "Any") rest ;
                    ret (fst aAst :: fst restAst, AstRef "Any")
                | _ =>
                    aAst <- elaborate fuel' env a None ;
                    restAst <- check_args (AstRef "Any") rest ;
                    ret (fst aAst :: fst restAst, AstRef "Any")
                end
            end
          in
          fs0 <- instantiate_funty (snd funcAst) ;
          argsRes <- check_args fs0 args ;
          retTyZ <- zonk (S (ast_size (snd argsRes))) (snd argsRes) ;
          let call_effs :=
            match strip_span (snd funcAst) with
            | AstFunTy _ _ _ e => e
            | AstPi _ _ _ e => e
            | _ => []
            end
          in
          pending <- get_pending ;
          set_pending (merge_effects pending (effect_labels call_effs)) ;;
          match expected with
          | Some exp => unify fuel' retTyZ exp
          | None => ret tt
          end ;;
          ret (AstApp (fst funcAst) (fst argsRes), retTyZ)
      | _ =>
          funcAst <- elaborate fuel' env func None ;
          let fix check_args (fs : AST) (as_ : list CST) {struct as_} : ElabM (list AST * AST) :=
            match as_ with
            | [] =>
                match fs with
                | AstFunTy _ [] ret_ty _ => ret ([], ret_ty)
                | _ => ret ([], fs)
                end
            | a :: rest =>
                match fs with
                | AstFunTy _params params ret_ty _ =>
                    match params with
                    | (pn, arg_ty) :: _rest_params =>
                        aAst <- elaborate fuel' env a (Some arg_ty) ;
                        let rest_params' :=
                          map (fun p => (fst p, subst_ast pn (fst aAst) (snd p)))
                            _rest_params
                        in
                        let ret' := subst_ast pn (fst aAst) ret_ty in
                        let next_fs :=
                          match rest_params' with
                          | [] => ret'
                          | _ => AstFunTy [] rest_params' ret' []
                          end
                        in
                        restAst <- check_args next_fs rest ;
                        ret (fst aAst :: fst restAst, snd restAst)
                    | [] =>
                        aAst <- elaborate fuel' env a None ;
                        restAst <- check_args (AstRef "Any") rest ;
                        ret (fst aAst :: fst restAst, AstRef "Any")
                    end
                | AstPi n arg_ty ret_ty _ =>
                    aAst <- elaborate fuel' env a (Some arg_ty) ;
                    let ret' := subst_ast n (fst aAst) ret_ty in
                    restAst <- check_args ret' rest ;
                    ret (fst aAst :: fst restAst, snd restAst)
                | AstMeta _ =>
                    argTyM <- fresh_meta ;
                    retTyM <- fresh_meta ;
                    unify fuel' fs (AstPi "x" argTyM retTyM []) ;;
                    aAst <- elaborate fuel' env a (Some argTyM) ;
                    restAst <- check_args retTyM rest ;
                    ret (fst aAst :: fst restAst, snd restAst)
                | AstRef _ =>
                    aAst <- elaborate fuel' env a None ;
                    restAst <- check_args (AstRef "Any") rest ;
                    ret (fst aAst :: fst restAst, AstRef "Any")
                | _ =>
                    aAst <- elaborate fuel' env a None ;
                    restAst <- check_args (AstRef "Any") rest ;
                    ret (fst aAst :: fst restAst, AstRef "Any")
                end
            end
          in
          fs0 <- instantiate_funty (snd funcAst) ;
          argsRes <- check_args fs0 args ;
          retTyZ <- zonk (S (ast_size (snd argsRes))) (snd argsRes) ;
          let call_effs :=
            match strip_span (snd funcAst) with
            | AstFunTy _ _ _ e => e
            | AstPi _ _ _ e => e
            | _ => []
            end
          in
          pending <- get_pending ;
          set_pending (merge_effects pending (effect_labels call_effs)) ;;
          match expected with
          | Some exp => unify fuel' retTyZ exp
          | None => ret tt
          end ;;
          ret (AstApp (fst funcAst) (fst argsRes), retTyZ)
      end

  | ImplicitAppCST func args _ =>
      (* Implicit application f[A,B] (value-level). Type apps use Option(T). *)
      funcAst <- elaborate fuel' env func None ;
      let fix check_targs (as_ : list CST) : ElabM (list AST) :=
        match as_ with
        | [] => ret []
        | a :: rest =>
            aAst <- elaborate fuel' env a (Some TypeUniverse) ;
            restAst <- check_targs rest ;
            ret (fst aAst :: restAst)
        end
      in
      argsRes <- check_targs args ;
      let app_ty := AstApp (fst funcAst) argsRes in
      if is_type_expected expected then
        ret (app_ty, TypeUniverse)
      else
        ret (AstImplicitApp (fst funcAst) argsRes, app_ty)

  | EnumCST name type_params variants _ =>
      let env_enum :=
        let fix bind_tps (tps : list string) (e0 : TypeEnv) : TypeEnv :=
          match tps with
          | [] => e0
          | tp :: rest => bind_tps rest (((tp, []), TypeUniverse) :: e0)
          end
        in
        bind_tps type_params env
      in
      let enum_ret :=
        match type_params with
        | [] => AstRef name
        | _ => AstApp (AstRef name) (map (fun tp => AstRef tp) type_params)
        end
      in
      let fix elab_fields (fs : list CST) : ElabM (list AST) :=
        match fs with
        | [] => ret []
        | f :: rest =>
            let fty :=
              match f with
              | SeqOf (Symbol _ _ :: Symbol ":" _ :: ty :: _) _ => ty
              | SeqOf (_ :: Symbol ":" _ :: ty :: _) _ => ty
              | other => other
              end
            in
            tyAst <- elaborate fuel' env_enum fty (Some TypeUniverse) ;
            restFs <- elab_fields rest ;
            ret (fst tyAst :: restFs)
        end
      in
      let fix elab_variant (v : CST) : ElabM (string * list AST * AST) :=
        match v with
        | AppCST (Symbol ":" _) (AppCST (Symbol vname _) args _ :: ret_ty :: []) _ =>
            fields <- elab_fields args ;
            retAst <- elaborate fuel' env_enum ret_ty (Some TypeUniverse) ;
            ret ((vname, fields), fst retAst)
        | AppCST (Symbol ":" _) (Symbol vname _ :: ret_ty :: []) _ =>
            retAst <- elaborate fuel' env_enum ret_ty (Some TypeUniverse) ;
            ret ((vname, []), fst retAst)
        | AppCST (Symbol vname _) args _ =>
            fields <- elab_fields args ;
            ret ((vname, fields), enum_ret)
        | Symbol vname _ => ret ((vname, []), enum_ret)
        | _ => throw "malformed enum variant"
        end
      in
      let fix elab_variants (vs : list CST)
          : ElabM (list (string * list AST * AST)) :=
        match vs with
        | [] => ret []
        | v :: rest =>
            vAst <- elab_variant v ;
            restVs <- elab_variants rest ;
            ret (vAst :: restVs)
        end
      in
      variantsAst <- elab_variants variants ;
      ret (AstEnum name type_params variantsAst, AstRef "Unit")

  | MatchCST expr cases _ =>
      exprAst <- elaborate fuel' env expr None ;
      let scrut_ty := snd exprAst in
      let fix elab_cases (cs : list (PatternCST * CST)) : ElabM (list (PatternAST * AST) * AST) :=
        match cs with
        | [] => throw "Empty match not allowed"
        | (PatWildcardCST _, body) :: _ =>
            bodyAst <- elaborate fuel' env body expected ;
            ret ([(PatWildcard, fst bodyAst)], snd bodyAst)
        | (PatVarCST v span, body) :: _ =>
            bodyAst <- elaborate fuel'
                         (((v, context span), scrut_ty) :: env) body expected ;
            ret ([(PatVar v, fst bodyAst)], snd bodyAst)
        | (PatConstructorCST name vars span, body) :: _ =>
            let ctor_ty0 :=
              match find (fun p => String.eqb (fst (fst p)) name) env with
              | Some (_, ty) => ty
              | None => AstRef "Any"
              end
            in
            (* Prenex-instantiate the ctor scheme, then unify its result with the
               scrutinee so binders see [A] from [Option[A]], not the scheme [T]. *)
            ctor_tyI <- instantiate_funty ctor_ty0 ;
            let ctor_ret :=
              match strip_span ctor_tyI with
              | AstFunTy _ _ r _ => r
              | AstPi _ _ r _ => r
              | other => other
              end
            in
            unify fuel' ctor_ret scrut_ty ;;
            ctor_tyZ <- zonk (S (ast_size ctor_tyI)) ctor_tyI ;
            let arg_tys :=
              match strip_span ctor_tyZ with
              | AstFunTy _ params _ _ =>
                  map snd (firstn (List.length vars) params)
              | _ =>
                  ctor_arg_tys
                    (S (Nat.add (ast_size ctor_tyZ) (List.length vars)))
                    ctor_tyZ (List.length vars)
              end
            in
            let fix add_vars (vs : list string) (tys : list AST) (e : TypeEnv)
                {struct vs} : TypeEnv :=
              match vs, tys with
              | [], _ => e
              | v :: rest_vs, ty :: rest_tys =>
                  add_vars rest_vs rest_tys (((v, context span), ty) :: e)
              | v :: rest_vs, [] =>
                  add_vars rest_vs [] (((v, context span), AstRef "Any") :: e)
              end
            in
            let case_env := add_vars vars arg_tys env in
            bodyAst <- elaborate fuel' case_env body expected ;
            ret ([(PatConstructor name vars, fst bodyAst)], snd bodyAst)
        end
      in
      let fix process_cases (cs : list (PatternCST * CST)) : ElabM (list (PatternAST * AST) * AST) :=
        match cs with
        | [] => ret ([], AstRef "Any")
        | [(pat, body) as single] => elab_cases [single]
        | (pat, body) as single :: (_ :: _) as rest =>
            res_first <- elab_cases [(pat, body)] ;
            res_rest <- process_cases rest ;
            unify fuel' (snd res_first) (snd res_rest) ;;
            ret (app (fst res_first) (fst res_rest), snd res_first)
        end
      in
      casesRes <- process_cases cases ;
      ret (AstMatch (fst exprAst) (fst casesRes), snd casesRes)

  | Tuple elems _ =>
      let fix elab_elems (es : list CST) : ElabM (list AST * AST) :=
        match es with
        | [] => ret ([], AstRef "Unit")
        | x :: xs =>
            let exp := if is_type_expected expected then Some TypeUniverse else None in
            xAst <- elaborate fuel' env x exp;
            restAst <- elab_elems xs;
            ret (fst xAst :: fst restAst, AstRef "Tuple")
        end
      in
      elemsRes <- elab_elems elems;
      ret (AstTuple (fst elemsRes), snd elemsRes)

  | ListLiteral elems _ =>
      let fix elab_elems (es : list CST) : ElabM (list AST * AST) :=
        match es with
        | [] => ret ([], AstRef "List")
        | x :: xs =>
            xAst <- elaborate fuel' env x None;
            restAst <- elab_elems xs;
            ret (fst xAst :: fst restAst, AstRef "List")
        end
      in
      elemsRes <- elab_elems elems;
      ret (AstTuple (fst elemsRes), snd elemsRes)

  | CommentCST msg _ => ret (AstRef "Unit", AstRef "Unit")
  | ImportCST lang alias modp syms _ =>
      ret (AstImport lang alias modp syms, AstRef "Unit")
  | SigValCST name tps params ret_ty span =>
      let fix elab_params (ps : list (string * CST)) : ElabM (list (string * AST)) :=
        match ps with
        | [] => ret []
        | (n, ty) :: rest =>
            tyAst <- elaborate fuel' env ty (Some TypeUniverse) ;
            restPs <- elab_params rest ;
            ret ((n, fst tyAst) :: restPs)
        end
      in
      paramsAst <- elab_params params ;
      let (ret_cst, decl_effs) := peel_effect_row_cst ret_ty in
      retAst <- elaborate fuel' env ret_cst (Some TypeUniverse) ;
      ret (AstSigVal name tps paramsAst (fst retAst) decl_effs, AstRef "Unit")
  | ModuleCST name params seal body span =>
      let fix elab_fparams (ps : list (string * CST)) (e0 : TypeEnv)
          : ElabM (list (string * AST) * TypeEnv) :=
        match ps with
        | [] => ret ([], e0)
        | (pn, pty) :: rest =>
            tyAst <- elaborate fuel' e0 pty None ;
            let pty_ast := fst tyAst in
            let pmodty :=
              match snd tyAst with
              | AstSignature _ decls =>
                  AstModTy (module_exports_of decls)
              | AstModTy ex => AstModTy ex
              | other => other
              end
            in
            let e1 := ((pn, context span), pmodty) :: e0 in
            restRes <- elab_fparams rest e1 ;
            ret ((pn, pty_ast) :: fst restRes, snd restRes)
        end
      in
      paramsRes <- elab_fparams params env ;
      let params_ast := fst paramsRes in
      let benv0 := snd paramsRes in
      let fix elab_mod_body (benv : TypeEnv) (cs : list CST)
          (acc_ex : list (string * AST))
          : ElabM (list AST * TypeEnv * list (string * AST)) :=
        match cs with
        | [] => ret ([], benv, rev acc_ex)
        | c :: cs' =>
            r <- elaborate fuel' benv c None ;
            let '(benv', acc_ex') :=
              match c, fst r, snd r with
              | DefCST n _ _ _ _ sp, AstDef _ tps ps rt _ _, ty =>
                  (((n, context sp), ty) :: benv, (n, ty) :: acc_ex)
              | TypeDeclCST n _ sp, AstTypeDecl _ opt, _ =>
                  (((n, context sp), AstTypeDecl n opt) :: benv,
                   (n, AstTypeDecl n opt) :: acc_ex)
              | ModuleCST n _ _ _ sp, _, ty =>
                  (((n, context sp), ty) :: benv, (n, ty) :: acc_ex)
              | SignatureCST n _ sp, _, ty =>
                  (((n, context sp), ty) :: benv, (n, ty) :: acc_ex)
              | _, _, _ => (benv, acc_ex)
              end
            in
            rest <- elab_mod_body benv' cs' acc_ex' ;
            match rest with
            | (asts, env_f, ex_f) => ret (fst r :: asts, env_f, ex_f)
            end
        end
      in
      bodyRes <- elab_mod_body benv0 body [] ;
      let body_ast := match bodyRes with (a, _, _) => a end in
      let full_ex := match bodyRes with (_, _, ex) => ex end in
      let fix keep_sealed (sealed : list (string * AST)) (ls : list AST) : list AST :=
        match ls with
        | [] => []
        | AstDef n tps ps rt b effs :: xs =>
            match lookup_export sealed n with
            | Some _ => AstDef n tps ps rt b effs :: keep_sealed sealed xs
            | None => keep_sealed sealed xs
            end
        | AstTypeDecl n opt :: xs =>
            match lookup_export sealed n with
            | Some (AstTypeDecl _ opt') => AstTypeDecl n opt' :: keep_sealed sealed xs
            | Some _ => AstTypeDecl n opt :: keep_sealed sealed xs
            | None => keep_sealed sealed xs
            end
        | AstSpan sp (AstDef n tps ps rt b effs) :: xs =>
            match lookup_export sealed n with
            | Some _ => AstSpan sp (AstDef n tps ps rt b effs) :: keep_sealed sealed xs
            | None => keep_sealed sealed xs
            end
        | AstSpan sp (AstTypeDecl n opt) :: xs =>
            match lookup_export sealed n with
            | Some (AstTypeDecl _ opt') =>
                AstSpan sp (AstTypeDecl n opt') :: keep_sealed sealed xs
            | Some _ => AstSpan sp (AstTypeDecl n opt) :: keep_sealed sealed xs
            | None => keep_sealed sealed xs
            end
        | AstModule n params seal0 body0 :: xs =>
            match lookup_export sealed n with
            | Some _ =>
                AstModule n params seal0 body0 :: keep_sealed sealed xs
            | None => keep_sealed sealed xs
            end
        | AstSignature n decls :: xs =>
            match lookup_export sealed n with
            | Some _ => AstSignature n decls :: keep_sealed sealed xs
            | None => keep_sealed sealed xs
            end
        | AstEnum n tps vars :: xs =>
            match lookup_export sealed n with
            | Some _ => AstEnum n tps vars :: keep_sealed sealed xs
            | None => keep_sealed sealed xs
            end
        | AstRecord n tps fields :: xs =>
            match lookup_export sealed n with
            | Some _ => AstRecord n tps fields :: keep_sealed sealed xs
            | None => keep_sealed sealed xs
            end
        | AstSpan sp (AstModule n params seal0 body0) :: xs =>
            match lookup_export sealed n with
            | Some _ =>
                AstSpan sp (AstModule n params seal0 body0)
                  :: keep_sealed sealed xs
            | None => keep_sealed sealed xs
            end
        | AstSpan sp (AstSignature n decls) :: xs =>
            match lookup_export sealed n with
            | Some _ =>
                AstSpan sp (AstSignature n decls) :: keep_sealed sealed xs
            | None => keep_sealed sealed xs
            end
        | x :: xs => x :: keep_sealed sealed xs
        end
      in
      let apply_sig_seal (opaque : bool) (sig_ty : AST) (seal_node : AST)
          : ElabM (option AST * AST * list AST * bool) :=
        match sig_ty with
        | AstSignature _ decls =>
            match seal_exports_check opaque full_ex decls with
            | inl err => throw err
            | inr sealed =>
                let body' := if opaque then keep_sealed sealed body_ast else body_ast in
                (* Both : and :> expose the ascribed view; opaque abstracts types
                   and drops extra body defs. *)
                ret (Some seal_node, AstModTy sealed, body', false)
            end
        | AstSigWith base eqs =>
            match apply_sig_with base eqs with
            | AstSignature _ decls =>
                match seal_exports_check opaque full_ex decls with
                | inl err => throw err
                | inr sealed =>
                    let body' := if opaque then keep_sealed sealed body_ast else body_ast in
                    ret (Some seal_node, AstModTy sealed, body', false)
                end
            | _ => throw "invalid signature enrichment for sealing"
            end
        | AstModTy ex =>
            ret (Some seal_node, AstModTy ex, body_ast, false)
        | _ => ret (Some seal_node, AstModTy full_ex, body_ast, false)
        end
      in
      sealRes <-
        match seal with
        | None => ret (None, AstModTy full_ex, body_ast, false)
        | Some (Symbol "#applicative" _) =>
            ret (Some (AstRef "#applicative"), AstModTy full_ex, body_ast, true)
        | Some (AppCST (Symbol "#opaque" _) [inner] _) =>
            sAst <- elaborate fuel' env inner None ;
            apply_sig_seal true (snd sAst) (AstApp (AstRef "#opaque") [fst sAst])
        | Some (AppCST (Symbol "#transparent" _) [inner] _) =>
            sAst <- elaborate fuel' env inner None ;
            apply_sig_seal false (snd sAst) (AstApp (AstRef "#transparent") [fst sAst])
        | Some s0 =>
            sAst <- elaborate fuel' env s0 None ;
            apply_sig_seal true (snd sAst) (fst sAst)
        end ;
      let mod_ty :=
        match params_ast, sealRes with
        | [], (_, ty, _, _) => ty
        | _, (s, _, b, _) => AstModule name params_ast s b
        end
      in
      match sealRes with
      | (s, _, b, is_app) =>
          let s' :=
            if is_app then
              match s with
              | None => Some (AstRef "#applicative")
              | Some other => Some (AstApp (AstRef "#applicative") [other])
              end
            else s
          in
          ret (AstModule name params_ast s' b, mod_ty)
      end
  | SignatureCST name body span =>
      let fix elab_sig_body (benv : TypeEnv) (cs : list CST)
          : ElabM (list AST) :=
        match cs with
        | [] => ret []
        | c :: cs' =>
            r <- elaborate fuel' benv c None ;
            let benv' :=
              match c, fst r with
              | TypeDeclCST n _ sp, AstTypeDecl _ opt =>
                  ((n, context sp), AstTypeDecl n opt) :: benv
              | SigValCST n _ _ _ sp, AstSigVal _ tps ps rt effs =>
                  ((n, context sp), AstFunTy tps ps rt effs) :: benv
              | _, _ => benv
              end
            in
            rest <- elab_sig_body benv' cs' ;
            ret (fst r :: rest)
        end
      in
      sig_ast <- elab_sig_body env body ;
      (* [signature Alias = Base with type t = T] expands to a single
         SigWithCST body member; store the enriched flat decls so sealing
         against Alias does not see a nested AstSigWith. *)
      let sig_ast' :=
        match sig_ast with
        | [AstSigWith base eqs] =>
            match apply_sig_with base eqs with
            | AstSignature _ decls =>
                flatten_sig_decls (sig_decls_fuel decls) decls
            | _ => sig_ast
            end
        | _ => sig_ast
        end
      in
      ret (AstSignature name sig_ast', AstSignature name sig_ast')
  | SigWithCST base eqs span =>
      baseAst <- elaborate fuel' env base None ;
      let fix elab_eqs (es : list (string * CST)) : ElabM (list (string * AST)) :=
        match es with
        | [] => ret []
        | (n, ty) :: rest =>
            tyAst <- elaborate fuel' env ty (Some TypeUniverse) ;
            restEq <- elab_eqs rest ;
            ret ((n, fst tyAst) :: restEq)
        end
      in
      eqsAst <- elab_eqs eqs ;
      let sig_view :=
        match strip_span (snd baseAst) with
        | AstSigWith b eqs0 => apply_sig_with b eqs0
        | other => other
        end
      in
      let flat_decls :=
        match sig_view with
        | AstSignature _ decls =>
            flatten_sig_decls (sig_decls_fuel decls) decls
        | _ => []
        end
      in
      let fix type_names (ds : list AST) : list string :=
        match ds with
        | [] => []
        | AstTypeDecl n _ :: rest => n :: type_names rest
        | _ :: rest => type_names rest
        end
      in
      let names := type_names flat_decls in
      let fix check_eq_names (es : list (string * AST)) : ElabM unit :=
        match es with
        | [] => ret tt
        | (n, _) :: rest =>
            if existsb (String.eqb n) names then check_eq_names rest
            else throw ("unknown type component in with type: " ++ n)
        end
      in
      check_eq_names eqsAst ;;
      (* Store the signature *type* view as the with-base so later
         flatten_sig_decls / sealing can apply equations (AstRef alone
         is not enough when [signature Alias = Base with type …]
         nests SigWith inside SignatureCST). *)
      let base_for_with :=
        match strip_span (snd baseAst) with
        | AstSignature _ _ as s => s
        | AstSigWith b e0 => apply_sig_with b e0
        | _ => fst baseAst
        end
      in
      let with_ast := AstSigWith base_for_with eqsAst in
      let enriched := apply_sig_with base_for_with eqsAst in
      ret (with_ast, enriched)
  | FileImportCST name path span =>
      (* Host CLI rewrites these to ModuleCST before elaborate; leftover is an error. *)
      throw ("unresolved file import: " ++ name ++ " " ++ path)
  | PackCST m sig span =>
      mAst <- elaborate fuel' env m None ;
      sigAst <- elaborate fuel' env sig None ;
      let mex :=
        match strip_span (snd mAst) with
        | AstModTy ex => ex
        | AstModule _ _ _ body => module_exports_of body
        | _ => module_exports_of [fst mAst]
        end
      in
      let sig_view :=
        match strip_span (snd sigAst) with
        | AstSigWith base eqs => apply_sig_with base eqs
        | other => other
        end
      in
      match sig_view with
      | AstSignature _ decls =>
          match seal_exports_check true mex decls with
          | inl err => throw err
          | inr sealed =>
              ret (AstPack (fst mAst) (fst sigAst), AstModTy sealed)
          end
      | AstModTy ex =>
          ret (AstPack (fst mAst) (fst sigAst), AstModTy ex)
      | other =>
          ret (AstPack (fst mAst) (fst sigAst), other)
      end
  | UnpackCST x sig e body span =>
      sigAst <- elaborate fuel' env sig None ;
      eAst <- elaborate fuel' env e None ;
      let sig_view :=
        match strip_span (snd sigAst) with
        | AstSigWith base eqs => apply_sig_with base eqs
        | other => other
        end
      in
      match sig_view with
      | AstSignature _ decls =>
          match strip_span (snd eAst) with
          | AstModTy ex =>
              match seal_exports_check true ex decls with
              | inl err => throw err
              | inr sealed =>
                  let modty := AstModTy sealed in
                  bodyAst <- elaborate fuel'
                               (((x, context span), modty) :: env) body expected ;
                  ret (AstUnpack x (fst sigAst) (fst eAst) (fst bodyAst),
                       snd bodyAst)
              end
          | other =>
              let expected_mod := AstModTy (module_exports_of decls) in
              if types_compat other expected_mod then
                bodyAst <- elaborate fuel'
                             (((x, context span), expected_mod) :: env)
                             body expected ;
                ret (AstUnpack x (fst sigAst) (fst eAst) (fst bodyAst),
                     snd bodyAst)
              else throw "unpack value does not match signature"
          end
      | AstModTy ex =>
          bodyAst <- elaborate fuel'
                       (((x, context span), AstModTy ex) :: env) body expected ;
          ret (AstUnpack x (fst sigAst) (fst eAst) (fst bodyAst), snd bodyAst)
      | _ =>
          bodyAst <- elaborate fuel'
                       (((x, context span), AstModTy []) :: env) body expected ;
          ret (AstUnpack x (fst sigAst) (fst eAst) (fst bodyAst), snd bodyAst)
      end
  | ModuleAliasCST name rhs span =>
      rhsAst <- elaborate fuel' env rhs None ;
      ret (AstModule name [] None
             match fst rhsAst with
             | AstModule _ _ _ b => b
             | other => [other]
             end,
           snd rhsAst)
  | FunctorAppCST func args span =>
      funcAst <- elaborate fuel' env func None ;
      let fix elab_args (as_ : list CST) : ElabM (list AST * list AST) :=
        match as_ with
        | [] => ret ([], [])
        | a :: rest =>
            aAst <- elaborate fuel' env a None ;
            restAst <- elab_args rest ;
            ret (fst aAst :: fst restAst, snd aAst :: snd restAst)
        end
      in
      argsRes <- elab_args args ;
      match snd funcAst with
      | AstModule fname ((_ :: _) as fparams) seal body =>
          let is_app :=
            match seal with
            | Some (AstRef "#applicative") => true
            | Some (AstApp (AstRef "#applicative") _) => true
            | _ => false
            end
          in
          let fix arg_exports (aty : AST) : list (string * AST) :=
            match strip_span aty with
            | AstModTy ex => ex
            | AstModule _ _ _ b => module_exports_of b
            | AstSignature _ decls => module_exports_of decls
            | _ => []
            end
          in
          (* Functor formals are often stored as AstRef "Printable"; resolve
             signature / ModTy names from the elaborator env. *)
          let resolve_formal (psig : AST) : AST :=
            let p0 :=
              match strip_span psig with
              | AstSigWith base eqs => apply_sig_with base eqs
              | other => other
              end
            in
            match strip_span p0 with
            | AstRef n =>
                match resolve_hygiene env n [] with
                | Some (ty, _) =>
                    match strip_span ty with
                    | AstSignature _ _ as s => s
                    | AstSigWith base eqs => apply_sig_with base eqs
                    | AstModTy _ as m => m
                    | _ => p0
                    end
                | None => p0
                end
            | other => other
            end
          in
          let fix check_fargs (ps : list (string * AST)) (arg_tys : list AST)
              : ElabM unit :=
            match ps, arg_tys with
            | [], [] => ret tt
            | [], _ => throw "functor arity mismatch"
            | _, [] => throw "functor arity mismatch"
            | (pn, psig) :: ps', aty :: atys' =>
                let psig' := resolve_formal psig in
                let aty' := strip_span aty in
                match psig', aty' with
                | AstSignature _ decls, AstModTy ex =>
                    match seal_exports_check true ex decls with
                    | inl err => throw err
                    | inr _ => check_fargs ps' atys'
                    end
                | AstSignature _ decls, AstModule _ _ _ b =>
                    match seal_exports_check true (module_exports_of b) decls with
                    | inl err => throw err
                    | inr _ => check_fargs ps' atys'
                    end
                | AstSignature _ decls, _ =>
                    let ex := arg_exports aty in
                    match seal_exports_check true ex decls with
                    | inl err => throw err
                    | inr _ => check_fargs ps' atys'
                    end
                | AstModTy sex, AstModTy aex =>
                    (* Require arg exports to cover the formal ModTy view. *)
                    let fake_decls :=
                      map
                        (fun p =>
                           match snd p with
                           | AstFunTy tps params ret effs =>
                               AstSigVal (fst p) tps params ret effs
                           | AstTypeDecl n opt => AstTypeDecl n opt
                           | other => other
                           end)
                        sex
                    in
                    match seal_exports_check true aex fake_decls with
                    | inl err => throw err
                    | inr _ => check_fargs ps' atys'
                    end
                | AstModTy sex, AstModule _ _ _ b =>
                    let aex := module_exports_of b in
                    let fake_decls :=
                      map
                        (fun p =>
                           match snd p with
                           | AstFunTy tps params ret effs =>
                               AstSigVal (fst p) tps params ret effs
                           | AstTypeDecl n opt => AstTypeDecl n opt
                           | other => other
                           end)
                        sex
                    in
                    match seal_exports_check true aex fake_decls with
                    | inl err => throw err
                    | inr _ => check_fargs ps' atys'
                    end
                | _, _ =>
                    if types_compat psig' aty then check_fargs ps' atys'
                    else throw ("functor argument mismatch for parameter: " ++ pn)
                end
            end
          in
          check_fargs fparams (snd argsRes) ;;
          let fix subst_ref (old new_name : string) (t : AST) {struct t} : AST :=
            match t with
            | AstRef n => if string_dec n old then AstRef new_name else t
            | AstFunTy tps params ret effs =>
                AstFunTy tps
                  (map (fun p => (fst p, subst_ref old new_name (snd p))) params)
                  (subst_ref old new_name ret) effs
            | AstPi n a r e =>
                AstPi n (subst_ref old new_name a) (subst_ref old new_name r) e
            | AstTypeDecl n (Some ty) =>
                AstTypeDecl n (Some (subst_ref old new_name ty))
            | AstApp f args' =>
                AstApp (subst_ref old new_name f)
                  (map (subst_ref old new_name) args')
            | AstSpan sp i => AstSpan sp (subst_ref old new_name i)
            | AstModTy ex =>
                AstModTy
                  (map (fun p => (fst p, subst_ref old new_name (snd p))) ex)
            | _ => t
            end
          in
          let fix subst_exports (ps : list (string * AST)) (arg_asts : list AST)
              (ex : list (string * AST)) : list (string * AST) :=
            match ps, arg_asts with
            | [], _ => ex
            | _, [] => ex
            | (pn, _) :: ps', a :: as' =>
                let new_name :=
                  match a with
                  | AstModule an _ _ _ => an
                  | AstRef an => an
                  | _ => pn
                  end
                in
                let ex' :=
                  map (fun p => (fst p, subst_ref pn new_name (snd p))) ex
                in
                subst_exports ps' as' ex'
            end
          in
          s <- get_state ;
          match s with
          | mkElabState n sol exts effs caps pending go_pkgs =>
              let arg_name :=
                match fst argsRes with
                | AstModule an _ _ _ :: _ => an
                | AstRef an :: _ => an
                | _ => "Arg"
                end
              in
              let pref :=
                if is_app then fname ++ "__" ++ arg_name
                else fname ++ "_g" ++ string_of_nat n
              in
              (if is_app then ret tt
               else set_state (mkElabState (S n) sol exts effs caps pending go_pkgs)) ;;
              let ex0 :=
                subst_exports fparams (fst argsRes) (module_exports_of body)
              in
              match seal with
              | Some (AstSignature _ decls) =>
                  match seal_exports_check true ex0 decls with
                  | inl err => throw err
                  | inr xs => ret (AstModule pref [] seal body, AstModTy xs)
                  end
              | Some (AstApp (AstRef "#applicative") [AstSignature _ decls]) =>
                  match seal_exports_check true ex0 decls with
                  | inl err => throw err
                  | inr xs => ret (AstModule pref [] seal body, AstModTy xs)
                  end
              | Some (AstSigWith base eqs) =>
                  match apply_sig_with base eqs with
                  | AstSignature _ decls =>
                      match seal_exports_check true ex0 decls with
                      | inl err => throw err
                      | inr xs => ret (AstModule pref [] seal body, AstModTy xs)
                      end
                  | _ => ret (AstModule pref [] seal body, AstModTy ex0)
                  end
              | Some (AstModTy e) =>
                  ret (AstModule pref [] seal body, AstModTy e)
              | _ =>
                  ret (AstModule pref [] seal body, AstModTy ex0)
              end
          end
      | AstModTy _ =>
          ret (AstFunctorApp (fst funcAst) (fst argsRes), AstModTy [])
      | _ =>
          ret (AstFunctorApp (fst funcAst) (fst argsRes), AstModTy [])
      end
  | ExternCST lang modp decls _ =>
      let (syms, _) := elab_extern_decls env decls in
      ret (AstImport lang "" modp syms, AstRef "Unit")
  | MacroDefCST _ _ _ => throw "MacroDefCST reached Elaborator"
  | Error msg _ => throw msg

  | EffectCST name type_params decls _ =>
      (* Bind each effect type_param as a fresh meta (via AstTypeDecl) so op
         types like get(): T unify with handler resume args (e.g. Int). *)
      let fix bind_tps (tps : list string) (e0 : TypeEnv) : ElabM TypeEnv :=
        match tps with
        | [] => ret e0
        | tp :: rest =>
            m <- fresh_meta ;
            bind_tps rest (((tp, []), AstTypeDecl tp (Some m)) :: e0)
        end
      in
      env_eff <- bind_tps type_params env ;
      let fix elab_ops (ds : list CST) : ElabM (list (string * list AST * AST)) :=
        match ds with
        | [] => ret []
        | d :: rest =>
            match parse_effect_sig d with
            | Some (op, params, ret_ty) =>
                let fix elab_params (ps : list (string * CST)) : ElabM (list AST) :=
                  match ps with
                  | [] => ret []
                  | (_, pty) :: ps' =>
                      tyAst <- elaborate fuel' env_eff pty (Some TypeUniverse) ;
                      restTys <- elab_params ps' ;
                      ret (fst tyAst :: restTys)
                  end
                in
                argTys <- elab_params params ;
                retAst <- elaborate fuel' env_eff ret_ty (Some TypeUniverse) ;
                restOps <- elab_ops rest ;
                ret ((op, argTys, fst retAst) :: restOps)
            | None => elab_ops rest
            end
        end
      in
      ops <- elab_ops decls ;
      register_effect name ops ;;
      ret (AstRef "Unit", AstRef "Unit")
  | DoCST op args _ =>
      let op_name := match op with Symbol n _ => n | _ => "" end in
      s <- get_state ;
      match s with
      | mkElabState _ _ _ reg active pending _ =>
          match find_active_op op_name active reg with
          | Some (eff, argTys, retTy) =>
              let fix check_do_args (as_ : list CST) (tys : list AST) : ElabM (list AST) :=
                match as_, tys with
                | [], _ => ret []
                | a :: rest, ty :: tys' =>
                    aAst <- elaborate fuel' env a (Some ty) ;
                    restAst <- check_do_args rest tys' ;
                    ret (fst aAst :: restAst)
                | a :: rest, [] =>
                    aAst <- elaborate fuel' env a None ;
                    restAst <- check_do_args rest [] ;
                    ret (fst aAst :: restAst)
                end
              in
              argsRes <- check_do_args args argTys ;
              set_pending (add_effect (UserEffect eff) pending) ;;
              (* Encode effect label with op so runtime dispatch is label+op. *)
              ret (AstDo (AstRef (eff ++ "/" ++ op_name)) argsRes, retTy)
          | None =>
              match find_registered_op op_name reg with
              | Some (eff, argTys, retTy) =>
                  (* No active handler: still type the perform, but leave effect pending
                     (Effekt-style required capability). Discharged by handle or
                     rejected at elaborate_top / def boundary when still pending. *)
                  let fix check_do_args (as_ : list CST) (tys : list AST) : ElabM (list AST) :=
                    match as_, tys with
                    | [], _ => ret []
                    | a :: rest, ty :: tys' =>
                        aAst <- elaborate fuel' env a (Some ty) ;
                        restAst <- check_do_args rest tys' ;
                        ret (fst aAst :: restAst)
                    | a :: rest, [] =>
                        aAst <- elaborate fuel' env a None ;
                        restAst <- check_do_args rest [] ;
                        ret (fst aAst :: restAst)
                    end
                  in
                  argsRes <- check_do_args args argTys ;
                  set_pending (add_effect (UserEffect eff) pending) ;;
                  ret (AstDo (AstRef (eff ++ "/" ++ op_name)) argsRes, retTy)
              | None =>
                  throw (append "Unknown effect operation: " op_name)
              end
          end
      end
  | HandleCST body eff shallow multishot handlers _ =>
      match String.eqb eff "io" with
      | true => throw "io is not a user-handleable effect"
      | false =>
      push_capability eff ;;
      old_pending <- get_pending ;
      set_pending [] ;;
      bodyAst <- elaborate fuel' env body None ;
      body_effs <- get_pending ;
      pop_capability ;;
      set_pending (merge_effects old_pending (remove_effect (UserEffect eff) body_effs)) ;;
      s <- get_state ;
      match s with
      | mkElabState _ _ _ reg _ _ _ =>
          let ops := match lookup_effect_ops eff reg with Some o => o | None => [] end in
          let body_ty := snd bodyAst in
          (* Partition Plotkin-style [case return(x) => …] from op cases. *)
          let fix partition (hs : list CST) (rets ops_hs : list CST)
              : list CST * list CST :=
            match hs with
            | [] => (rets, ops_hs)
            | h :: rest =>
                match parse_handler_case h with
                | Some (op, _, _) =>
                    if string_dec op "return" then
                      partition rest (h :: rets) ops_hs
                    else partition rest rets (h :: ops_hs)
                | None => partition rest rets (h :: ops_hs)
                end
            end
          in
          match partition handlers [] [] with
          | (ret_cs, op_cs) =>
          let fix rev (xs ys : list CST) : list CST :=
            match xs with
            | [] => ys
            | x :: xs' => rev xs' (x :: ys)
            end
          in
          let ret_cs := rev ret_cs [] in
          let op_cs := rev op_cs [] in
          ret_clause <-
            match ret_cs with
            | [] => ret None
            | h :: _ =>
                match parse_handler_case h with
                | Some (_, binders, hbody) =>
                    let x :=
                      match binders with
                      | b :: _ => b
                      | [] => "x"
                      end
                    in
                    hAst <- elaborate fuel'
                              (((x, []), body_ty) :: env) hbody None ;
                    ret (Some (x, fst hAst, snd hAst))
                | None => throw "malformed return clause"
                end
            end ;
          let answer_ty :=
            match ret_clause with
            | Some (_, _, ty) => ty
            | None => body_ty
            end
          in
          let fix elab_handlers (hs : list CST) : ElabM (list (string * AST)) :=
            match hs with
            | [] => ret []
            | h :: rest =>
                match parse_handler_case h with
                | Some (op, binders, hbody) =>
                    match lookup_op op ops with
                    | Some (argTys, opRet) =>
                        let resume_ty := AstPi "v" opRet answer_ty [] in
                        let fix bind_args (bs : list string) (tys : list AST)
                            (e0 : TypeEnv) : TypeEnv :=
                          match bs, tys with
                          | [], _ => (("resume", []), resume_ty) :: e0
                          | b :: bs', ty :: tys' =>
                              bind_args bs' tys' (((b, []), ty) :: e0)
                          | b :: bs', [] =>
                              bind_args bs' [] (((b, []), AstRef "Any") :: e0)
                          end
                        in
                        let henv := bind_args binders argTys env in
                        hAst <- elaborate fuel' henv hbody (Some answer_ty) ;
                        restH <- elab_handlers rest ;
                        ret ((op,
                              build_handler_lam binders argTys resume_ty (fst hAst))
                             :: restH)
                    | None =>
                        (* Forwarding: unknown/undeclared op still elaborates; runtime
                           re-performs to an outer handler. *)
                        let resume_ty := AstPi "v" (AstRef "Any") answer_ty [] in
                        let fix bind_any (bs : list string) (e0 : TypeEnv) : TypeEnv :=
                          match bs with
                          | [] => (("resume", []), resume_ty) :: e0
                          | b :: bs' => bind_any bs' (((b, []), AstRef "Any") :: e0)
                          end
                        in
                        hAst <- elaborate fuel' (bind_any binders env) hbody
                                  (Some answer_ty) ;
                        restH <- elab_handlers rest ;
                        ret ((op,
                              build_handler_lam binders
                                (map (fun _ => AstRef "Any") binders)
                                resume_ty (fst hAst)) :: restH)
                    end
                | None => throw "malformed handler case"
                end
            end
          in
          hsOps <- elab_handlers op_cs ;
          (* Incomplete handlers allowed (Effekt forwarding): missing ops are
             not installed; runtime continue-search / re-perform outward. *)
          let hsRes0 :=
            match ret_clause with
            | Some (x, body, _) =>
                ("return", AstLam x body_ty body) :: hsOps
            | None => hsOps
            end
          in
          let hsRes :=
            let flags :=
              app
                (if shallow then [("__chester_shallow__", AstBoolLit true)] else [])
                (if multishot then [("__chester_multishot__", AstBoolLit true)]
                 else [])
            in
            app flags hsRes0
          in
          match expected with
          | Some exp => unify fuel' answer_ty exp
          | None => ret tt
          end ;;
          ret (AstHandle (fst bodyAst) (UserEffect eff) hsRes, answer_ty)
          end
      end
      end
  | RecordCST name type_params fields _ =>
      let fix elab_fs (fs : list CST) : ElabM (list (string * AST)) :=
        match fs with
        | [] => ret []
        | f :: rest =>
            let (fname, fty) := extract_arg_cst f in
            tyAst <- elaborate fuel' env fty (Some (AstUniverse 0)) ;
            restFs <- elab_fs rest ;
            ret ((fname, fst tyAst) :: restFs)
        end
      in
      fsRes <- elab_fs fields ;
      ret (AstRecord name type_params fsRes, AstRef "Unit")
      
  | FieldAccessCST expr field _ =>
      exprAst <- elaborate fuel' env expr None ;
      match snd exprAst with
      | AstModTy exports =>
          match lookup_export exports field with
          | Some (AstTypeDecl _ (Some def)) =>
              if is_type_expected expected then ret (def, TypeUniverse)
              else throw ("type component used as value: " ++ field)
          | Some (AstTypeDecl _ None) =>
              if is_type_expected expected then
                (* Abstract type component: use the field name as the type ref
                   so [M.t] unifies with other [M.t] / [t] annotations. *)
                ret (AstRef field, TypeUniverse)
              else throw ("type component used as value: " ++ field)
          | Some ty =>
              if is_type_expected expected then
                (* Enum/ctor FunTy or other export: [M.T] → [AstRef T] in type pos. *)
                ret (AstRef field, TypeUniverse)
              else ret (AstFieldAccess (fst exprAst) field, ty)
          | None => throw ("module has no export: " ++ field)
          end
      | _ =>
          if is_type_expected expected then
            ret (AstRef field, TypeUniverse)
          else
            (* Residual FFI / unresolved receiver — type as Any. *)
            ret (AstFieldAccess (fst exprAst) field, AstRef "Any")
      end

  | TypeDeclCST name opt span =>
      match opt with
      | Some ty =>
          tyAst <- elaborate fuel' env ty (Some TypeUniverse) ;
          ret (AstTypeDecl name (Some (fst tyAst)), TypeUniverse)
      | None =>
          ret (AstTypeDecl name None, TypeUniverse)
      end

  | ExtensionCST ext_name tparams target_ty meths span =>
      (* Bind extension type params so method bodies see rigid [T], and quantify
         them on each method FunTy so call-site prenex-instantiate covers
         [Option[T]] (not a free leftover [T]). *)
      let fix bind_ext_tps (tps : list string) (e0 : TypeEnv) : TypeEnv :=
        match tps with
        | [] => e0
        | tp :: rest =>
            bind_ext_tps rest (((tp, []), TypeUniverse) :: e0)
        end
      in
      let env_ext := bind_ext_tps tparams env in
      targetTy_res <- elaborate fuel' env_ext target_ty (Some (AstUniverse 0)) ;
      s <- get_state ;
      match s with
      | mkElabState n sol exts effs caps pending go_pkgs =>
          let ext_meths := extract_ext_methods meths ext_name in
          let new_ext := (ext_name, fst targetTy_res, ext_meths) in
          set_state (mkElabState n sol (new_ext :: exts) effs caps pending go_pkgs) ;;
          let fix elab_meths (ms : list CST) : ElabM (list AST) :=
            match ms with
            | [] => ret []
            | m :: rest =>
                ast <- elaborate fuel' env_ext m None ;
                rest_ast <- elab_meths rest ;
                let renamed_ast :=
                  match m, fst ast with
                  | DefCST n _ _ _ _ _, AstDef _ tps ps rt b effs =>
                      let tps' := app tparams tps in
                      AstDef (append (append ext_name "_") n) tps' ps rt b
                        effs
                  | _, a => a
                  end
                in
                ret (renamed_ast :: rest_ast)
            end
          in
          meths_res <- elab_meths meths ;
          ret (AstExtension ext_name tparams (fst targetTy_res) meths_res, AstRef "Unit")
      end
  end
  end.

(* 
  --- Unification Tests ---
*)
Definition test_unify_env := mkElabState 0 empty_state [] [] [] [] [].

Definition meta1 := AstMeta 1.
Definition int_ty := AstRef "Int"%string.

Definition test_unify_run : ElabM unit :=
  unify (Chester.CoreChecker.ast_size meta1 + Chester.CoreChecker.ast_size int_ty) meta1 int_ty.

Definition test_zonk_run : ElabM AST :=
  bind test_unify_run (fun _ => zonk (S (Chester.CoreChecker.ast_size meta1)) meta1).

Eval compute in test_zonk_run test_unify_env.


Definition core_env_of_elab (env : TypeEnv) : Chester.CoreChecker.TypeEnv :=
  map (fun entry : (string * list nat) * AST => (fst (fst entry), snd entry)) env.

Definition elaborate_top (env : TypeEnv) (expr : CST) (expected : option AST) : ElabM (AST * AST) :=
  res <- elaborate (cst_fuel expr) env expr expected ;
  zexpr <- zonk (S (ast_size (fst res))) (fst res) ;
  zty <- zonk (S (ast_size (snd res))) (snd res) ;
  let res' := (zexpr, zty) in
  pending <- get_pending ;
  let fix check_pending (ps : EffectSet) : ElabM (AST * AST) :=
    match ps with
    | [] =>
        match Chester.CoreChecker.infer_check (core_env_of_elab env) (fst res') None with
        | TyOk _ => ret res'
        | TyErr msg => throw (append "Core check: " msg)
        end
    | EffectRowVar _ :: rest => check_pending rest
    (* [div] is a termination classifier, not a capability that needs a handler. *)
    | UserEffect e :: rest =>
        if string_dec e "div" then check_pending rest
        else throw (append "Unhandled effect: " e)
    | BuiltinEffect e :: rest =>
        (* [io] is a phantom FFI row for stdlib print, not a handleable effect. *)
        if orb (if string_dec e "div" then true else false)
               (if string_dec e "io" then true else false)
        then check_pending rest
        else throw (append "Unhandled effect: " e)
    end
  in check_pending pending.
