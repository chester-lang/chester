From Stdlib Require Import Ascii.
From Stdlib Require Import Strings.String.
From Stdlib Require Import List.
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
  | mkElabState n sol exts effs caps pending =>
      set_state (mkElabState (S n) sol exts effs caps pending) ;;
      ret (AstMeta n)
  end.

Definition TypeEnv := list ((string * list nat) * AST).
Definition init_elab_state := mkElabState 0 empty_state [] [] [] [].

Definition effect_eqb (e1 e2 : EffectRef) : bool :=
  match e1, e2 with
  | UserEffect a, UserEffect b => if string_dec a b then true else false
  | BuiltinEffect a, BuiltinEffect b => if string_dec a b then true else false
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

Definition get_pending : ElabM EffectSet :=
  s <- get_state ;
  match s with mkElabState _ _ _ _ _ pending => ret pending end.

Definition set_pending (pending : EffectSet) : ElabM unit :=
  s <- get_state ;
  match s with
  | mkElabState n sol exts effs caps _ =>
      set_state (mkElabState n sol exts effs caps pending)
  end.

Definition push_capability (eff : string) : ElabM unit :=
  s <- get_state ;
  match s with
  | mkElabState n sol exts effs caps pending =>
      set_state (mkElabState n sol exts effs (eff :: caps) pending)
  end.

Definition pop_capability : ElabM unit :=
  s <- get_state ;
  match s with
  | mkElabState n sol exts effs (_ :: caps) pending =>
      set_state (mkElabState n sol exts effs caps pending)
  | mkElabState n sol exts effs [] pending =>
      set_state (mkElabState n sol exts effs [] pending)
  end.

Definition register_effect (name : string) (ops : list (string * list AST * AST)) : ElabM unit :=
  s <- get_state ;
  match s with
  | mkElabState n sol exts effs caps pending =>
      set_state (mkElabState n sol exts ((name, ops) :: effs) caps pending)
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

Fixpoint build_handler_lam (binders : list string) (body : AST) : AST :=
  match binders with
  | [] => AstLam "resume" (AstRef "Any") body
  | b :: rest => AstLam b (AstRef "Any") (build_handler_lam rest body)
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

Fixpoint unify (fuel : nat) (t1 t2 : AST) {struct fuel} : ElabM unit :=
  match fuel with
  | 0 => ret tt
  | S fuel' => 
      let fix unify_list (l1 l2 : list AST) : ElabM unit :=
        match l1, l2 with
        | [], [] => ret tt
        | x :: xs, y :: ys => unify fuel' x y ;; unify_list xs ys
        | _, _ => ret tt
        end in
      let t1' := strip_span (whnf t1) in
      let t2' := strip_span (whnf t2) in
      match t1', t2' with
      | AstMeta m1, AstMeta m2 =>
          if Nat.eqb m1 m2 then ret tt else
          s <- get_state ;
          match s with
          | mkElabState n sol exts effs caps pending =>
              match type_metas sol m1 with
              | Solved v1 => unify fuel' v1 t2'
              | _ => match type_metas sol m2 with
                     | Solved v2 => unify fuel' t1' v2
                     | _ => set_state (mkElabState n (update_type_state m1 (Solved t2') sol) exts effs caps pending)
                     end
              end
          end
      | AstMeta m1, _ =>
          s <- get_state ;
          match s with
          | mkElabState n sol exts effs caps pending =>
              match type_metas sol m1 with
              | Solved v1 => unify fuel' v1 t2'
              | _ => set_state (mkElabState n (update_type_state m1 (Solved t2') sol) exts effs caps pending)
              end
          end
      | _, AstMeta m2 =>
          s <- get_state ;
          match s with
          | mkElabState n sol exts effs caps pending =>
              match type_metas sol m2 with
              | Solved v2 => unify fuel' t1' v2
              | _ => set_state (mkElabState n (update_type_state m2 (Solved t1') sol) exts effs caps pending)
              end
          end
      | AstApp f1 a1, AstApp f2 a2 =>
          unify fuel' f1 f2 ;; unify_list a1 a2
      | AstPi n1 ty1 ret1 eff1, AstPi n2 ty2 ret2 eff2 =>
          unify fuel' ty1 ty2 ;; unify fuel' ret1 ret2
      | _, _ => ret tt
      end
  end.

Fixpoint zonk (fuel : nat) (expr : AST) : ElabM AST :=
  match fuel with
  | 0 => ret expr
  | S fuel' =>
      match strip_span expr with
      | AstMeta m =>
          s <- get_state ;
          match s with
          | mkElabState n sol exts effs caps pending =>
              match type_metas sol m with
              | Solved v => zonk fuel' v
              | _ => ret (AstMeta m)
              end
          end
      | AstApp f args =>
          f' <- zonk fuel' f ;
          let fix zonk_list (l : list AST) : ElabM (list AST) :=
            match l with
            | [] => ret []
            | x :: xs => x' <- zonk fuel' x ; xs' <- zonk_list xs ; ret (x' :: xs')
            end in
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
      | _ => ret expr
      end
  end.


Fixpoint get_base_type (t : AST) : AST :=
  match t with
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

Fixpoint elaborate (fuel : nat) (env : TypeEnv) (expr : CST) (expected : option AST) {struct fuel} : ElabM (AST * AST) :=
  match fuel with
  | 0 => throw "Out of fuel"
  | S fuel' =>
  match expr with
  | Symbol name span =>
      match resolve_hygiene env name (context span) with
      | Some (ty, resolved_ctx) => 
          match expected with
          | Some exp => unify 100 ty exp ;; ret (AstRef (mangle_name name resolved_ctx), ty)
          | None => ret (AstRef (mangle_name name resolved_ctx), ty)
          end
      | None => ret (AstRef name, AstRef "Any")
      end
  | StringLiteral s _ => 
      match expected with Some exp => unify 100 StringType exp | None => ret tt end ;;
      ret (AstStringLit s, StringType)
  | IntegerLiteral s _ => 
      match expected with Some exp => unify 100 IntType exp | None => ret tt end ;;
      ret (AstIntLit (string_to_nat s), IntType)
  | BoolLiteral b _ => 
      match expected with Some exp => unify 100 BoolType exp | None => ret tt end ;;
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
          let fix check_args (fs : AST) (as_ : list CST) {struct as_} : ElabM (list AST * (AST * EffectSet)) :=
              match as_, fs with
              | [], AstFunTy _ _ ret_ty effs => ret ([], (ret_ty, effs))
              | [], AstPi _ _ ret_ty effs => ret ([], (ret_ty, effs))
              | [], ty => ret ([], (ty, []))
              | a :: rest, AstFunTy tps ((_, arg_ty) :: params) ret_ty effs =>
                  aAst <- elaborate fuel' env a (Some arg_ty);
                  restAst <- check_args (AstFunTy tps params ret_ty effs) rest;
                  ret (fst aAst :: fst restAst, snd restAst)
              | a :: rest, AstFunTy tps [] ret_ty effs =>
                  aAst <- elaborate fuel' env a None;
                  restAst <- check_args (AstFunTy tps [] ret_ty effs) rest;
                  ret (fst aAst :: fst restAst, snd restAst)
              | a :: rest, AstPi _ arg_ty ret_ty effs =>
                  aAst <- elaborate fuel' env a (Some arg_ty);
                  restAst <- check_args ret_ty rest;
                  match snd restAst with
                  | (rty, e) => ret (fst aAst :: fst restAst, (rty, merge_effects effs e))
                  end
              | a :: rest, AstMeta _ =>
                  argTyM <- fresh_meta;
                  retTyM <- fresh_meta;
                  unify 100 fs (AstPi "x" argTyM retTyM []);;
                  aAst <- elaborate fuel' env a (Some argTyM);
                  restAst <- check_args retTyM rest;
                  ret (fst aAst :: fst restAst, snd restAst)
              | a :: rest, AstRef _ =>
                  aAst <- elaborate fuel' env a None;
                  restAst <- check_args (AstRef "Any") rest;
                  ret (fst aAst :: fst restAst, (AstRef "Any", snd (snd restAst)))
              | _, _ => ret ([], (AstRef "Any", []))
              end
          in
          argsRes <- check_args (snd funcAst) args;
          match snd argsRes with
          | (ret_ty, call_effs) =>
              pending <- get_pending ;
              set_pending (merge_effects pending call_effs) ;;
              match expected with
              | Some exp => unify 100 ret_ty exp
              | None => ret tt
              end;; ret (AstApp (fst funcAst) (fst argsRes), ret_ty)
          end
      end
  | Block stmts ret_expr _ => 
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
            | DefCST name _ _ ret_ty _ span =>
                res <- elaborate fuel' current_env x None ;
                let new_env := ((name, context span), snd res) :: current_env in
                rest <- map_elabs new_env xs ;
                ret (fst res :: fst rest, snd rest)
            | _ =>
                res <- elaborate fuel' current_env x None ;
                rest <- map_elabs current_env xs ;
                ret (fst res :: fst rest, snd rest)
            end
        end
      in
      stmtsRes <- map_elabs env stmts ;
      let stmtsAst := fst stmtsRes in
      let final_env := snd stmtsRes in
      retAst <- elaborate fuel' final_env ret_expr None ;
      ret (AstBlock stmtsAst (fst retAst), snd retAst)
  
  | LetCST name value body span =>
      valueAst <- elaborate fuel' env value None ;
      bodyAst <- elaborate fuel' (((name, context span), snd valueAst) :: env) body expected ;
      ret (AstBlock [AstLet (mangle_name name (context span)) (fst valueAst)] (fst bodyAst), snd bodyAst)
      
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
      let body_env := build_env paramsAst env in
      retAst <- elaborate fuel' env ret_ty (Some TypeUniverse) ;
      old_pending <- get_pending ;
      set_pending [] ;;
      bodyAst <- elaborate fuel' body_env body (Some (fst retAst)) ;
      body_effs <- get_pending ;
      set_pending (merge_effects old_pending body_effs) ;;
      let fun_ty := AstFunTy type_params paramsAst (fst retAst) body_effs in
      ret (AstDef name type_params paramsAst (fst retAst) (fst bodyAst), fun_ty)

  | LamCST arg_name opt_arg_ty body span =>
      argTyAst <- (match opt_arg_ty with
                   | Some ty => elaborate fuel' env ty (Some TypeUniverse)
                   | None => m <- fresh_meta ; ret (m, TypeUniverse)
                   end) ;
      old_pending <- get_pending ;
      set_pending [] ;;
      bodyAst <- elaborate fuel' (((arg_name, context span), fst argTyAst) :: env) body None ;
      body_effs <- get_pending ;
      set_pending (merge_effects old_pending body_effs) ;;
      let arrTy := AstPi arg_name (fst argTyAst) (snd bodyAst) body_effs in
      ret (AstLam (mangle_name arg_name (context span)) (fst argTyAst) (fst bodyAst), arrTy)

  | AppCST func args span =>
      match func with
      | FieldAccessCST expr field fsp =>
          exprAst <- elaborate fuel' env expr None ;
          s <- get_state ;
          let full_name := match s with mkElabState _ _ exts _ _ _ => find_ext_method (snd exprAst) field exts end in
          if string_dec full_name EmptyString then
            let fix elab_args (as_ : list CST) : ElabM (list AST) :=
              match as_ with
              | [] => ret []
              | a :: rest =>
                  aAst <- elaborate fuel' env a None ;
                  restAst <- elab_args rest ;
                  ret (fst aAst :: restAst)
              end
            in
            argsAst <- elab_args args ;
            ret (AstApp (AstFieldAccess (fst exprAst) field) argsAst, AstRef "Unknown")
          else
            let new_cst := AppCST (Symbol full_name fsp) (expr :: args) span in
            elaborate fuel' env new_cst expected
      | ImplicitAppCST inner_func _targs _tspan =>
          (* Erase the implicit [A,B] telescope; elaborate fuel' only the inner function and explicit args *)
          funcAst <- elaborate fuel' env inner_func None ;
          let fix check_args (fs : AST) (as_ : list CST) {struct as_} : ElabM (list AST * AST) :=
            match as_ with
            | [] => ret ([], fs)
            | a :: rest =>
                match fs with
                | AstFunTy _tparams params ret_ty _ =>
                    (* Non-curried: elaborate fuel' all explicit args against the param list *)
                    match params with
                    | (_, arg_ty) :: _rest_params =>
                        aAst <- elaborate fuel' env a (Some arg_ty) ;
                        restAst <- check_args (AstFunTy _tparams _rest_params ret_ty []) rest ;
                        ret (fst aAst :: fst restAst, snd restAst)
                    | [] =>
                        aAst <- elaborate fuel' env a None ;
                        restAst <- check_args (AstRef "Any") rest ;
                        ret (fst aAst :: fst restAst, AstRef "Any")
                    end
                | AstPi _ arg_ty ret_ty _ =>
                    aAst <- elaborate fuel' env a (Some arg_ty) ;
                    restAst <- check_args ret_ty rest ;
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
          argsRes <- check_args (snd funcAst) args ;
          match expected with
          | Some exp => unify 100 (snd argsRes) exp
          | None => ret tt
          end ;;
          ret (AstApp (fst funcAst) (fst argsRes), snd argsRes)
      | _ =>
          funcAst <- elaborate fuel' env func None ;
          let fix check_args (fs : AST) (as_ : list CST) {struct as_} : ElabM (list AST * AST) :=
            match as_ with
            | [] => ret ([], fs)
            | a :: rest =>
                match fs with
                | AstFunTy _tparams params ret_ty _ =>
                    match params with
                    | (_, arg_ty) :: _rest_params =>
                        aAst <- elaborate fuel' env a (Some arg_ty) ;
                        restAst <- check_args (AstFunTy _tparams _rest_params ret_ty []) rest ;
                        ret (fst aAst :: fst restAst, snd restAst)
                    | [] =>
                        aAst <- elaborate fuel' env a None ;
                        restAst <- check_args (AstRef "Any") rest ;
                        ret (fst aAst :: fst restAst, AstRef "Any")
                    end
                | AstPi _ arg_ty ret_ty _ =>
                    aAst <- elaborate fuel' env a (Some arg_ty) ;
                    restAst <- check_args ret_ty rest ;
                    ret (fst aAst :: fst restAst, snd restAst)
                | AstMeta _ =>
                    argTyM <- fresh_meta ;
                    retTyM <- fresh_meta ;
                    unify 100 fs (AstPi "x" argTyM retTyM []) ;;
                    aAst <- elaborate fuel' env a (Some argTyM) ;
                    restAst <- check_args retTyM rest ;
                    ret (fst aAst :: fst restAst, snd restAst)
                | AstRef _ =>
                    aAst <- elaborate fuel' env a None ;
                    restAst <- check_args (AstRef "Any") rest ;
                    ret (fst aAst :: fst restAst, AstRef "Any")
                | _ => ret ([], AstRef "Any")
                end
            end
          in
          argsRes <- check_args (snd funcAst) args ;
          match expected with
          | Some exp => unify 100 (snd argsRes) exp
          | None => ret tt
          end ;;
          ret (AstApp (fst funcAst) (fst argsRes), snd argsRes)
      end

  | ImplicitAppCST func args _ =>
      (* Standalone implicit application f[A,B] without explicit args — used in type position *)
      funcAst <- elaborate fuel' env func None ;
      let fix check_targs (as_ : list CST) : ElabM (list AST) :=
        match as_ with
        | [] => ret []
        | a :: rest =>
            aAst <- elaborate fuel' env a (Some (AstRef "TypeUniverse")) ;
            restAst <- check_targs rest ;
            ret (fst aAst :: restAst)
        end
      in
      argsRes <- check_targs args ;
      ret (AstImplicitApp (fst funcAst) argsRes, AstRef "TypeUniverse")

  | EnumCST name type_params variants _ =>
      let fix elab_variant (v : CST) : string * list AST * AST :=
        match v with
        | AppCST (Symbol ":" _) (AppCST (Symbol vname _) args _ :: ret_ty :: []) _ =>
            let fix elab_fields (fs : list CST) : list AST :=
              match fs with
              | [] => []
              | _ :: rest => AstRef "Any" :: elab_fields rest
              end
            in
            ((vname, elab_fields args), AstRef "Any")
        | AppCST (Symbol ":" _) (Symbol vname _ :: ret_ty :: []) _ =>
            ((vname, []), AstRef "Any")
        | AppCST (Symbol vname _) args _ =>
            let fix elab_fields (fs : list CST) : list AST :=
              match fs with
              | [] => []
              | _ :: rest => AstRef "Any" :: elab_fields rest
              end
            in
            ((vname, elab_fields args), AstRef "Any")
        | Symbol vname _ => ((vname, []), AstRef "Any")
        | _ => (("unknown", []), AstRef "Any")
        end
      in
      let fix elab_variants (vs : list CST) : list (string * list AST * AST) :=
        match vs with
        | [] => []
        | v :: rest => elab_variant v :: elab_variants rest
        end
      in
      ret (AstEnum name type_params (elab_variants variants), AstRef "Unit")

  | MatchCST expr cases _ =>
      exprAst <- elaborate fuel' env expr None ;
      let fix elab_cases (cs : list (PatternCST * CST)) : ElabM (list (PatternAST * AST) * AST) :=
        match cs with
        | [] => throw "Empty match not allowed"
        | (PatWildcardCST _, body) :: _ =>
            bodyAst <- elaborate fuel' env body expected ;
            ret ([(PatWildcard, fst bodyAst)], snd bodyAst)
        | (PatVarCST v span, body) :: _ =>
            m <- fresh_meta ;
            bodyAst <- elaborate fuel' (((v, context span), m) :: env) body expected ;
            ret ([(PatVar v, fst bodyAst)], snd bodyAst)
        | (PatConstructorCST name vars span, body) :: _ =>
            let fix add_vars (vs : list string) (e : TypeEnv) {struct vs} : ElabM TypeEnv :=
              match vs with
              | [] => ret e
              | v :: rest_vs =>
                  m <- fresh_meta ;
                  add_vars rest_vs (((v, context span), m) :: e)
              end
            in
            case_env <- add_vars vars env ;
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
            unify 100 (snd res_first) (snd res_rest) ;;
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
            xAst <- elaborate fuel' env x None;
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
  | MacroDefCST _ _ _ => throw "MacroDefCST reached Elaborator"
  | Error msg _ => throw msg

  | EffectCST name type_params decls _ =>
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
                      tyAst <- elaborate fuel' env pty (Some TypeUniverse) ;
                      restTys <- elab_params ps' ;
                      ret (fst tyAst :: restTys)
                  end
                in
                argTys <- elab_params params ;
                retAst <- elaborate fuel' env ret_ty (Some TypeUniverse) ;
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
      | mkElabState _ _ _ reg active pending =>
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
              ret (AstDo (AstRef op_name) argsRes, retTy)
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
                  ret (AstDo (AstRef op_name) argsRes, retTy)
              | None =>
                  throw (append "Unknown effect operation: " op_name)
              end
          end
      end
  | HandleCST body eff handlers _ =>
      push_capability eff ;;
      old_pending <- get_pending ;
      set_pending [] ;;
      bodyAst <- elaborate fuel' env body None ;
      body_effs <- get_pending ;
      pop_capability ;;
      set_pending (merge_effects old_pending (remove_effect (UserEffect eff) body_effs)) ;;
      s <- get_state ;
      match s with
      | mkElabState _ _ _ reg _ _ =>
          let ops := match lookup_effect_ops eff reg with Some o => o | None => [] end in
          let fix elab_handlers (hs : list CST) : ElabM (list (string * AST)) :=
            match hs with
            | [] => ret []
            | h :: rest =>
                match parse_handler_case h with
                | Some (op, binders, hbody) =>
                    let retTy := match lookup_op op ops with
                                 | Some (_, r) => r
                                 | None => AstRef "Any"
                                 end in
                    let resume_ty := AstPi "v" retTy (AstRef "Any") [] in
                    let fix bind_args (bs : list string) (e0 : TypeEnv) : TypeEnv :=
                      match bs with
                      | [] => (("resume", []), resume_ty) :: e0
                      | b :: bs' => bind_args bs' (((b, []), AstRef "Any") :: e0)
                      end
                    in
                    let henv := bind_args binders env in
                    hAst <- elaborate fuel' henv hbody None ;
                    restH <- elab_handlers rest ;
                    ret ((op, build_handler_lam binders (fst hAst)) :: restH)
                | None => elab_handlers rest
                end
            end
          in
          hsRes <- elab_handlers handlers ;
          ret (AstHandle (fst bodyAst) (UserEffect eff) hsRes, snd bodyAst)
      end
  | RecordCST name type_params fields _ => 
      ret (AstRecord name type_params [], AstRef "Unit")
      
  | FieldAccessCST expr field _ =>
      exprAst <- elaborate fuel' env expr None ;
      ret (AstFieldAccess (fst exprAst) field, AstRef "Type")
      
  | ExtensionCST ext_name tparams target_ty meths span =>
      targetTy_res <- elaborate fuel' env target_ty (Some (AstUniverse 0)) ;
      s <- get_state ;
      match s with
      | mkElabState n sol exts effs caps pending =>
          let ext_meths := extract_ext_methods meths ext_name in
          let new_ext := (ext_name, fst targetTy_res, ext_meths) in
          set_state (mkElabState n sol (new_ext :: exts) effs caps pending) ;;
          let fix elab_meths (ms : list CST) : ElabM (list AST) :=
            match ms with
            | [] => ret []
            | m :: rest =>
                ast <- elaborate fuel' env m None ;
                rest_ast <- elab_meths rest ;
                let renamed_ast := match m, fst ast with
                                   | DefCST n _ _ _ _ _, AstDef _ tps ps rt b =>
                                       AstDef (append (append ext_name "_") n) tps ps rt b
                                   | _, a => a
                                   end in
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
Definition test_unify_env := mkElabState 0 empty_state [] [] [] [].

Definition meta1 := AstMeta 1.
Definition int_ty := AstRef "Int"%string.

Definition test_unify_run : ElabM unit := unify 100 meta1 int_ty.

Definition test_zonk_run : ElabM AST :=
  bind test_unify_run (fun _ => zonk 100 meta1).

Eval compute in test_zonk_run test_unify_env.


Definition elaborate_top (env : TypeEnv) (expr : CST) (expected : option AST) : ElabM (AST * AST) :=
  res <- elaborate 1000 env expr expected ;
  pending <- get_pending ;
  match pending with
  | [] => ret res
  | UserEffect e :: _ => throw (append "Unhandled effect: " e)
  | BuiltinEffect e :: _ => throw (append "Unhandled effect: " e)
  end.
