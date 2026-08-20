From Stdlib Require Import Ascii.
From Stdlib Require Import Strings.String.
From Stdlib Require Import List.
Require Import Chester.CST.
Require Import Chester.AST.
Require Import Chester.Formatter.
Open Scope string_scope.
Import ListNotations.

Inductive ElabState :=
| mkElabState : nat -> ElabState.

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
  | mkElabState n =>
      set_state (mkElabState (S n)) ;;
      ret (AstMeta n)
  end.

Definition TypeEnv := list (string * AST).

Fixpoint zonk (a : AST) : AST :=
  a.
Definition init_elab_state := mkElabState 0.
Fixpoint lookup_type (name : string) (env : TypeEnv) : option AST :=
  match env with
  | [] => None
  | (n, ty) :: rest =>
      if string_dec name n then Some ty else lookup_type name rest
  end.

Definition StringType := AstRef "String".
Definition IntType := AstRef "Int".
Definition BoolType := AstRef "Bool".
Definition TypeUniverse := AstRef "Type".

Fixpoint unify (fuel : nat) (t1 t2 : AST) : ElabM unit :=
  match fuel with
  | 0 => ret tt
  | S fuel' => ret tt
  end.

Fixpoint elaborate (env : TypeEnv) (expr : CST) (expected : option AST) {struct expr} : ElabM (AST * AST) :=
  match expr with
  | Symbol name _ =>
      match lookup_type name env with
      | Some ty => 
          match expected with
          | Some exp => unify 100 ty exp ;; ret (AstRef name, ty)
          | None => ret (AstRef name, ty)
          end
      | None => ret (AstRef name, AstRef "Any")
      end
  | StringLiteral s _ => 
      match expected with Some exp => unify 100 StringType exp | None => ret tt end ;;
      ret (AstStringLit s, StringType)
  | IntegerLiteral _ _ => 
      match expected with Some exp => unify 100 IntType exp | None => ret tt end ;;
      ret (AstIntLit 42, IntType)
  | BoolLiteral b _ => 
      match expected with Some exp => unify 100 BoolType exp | None => ret tt end ;;
      ret (AstBoolLit b, BoolType)
  | SeqOf exprs span =>
      match exprs with
      | [] => throw "Empty SeqOf"
      | func :: args =>
          funcAst <- elaborate env func None;
          match fst funcAst with
          | AstRef name => if string_dec name "\\" then throw "Lambda!" else ret tt
          | _ => ret tt
          end ;;
          let fix check_args (fs : AST) (as_ : list CST) {struct as_} : ElabM (list AST * AST) :=
              match as_ with
              | [] => ret ([], fs)
              | a :: rest =>
                  match fs with
                  | AstPi _ arg_ty ret_ty _ =>
                      aAst <- elaborate env a (Some arg_ty);
                      restAst <- check_args ret_ty rest;
                      ret (fst aAst :: fst restAst, snd restAst)
                  | AstMeta _ =>
                      argTyM <- fresh_meta;
                      retTyM <- fresh_meta;
                      unify 100 fs (AstPi "x" argTyM retTyM []);;
                      aAst <- elaborate env a (Some argTyM);
                      restAst <- check_args retTyM rest;
                      ret (fst aAst :: fst restAst, snd restAst)
                  | AstRef _ =>
                      aAst <- elaborate env a None;
                      restAst <- check_args (AstRef "Any") rest;
                      ret (fst aAst :: fst restAst, AstRef "Any")
                  | _ => ret ([], AstRef "Any")
                  end
              end
          in
          argsRes <- check_args (snd funcAst) args;
          match expected with
          | Some exp => unify 100 (snd argsRes) exp
          | None => ret tt
          end;; ret (AstApp (fst funcAst) (fst argsRes), snd argsRes)
      end
  | Block stmts ret_expr _ => 
      let fix map_elabs (current_env : list (string * AST)) (ls : list CST) : ElabM (list AST * list (string * AST)) :=
        match ls with
        | [] => ret ([], current_env)
        | x :: xs => 
            match x with
            | LetCST name value _ _ =>
                valueAst <- elaborate current_env value None ;
                let new_env := (name, snd valueAst) :: current_env in
                rest <- map_elabs new_env xs ;
                ret (AstLet name (fst valueAst) :: fst rest, snd rest)
            | DefCST name _ _ ret_ty _ _ =>
                tyAst <- elaborate current_env ret_ty (Some TypeUniverse) ;
                let new_env := (name, fst tyAst) :: current_env in
                res <- elaborate current_env x None ;
                rest <- map_elabs new_env xs ;
                ret (fst res :: fst rest, snd rest)
            | _ =>
                res <- elaborate current_env x None ;
                rest <- map_elabs current_env xs ;
                ret (fst res :: fst rest, snd rest)
            end
        end
      in
      stmtsRes <- map_elabs env stmts ;
      let stmtsAst := fst stmtsRes in
      let final_env := snd stmtsRes in
      retAst <- elaborate final_env ret_expr None ;
      ret (AstBlock stmtsAst (fst retAst), snd retAst)
  
  | LetCST name value body _ =>
      valueAst <- elaborate env value None ;
      bodyAst <- elaborate ((name, snd valueAst) :: env) body expected ;
      ret (AstBlock [AstLet name (fst valueAst)] (fst bodyAst), snd bodyAst)
      
  | IfCST cond thenB elseB _ =>
      condAst <- elaborate env cond None ;
      thenAst <- elaborate env thenB expected ;
      elseAst <- elaborate env elseB expected ;
      ret (AstIf (fst condAst) (fst thenAst) (fst elseAst), snd thenAst)
      
  | DefCST name type_params params ret_ty body _ =>
      let fix map_params (ps : list (string * CST)) : ElabM (list (string * AST)) :=
        match ps with
        | [] => ret []
        | (pname, pty) :: rest =>
            tyAst <- elaborate env pty (Some TypeUniverse) ;
            restAst <- map_params rest ;
            ret ((pname, fst tyAst) :: restAst)
        end
      in
      paramsAst <- map_params params ;
      let fix build_env (ps : list (string * AST)) (env0 : TypeEnv) {struct ps} : TypeEnv :=
        match ps with
        | [] => env0
        | (pname, pty) :: rest => build_env rest ((pname, pty) :: env0)
        end
      in
      let body_env := build_env paramsAst env in
      retAst <- elaborate env ret_ty (Some TypeUniverse) ;
      bodyAst <- elaborate body_env body (Some (fst retAst)) ;
      ret (AstDef name type_params paramsAst (fst retAst) (fst bodyAst), AstRef "Unit")

  | LamCST arg_name opt_arg_ty body _ =>
      argTyAst <- (match opt_arg_ty with
                   | Some ty => elaborate env ty (Some TypeUniverse)
                   | None => m <- fresh_meta ; ret (m, TypeUniverse)
                   end) ;
      bodyAst <- elaborate ((arg_name, fst argTyAst) :: env) body None ;
      let arrTy := AstPi arg_name (fst argTyAst) (snd bodyAst) [] in
      ret (AstLam arg_name (fst argTyAst) (fst bodyAst), arrTy)

  | AppCST func args _ =>
      funcAst <- elaborate env func None ;
      let fix check_args (fs : AST) (as_ : list CST) {struct as_} : ElabM (list AST * AST) :=
        match as_ with
        | [] => ret ([], fs)
        | a :: rest =>
            match fs with
            | AstPi _ arg_ty ret_ty _ =>
                aAst <- elaborate env a (Some arg_ty) ;
                restAst <- check_args ret_ty rest ;
                ret (fst aAst :: fst restAst, snd restAst)
            | AstMeta _ =>
                argTyM <- fresh_meta ;
                retTyM <- fresh_meta ;
                unify 100 fs (AstPi "x" argTyM retTyM []) ;;
                aAst <- elaborate env a (Some argTyM) ;
                restAst <- check_args retTyM rest ;
                ret (fst aAst :: fst restAst, snd restAst)
            | AstRef _ =>
                aAst <- elaborate env a None;
                restAst <- check_args (AstRef "Any") rest;
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

  | TypeAppCST func args _ =>
      funcAst <- elaborate env func None ;
      let fix check_args (as_ : list CST) : ElabM (list AST) :=
        match as_ with
        | [] => ret []
        | a :: rest =>
            aAst <- elaborate env a (Some (AstRef "TypeUniverse")) ;
            restAst <- check_args rest ;
            ret (fst aAst :: restAst)
        end
      in
      argsRes <- check_args args ;
      ret (AstTypeApp (fst funcAst) argsRes, AstRef "TypeUniverse")

  | EnumCST name type_params _ _ =>
      ret (AstEnum name type_params [], AstRef "Unit")

  | MatchCST expr cases _ =>
      exprAst <- elaborate env expr None ;
      let fix elab_cases (cs : list (PatternCST * CST)) : ElabM (list (PatternAST * AST) * AST) :=
        match cs with
        | [] => throw "Empty match not allowed"
        | (PatWildcardCST _, body) :: _ =>
            bodyAst <- elaborate env body expected ;
            ret ([(PatWildcard, fst bodyAst)], snd bodyAst)
        | (PatVarCST v _, body) :: _ =>
            m <- fresh_meta ;
            bodyAst <- elaborate ((v, m) :: env) body expected ;
            ret ([(PatVar v, fst bodyAst)], snd bodyAst)
        | (PatConstructorCST name vars _, body) :: _ =>
            let fix add_vars (vs : list string) (e : TypeEnv) {struct vs} : ElabM TypeEnv :=
              match vs with
              | [] => ret e
              | v :: rest_vs =>
                  m <- fresh_meta ;
                  add_vars rest_vs ((v, m) :: e)
              end
            in
            case_env <- add_vars vars env ;
            bodyAst <- elaborate case_env body expected ;
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
            xAst <- elaborate env x None;
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
            xAst <- elaborate env x None;
            restAst <- elab_elems xs;
            ret (fst xAst :: fst restAst, AstRef "List")
        end
      in
      elemsRes <- elab_elems elems;
      ret (AstTuple (fst elemsRes), snd elemsRes)

  | CommentCST msg _ => ret (AstRef "Unit", AstRef "Unit")
  | Error msg _ => throw msg

  | RecordCST name type_params fields _ => 
      ret (AstRecord name type_params [], AstRef "Unit")
      
  | FieldAccessCST expr field _ =>
      exprAst <- elaborate env expr None ;
      ret (AstFieldAccess (fst exprAst) field, AstRef "Type")
  end.
