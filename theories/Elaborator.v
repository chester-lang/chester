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
  | BoolLiteral b _ => ret (AstBoolLit b, BoolType)
  | IntegerLiteral n _ => ret (AstIntLit 42, IntType)
  | StringLiteral s _ => ret (AstStringLit s, StringType)
  | SeqOf exprs _ => throw "SeqOf not implemented in elaborator"
  | Block stmts ret_expr _ => 
      let fix map_elabs (ls : list CST) : ElabM (list AST) :=
        match ls with
        | [] => ret []
        | x :: xs => 
            res <- elaborate env x None ;
            let (a, _) := res in
            rest <- map_elabs xs ;
            ret (a :: rest)
        end
      in
      stmtsAst <- map_elabs stmts ;
      retAst <- elaborate env ret_expr None ;
      ret (AstBlock stmtsAst (fst retAst), snd retAst)
  
  | LetCST name value body _ =>
      valueAst <- elaborate env value None ;
      bodyAst <- elaborate ((name, snd valueAst) :: env) body expected ;
      ret (AstLet name (fst valueAst) (fst bodyAst), snd bodyAst)
      
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
      let fix build_env (ps : list (string * AST)) (env : TypeEnv) : TypeEnv :=
        match ps with
        | [] => env
        | (pname, pty) :: rest => build_env rest ((pname, pty) :: env)
        end
      in
      let body_env := build_env paramsAst env in
      retAst <- elaborate env ret_ty (Some TypeUniverse) ;
      bodyAst <- elaborate body_env body (Some (fst retAst)) ;
      ret (AstDef name type_params paramsAst (fst retAst) (fst bodyAst), AstRef "Unit")
      
  | EnumCST _ _ _ _ => throw "EnumCST not implemented in elaborator"
  | RecordCST _ _ _ _ => throw "RecordCST not implemented in elaborator"
  | _ => throw "Unsupported CST node for elaboration"
  end.
