From Stdlib Require Import Strings.String.
From Stdlib Require Import List.
Import ListNotations.
Open Scope string_scope.
Require Import Chester.CST.

Fixpoint split_at_eq (acc : list CST) (ls : list CST) : option (list CST * list CST) :=
  match ls with
  | [] => None
  | Symbol "=" _ :: rest => Some (rev acc, rest)
  | x :: rest => split_at_eq (x :: acc) rest
  end.

Fixpoint collapse_apps_aux (elems : list CST) (acc : list CST) : list CST :=
  match elems with
  | [] => rev acc
  | Symbol "." _ :: Symbol field span :: rest =>
      match acc with
      | expr :: acc_rest => collapse_apps_aux rest (FieldAccessCST expr field span :: acc_rest)
      | [] => collapse_apps_aux rest (Symbol field span :: Symbol "." empty_span :: [])
      end
  | Tuple args span :: rest =>
      match acc with
      | func :: acc_rest => collapse_apps_aux rest (AppCST func args span :: acc_rest)
      | [] => collapse_apps_aux rest (Tuple args span :: [])
      end
  | ListLiteral targs tspan :: rest =>
      match acc with
      | expr :: acc_rest => collapse_apps_aux rest (TypeAppCST expr targs tspan :: acc_rest)
      | [] => collapse_apps_aux rest (ListLiteral targs tspan :: [])
      end
  | x :: rest => collapse_apps_aux rest (x :: acc)
  end.
Definition collapse_apps elems := collapse_apps_aux elems [].

Fixpoint expand_if (elems : list CST) (span : Span) : option CST :=
  match elems with
  | Symbol "if" _ :: cond :: Symbol "then" _ :: thenB :: Symbol "else" _ :: rest =>
      match expand_if rest span with
      | Some else_if_cst => Some (IfCST cond thenB else_if_cst span)
      | None =>
          match rest with
          | [elseB] => Some (IfCST cond thenB elseB span)
          | _ => None
          end
      end
  | Symbol "if" _ :: cond :: Symbol "then" _ :: thenB :: [] =>
      Some (IfCST cond thenB (Tuple [] span) span)
  | _ => None
  end.

Fixpoint expand_seq_expr (elems : list CST) (span : Span) : CST :=
  let collapsed := collapse_apps elems in
  match expand_if collapsed span with
  | Some if_cst => if_cst
  | None =>
      match collapsed with
      | [] => Tuple [] span
      | [x] => x
      | Symbol "\" _ :: Symbol arg _ :: Symbol "=>" _ :: body =>
          let body_cst := match body with | [] => Tuple [] span | [x] => x | _ => SeqOf body span end in
          LamCST arg None body_cst span
      | Symbol "match" _ :: expr :: Block cases _ _ :: [] =>
          (* Extract case branches into PatternCST * CST pairs.
             Bodies are left as SeqOf so expand_cst can recurse into them later.
             We CANNOT call expand_seq_expr here (same fixpoint) — Coq forbids it. *)
          let fix extract_vars (args : list CST) : list string :=
            match args with
            | [] => []
            | Symbol vname _ :: rest => vname :: extract_vars rest
            | _ :: rest => extract_vars rest
            end
          in
          let fix extract_cases (cs : list CST) : list (PatternCST * CST) :=
            match cs with
            | [] => []
            (* case _ => body *)
            | SeqOf (Symbol "case" _ :: Symbol "_" _ :: Symbol "=>" _ :: body) sp :: rest =>
                let body_cst := match body with
                  | [] => Tuple [] sp
                  | [b] => b
                  | _ => SeqOf body sp
                  end in
                (PatWildcardCST sp, body_cst) :: extract_cases rest
            (* case Constructor(vars...) => body  -- field-access form: Ctor.Variant(vars) *)
            | SeqOf (Symbol "case" _ ::
                     FieldAccessCST (Symbol _ _) vname _ ::
                     Tuple args _ ::
                     Symbol "=>" _ :: body) sp :: rest =>
                let body_cst := match body with
                  | [] => Tuple [] sp
                  | [b] => b
                  | _ => SeqOf body sp
                  end in
                (PatConstructorCST vname (extract_vars args) sp, body_cst) :: extract_cases rest
            (* case Constructor(vars...) => body *)
            | SeqOf (Symbol "case" _ ::
                     AppCST (Symbol vname _) args _ ::
                     Symbol "=>" _ :: body) sp :: rest =>
                let body_cst := match body with
                  | [] => Tuple [] sp
                  | [b] => b
                  | _ => SeqOf body sp
                  end in
                (PatConstructorCST vname (extract_vars args) sp, body_cst) :: extract_cases rest
            (* case VarName => body  (bare name: could be wildcard-like binding) *)
            | SeqOf (Symbol "case" _ :: Symbol vname _ :: Symbol "=>" _ :: body) sp :: rest =>
                let body_cst := match body with
                  | [] => Tuple [] sp
                  | [b] => b
                  | _ => SeqOf body sp
                  end in
                (* "_" is wildcard; anything else is a variable binding *)
                let pat := if string_dec vname "_"
                           then PatWildcardCST sp
                           else PatVarCST vname sp in
                (pat, body_cst) :: extract_cases rest
            | _ :: rest => extract_cases rest
            end
          in
          MatchCST expr (extract_cases cases) span
      | _ => SeqOf collapsed span
      end
  end.

Fixpoint expand_cst (c : CST) : CST :=
  match c with
  | Block stmts tail span =>
      let expanded_stmts := map expand_cst stmts in
      let expanded_tail := expand_cst tail in
      
      let fix process_stmts (ss : list CST) : list CST :=
        match ss with
        | [] => []
        | SeqOf (Symbol "let" _ :: Symbol name _ :: Symbol "=" _ :: val_exprs) s :: rest =>
            let val_cst := match val_exprs with [v] => v | _ => expand_seq_expr val_exprs s end in
            LetCST name val_cst (Symbol "Unit" empty_span) s :: process_stmts rest
            
        | SeqOf (Symbol "def" _ :: AppCST (Symbol name _) args _ :: rest_def) s :: rest =>
            match split_at_eq [] rest_def with
            | Some (ty_exprs, body_exprs) =>
                let ret_ty := match ty_exprs with 
                              | Symbol ":" _ :: tys => expand_seq_expr tys s 
                              | _ => Symbol "Unknown" empty_span 
                              end in
                let body_cst := match body_exprs with [b] => b | _ => expand_seq_expr body_exprs s end in
                let extract_arg (a: CST) : (string * CST) := 
                   match a with
                   | Symbol n _ => (n, Symbol "Unknown" empty_span)
                   | SeqOf (Symbol n _ :: Symbol ":" _ :: ty :: _) _ => (n, ty)
                   | _ => ("unknown", Symbol "Unknown" empty_span)
                   end
                in
                let params := map extract_arg args in
                DefCST name [] params ret_ty body_cst s :: process_stmts rest
            | None => SeqOf (Symbol "def" empty_span :: Symbol name empty_span :: Tuple args empty_span :: rest_def) s :: process_stmts rest
            end
            
        | SeqOf (Symbol "def" _ :: AppCST (TypeAppCST (Symbol name _) targs _) args _ :: rest_def) s :: rest =>
            match split_at_eq [] rest_def with
            | Some (ty_exprs, body_exprs) =>
                let ret_ty := match ty_exprs with 
                              | Symbol ":" _ :: tys => expand_seq_expr tys s 
                              | _ => Symbol "Unknown" empty_span 
                              end in
                let body_cst := match body_exprs with [b] => b | _ => expand_seq_expr body_exprs s end in
                let extract_arg (a: CST) : (string * CST) := 
                   match a with
                   | Symbol n _ => (n, Symbol "Unknown" empty_span)
                   | SeqOf (Symbol n _ :: Symbol ":" _ :: ty :: _) _ => (n, ty)
                   | _ => ("unknown", Symbol "Unknown" empty_span)
                   end
                in
                let extract_targ (a: CST) : string := 
                   match a with Symbol n _ => n | _ => "T" end 
                in
                let type_params := map extract_targ targs in
                let params := map extract_arg args in
                DefCST name type_params params ret_ty body_cst s :: process_stmts rest
            | None => SeqOf (Symbol "def" empty_span :: Symbol name empty_span :: ListLiteral targs empty_span :: Tuple args empty_span :: rest_def) s :: process_stmts rest
            end

        | SeqOf (Symbol "enum" _ :: Symbol name _ :: Block variants _ _ :: []) s :: rest =>
            let fix extract_variants (vs : list CST) : list CST :=
              match vs with
              | [] => []
              | SeqOf (Symbol "case" _ :: AppCST (Symbol vname _) vargs _ :: []) _ :: vrest =>
                  AppCST (Symbol vname empty_span) vargs empty_span :: extract_variants vrest
              | SeqOf (Symbol "case" _ :: Symbol vname _ :: []) _ :: vrest =>
                  Symbol vname empty_span :: extract_variants vrest
              | _ :: vrest => extract_variants vrest
              end
            in
            EnumCST name [] (extract_variants variants) s :: process_stmts rest
            
        | SeqOf (Symbol "enum" _ :: TypeAppCST (Symbol name _) targs _ :: Block variants _ _ :: []) s :: rest =>
            let fix extract_variants (vs : list CST) : list CST :=
              match vs with
              | [] => []
              | SeqOf (Symbol "case" _ :: AppCST (Symbol vname _) vargs _ :: []) _ :: vrest =>
                  AppCST (Symbol vname empty_span) vargs empty_span :: extract_variants vrest
              | SeqOf (Symbol "case" _ :: Symbol vname _ :: []) _ :: vrest =>
                  Symbol vname empty_span :: extract_variants vrest
              | _ :: vrest => extract_variants vrest
              end
            in
            let extract_targ (a: CST) : string := 
               match a with Symbol n _ => n | _ => "T" end 
            in
            let type_params := map extract_targ targs in
            EnumCST name type_params (extract_variants variants) s :: process_stmts rest

        | SeqOf (Symbol "record" _ :: AppCST (Symbol name _) fields _ :: []) s :: rest =>
            RecordCST name [] fields s :: process_stmts rest
            
        | SeqOf (Symbol "record" _ :: AppCST (TypeAppCST (Symbol name _) targs _) fields _ :: []) s :: rest =>
            let extract_targ (a: CST) : string := 
               match a with Symbol n _ => n | _ => "T" end 
            in
            let type_params := map extract_targ targs in
            RecordCST name type_params fields s :: process_stmts rest
            
        | stmt :: rest =>
            stmt :: process_stmts rest
        end
      in
      Block (process_stmts expanded_stmts) expanded_tail span

  | Tuple elems span => Tuple (map expand_cst elems) span
  | ListLiteral elems span => ListLiteral (map expand_cst elems) span
  
  | SeqOf elems span => 
      let expanded_elems := map expand_cst elems in
      expand_seq_expr expanded_elems span
      
  | FieldAccessCST expr field span => FieldAccessCST (expand_cst expr) field span
  | MatchCST expr cases span => MatchCST (expand_cst expr) cases span
  | _ => c
  end.

Definition expand_cst_top (expr : CST) : CST := expand_cst expr.
