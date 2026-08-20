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
          MatchCST expr [] span
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
