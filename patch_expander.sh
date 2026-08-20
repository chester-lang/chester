cat << 'INNER' > theories/Expander.v
From Stdlib Require Import Strings.String.
From Stdlib Require Import List.
Import ListNotations.
Open Scope string_scope.
Require Import Chester.CST.

Fixpoint expand_cst (c : CST) : CST :=
  match c with
  | Block stmts tail span =>
      let expanded_stmts := map expand_cst stmts in
      let expanded_tail := expand_cst tail in
      
      let fix process_stmts (ss : list CST) : list CST :=
        match ss with
        | [] => []
        | SeqOf (Symbol "let" _ :: Symbol name _ :: Symbol "=" _ :: val_exprs) s :: rest =>
            let val_cst := match val_exprs with [v] => v | _ => SeqOf val_exprs s end in
            LetCST name val_cst (Symbol "Unit" empty_span) s :: process_stmts rest
            
        | SeqOf (Symbol "def" _ :: Symbol name _ :: Tuple args _ :: Symbol "=" _ :: body_exprs) s :: rest =>
            let body_cst := match body_exprs with [b] => b | _ => SeqOf body_exprs s end in
            let extract_arg (a: CST) : (string * CST) := 
               match a with Symbol n _ => (n, Symbol "Unknown" empty_span) | _ => ("unknown", Symbol "Unknown" empty_span) end 
            in
            let params := map extract_arg args in
            DefCST name [] params (Symbol "Unknown" empty_span) body_cst s :: process_stmts rest
            
        | SeqOf (Symbol "def" _ :: Symbol name _ :: ListLiteral targs _ :: Tuple args _ :: Symbol "=" _ :: body_exprs) s :: rest =>
            let body_cst := match body_exprs with [b] => b | _ => SeqOf body_exprs s end in
            let extract_arg (a: CST) : (string * CST) := 
               match a with Symbol n _ => (n, Symbol "Unknown" empty_span) | _ => ("unknown", Symbol "Unknown" empty_span) end 
            in
            let extract_targ (a: CST) : string := 
               match a with Symbol n _ => n | _ => "T" end 
            in
            let type_params := map extract_targ targs in
            let params := map extract_arg args in
            DefCST name type_params params (Symbol "Unknown" empty_span) body_cst s :: process_stmts rest

        | SeqOf (Symbol "record" _ :: Symbol name _ :: Block fields _ _ :: []) s :: rest =>
            RecordCST name [] fields s :: process_stmts rest
            
        | SeqOf (Symbol "record" _ :: Symbol name _ :: ListLiteral targs _ :: Block fields _ _ :: []) s :: rest =>
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
  | SeqOf elems span => SeqOf (map expand_cst elems) span
  | FieldAccessCST expr field span => FieldAccessCST (expand_cst expr) field span
  | _ => c
  end.
INNER
