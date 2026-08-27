From Stdlib Require Import Strings.String.
From Stdlib Require Import List.
From Stdlib Require Import Ascii.
Import ListNotations.
Open Scope string_scope.

Require Import Chester.AST.
Require Import Chester.TypeScriptAST.
Require Import Chester.GoAST.

Definition digit_char (d : nat) : string :=
  match d with
  | 0 => "0" | 1 => "1" | 2 => "2" | 3 => "3" | 4 => "4"
  | 5 => "5" | 6 => "6" | 7 => "7" | 8 => "8" | _ => "9"
  end.

(* nat_to_string for small field indices (up to 99 is plenty for enum variants) *)
Definition nat_to_string (n : nat) : string :=
  let tens := Nat.div n 10 in
  let ones := Nat.modulo n 10 in
  if Nat.eqb tens 0 then digit_char ones
  else digit_char tens ++ digit_char ones.

Definition effect_label (eff : EffectRef) : string :=
  match eff with
  | UserEffect n => n
  | BuiltinEffect n => n
  end.

(* 
  TypeScript Backend
*)
Fixpoint emit_ts_expr (ast : AST) {struct ast} : TypeScriptExpr :=
  let fix map_ts_expr (ls : list AST) : list TypeScriptExpr :=
    match ls with
    | [] => []
    | x :: xs => emit_ts_expr x :: map_ts_expr xs
    end
  in
  match ast with
  | AstRef name => TsIdentifier name
  | AstTuple elems => TsArray (map_ts_expr elems)
  | AstStringLit s => TsStringLiteral s
  | AstIntLit n => TsNumberLiteral (nat_to_string n)
  | AstBlock stmts ret =>
      let fix map_ts_stmt (ls : list AST) : list TypeScriptStmt :=
        match ls with
        | [] => []
        | x :: xs => emit_ts_stmt x :: map_ts_stmt xs
        end
      in TsIIFE (map_ts_stmt stmts ++ [TsReturn (emit_ts_expr ret)])
  | AstApp func args => TsCall (emit_ts_expr func) (map_ts_expr args)
  | AstImplicitApp func _args => emit_ts_expr func  (* type args erased — no runtime representation *)
  | AstLam argName argTy body => TsArrow [argName] (emit_ts_block body)
  | AstPi argName argTy retTy effs => TsIdentifier "any"
  | AstFunTy _tparams _params _ret_ty _effs => TsIdentifier "any"  (* non-curried fun type — erased *)
  | AstDo op args =>
      let op_name := match op with AstRef n => n | _ => "unknown" end in
      TsCall (TsIdentifier "__chester_perform") [TsStringLiteral op_name; TsArray (map_ts_expr args)]
  | AstHandle action eff handlers =>
      let fix emit_hs (hs : list (string * AST)) : list (string * TypeScriptExpr) :=
        match hs with
        | [] => []
        | (op, fn) :: rest => (op, emit_ts_expr fn) :: emit_hs rest
        end
      in
      TsCall (TsIdentifier "__chester_handle")
        [TsStringLiteral (effect_label eff);
         TsArrow [] (emit_ts_block action);
         TsObjectLiteral (emit_hs handlers)]
  | AstBoolLit b => TsBooleanLiteral b
  | AstLet name value => TsIIFE [TsLet name (emit_ts_expr value)]
  | AstIf cond true_br false_br => TsIIFE [TsIfStmt (emit_ts_expr cond) (emit_ts_block true_br) (emit_ts_block false_br)]
  | AstDef name _ params _ body => TsIIFE [TsFunctionDecl name (map fst params) (emit_ts_block body)]
  | AstEnum _ _ _ => TsIdentifier "null"
  | AstExtension _ _ _ _ => TsIdentifier "null"
  | AstMatch expr cases => TsIIFE (let fix emit_cases (cs : list (PatternAST * AST)) : list TypeScriptStmt :=
        match cs with
        | [] => [TsThrow "Non-exhaustive match"]
        | (pat, body) :: rest =>
            match pat with
            | PatConstructor cname vars =>
                let cond := TsCall (TsIdentifier "prim__string_eq") [TsPropertyAccess (TsIdentifier "_match_val") "_tag"; TsStringLiteral cname] in
                let body_ts := emit_ts_block body in
                let fix bind_vars (vs : list string) (idx : nat) (acc : list TypeScriptStmt) : list TypeScriptStmt :=
                  match vs with
                  | [] => acc
                  | v :: vs' => bind_vars vs' (S idx) (TsLet v (TsPropertyAccess (TsIdentifier "_match_val") ("_f" ++ nat_to_string idx)) :: acc)
                  end
                in
                [TsIfStmt cond (bind_vars vars 0 [] ++ body_ts) (emit_cases rest)]
            | PatWildcard => [TsIfStmt (TsBooleanLiteral true) (emit_ts_block body) (emit_cases rest)]
            | PatVar v => TsLet v (TsIdentifier "_match_val") :: emit_ts_block body
            end
        end
      in TsLet "_match_val" (emit_ts_expr expr) :: emit_cases cases)
  | AstRecord name _ _ => TsIdentifier "null"
  | AstFieldAccess expr field => TsPropertyAccess (emit_ts_expr expr) field
  | AstMeta id => TsIdentifier ("/* ?meta_" ++ nat_to_string id ++ " */")
  | AstUniverse _ => TsIIFE [TsThrow "Universe in term"]
  | AstError e => TsIIFE [TsThrow e]
  | AstSpan _ inner => emit_ts_expr inner
  end

with emit_ts_stmt (ast : AST) {struct ast} : TypeScriptStmt :=
  match ast with
  | AstLet name value => TsLet name (emit_ts_expr value)
  | AstDef name _ params _ body => TsFunctionDecl name (map fst params) (emit_ts_block body)
  | AstRecord name _ _ => TsInterface name
  | AstExtension _ _ _ meths =>
      let fix map_meths (ls : list AST) : list TypeScriptStmt :=
        match ls with
        | [] => []
        | x :: xs => emit_ts_stmt x :: map_meths xs
        end
      in TsBlock (map_meths meths)
  | AstEnum name _ variants =>
      let fix emit_variant (v : string * list AST * AST) : string * TypeScriptExpr :=
        let vname := fst (fst v) in
        let fields := snd (fst v) in
        let fix field_names (n : nat) (fs : list AST) : list string :=
          match fs with
          | [] => []
          | _ :: rest => ("_f" ++ nat_to_string n) :: field_names (S n) rest
          end
        in
        let params := field_names 0 fields in
        let fix field_pairs (ps : list string) : list (string * TypeScriptExpr) :=
          match ps with
          | [] => []
          | p :: rest => (p, TsIdentifier p) :: field_pairs rest
          end
        in
        let body := TsObjectLiteral (("_tag", TsStringLiteral vname) :: field_pairs params) in
        match params with
        | [] => (vname, body)
        | _ => (vname, TsArrow params [TsReturn body])
        end
      in
      let fix emit_variants (vs : list (string * list AST * AST)) : list (string * TypeScriptExpr) :=
        match vs with
        | [] => []
        | v :: rest => emit_variant v :: emit_variants rest
        end
      in
      TsConst name (TsObjectLiteral (emit_variants variants))
  | AstRef name => TsExprStmt (TsIdentifier name)
  | AstTuple elems => 
      let fix map_ts_expr (ls : list AST) : list TypeScriptExpr :=
        match ls with
        | [] => []
        | x :: xs => emit_ts_expr x :: map_ts_expr xs
        end
      in TsExprStmt (TsArray (map_ts_expr elems))
  | AstStringLit s => TsExprStmt (TsStringLiteral s)
  | AstIntLit n => TsExprStmt (TsNumberLiteral (nat_to_string n))
  | AstBlock stmts ret => 
      let fix map_ts_stmt (ls : list AST) : list TypeScriptStmt :=
        match ls with
        | [] => []
        | x :: xs => emit_ts_stmt x :: map_ts_stmt xs
        end
      in TsExprStmt (TsIIFE (map_ts_stmt stmts ++ [TsReturn (emit_ts_expr ret)]))
  | AstApp func args => 
      let fix map_ts_expr (ls : list AST) : list TypeScriptExpr :=
        match ls with
        | [] => []
        | x :: xs => emit_ts_expr x :: map_ts_expr xs
        end
      in TsExprStmt (TsCall (emit_ts_expr func) (map_ts_expr args))
  | AstImplicitApp func _args => TsExprStmt (emit_ts_expr func)  (* type args erased *)
  | AstLam argName argTy body => TsExprStmt (TsArrow [argName] (emit_ts_block body))
  | AstPi argName argTy retTy effs => TsExprStmt (TsIdentifier "any")
  | AstFunTy _tparams _params _ret_ty _effs => TsExprStmt (TsIdentifier "any")
  | AstDo op args =>
      let op_name := match op with AstRef n => n | _ => "unknown" end in
      let fix map_ts_expr (ls : list AST) : list TypeScriptExpr :=
        match ls with
        | [] => []
        | x :: xs => emit_ts_expr x :: map_ts_expr xs
        end
      in
      TsExprStmt (TsCall (TsIdentifier "__chester_perform")
        [TsStringLiteral op_name; TsArray (map_ts_expr args)])
  | AstHandle action eff handlers =>
      let fix emit_hs (hs : list (string * AST)) : list (string * TypeScriptExpr) :=
        match hs with
        | [] => []
        | (op, fn) :: rest => (op, emit_ts_expr fn) :: emit_hs rest
        end
      in
      TsExprStmt (TsCall (TsIdentifier "__chester_handle")
        [TsStringLiteral (effect_label eff);
         TsArrow [] (emit_ts_block action);
         TsObjectLiteral (emit_hs handlers)])
  | AstBoolLit b => TsExprStmt (TsBooleanLiteral b)
  | AstIf cond true_br false_br => TsExprStmt (TsIIFE [TsIfStmt (emit_ts_expr cond) (emit_ts_block true_br) (emit_ts_block false_br)])
  | AstMatch expr cases => TsExprStmt (TsIIFE (let fix emit_cases (cs : list (PatternAST * AST)) : list TypeScriptStmt :=
        match cs with
        | [] => [TsThrow "Non-exhaustive match"]
        | (pat, body) :: rest =>
            match pat with
            | PatConstructor cname vars =>
                let cond := TsCall (TsIdentifier "prim__string_eq") [TsPropertyAccess (TsIdentifier "_match_val") "_tag"; TsStringLiteral cname] in
                let body_ts := emit_ts_block body in
                let fix bind_vars (vs : list string) (idx : nat) (acc : list TypeScriptStmt) : list TypeScriptStmt :=
                  match vs with
                  | [] => acc
                  | v :: vs' => bind_vars vs' (S idx) (TsLet v (TsIndexAccess (TsPropertyAccess (TsIdentifier "_match_val") "args") (TsNumberLiteral (nat_to_string idx))) :: acc)
                  end
                in
                [TsIfStmt cond (bind_vars vars 0 [] ++ body_ts) (emit_cases rest)]
            | PatWildcard => [TsIfStmt (TsBooleanLiteral true) (emit_ts_block body) (emit_cases rest)]
            | PatVar v => TsLet v (TsIdentifier "_match_val") :: emit_ts_block body
            end
        end
      in TsLet "_match_val" (emit_ts_expr expr) :: emit_cases cases))
  | AstFieldAccess expr field => TsExprStmt (TsPropertyAccess (emit_ts_expr expr) field)
  | AstMeta id => TsExprStmt (TsIdentifier ("/* ?meta_" ++ nat_to_string id ++ " */"))
  | AstUniverse _ => TsExprStmt (TsIIFE [TsThrow "Universe in term"])
  | AstError e => TsExprStmt (TsIIFE [TsThrow e])
  | AstSpan _ inner => emit_ts_stmt inner
  end

with emit_ts_block (ast : AST) {struct ast} : list TypeScriptStmt :=
  match ast with
  | AstBlock stmts ret =>
      let fix map_ts_stmt (ls : list AST) : list TypeScriptStmt :=
        match ls with
        | [] => []
        | x :: xs => emit_ts_stmt x :: map_ts_stmt xs
        end
      in map_ts_stmt stmts ++ [TsReturn (emit_ts_expr ret)]
  | AstIf cond true_br false_br => [TsIfStmt (emit_ts_expr cond) (emit_ts_block true_br) (emit_ts_block false_br)]
  | AstMatch expr cases => let fix emit_cases (cs : list (PatternAST * AST)) : list TypeScriptStmt :=
        match cs with
        | [] => [TsThrow "Non-exhaustive match"]
        | (pat, body) :: rest =>
            match pat with
            | PatConstructor cname vars =>
                let cond := TsCall (TsIdentifier "prim__string_eq") [TsPropertyAccess (TsIdentifier "_match_val") "_tag"; TsStringLiteral cname] in
                let body_ts := emit_ts_block body in
                let fix bind_vars (vs : list string) (idx : nat) (acc : list TypeScriptStmt) : list TypeScriptStmt :=
                  match vs with
                  | [] => acc
                  | v :: vs' => bind_vars vs' (S idx) (TsLet v (TsIndexAccess (TsPropertyAccess (TsIdentifier "_match_val") "args") (TsNumberLiteral (nat_to_string idx))) :: acc)
                  end
                in
                [TsIfStmt cond (bind_vars vars 0 [] ++ body_ts) (emit_cases rest)]
            | PatWildcard => [TsIfStmt (TsBooleanLiteral true) (emit_ts_block body) (emit_cases rest)]
            | PatVar v => TsLet v (TsIdentifier "_match_val") :: emit_ts_block body
            end
        end
      in TsLet "_match_val" (emit_ts_expr expr) :: emit_cases cases
  | AstUniverse _ => [TsThrow "Universe in term"]
  | AstError e => [TsThrow e]
  | AstSpan _ inner => emit_ts_block inner
  | AstRef name => [TsReturn (TsIdentifier name)]
  | AstTuple elems => 
      let fix map_ts_expr (ls : list AST) : list TypeScriptExpr :=
        match ls with
        | [] => []
        | x :: xs => emit_ts_expr x :: map_ts_expr xs
        end
      in [TsReturn (TsArray (map_ts_expr elems))]
  | AstStringLit s => [TsReturn (TsStringLiteral s)]
  | AstIntLit n => [TsReturn (TsNumberLiteral (nat_to_string n))]
  | AstApp func args => 
      let fix map_ts_expr (ls : list AST) : list TypeScriptExpr :=
        match ls with
        | [] => []
        | x :: xs => emit_ts_expr x :: map_ts_expr xs
        end
      in [TsReturn (TsCall (emit_ts_expr func) (map_ts_expr args))]
  | AstImplicitApp func _args => [TsReturn (emit_ts_expr func)]  (* type args erased *)
  | AstLam argName argTy body => [TsReturn (TsArrow [argName] (emit_ts_block body))]
  | AstPi argName argTy retTy effs => [TsReturn (TsIdentifier "any")]
  | AstFunTy _tparams _params _ret_ty _effs => [TsReturn (TsIdentifier "any")]
  | AstDo op args =>
      let op_name := match op with AstRef n => n | _ => "unknown" end in
      let fix map_ts_expr (ls : list AST) : list TypeScriptExpr :=
        match ls with
        | [] => []
        | x :: xs => emit_ts_expr x :: map_ts_expr xs
        end
      in
      [TsReturn (TsCall (TsIdentifier "__chester_perform")
        [TsStringLiteral op_name; TsArray (map_ts_expr args)])]
  | AstHandle action eff handlers =>
      let fix emit_hs (hs : list (string * AST)) : list (string * TypeScriptExpr) :=
        match hs with
        | [] => []
        | (op, fn) :: rest => (op, emit_ts_expr fn) :: emit_hs rest
        end
      in
      [TsReturn (TsCall (TsIdentifier "__chester_handle")
        [TsStringLiteral (effect_label eff);
         TsArrow [] (emit_ts_block action);
         TsObjectLiteral (emit_hs handlers)])]
  | AstBoolLit b => [TsReturn (TsBooleanLiteral b)]
  | AstLet name value => [TsReturn (TsIIFE [TsLet name (emit_ts_expr value)])]
  | AstDef name _ params _ body => [TsReturn (TsIIFE [TsFunctionDecl name (map fst params) (emit_ts_block body)])]
  | AstEnum _ _ _ => [TsReturn (TsIdentifier "null")]
  | AstExtension _ _ _ _ => [TsReturn (TsIdentifier "null")]
  | AstRecord name _ _ => [TsReturn (TsIdentifier "null")]
  | AstFieldAccess expr field => [TsReturn (TsPropertyAccess (emit_ts_expr expr) field)]
  | AstMeta id => [TsReturn (TsIdentifier ("/* ?meta_" ++ nat_to_string id ++ " */"))]
  end.

Definition emit_ts (ast : AST) : TypeScriptStmt :=
  match ast with
  | AstBlock stmts ret =>
      let fix map_ts_stmt (ls : list AST) : list TypeScriptStmt :=
        match ls with
        | [] => []
        | x :: xs => emit_ts_stmt x :: map_ts_stmt xs
        end
      in
      (* Emit as flat top-level sequence so declarations are globally scoped *)
      TsBlock (map_ts_stmt stmts ++ [TsExprStmt (emit_ts_expr ret)])
  | _ => emit_ts_stmt ast
  end.



(* 
  Golang Backend
*)
Fixpoint emit_go_expr (ast : AST) {struct ast} : GoExpr :=
  let fix map_go_expr (ls : list AST) : list GoExpr :=
    match ls with
    | [] => []
    | x :: xs => emit_go_expr x :: map_go_expr xs
    end
  in
  match ast with
  | AstRef name => GoIdentifier name
  | AstTuple elems => GoArray (map_go_expr elems)
  | AstStringLit s => GoStringLiteral s
  | AstIntLit n => GoIntLiteral (nat_to_string n)
  | AstBlock stmts ret => 
      let fix map_go_stmt (ls : list AST) : list GoStmt :=
        match ls with
        | [] => []
        | x :: xs => emit_go_stmt x :: map_go_stmt xs
        end
      in GoCall (GoFuncLiteral [] (map_go_stmt stmts ++ [GoReturn (emit_go_expr ret)])) []
  | AstApp func args => GoCall (emit_go_expr func) (map_go_expr args)
  | AstImplicitApp func _args => emit_go_expr func  (* type args erased *)
  | AstFunTy _tparams _params _ret_ty _effs => GoIdentifier "interface{}"
  | AstLam argName argTy body => GoFuncLiteral [argName] (emit_go_block body)
  | AstPi argName argTy retTy effs => GoIdentifier "interface{}"
  | AstDo op args => GoCall (emit_go_expr op) (map_go_expr args)
  | AstHandle action eff handlers => emit_go_expr action
  | AstBoolLit b => GoBoolLiteral b
  | AstLet name value => GoCall (GoFuncLiteral [] [GoLet name (emit_go_expr value)]) []
  | AstIf cond true_br false_br => GoCall (GoFuncLiteral [] [GoIfStmt (emit_go_expr cond) (emit_go_block true_br) (emit_go_block false_br)]) []
  | AstDef name _ params _ body => GoCall (GoFuncLiteral [] [GoFuncDecl name (map fst params) (emit_go_block body)]) []
  | AstEnum _ _ _ => GoIdentifier "nil"
  | AstExtension _ _ _ _ => GoIdentifier "nil"
  | AstMatch expr cases => 
      let fix emit_cases (cs : list (PatternAST * AST)) : list GoStmt :=
        match cs with
        | [] => [GoPanic "Non-exhaustive match"]
        | (pat, body) :: rest =>
            match pat with
            | PatConstructor cname vars =>
                let cond := GoCall (GoIdentifier "_ok && _tag[""_tag""] ==") [GoStringLiteral cname] in
                let body_go := emit_go_block body in
                let fix bind_vars (vs : list string) (idx : nat) (acc : list GoStmt) : list GoStmt :=
                  match vs with
                  | [] => acc
                  | v :: vs' => bind_vars vs' (S idx) (GoLet v (GoIndex (GoTypeAssert (GoSelector (GoIdentifier "_tag") "args") "[]interface{}") (GoIntLiteral (nat_to_string idx))) :: acc)
                  end
                in
                [GoIfStmt cond (bind_vars vars 0 [] ++ body_go) (emit_cases rest)]
            | PatWildcard => [GoIfStmt (GoBoolLiteral true) (emit_go_block body) (emit_cases rest)]
            | PatVar v => GoLet v (GoIdentifier "_match_val") :: emit_go_block body
            end
        end
      in GoCall (GoFuncLiteral [] (GoLet "_match_val" (emit_go_expr expr) :: emit_cases cases)) []
  | AstRecord name _ _ => GoIdentifier "nil"
  | AstFieldAccess expr field => GoSelector (emit_go_expr expr) field
  | AstMeta id => GoIdentifier ("/* ?meta_" ++ nat_to_string id ++ " */")
  | AstUniverse _ => GoCall (GoFuncLiteral [] [GoPanic "Universe in term"]) []
  | AstError e => GoCall (GoFuncLiteral [] [GoPanic e]) []
  | AstSpan _ inner => emit_go_expr inner
  end

with emit_go_stmt (ast : AST) {struct ast} : GoStmt :=
  match ast with
  | AstLet name value => GoLet name (emit_go_expr value)
  | AstDef name _ params _ body => GoFuncDecl name (map fst params) (emit_go_block body)
  | AstRecord name _ _ => GoStruct name
  | AstEnum _ _ _ => GoEmpty
  | AstExtension _ _ _ meths =>
      let fix map_meths (ls : list AST) : list GoStmt :=
        match ls with
        | [] => []
        | x :: xs => emit_go_stmt x :: map_meths xs
        end
      in GoBlock (map_meths meths)
  | AstRef name => GoExprStmt (GoIdentifier name)
  | AstTuple elems => 
      let fix map_go_expr (ls : list AST) : list GoExpr :=
        match ls with
        | [] => []
        | x :: xs => emit_go_expr x :: map_go_expr xs
        end
      in GoExprStmt (GoArray (map_go_expr elems))
  | AstStringLit s => GoExprStmt (GoStringLiteral s)
  | AstIntLit n => GoExprStmt (GoIntLiteral (nat_to_string n))
  | AstBlock stmts ret => 
      let fix map_go_stmt (ls : list AST) : list GoStmt :=
        match ls with
        | [] => []
        | x :: xs => emit_go_stmt x :: map_go_stmt xs
        end
      in GoExprStmt (GoCall (GoFuncLiteral [] (map_go_stmt stmts ++ [GoReturn (emit_go_expr ret)])) [])
  | AstApp func args => 
      let fix map_go_expr (ls : list AST) : list GoExpr :=
        match ls with
        | [] => []
        | x :: xs => emit_go_expr x :: map_go_expr xs
        end
      in GoExprStmt (GoCall (emit_go_expr func) (map_go_expr args))
  | AstImplicitApp func _args => GoExprStmt (emit_go_expr func)  (* type args erased *)
  | AstFunTy _tparams _params _ret_ty _effs => GoExprStmt (GoIdentifier "interface{}")
  | AstLam argName argTy body => GoExprStmt (GoFuncLiteral [argName] (emit_go_block body))
  | AstPi argName argTy retTy effs => GoExprStmt (GoIdentifier "interface{}")
  | AstDo op args => 
      let fix map_go_expr (ls : list AST) : list GoExpr :=
        match ls with
        | [] => []
        | x :: xs => emit_go_expr x :: map_go_expr xs
        end
      in GoExprStmt (GoCall (emit_go_expr op) (map_go_expr args))
  | AstHandle action eff handlers => GoExprStmt (GoIdentifier "interface{}")
  | AstBoolLit b => GoExprStmt (GoBoolLiteral b)
  | AstIf cond true_br false_br => GoExprStmt (GoCall (GoFuncLiteral [] [GoIfStmt (emit_go_expr cond) (emit_go_block true_br) (emit_go_block false_br)]) [])
  | AstMatch expr cases => 
      let fix emit_cases (cs : list (PatternAST * AST)) : list GoStmt :=
        match cs with
        | [] => [GoPanic "Non-exhaustive match"]
        | (pat, body) :: rest =>
            match pat with
            | PatConstructor cname vars =>
                let cond := GoCall (GoIdentifier "_ok && _tag[""_tag""] ==") [GoStringLiteral cname] in
                let body_go := emit_go_block body in
                let fix bind_vars (vs : list string) (idx : nat) (acc : list GoStmt) : list GoStmt :=
                  match vs with
                  | [] => acc
                  | v :: vs' => bind_vars vs' (S idx) (GoLet v (GoIndex (GoTypeAssert (GoSelector (GoIdentifier "_tag") "args") "[]interface{}") (GoIntLiteral (nat_to_string idx))) :: acc)
                  end
                in
                [GoIfStmt cond (bind_vars vars 0 [] ++ body_go) (emit_cases rest)]
            | PatWildcard => [GoIfStmt (GoBoolLiteral true) (emit_go_block body) (emit_cases rest)]
            | PatVar v => GoLet v (GoIdentifier "_match_val") :: emit_go_block body
            end
        end
      in GoExprStmt (GoCall (GoFuncLiteral [] (GoLet "_match_val" (emit_go_expr expr) :: emit_cases cases)) [])
  | AstFieldAccess expr field => GoExprStmt (GoSelector (emit_go_expr expr) field)
  | AstMeta id => GoExprStmt (GoIdentifier ("/* ?meta_" ++ nat_to_string id ++ " */"))
  | AstUniverse _ => GoExprStmt (GoCall (GoFuncLiteral [] [GoPanic "Universe in term"]) [])
  | AstError e => GoExprStmt (GoCall (GoFuncLiteral [] [GoPanic e]) [])
  | AstSpan _ inner => emit_go_stmt inner
  end

with emit_go_block (ast : AST) {struct ast} : list GoStmt :=
  match ast with
  | AstBlock stmts ret =>
      let fix map_go_stmt (ls : list AST) : list GoStmt :=
        match ls with
        | [] => []
        | x :: xs => emit_go_stmt x :: map_go_stmt xs
        end
      in map_go_stmt stmts ++ [GoReturn (emit_go_expr ret)]
  | AstIf cond true_br false_br => [GoIfStmt (emit_go_expr cond) (emit_go_block true_br) (emit_go_block false_br)]
  | AstMatch expr cases => 
      let fix emit_cases (cs : list (PatternAST * AST)) : list GoStmt :=
        match cs with
        | [] => [GoPanic "Non-exhaustive match"]
        | (pat, body) :: rest =>
            match pat with
            | PatConstructor cname vars =>
                let cond := GoCall (GoIdentifier "_ok && _tag[""_tag""] ==") [GoStringLiteral cname] in
                let body_go := emit_go_block body in
                let fix bind_vars (vs : list string) (idx : nat) (acc : list GoStmt) : list GoStmt :=
                  match vs with
                  | [] => acc
                  | v :: vs' => bind_vars vs' (S idx) (GoLet v (GoIndex (GoTypeAssert (GoSelector (GoIdentifier "_tag") "args") "[]interface{}") (GoIntLiteral (nat_to_string idx))) :: acc)
                  end
                in
                [GoIfStmt cond (bind_vars vars 0 [] ++ body_go) (emit_cases rest)]
            | PatWildcard => [GoIfStmt (GoBoolLiteral true) (emit_go_block body) (emit_cases rest)]
            | PatVar v => GoLet v (GoIdentifier "_match_val") :: emit_go_block body
            end
        end
      in GoLet "_match_val" (emit_go_expr expr) :: emit_cases cases
  | AstUniverse _ => [GoPanic "Universe in term"]
  | AstError e => [GoPanic e]
  | AstSpan _ inner => emit_go_block inner
  | AstRef name => [GoReturn (GoIdentifier name)]
  | AstTuple elems => 
      let fix map_go_expr (ls : list AST) : list GoExpr :=
        match ls with
        | [] => []
        | x :: xs => emit_go_expr x :: map_go_expr xs
        end
      in [GoReturn (GoArray (map_go_expr elems))]
  | AstStringLit s => [GoReturn (GoStringLiteral s)]
  | AstIntLit n => [GoReturn (GoIntLiteral (nat_to_string n))]
  | AstApp func args => 
      let fix map_go_expr (ls : list AST) : list GoExpr :=
        match ls with
        | [] => []
        | x :: xs => emit_go_expr x :: map_go_expr xs
        end
      in [GoReturn (GoCall (emit_go_expr func) (map_go_expr args))]
  | AstImplicitApp func _args => [GoReturn (emit_go_expr func)]  (* type args erased *)
  | AstFunTy _tparams _params _ret_ty _effs => [GoReturn (GoIdentifier "interface{}")]
  | AstLam argName argTy body => [GoReturn (GoFuncLiteral [argName] (emit_go_block body))]
  | AstPi argName argTy retTy effs => [GoReturn (GoIdentifier "interface{}")]
  | AstDo op args => 
      let fix map_go_expr (ls : list AST) : list GoExpr :=
        match ls with
        | [] => []
        | x :: xs => emit_go_expr x :: map_go_expr xs
        end
      in [GoReturn (GoCall (emit_go_expr op) (map_go_expr args))]
  | AstHandle action eff handlers => [GoReturn (GoIdentifier "interface{}")]
  | AstBoolLit b => [GoReturn (GoBoolLiteral b)]
  | AstLet name value => [GoReturn (GoCall (GoFuncLiteral [] [GoLet name (emit_go_expr value)]) [])]
  | AstDef name _ params _ body => [GoReturn (GoCall (GoFuncLiteral [] [GoFuncDecl name (map fst params) (emit_go_block body)]) [])]
  | AstEnum _ _ _ => [GoReturn (GoIdentifier "nil")]
  | AstExtension _ _ _ _ => [GoReturn (GoIdentifier "nil")]
  | AstRecord name _ _ => [GoReturn (GoIdentifier "nil")]
  | AstFieldAccess expr field => [GoReturn (GoSelector (emit_go_expr expr) field)]
  | AstMeta id => [GoReturn (GoIdentifier ("/* ?meta_" ++ nat_to_string id ++ " */"))]
  end.

Definition emit_go (ast : AST) : GoStmt :=
  match ast with
  | AstBlock stmts ret =>
      let fix map_go_stmt (ls : list AST) : list GoStmt :=
        match ls with
        | [] => []
        | x :: xs => emit_go_stmt x :: map_go_stmt xs
        end
      in GoExprStmt (GoCall (GoFuncLiteral [] (map_go_stmt stmts ++ [GoExprStmt (emit_go_expr ret)])) [])
  | _ => emit_go_stmt ast
  end.

