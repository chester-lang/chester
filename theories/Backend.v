From Stdlib Require Import Strings.String.
From Stdlib Require Import List.
From Stdlib Require Import Ascii.
Import ListNotations.
Open Scope string_scope.

Require Import Chester.AST.
Require Import Chester.TypeScriptAST.
Require Import Chester.GoAST.

(* We mock nat_to_string to keep the example clean *)
Definition nat_to_string (n : nat) : string := "<nat>".

(* 
  TypeScript Backend
*)
Fixpoint emit_ts (ast : AST) {struct ast} : TypeScriptAST :=
  let fix map_ts (ls : list AST) : list TypeScriptAST :=
    match ls with
    | [] => []
    | x :: xs => emit_ts x :: map_ts xs
    end
  in
  match ast with
  | AstRef name => TsIdentifier name
  | AstTuple elems => TsArray (map_ts elems)
  | AstStringLit s => TsStringLiteral s
  | AstIntLit n => TsNumberLiteral (nat_to_string n)
  | AstBlock stmts ret => TsBlock (map_ts stmts) (emit_ts ret)
  | AstApp func args => TsCall (emit_ts func) (map_ts args)
  | AstLam argName argTy body => TsArrow [argName] (emit_ts body)
  | AstPi argName argTy retTy effs => TsIdentifier "any"
  | AstDo op args => TsAwait (TsCall (emit_ts op) (map_ts args))
  | AstHandle action eff handlers => TsIdentifier "any"
  | AstBoolLit b => TsBooleanLiteral b
  | AstLet name value => TsLet name (emit_ts value)
  | AstIf cond true_br false_br => TsIf (emit_ts cond) (emit_ts true_br) (emit_ts false_br)
  | AstDef name _ params _ body => TsFunctionDecl name (map fst params) (TsBlock [] (emit_ts body))
  | AstEnum _ _ _ => TsEmpty
  | AstMatch expr cases =>
      let fix emit_cases (cs : list (PatternAST * AST)) : TypeScriptAST :=
        match cs with
        | [] => TsThrow "Non-exhaustive match"
        | (pat, body) :: rest =>
            match pat with
            | PatConstructor cname vars =>
                let cond := TsCall (TsPropertyAccess (TsIdentifier "_match_val") "===") [TsStringLiteral cname] in
                let body_ts := emit_ts body in
                let fix bind_vars (vs : list string) (idx : nat) (acc : TypeScriptAST) : TypeScriptAST :=
                  match vs with
                  | [] => acc
                  | v :: vs' => bind_vars vs' (S idx) (TsLet v (TsIndexAccess (TsPropertyAccess (TsIdentifier "_match_val") "args") (TsNumberLiteral (nat_to_string idx))))
                  end
                in
                TsIf cond (bind_vars vars 0 body_ts) (emit_cases rest)
            | PatWildcard => TsIf (TsBooleanLiteral true) (emit_ts body) (emit_cases rest)
            | PatVar v => TsBlock [TsLet v (TsIdentifier "_match_val")] (emit_ts body)
            end
        end
      in
      TsIIFE (TsBlock [TsLet "_match_val" (emit_ts expr)] (emit_cases cases))
  | AstRecord name _ _ => TsInterface name
  | AstFieldAccess expr field => TsPropertyAccess (emit_ts expr) field
  | AstMeta id => TsIdentifier ("/* ?meta_" ++ nat_to_string id ++ " */")
  | AstError e => TsThrow e
  | AstSpan _ inner => emit_ts inner
  end.

(* 
  Golang Backend
*)
Fixpoint emit_go (ast : AST) {struct ast} : GoAST :=
  let fix map_go (ls : list AST) : list GoAST :=
    match ls with
    | [] => []
    | x :: xs => emit_go x :: map_go xs
    end
  in
  match ast with
  | AstRef name => GoIdentifier name
  | AstTuple elems => GoArray (map_go elems)
  | AstStringLit s => GoStringLiteral s
  | AstIntLit n => GoIntLiteral (nat_to_string n)
  | AstBlock stmts ret => GoBlock (map_go stmts) (emit_go ret)
  | AstApp func args => GoCall (emit_go func) (map_go args)
  | AstLam argName argTy body => GoFuncLiteral [argName] (emit_go body)
  | AstPi argName argTy retTy effs => GoIdentifier "interface{}"
  | AstDo op args => GoCall (emit_go op) (map_go args)
  | AstHandle action eff handlers => GoIdentifier "interface{}"
  | AstBoolLit b => GoBoolLiteral b
  | AstLet name value => GoLet name (emit_go value)
  | AstIf cond true_br false_br => GoIf (emit_go cond) (emit_go true_br) (emit_go false_br)
  | AstDef name _ params _ body => GoFuncDecl name (map fst params) (GoBlock [] (emit_go body))
  | AstEnum _ _ _ => GoEmpty
  | AstMatch expr cases =>
      let fix emit_cases (cs : list (PatternAST * AST)) : GoAST :=
        match cs with
        | [] => GoPanic "Non-exhaustive match"
        | (pat, body) :: rest =>
            match pat with
            | PatConstructor cname vars =>
                let cond := GoCall (GoIdentifier "_ok && _tag[""_tag""] ==") [GoStringLiteral cname] in
                let body_go := emit_go body in
                let fix bind_vars (vs : list string) (idx : nat) (acc : GoAST) : GoAST :=
                  match vs with
                  | [] => acc
                  | v :: vs' => bind_vars vs' (S idx) (GoLet v (GoIndex (GoTypeAssert (GoSelector (GoIdentifier "_tag") "args") "[]interface{}") (GoIntLiteral (nat_to_string idx))))
                  end
                in
                GoIf cond (bind_vars vars 0 body_go) (emit_cases rest)
            | PatWildcard => GoIf (GoBoolLiteral true) (emit_go body) (emit_cases rest)
            | PatVar v => GoBlock [GoLet v (GoIdentifier "_match_val")] (emit_go body)
            end
        end
      in
      GoCall (GoFuncLiteral [] (GoBlock [GoLet "_match_val" (emit_go expr)] (emit_cases cases))) []
  | AstRecord name _ _ => GoStruct name
  | AstFieldAccess expr field => GoSelector (emit_go expr) field
  | AstMeta id => GoIdentifier ("/* ?meta_" ++ nat_to_string id ++ " */")
  | AstError e => GoPanic e
  | AstSpan _ inner => emit_go inner
  end.
