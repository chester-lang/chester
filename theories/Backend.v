From Stdlib Require Import Strings.String.
From Stdlib Require Import List.
From Stdlib Require Import Ascii.
Import ListNotations.

Require Import Chester.AST.
Require Import Chester.TypeScriptAST.
Require Import Chester.GoAST.

(* We mock nat_to_string to keep the example clean *)
Definition nat_to_string (n : nat) : string := "<nat>".

(* 
  TypeScript Backend
*)
Fixpoint emit_ts (expr : AST) {struct expr} : TypeScriptAST :=
  let fix map_ts (ls : list AST) : list TypeScriptAST :=
    match ls with
    | [] => []
    | x :: xs => emit_ts x :: map_ts xs
    end
  in
  match expr with
  | AstRef name => TsIdentifier name
  | AstTuple elems => TsArray (map_ts elems)
  | AstStringLit s => TsStringLiteral s
  | AstIntLit n => TsNumberLiteral (nat_to_string n)
  | AstBlock stmts ret => TsBlock (map_ts stmts) (emit_ts ret)
  | AstApp func args => TsCall (emit_ts func) (map_ts args)
  | AstLam argName argTy body => TsArrow [argName] (emit_ts body)
  | AstPi argName argTy retTy effs => TsRaw ("(" ++ argName ++ ": any) => any")
  | AstDo op args => TsAwait (TsCall (emit_ts op) (map_ts args))
  | AstHandle action eff handlers => TsRaw ("/* handle */")
  | AstLet name value body => TsRaw ("/* let " ++ name ++ " */")
  | AstIf cond thenB elseB => TsRaw "/* if */"
  | AstDef name _ _ _ _ => TsRaw ("/* def " ++ name ++ " */")
  | AstEnum name _ _ => TsRaw ("/* enum " ++ name ++ " */")
  | AstRecord name _ _ => TsRaw ("/* record " ++ name ++ " */")
  | AstMeta id => TsRaw ("/* ?meta_" ++ nat_to_string id ++ " */")
  | AstError e => TsRaw ("/* ERROR: " ++ e ++ " */")
  end.

(* 
  Golang Backend
*)
Fixpoint emit_go (expr : AST) {struct expr} : GoAST :=
  let fix map_go (ls : list AST) : list GoAST :=
    match ls with
    | [] => []
    | x :: xs => emit_go x :: map_go xs
    end
  in
  match expr with
  | AstRef name => GoIdentifier name
  | AstTuple elems => GoArray (map_go elems)
  | AstStringLit s => GoStringLiteral s
  | AstIntLit n => GoIntLiteral (nat_to_string n)
  | AstBlock stmts ret => GoBlock (map_go stmts) (emit_go ret)
  | AstApp func args => GoCall (emit_go func) (map_go args)
  | AstLam argName argTy body => GoFuncLiteral [argName] (emit_go body)
  | AstPi argName argTy retTy effs => GoRaw ("func(" ++ argName ++ " interface{}) interface{}")
  | AstDo op args => GoCall (emit_go op) (map_go args)
  | AstHandle action eff handlers => GoRaw ("/* handle */")
  | AstLet name value body => GoRaw ("/* let " ++ name ++ " */")
  | AstIf cond thenB elseB => GoRaw "/* if */"
  | AstDef name _ _ _ _ => GoRaw ("/* def " ++ name ++ " */")
  | AstEnum name _ _ => GoRaw ("/* enum " ++ name ++ " */")
  | AstRecord name _ _ => GoRaw ("/* record " ++ name ++ " */")
  | AstMeta id => GoRaw ("/* ?meta_" ++ nat_to_string id ++ " */")
  | AstError e => GoRaw ("/* ERROR: " ++ e ++ " */")
  end.
