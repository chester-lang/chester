From Stdlib Require Import Strings.String.
From Stdlib Require Import List.
Import ListNotations.

(* 
  TypeScript AST Representation for the Backend 
*)

Inductive TypeScriptAST : Type :=
  | TsNumberLiteral : string -> TypeScriptAST
  | TsStringLiteral : string -> TypeScriptAST
  | TsBooleanLiteral : bool -> TypeScriptAST
  | TsIdentifier : string -> TypeScriptAST
  | TsPropertyAccess : TypeScriptAST -> string -> TypeScriptAST
  | TsCall : TypeScriptAST -> list TypeScriptAST -> TypeScriptAST
  | TsArrow : list string -> TypeScriptAST -> TypeScriptAST
  | TsBlock : list TypeScriptAST -> TypeScriptAST -> TypeScriptAST
  | TsArray : list TypeScriptAST -> TypeScriptAST
  | TsAwait : TypeScriptAST -> TypeScriptAST
  | TsRaw : string -> TypeScriptAST.

(* Helper function to stringify TS AST (pretty printing) *)
Fixpoint concat_strings (sep : string) (ls : list string) : string :=
  match ls with
  | [] => ""
  | [x] => x
  | x :: xs => x ++ sep ++ concat_strings sep xs
  end.

Fixpoint stringify_ts (expr : TypeScriptAST) {struct expr} : string :=
  let fix map_ts (ls : list TypeScriptAST) : list string :=
    match ls with
    | [] => []
    | x :: xs => stringify_ts x :: map_ts xs
    end
  in
  match expr with
  | TsNumberLiteral n => n
  | TsStringLiteral s => """" ++ s ++ """"
  | TsBooleanLiteral b => if b then "true" else "false"
  | TsIdentifier name => name
  | TsPropertyAccess obj prop => stringify_ts obj ++ "." ++ prop
  | TsCall callee args => stringify_ts callee ++ "(" ++ concat_strings ", " (map_ts args) ++ ")"
  | TsArrow params body => "(" ++ concat_strings ", " params ++ ") => " ++ stringify_ts body
  | TsBlock stmts ret => "{ " ++ concat_strings "; " (map_ts stmts) ++ "; return " ++ stringify_ts ret ++ "; }"
  | TsArray elements => "[" ++ concat_strings ", " (map_ts elements) ++ "]"
  | TsAwait e => "await " ++ stringify_ts e
  | TsRaw s => s
  end.
