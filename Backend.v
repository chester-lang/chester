From Stdlib Require Import Strings.String.
From Stdlib Require Import List.
From Stdlib Require Import Ascii.
Import ListNotations.

Require Import Chester.AST.

(* String Utilities *)
Fixpoint concat_strings (sep : string) (ls : list string) : string :=
  match ls with
  | [] => ""
  | [x] => x
  | x :: xs => x ++ sep ++ concat_strings sep xs
  end.

(* We mock nat_to_string to keep the example clean *)
Definition nat_to_string (n : nat) : string := "<nat>".

(* 
  TypeScript Backend
*)
Fixpoint emit_ts (expr : AST) {struct expr} : string :=
  let fix map_ts (ls : list AST) : list string :=
    match ls with
    | [] => []
    | x :: xs => emit_ts x :: map_ts xs
    end
  in
  match expr with
  | AstRef name => name
  | AstTuple elems => 
      "[" ++ concat_strings ", " (map_ts elems) ++ "]"
  | AstStringLit s => 
      """" ++ s ++ """"
  | AstIntLit n => 
      nat_to_string n
  | AstBlock stmts ret =>
      "{ " ++ concat_strings "; " (map_ts stmts) ++ "; return " ++ emit_ts ret ++ "; }"
  | AstApp func args =>
      emit_ts func ++ "(" ++ concat_strings ", " (map_ts args) ++ ")"
  | AstLam argName argTy body =>
      "(" ++ argName ++ " : " ++ emit_ts argTy ++ ") => " ++ emit_ts body
  | AstPi argName argTy retTy effs =>
      "(" ++ argName ++ " : " ++ emit_ts argTy ++ ") => " ++ emit_ts retTy
  | AstDo op args =>
      "await " ++ emit_ts op ++ "(" ++ concat_strings ", " (map_ts args) ++ ")"
  | AstHandle action eff handlers =>
      "/* handle effect */ (" ++ emit_ts action ++ ")"
  | AstMeta id =>
      "/* ?meta_" ++ nat_to_string id ++ " */ any"
  | AstError e =>
      "/* ERROR: " ++ e ++ " */"
  end.

(* 
  Golang Backend
*)
Fixpoint emit_go (expr : AST) {struct expr} : string :=
  let fix map_go (ls : list AST) : list string :=
    match ls with
    | [] => []
    | x :: xs => emit_go x :: map_go xs
    end
  in
  match expr with
  | AstRef name => name
  | AstTuple elems => 
      (* Go has no generic tuples, mock as interface array *)
      "[]interface{}{" ++ concat_strings ", " (map_go elems) ++ "}"
  | AstStringLit s => 
      """" ++ s ++ """"
  | AstIntLit n => 
      nat_to_string n
  | AstBlock stmts ret =>
      "func() interface{} { " ++ concat_strings "; " (map_go stmts) ++ "; return " ++ emit_go ret ++ " }()"
  | AstApp func args =>
      emit_go func ++ "(" ++ concat_strings ", " (map_go args) ++ ")"
  | AstLam argName argTy body =>
      "func(" ++ argName ++ " " ++ emit_go argTy ++ ") interface{} { return " ++ emit_go body ++ " }"
  | AstPi argName argTy retTy effs =>
      "func(" ++ argName ++ " " ++ emit_go argTy ++ ") " ++ emit_go retTy
  | AstDo op args =>
      emit_go op ++ "(" ++ concat_strings ", " (map_go args) ++ ")"
  | AstHandle action eff handlers =>
      "/* handle effect */ " ++ emit_go action
  | AstMeta id =>
      "/* ?meta_" ++ nat_to_string id ++ " */ interface{}"
  | AstError e =>
      "/* ERROR: " ++ e ++ " */"
  end.
