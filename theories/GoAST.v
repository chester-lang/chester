From Stdlib Require Import Strings.String.
From Stdlib Require Import List.
Import ListNotations.

(* 
  Golang AST Representation for the Backend 
*)

Inductive GoAST : Type :=
  | GoIntLiteral : string -> GoAST
  | GoStringLiteral : string -> GoAST
  | GoBoolLiteral : bool -> GoAST
  | GoIdentifier : string -> GoAST
  | GoSelector : GoAST -> string -> GoAST
  | GoIndex : GoAST -> GoAST -> GoAST
  | GoCall : GoAST -> list GoAST -> GoAST
  | GoFuncLiteral : list string -> GoAST -> GoAST
  | GoBlock : list GoAST -> GoAST -> GoAST
  | GoArray : list GoAST -> GoAST
  | GoLet : string -> GoAST -> GoAST
  | GoIf : GoAST -> GoAST -> GoAST -> GoAST
  | GoFuncDecl : string -> list string -> GoAST -> GoAST
  | GoStruct : string -> GoAST
  | GoTypeAssert : GoAST -> string -> GoAST
  | GoPanic : string -> GoAST
  | GoEmpty : GoAST.

(* Helper function to stringify Go AST (pretty printing) *)
Fixpoint concat_strings (sep : string) (ls : list string) : string :=
  match ls with
  | [] => ""
  | [x] => x
  | x :: xs => x ++ sep ++ concat_strings sep xs
  end.

Fixpoint stringify_go (expr : GoAST) {struct expr} : string :=
  let fix map_go (ls : list GoAST) : list string :=
    match ls with
    | [] => []
    | x :: xs => stringify_go x :: map_go xs
    end
  in
  match expr with
  | GoIntLiteral n => n
  | GoStringLiteral s => """" ++ s ++ """"
  | GoBoolLiteral b => if b then "true" else "false"
  | GoIdentifier name => name
  | GoSelector obj prop => stringify_go obj ++ "." ++ prop
  | GoIndex obj idx => stringify_go obj ++ "[" ++ stringify_go idx ++ "]"
  | GoCall callee args => stringify_go callee ++ "(" ++ concat_strings ", " (map_go args) ++ ")"
  | GoFuncLiteral params body => "func(" ++ concat_strings " interface{}, " params ++ " interface{}) interface{} { return " ++ stringify_go body ++ " }"
  | GoBlock stmts ret => "func() interface{} { " ++ concat_strings " " (map_go stmts) ++ "return " ++ stringify_go ret ++ " }()"
  | GoArray elements => "[]interface{}{" ++ concat_strings ", " (map_go elements) ++ "}"
  | GoLet name val => name ++ " := " ++ stringify_go val ++ "; "
  | GoIf cond thenB elseB => "if " ++ stringify_go cond ++ " { " ++ stringify_go thenB ++ " } else { " ++ stringify_go elseB ++ " }"
  | GoFuncDecl name params body => "func " ++ name ++ "(" ++ concat_strings " interface{}, " params ++ " interface{}) interface{} " ++ stringify_go body
  | GoStruct name => "type " ++ name ++ " struct{}; "
  | GoTypeAssert expr ty => stringify_go expr ++ ".(" ++ ty ++ ")"
  | GoPanic msg => "panic(""" ++ msg ++ """)"
  | GoEmpty => ""
  end.
