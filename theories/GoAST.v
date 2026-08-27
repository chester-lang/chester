From Stdlib Require Import Strings.String.
From Stdlib Require Import List.
From Stdlib Require Import Ascii.
Import ListNotations.
Open Scope string_scope.

(*
  Golang AST Representation for the Backend
*)

Definition go_quote : string := String (ascii_of_nat 34) "".
Definition go_colon_space : string :=
  String (ascii_of_nat 58) (String (ascii_of_nat 32) "").

Inductive GoStmt : Type :=
  | GoExprStmt : GoExpr -> GoStmt
  | GoLet : string -> GoExpr -> GoStmt
  | GoAssign : string -> GoExpr -> GoStmt
  | GoIfStmt : GoExpr -> list GoStmt -> list GoStmt -> GoStmt
  | GoReturn : GoExpr -> GoStmt
  | GoPanic : string -> GoStmt
  | GoFuncDecl : string -> list string -> list GoStmt -> GoStmt
  | GoStruct : string -> GoStmt
  | GoEmpty : GoStmt
  | GoBlock : list GoStmt -> GoStmt

with GoExpr : Type :=
  | GoIntLiteral : string -> GoExpr
  | GoStringLiteral : string -> GoExpr
  | GoBoolLiteral : bool -> GoExpr
  | GoIdentifier : string -> GoExpr
  | GoSelector : GoExpr -> string -> GoExpr
  | GoIndex : GoExpr -> GoExpr -> GoExpr
  | GoCall : GoExpr -> list GoExpr -> GoExpr
  | GoFuncLiteral : list string -> list GoStmt -> GoExpr
  | GoArray : list GoExpr -> GoExpr
  | GoMapLiteral : list (string * GoExpr) -> GoExpr
  | GoTypeAssert : GoExpr -> string -> GoExpr.

(* Helper function to stringify Go AST (pretty printing) *)
Fixpoint concat_strings (sep : string) (ls : list string) : string :=
  match ls with
  | [] => ""
  | [x] => x
  | x :: xs => x ++ sep ++ concat_strings sep xs
  end.

Fixpoint stringify_go_stmt (stmt : GoStmt) {struct stmt} : string :=
  let fix map_go_stmt (ls : list GoStmt) : list string :=
    match ls with
    | [] => []
    | x :: xs => stringify_go_stmt x :: map_go_stmt xs
    end
  in
  match stmt with
  | GoExprStmt expr => stringify_go_expr expr ++ "; "
  | GoLet name val => name ++ " := " ++ stringify_go_expr val ++ "; "
  | GoAssign name val => name ++ " = " ++ stringify_go_expr val ++ "; "
  | GoIfStmt cond thenB elseB =>
      let thenStr := concat_strings " " (map_go_stmt thenB) in
      let elseStr := concat_strings " " (map_go_stmt elseB) in
      "if " ++ stringify_go_expr cond ++ " { " ++ thenStr ++ "} else { " ++ elseStr ++ "}"
  | GoReturn expr => "return " ++ stringify_go_expr expr ++ "; "
  | GoPanic msg => "panic(" ++ go_quote ++ msg ++ go_quote ++ "); "
  | GoFuncDecl name params body =>
      match params with
      | [] =>
          "func " ++ name ++ "() interface{} { " ++ concat_strings " " (map_go_stmt body) ++ "}"
      | _ =>
          "func " ++ name ++ "(" ++ concat_strings " interface{}, " params ++ " interface{}) interface{} { " ++ concat_strings " " (map_go_stmt body) ++ "}"
      end
  | GoStruct name => "type " ++ name ++ " struct{}; "
  | GoEmpty => ""
  | GoBlock stmts => concat_strings " " (map_go_stmt stmts)
  end

with stringify_go_expr (expr : GoExpr) {struct expr} : string :=
  let fix map_go_expr (ls : list GoExpr) : list string :=
    match ls with
    | [] => []
    | x :: xs => stringify_go_expr x :: map_go_expr xs
    end
  in
  match expr with
  | GoIntLiteral n => n
  | GoStringLiteral s => go_quote ++ s ++ go_quote
  | GoBoolLiteral b => if b then "true" else "false"
  | GoIdentifier name => name
  | GoSelector obj prop => stringify_go_expr obj ++ "." ++ prop
  | GoIndex obj idx => stringify_go_expr obj ++ "[" ++ stringify_go_expr idx ++ "]"
  | GoCall callee args => stringify_go_expr callee ++ "(" ++ concat_strings ", " (map_go_expr args) ++ ")"
  | GoFuncLiteral params body =>
      let fix map_go_stmt (ls : list GoStmt) : list string :=
        match ls with
        | [] => []
        | x :: xs => stringify_go_stmt x :: map_go_stmt xs
        end
      in
      match params with
      | [] => "func() interface{} { " ++ concat_strings " " (map_go_stmt body) ++ "}"
      | _ => "func(" ++ concat_strings " interface{}, " params ++ " interface{}) interface{} { " ++ concat_strings " " (map_go_stmt body) ++ "}"
      end
  | GoArray elements => "[]interface{}{" ++ concat_strings ", " (map_go_expr elements) ++ "}"
  | GoMapLiteral pairs =>
      let fix map_pairs (ps : list (string * GoExpr)) : list string :=
        match ps with
        | [] => []
        | (k, v) :: rest =>
            (go_quote ++ k ++ go_quote ++ go_colon_space ++ stringify_go_expr v) :: map_pairs rest
        end
      in
      "map[string]interface{}{" ++ concat_strings ", " (map_pairs pairs) ++ "}"
  | GoTypeAssert expr ty => stringify_go_expr expr ++ ".(" ++ ty ++ ")"
  end.
