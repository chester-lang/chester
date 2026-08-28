From Stdlib Require Import Strings.String.
From Stdlib Require Import List.
From Stdlib Require Import Ascii.
Open Scope string_scope.
Import ListNotations.

Definition ts_quote : string := String (ascii_of_nat 34) "".

(* 
  TypeScript AST Representation for the Backend 
*)

Inductive TypeScriptStmt : Type :=
  | TsExprStmt : TypeScriptExpr -> TypeScriptStmt
  | TsLet : string -> TypeScriptExpr -> TypeScriptStmt
  | TsVar : string -> TypeScriptExpr -> TypeScriptStmt (* mutable let *)
  | TsAssign : string -> TypeScriptExpr -> TypeScriptStmt
  | TsConst : string -> TypeScriptExpr -> TypeScriptStmt
  | TsIfStmt : TypeScriptExpr -> list TypeScriptStmt -> list TypeScriptStmt -> TypeScriptStmt
  | TsReturn : TypeScriptExpr -> TypeScriptStmt
  | TsThrow : string -> TypeScriptStmt
  | TsFunctionDecl : string -> list string -> list TypeScriptStmt -> TypeScriptStmt
  | TsExportFunction : string -> list string -> list TypeScriptStmt -> TypeScriptStmt
  | TsImportNamespace : string -> string -> TypeScriptStmt
  | TsImportNamed : string -> list string -> TypeScriptStmt
  | TsInterface : string -> TypeScriptStmt
  | TsEmpty : TypeScriptStmt
  | TsBlock : list TypeScriptStmt -> TypeScriptStmt (* flat concatenation, no IIFE *)

with TypeScriptExpr : Type :=
  | TsNumberLiteral : string -> TypeScriptExpr
  | TsStringLiteral : string -> TypeScriptExpr
  | TsBooleanLiteral : bool -> TypeScriptExpr
  | TsIdentifier : string -> TypeScriptExpr
  | TsPropertyAccess : TypeScriptExpr -> string -> TypeScriptExpr
  | TsIndexAccess : TypeScriptExpr -> TypeScriptExpr -> TypeScriptExpr
  | TsCall : TypeScriptExpr -> list TypeScriptExpr -> TypeScriptExpr
  | TsArrow : list string -> list TypeScriptStmt -> TypeScriptExpr
  | TsArray : list TypeScriptExpr -> TypeScriptExpr
  | TsAwait : TypeScriptExpr -> TypeScriptExpr
  | TsIIFE : list TypeScriptStmt -> TypeScriptExpr
  | TsObjectLiteral : list (string * TypeScriptExpr) -> TypeScriptExpr.

(* Helper function to stringify TS AST (pretty printing) *)
Fixpoint concat_strings (sep : string) (ls : list string) : string :=
  match ls with
  | [] => ""
  | [x] => x
  | x :: xs => x ++ sep ++ concat_strings sep xs
  end.

Fixpoint stringify_ts_stmt (stmt : TypeScriptStmt) {struct stmt} : string :=
  let fix map_ts_stmt (ls : list TypeScriptStmt) : list string :=
    match ls with
    | [] => []
    | x :: xs => stringify_ts_stmt x :: map_ts_stmt xs
    end
  in
  match stmt with
  | TsExprStmt expr => stringify_ts_expr expr ++ "; "
  | TsLet name val => "const " ++ name ++ " = " ++ stringify_ts_expr val ++ "; "
  | TsVar name val => "let " ++ name ++ " = " ++ stringify_ts_expr val ++ "; "
  | TsAssign name val => name ++ " = " ++ stringify_ts_expr val ++ "; "
  | TsConst name val => "const " ++ name ++ " = " ++ stringify_ts_expr val ++ "; "
  | TsIfStmt cond thenB elseB => 
      let thenStr := concat_strings "" (map_ts_stmt thenB) in
      let elseStr := concat_strings "" (map_ts_stmt elseB) in
      "if (" ++ stringify_ts_expr cond ++ ") { " ++ thenStr ++ "} else { " ++ elseStr ++ "}"
  | TsReturn expr => "return " ++ stringify_ts_expr expr ++ "; "
  | TsThrow msg => "throw new Error('" ++ msg ++ "'); "
  | TsFunctionDecl name params body => 
      "function " ++ name ++ "(" ++ concat_strings ", " params ++ ") { " ++ concat_strings "" (map_ts_stmt body) ++ "}"
  | TsExportFunction name params body =>
      "export function " ++ name ++ "(" ++ concat_strings ", " params ++ ") { " ++ concat_strings "" (map_ts_stmt body) ++ "}"
  | TsImportNamespace alias mod =>
      "import * as " ++ alias ++ " from " ++ ts_quote ++ mod ++ ts_quote ++ "; "
  | TsImportNamed mod names =>
      "import { " ++ concat_strings ", " names ++ " } from " ++ ts_quote ++ mod ++ ts_quote ++ "; "
  | TsInterface name => "interface " ++ name ++ " { [key: string]: any }; "
  | TsEmpty => ""
  | TsBlock stmts => concat_strings "" (map_ts_stmt stmts)
  end

with stringify_ts_expr (expr : TypeScriptExpr) {struct expr} : string :=
  let fix map_ts_expr (ls : list TypeScriptExpr) : list string :=
    match ls with
    | [] => []
    | x :: xs => stringify_ts_expr x :: map_ts_expr xs
    end
  in
  match expr with
  | TsNumberLiteral n => n
  | TsStringLiteral s => """" ++ s ++ """"
  | TsBooleanLiteral b => if b then "true" else "false"
  | TsIdentifier name => name
  | TsPropertyAccess obj prop => stringify_ts_expr obj ++ "." ++ prop
  | TsIndexAccess obj idx => stringify_ts_expr obj ++ "[" ++ stringify_ts_expr idx ++ "]"
  | TsCall callee args =>
      let callee_str := stringify_ts_expr callee in
      if string_dec callee_str "\" then
        match args with
        | arg :: arrow :: body =>
            "(" ++ stringify_ts_expr arg ++ ") => " ++ concat_strings " " (map_ts_expr body)
        | _ => callee_str ++ "(" ++ concat_strings ", " (map_ts_expr args) ++ ")"
        end
      else if string_dec callee_str "if" then
        match args with
        | cond :: thenSym :: thenB :: elseSym :: elseB :: _ =>
            "if (" ++ stringify_ts_expr cond ++ ") { " ++ stringify_ts_expr thenB ++ " } else { " ++ stringify_ts_expr elseB ++ " }"
        | cond :: thenSym :: thenB :: _ =>
            "if (" ++ stringify_ts_expr cond ++ ") { " ++ stringify_ts_expr thenB ++ " }"
        | _ => callee_str ++ "(" ++ concat_strings ", " (map_ts_expr args) ++ ")"
        end
      else if string_dec callee_str "def" then
        match args with
        | name :: typeParams :: params :: retTy :: body :: _ =>
            "function " ++ stringify_ts_expr name ++ "(" ++ stringify_ts_expr params ++ ") { return " ++ stringify_ts_expr body ++ "; }"
        | _ => callee_str ++ "(" ++ concat_strings ", " (map_ts_expr args) ++ ")"
        end
      else
        callee_str ++ "(" ++ concat_strings ", " (map_ts_expr args) ++ ")"
  | TsArrow params body => 
      let fix map_ts_stmt (ls : list TypeScriptStmt) : list string :=
        match ls with
        | [] => []
        | x :: xs => stringify_ts_stmt x :: map_ts_stmt xs
        end
      in
      "(" ++ concat_strings ", " params ++ ") => { " ++ concat_strings "" (map_ts_stmt body) ++ "}"
  | TsArray elements => "[" ++ concat_strings ", " (map_ts_expr elements) ++ "]"
  | TsAwait e => "(await " ++ stringify_ts_expr e ++ ")"
  | TsIIFE body => 
      let fix map_ts_stmt (ls : list TypeScriptStmt) : list string :=
        match ls with
        | [] => []
        | x :: xs => stringify_ts_stmt x :: map_ts_stmt xs
        end
      in
      "(() => { " ++ concat_strings "" (map_ts_stmt body) ++ "})()"
  | TsObjectLiteral fields =>
      let fix map_fields (fs : list (string * TypeScriptExpr)) : list string :=
        match fs with
        | [] => []
        | (k, v) :: rest => (k ++ ": " ++ stringify_ts_expr v) :: map_fields rest
        end
      in
      "{" ++ concat_strings ", " (map_fields fields) ++ "}"
  end.
