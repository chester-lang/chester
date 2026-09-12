From Stdlib Require Import Strings.String.
From Stdlib Require Import List.
From Stdlib Require Import Ascii.
Import ListNotations.
Open Scope string_scope.

(*
  Golang AST Representation for the Backend

  Bindings carry Go type strings. Use "interface{}" when the Chester type is
  unknown or intentionally dynamic (effects, lists, enums).
*)

Definition go_quote : string := String (ascii_of_nat 96) "".
Definition go_colon_space : string :=
  String (ascii_of_nat 58) (String (ascii_of_nat 32) "").

Definition go_iface : string := "interface{}".

Inductive GoStmt : Type :=
  | GoExprStmt : GoExpr -> GoStmt
  | GoLet : string -> string -> GoExpr -> GoStmt
  | GoDiscardBinding : string -> GoStmt
  | GoAssign : string -> GoExpr -> GoStmt
  | GoIfStmt : GoExpr -> list GoStmt -> list GoStmt -> GoStmt
  | GoReturn : GoExpr -> GoStmt
  | GoPanic : string -> GoStmt
  | GoFuncDecl : string -> list (string * string) -> string -> list GoStmt -> GoStmt
  | GoLocalFuncDecl : string -> list (string * string) -> string -> list GoStmt -> GoStmt
  | GoStruct : string -> list (string * string) -> GoStmt
  | GoEmpty : GoStmt
  | GoBlock : list GoStmt -> GoStmt
  | GoImport : string -> GoStmt

with GoExpr : Type :=
  | GoIntLiteral : string -> GoExpr
  | GoStringLiteral : string -> GoExpr
  | GoBoolLiteral : bool -> GoExpr
  | GoIdentifier : string -> GoExpr
  | GoSelector : GoExpr -> string -> GoExpr
  | GoIndex : GoExpr -> GoExpr -> GoExpr
  | GoCall : GoExpr -> list GoExpr -> GoExpr
  | GoFuncLiteral : list (string * string) -> string -> list GoStmt -> GoExpr
  | GoArray : list GoExpr -> GoExpr
  | GoMapLiteral : list (string * GoExpr) -> GoExpr
  | GoTypeAssert : GoExpr -> string -> GoExpr.

Fixpoint concat_strings (sep : string) (ls : list string) : string :=
  match ls with
  | [] => ""
  | [x] => x
  | x :: xs => x ++ sep ++ concat_strings sep xs
  end.

Definition go_param_decl (p : string * string) : string :=
  fst p ++ " " ++ snd p.

Definition stringify_go_params (params : list (string * string)) : string :=
  concat_strings ", " (map go_param_decl params).

Definition stringify_go_field (f : string * string) : string :=
  fst f ++ " " ++ snd f.

Fixpoint stringify_go_stmt (stmt : GoStmt) {struct stmt} : string :=
  let fix map_go_stmt (ls : list GoStmt) : list string :=
    match ls with
    | [] => []
    | x :: xs => stringify_go_stmt x :: map_go_stmt xs
    end
  in
  match stmt with
  | GoExprStmt expr => stringify_go_expr expr ++ "; "
  | GoLet name ty val =>
      "var " ++ name ++ " " ++ ty ++ " = " ++ stringify_go_expr val ++ "; "
  | GoDiscardBinding name => "_ = " ++ name ++ "; "
  | GoAssign name val => name ++ " = " ++ stringify_go_expr val ++ "; "
  | GoIfStmt cond thenB elseB =>
      let thenStr := concat_strings " " (map_go_stmt thenB) in
      let elseStr := concat_strings " " (map_go_stmt elseB) in
      "if " ++ stringify_go_expr cond ++ " { " ++ thenStr ++ "} else { " ++ elseStr ++ "}"
  | GoReturn expr => "return " ++ stringify_go_expr expr ++ "; "
  | GoPanic msg => "panic(" ++ go_quote ++ msg ++ go_quote ++ "); "

  | GoLocalFuncDecl name params ret body =>
      let bodyStr := concat_strings " " (map_go_stmt body) in
      "var " ++ name ++ " " ++ go_iface ++ "; " ++ name ++ " = func(" ++ stringify_go_params params ++ ") " ++ ret ++ " {
" ++ bodyStr ++ "
}; "
  | GoFuncDecl name params ret body =>
      "func " ++ name ++ "(" ++ stringify_go_params params ++ ") " ++ ret ++ " { " ++ concat_strings " " (map_go_stmt body) ++ "}"
  | GoStruct name fields =>
      match fields with
      | [] => "type " ++ name ++ " struct{}; "
      | _ => "type " ++ name ++ " struct{ " ++ concat_strings "; " (map stringify_go_field fields) ++ " }; "
      end
  | GoEmpty => ""
  | GoBlock stmts => concat_strings (String (ascii_of_nat 10) "") (map_go_stmt stmts)
  | GoImport mod => "import " ++ go_quote ++ mod ++ go_quote ++ "; "
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
  | GoFuncLiteral params ret body =>
      let fix map_go_stmt (ls : list GoStmt) : list string :=
        match ls with
        | [] => []
        | x :: xs => stringify_go_stmt x :: map_go_stmt xs
        end
      in
      "func(" ++ stringify_go_params params ++ ") " ++ ret ++ " { " ++ concat_strings " " (map_go_stmt body) ++ "}"
  | GoArray elements => "[]" ++ go_iface ++ "{" ++ concat_strings ", " (map_go_expr elements) ++ "}"
  | GoMapLiteral pairs =>
      let fix map_pairs (ps : list (string * GoExpr)) : list string :=
        match ps with
        | [] => []
        | (k, v) :: rest =>
            (go_quote ++ k ++ go_quote ++ go_colon_space ++ stringify_go_expr v) :: map_pairs rest
        end
      in
      "map[string]" ++ go_iface ++ "{" ++ concat_strings ", " (map_pairs pairs) ++ "}"
  | GoTypeAssert expr ty => stringify_go_expr expr ++ ".(" ++ ty ++ ")"
  end.
