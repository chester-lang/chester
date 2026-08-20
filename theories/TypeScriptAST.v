From Stdlib Require Import Strings.String.
From Stdlib Require Import List.
Open Scope string_scope.
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
  | TsIndexAccess : TypeScriptAST -> TypeScriptAST -> TypeScriptAST
  | TsCall : TypeScriptAST -> list TypeScriptAST -> TypeScriptAST
  | TsArrow : list string -> TypeScriptAST -> TypeScriptAST
  | TsBlock : list TypeScriptAST -> TypeScriptAST -> TypeScriptAST
  | TsArray : list TypeScriptAST -> TypeScriptAST
  | TsAwait : TypeScriptAST -> TypeScriptAST
  | TsLet : string -> TypeScriptAST -> TypeScriptAST
  | TsIf : TypeScriptAST -> TypeScriptAST -> TypeScriptAST -> TypeScriptAST
  | TsFunctionDecl : string -> list string -> TypeScriptAST -> TypeScriptAST
  | TsInterface : string -> TypeScriptAST
  | TsIIFE : TypeScriptAST -> TypeScriptAST
  | TsThrow : string -> TypeScriptAST
  | TsEmpty : TypeScriptAST.

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
  | TsIndexAccess obj idx => stringify_ts obj ++ "[" ++ stringify_ts idx ++ "]"
  | TsCall callee args =>
      let callee_str := stringify_ts callee in
      if string_dec callee_str "\\" then
        match args with
        | arg :: arrow :: body =>
            "(" ++ stringify_ts arg ++ ") => " ++ concat_strings " " (map_ts body)
        | _ => callee_str ++ "(" ++ concat_strings ", " (map_ts args) ++ ")"
        end
      else if string_dec callee_str "if" then
        match args with
        | cond :: thenSym :: thenB :: elseSym :: elseB :: _ =>
            "if (" ++ stringify_ts cond ++ ") { " ++ stringify_ts thenB ++ " } else { " ++ stringify_ts elseB ++ " }"
        | cond :: thenSym :: thenB :: _ =>
            "if (" ++ stringify_ts cond ++ ") { " ++ stringify_ts thenB ++ " }"
        | _ => callee_str ++ "(" ++ concat_strings ", " (map_ts args) ++ ")"
        end
      else if string_dec callee_str "def" then
        match args with
        | name :: typeParams :: params :: retTy :: body :: _ =>
            "function " ++ stringify_ts name ++ "(" ++ stringify_ts params ++ ") { return " ++ stringify_ts body ++ "; }"
        | _ => callee_str ++ "(" ++ concat_strings ", " (map_ts args) ++ ")"
        end
      else
        callee_str ++ "(" ++ concat_strings ", " (map_ts args) ++ ")"
  | TsArrow params body => "(" ++ concat_strings ", " params ++ ") => " ++ stringify_ts body
  | TsBlock stmts ret => "{ " ++ concat_strings "" (map (fun s => s ++ "; ") (map_ts stmts)) ++ "return " ++ stringify_ts ret ++ "; }"
  | TsArray elements => "[" ++ concat_strings ", " (map_ts elements) ++ "]"
  | TsAwait e => "await " ++ stringify_ts e
  | TsLet name val => "const " ++ name ++ " = " ++ stringify_ts val ++ "; "
  | TsIf cond thenB elseB => "if (" ++ stringify_ts cond ++ ") { " ++ stringify_ts thenB ++ " } else { " ++ stringify_ts elseB ++ " }"
  | TsFunctionDecl name params body => "function " ++ name ++ "(" ++ concat_strings ", " params ++ ") " ++ stringify_ts body
  | TsInterface name => "interface " ++ name ++ " { [key: string]: any }; "
  | TsIIFE body => "(() => " ++ stringify_ts body ++ ")()"
  | TsThrow msg => "(() => { throw new Error('" ++ msg ++ "'); })()"
  | TsEmpty => ""
  end.
