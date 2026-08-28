From Stdlib Require Import Strings.String.
From Stdlib Require Import List.
From Stdlib Require Import Ascii.
Import ListNotations.
Open Scope string_scope.

(* Rocq AST representation for the Chester backend. *)

Inductive RocqStmt : Type :=
  | RocqDefinition : string -> list string -> RocqExpr -> RocqStmt
  | RocqInductive : string -> RocqStmt
  | RocqEmpty : RocqStmt
  | RocqBlock : list RocqStmt -> RocqStmt

with RocqExpr : Type :=
  | RocqNat : string -> RocqExpr
  | RocqString : string -> RocqExpr
  | RocqBool : bool -> RocqExpr
  | RocqIdentifier : string -> RocqExpr
  | RocqProj : RocqExpr -> string -> RocqExpr
  | RocqIndex : RocqExpr -> RocqExpr -> RocqExpr
  | RocqApp : RocqExpr -> list RocqExpr -> RocqExpr
  | RocqLam : list string -> RocqExpr -> RocqExpr
  | RocqLetIn : string -> RocqExpr -> RocqExpr -> RocqExpr
  | RocqIf : RocqExpr -> RocqExpr -> RocqExpr -> RocqExpr
  | RocqMatch : RocqExpr -> list (string * list string * RocqExpr) -> RocqExpr
  | RocqList : list RocqExpr -> RocqExpr
  | RocqTuple : list RocqExpr -> RocqExpr
  | RocqPairList : list (string * RocqExpr) -> RocqExpr
  | RocqUnit : RocqExpr
  | RocqComment : string -> RocqExpr.

Fixpoint concat_strings (sep : string) (ls : list string) : string :=
  match ls with
  | [] => ""
  | [x] => x
  | x :: xs => x ++ sep ++ concat_strings sep xs
  end.

Definition rocq_quote : string := String (ascii_of_nat 34) "".

Fixpoint stringify_rocq_expr (expr : RocqExpr) {struct expr} : string :=
  let fix map_exprs (ls : list RocqExpr) : list string :=
    match ls with
    | [] => []
    | x :: xs => stringify_rocq_expr x :: map_exprs xs
    end
  in
  match expr with
  | RocqNat n => n ++ "%nat"
  | RocqString s => rocq_quote ++ s ++ rocq_quote ++ "%string"
  | RocqBool true => "true"
  | RocqBool false => "false"
  | RocqIdentifier name => name
  | RocqProj obj field => stringify_rocq_expr obj ++ "." ++ field
  | RocqIndex obj idx =>
      stringify_rocq_expr obj ++ " (" ++ stringify_rocq_expr idx ++ ")"
  | RocqApp callee args =>
      match args with
      | [] => stringify_rocq_expr callee
      | _ => "(" ++ stringify_rocq_expr callee ++ " " ++ concat_strings " " (map_exprs args) ++ ")"
      end
  | RocqLam params body =>
      match params with
      | [] => "(fun _ : unit => " ++ stringify_rocq_expr body ++ ")"
      | [p] => "(fun " ++ p ++ " : chester_dyn => " ++ stringify_rocq_expr body ++ ")"
      | _ =>
          "(fun " ++ concat_strings " " params ++ " : chester_dyn => " ++ stringify_rocq_expr body ++ ")"
      end
  | RocqLetIn name val body =>
      "(let " ++ name ++ " := " ++ stringify_rocq_expr val ++ " in " ++ stringify_rocq_expr body ++ ")"
  | RocqIf cond thenB elseB =>
      "(if " ++ stringify_rocq_expr cond ++ " then " ++ stringify_rocq_expr thenB
      ++ " else " ++ stringify_rocq_expr elseB ++ ")"
  | RocqMatch scrut cases =>
      let fix stringify_cases (cs : list (string * list string * RocqExpr)) : string :=
        match cs with
        | [] => ""
        | (cname, vars, body) :: rest =>
            let pat :=
              match vars with
              | [] => cname
              | _ => cname ++ " " ++ concat_strings " " vars
              end
            in
            pat ++ " => " ++ stringify_rocq_expr body
            ++ match rest with [] => "" | _ => " | " ++ stringify_cases rest end
        end
      in
      "(match " ++ stringify_rocq_expr scrut ++ " with " ++ stringify_cases cases ++ " end)"
  | RocqList elems => "[" ++ concat_strings "; " (map_exprs elems) ++ "]"
  | RocqTuple elems => "(" ++ concat_strings ", " (map_exprs elems) ++ ")"
  | RocqPairList pairs =>
      let fix map_pairs (ps : list (string * RocqExpr)) : list string :=
        match ps with
        | [] => []
        | (k, v) :: rest =>
            ("(" ++ rocq_quote ++ k ++ rocq_quote ++ "%string, " ++ stringify_rocq_expr v ++ ")")
            :: map_pairs rest
        end
      in
      "[" ++ concat_strings "; " (map_pairs pairs) ++ "]"
  | RocqUnit => "chester_unit"
  | RocqComment msg => "(* " ++ msg ++ " *)"
  end.

Fixpoint stringify_rocq_stmt (stmt : RocqStmt) {struct stmt} : string :=
  let fix map_stmts (ls : list RocqStmt) : list string :=
    match ls with
    | [] => []
    | x :: xs => stringify_rocq_stmt x :: map_stmts xs
    end
  in
  match stmt with
  | RocqDefinition name params body =>
      match params with
      | [] => "Definition " ++ name ++ " : chester_dyn := " ++ stringify_rocq_expr body ++ "."
      | _ =>
          "Definition " ++ name ++ " " ++ concat_strings " " params
          ++ " : chester_dyn := " ++ stringify_rocq_expr body ++ "."
      end
  | RocqInductive name => "Inductive " ++ name ++ " : Type := ."
  | RocqEmpty => ""
  | RocqBlock stmts => concat_strings "\n" (map_stmts stmts)
  end.
