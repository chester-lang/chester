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
  | AstBoolLit b => TsBooleanLiteral b
  | AstLet name value body => 
      TsBlock [TsRaw ("const " ++ name ++ " = " ++ stringify_ts (emit_ts value))] (emit_ts body)
  | AstIf cond thenB elseB => 
      TsRaw ("(" ++ stringify_ts (emit_ts cond) ++ " ? " ++ stringify_ts (emit_ts thenB) ++ " : " ++ stringify_ts (emit_ts elseB) ++ ")")
  | AstDef name type_params params ret_ty body => 
      let fix get_param_names (ps : list (string * AST)) : list string :=
        match ps with
        | [] => []
        | (n, _) :: rest => n :: get_param_names rest
        end
      in
      TsRaw ("function " ++ name ++ "(" ++ concat_strings ", " (get_param_names params) ++ ") { return " ++ stringify_ts (emit_ts body) ++ "; }")
  | AstEnum name _ _ => TsRaw ("type " ++ name ++ " = any; /* simplified enum */")
  | AstMatch expr cases =>
      let fix emit_cases (cs : list (PatternAST * AST)) : string :=
        match cs with
        | [] => "throw new Error('Non-exhaustive match');"%string
        | (pat, body) :: rest =>
            match pat with
            | PatWildcard => "return " ++ stringify_ts (emit_ts body) ++ ";"
            | PatVar v => "const " ++ v ++ " = _match_val; return " ++ stringify_ts (emit_ts body) ++ ";"
            | PatConstructor cname vars =>
                "if (_match_val._tag === '" ++ cname ++ "') { " ++
                (let fix bind_vars (vs : list string) (idx : nat) : string :=
                   match vs with
                   | [] => ""%string
                   | v :: v_rest => "const " ++ v ++ " = _match_val.args[" ++ nat_to_string idx ++ "]; " ++ bind_vars v_rest (S idx)
                   end
                 in bind_vars vars 0) ++
                "return " ++ stringify_ts (emit_ts body) ++ "; } " ++ emit_cases rest
            end
        end
      in
      TsRaw ("(() => { const _match_val = " ++ stringify_ts (emit_ts expr) ++ "; " ++ emit_cases cases ++ " })()")
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
  | AstBoolLit b => GoBoolLiteral b
  | AstLet name value body => 
      GoBlock [GoRaw (name ++ " := " ++ stringify_go (emit_go value))] (emit_go body)
  | AstIf cond thenB elseB => 
      GoRaw ("func() interface{} { if " ++ stringify_go (emit_go cond) ++ " { return " ++ stringify_go (emit_go thenB) ++ " } else { return " ++ stringify_go (emit_go elseB) ++ " } }()")
  | AstDef name type_params params ret_ty body => 
      let fix get_param_names (ps : list (string * AST)) : list string :=
        match ps with
        | [] => []
        | (n, _) :: rest => n :: get_param_names rest
        end
      in
      GoRaw ("func " ++ name ++ "(" ++ concat_strings " interface{}, " (get_param_names params) ++ " interface{}) interface{} { return " ++ stringify_go (emit_go body) ++ " }")
  | AstEnum name _ _ => GoRaw ("type " ++ name ++ " interface{} /* simplified enum */")
  | AstMatch expr cases =>
      let fix emit_cases (cs : list (PatternAST * AST)) : string :=
        match cs with
        | [] => "panic(""Non-exhaustive match"")"
        | (pat, body) :: rest =>
            match pat with
            | PatWildcard => "return " ++ stringify_go (emit_go body)
            | PatVar v => v ++ " := _match_val; return " ++ stringify_go (emit_go body)
            | PatConstructor cname vars =>
                "if _tag, _ok := _match_val.(map[string]interface{}); _ok && _tag[""_tag""] == """ ++ cname ++ """ { " ++
                (let fix bind_vars (vs : list string) (idx : nat) : string :=
                   match vs with
                   | [] => ""
                   | v :: v_rest => v ++ " := _tag[""args""].([]interface{})[" ++ nat_to_string idx ++ "]; " ++ bind_vars v_rest (S idx)
                   end
                 in bind_vars vars 0) ++
                "return " ++ stringify_go (emit_go body) ++ " }; " ++ emit_cases rest
            end
        end
      in
      GoRaw ("func() interface{} { _match_val := " ++ stringify_go (emit_go expr) ++ "; " ++ emit_cases cases ++ " }()")
  | AstRecord name _ _ => GoRaw ("/* record " ++ name ++ " */")
  | AstMeta id => GoRaw ("/* ?meta_" ++ nat_to_string id ++ " */")
  | AstError e => GoRaw ("/* ERROR: " ++ e ++ " */")
  end.
