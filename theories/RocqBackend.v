From Stdlib Require Import Strings.String.
From Stdlib Require Import List.
From Stdlib Require Import Ascii.
Import ListNotations.
Open Scope string_scope.

Require Import Chester.AST.
Require Import Chester.RocqAST.

Definition digit_char (d : nat) : string :=
  match d with
  | 0 => "0" | 1 => "1" | 2 => "2" | 3 => "3" | 4 => "4"
  | 5 => "5" | 6 => "6" | 7 => "7" | 8 => "8" | _ => "9"
  end.

Definition nat_to_string (n : nat) : string :=
  let tens := Nat.div n 10 in
  let ones := Nat.modulo n 10 in
  if Nat.eqb tens 0 then digit_char ones
  else digit_char tens ++ digit_char ones.

Definition effect_label (eff : EffectRef) : string :=
  match eff with
  | UserEffect n => n
  | BuiltinEffect n => n
  | EffectRowVar n => n
  end.

Definition rocq_direct_call (name : string) : bool :=
  let fix starts_with (pre s : string) : bool :=
    match pre, s with
    | EmptyString, _ => true
    | String pc pre', String sc s' =>
        if Ascii.eqb pc sc then starts_with pre' s' else false
    | _, _ => false
    end
  in
  if string_dec name "int_add" then true
  else if string_dec name "int_eq" then true
  else if string_dec name "Unit" then true
  else if starts_with "prim__" name then true
  else if starts_with "__chester_" name then true
  else false.

Fixpoint rocq_call (callee : RocqExpr) (args : list RocqExpr) : RocqExpr :=
  match args with
  | [] => callee
  | a :: rest => rocq_call (RocqApp callee [a]) rest
  end.

Definition rocq_call_emitted (direct : bool) (callee : RocqExpr) (args : list RocqExpr) : RocqExpr :=
  rocq_call callee args.

Definition rocq_bool_cond (e : RocqExpr) : RocqExpr := e.

Fixpoint effect_label_rocq_lits (es : EffectSet) : list RocqExpr :=
  match es with
  | [] => []
  | EffectRowVar _ :: xs => effect_label_rocq_lits xs
  | e :: xs => RocqString (effect_label e) :: effect_label_rocq_lits xs
  end.

Fixpoint emit_rocq_lam_params (params : list string) (body : RocqExpr) : RocqExpr :=
  match params with
  | [] => body
  | p :: rest => RocqLam [p] (emit_rocq_lam_params rest body)
  end.

Fixpoint emit_rocq_expr (ast : AST) {struct ast} : RocqExpr :=
  let fix emit_match_cases (cs : list (PatternAST * AST)) : list (string * list string * RocqExpr) :=
    match cs with
    | [] => [("False", [], RocqComment "Non-exhaustive match")]
    | (pat, body) :: rest =>
        match pat with
        | PatConstructor cname vars =>
            (cname, vars, emit_rocq_expr body) :: emit_match_cases rest
        | PatWildcard => ("_", [], emit_rocq_expr body) :: emit_match_cases rest
        | PatVar v => (v, [], emit_rocq_expr body) :: emit_match_cases rest
        end
  end
  in
  let fix map_rocq_expr (ls : list AST) : list RocqExpr :=
    match ls with
    | [] => []
    | x :: xs => emit_rocq_expr x :: map_rocq_expr xs
    end
  in
  match ast with
  | AstRef name => RocqIdentifier name
  | AstTuple elems => RocqTuple (map_rocq_expr elems)
  | AstStringLit s => RocqString s
  | AstIntLit n => RocqNat (nat_to_string n)
  | AstBlock stmts ret =>
      let fix fold_stmts (ls : list AST) (acc : RocqExpr) {struct ls} : RocqExpr :=
        match ls with
        | [] => acc
        | AstLet name value :: xs =>
            fold_stmts xs (RocqLetIn name (emit_rocq_expr value) acc)
        | AstVar name value :: xs =>
            fold_stmts xs
              (RocqLetIn name
                 (rocq_call (RocqIdentifier "chester_var") [emit_rocq_expr value])
                 acc)
        | AstAssign name value :: xs =>
            fold_stmts xs
              (RocqLetIn "_"
                 (rocq_call (RocqIdentifier "chester_set")
                    [RocqString name; emit_rocq_expr value])
                 acc)
        | AstDef name _ params _ body :: xs =>
            fold_stmts xs
              (RocqLetIn name
                 (emit_rocq_lam_params (map fst params) (emit_rocq_expr body))
                 acc)
        | s :: xs =>
            fold_stmts xs
              (RocqLetIn "_"
                 (rocq_call (RocqIdentifier "chester_expr_stmt") [emit_rocq_expr s])
                 acc)
        end
      in fold_stmts stmts (emit_rocq_expr ret)
  | AstApp func args =>
      let direct := match func with AstRef n => rocq_direct_call n | _ => false end in
      rocq_call_emitted direct (emit_rocq_expr func) (map_rocq_expr args)
  | AstImplicitApp func _args => emit_rocq_expr func
  | AstFunTy _ _ _ _ => RocqIdentifier "chester_dyn"
  | AstLam argName _ body => RocqLam [argName] (emit_rocq_expr body)
  | AstPi _ _ _ _ => RocqIdentifier "chester_dyn"
  | AstDo op args =>
      let op_name := match op with AstRef n => n | _ => "unknown" end in
      rocq_call (RocqIdentifier "__chester_perform")
        [RocqString op_name; RocqList (map_rocq_expr args)]
  | AstHandle action eff handlers =>
      let fix emit_hs (hs : list (string * AST)) : list (string * RocqExpr) :=
        match hs with
        | [] => []
        | (op, fn) :: rest => (op, emit_rocq_expr fn) :: emit_hs rest
        end
      in
      rocq_call (RocqIdentifier "__chester_handle")
        [RocqString (effect_label eff);
         RocqLam [] (emit_rocq_expr action);
         RocqPairList (emit_hs handlers)]
  | AstBoolLit b => RocqBool b
  | AstLet name value => RocqLetIn name (emit_rocq_expr value) RocqUnit
  | AstVar _ value => rocq_call (RocqIdentifier "chester_var") [emit_rocq_expr value]
  | AstAssign name value =>
      rocq_call (RocqIdentifier "chester_set") [RocqString name; emit_rocq_expr value]
  | AstBox e caps =>
      rocq_call (RocqIdentifier "__chester_box")
        [RocqList (effect_label_rocq_lits caps);
         RocqLam [] (emit_rocq_expr e)]
  | AstUnbox e => rocq_call (emit_rocq_expr e) []
  | AstIf cond true_br false_br =>
      RocqIf (rocq_bool_cond (emit_rocq_expr cond))
        (emit_rocq_expr true_br)
        (emit_rocq_expr false_br)
  | AstDef _ _ params _ body =>
      emit_rocq_lam_params (map fst params) (emit_rocq_expr body)
  | AstEnum _ _ _ => RocqUnit
  | AstExtension _ _ _ _ => RocqUnit
  | AstMatch expr cases =>
      RocqMatch (emit_rocq_expr expr) (emit_match_cases cases)
  | AstRecord _ _ _ => RocqUnit
  | AstFieldAccess expr field => RocqProj (emit_rocq_expr expr) field
  | AstMeta id => RocqComment ("?meta_" ++ nat_to_string id)
  | AstUniverse _ => RocqComment "Universe in term"
  | AstError e => RocqComment e
  | AstSpan _ inner => emit_rocq_expr inner
  end.

Fixpoint emit_rocq_stmt (ast : AST) {struct ast} : RocqStmt :=
  match ast with
  | AstLet name value => RocqDefinition name [] (emit_rocq_expr value)
  | AstVar name value =>
      RocqDefinition name [] (rocq_call (RocqIdentifier "chester_var") [emit_rocq_expr value])
  | AstAssign name value =>
      RocqDefinition ("_assign_" ++ name) []
        (rocq_call (RocqIdentifier "chester_set") [RocqString name; emit_rocq_expr value])
  | AstDef name _ params _ body =>
      RocqDefinition name (map fst params) (emit_rocq_expr body)
  | AstRecord name _ _ => RocqInductive name
  | AstEnum _ _ _ => RocqEmpty
  | AstExtension _ _ _ meths =>
      let fix map_meths (ls : list AST) : list RocqStmt :=
        match ls with
        | [] => []
        | x :: xs => emit_rocq_stmt x :: map_meths xs
        end
      in RocqBlock (map_meths meths)
  | AstRef name => RocqDefinition ("_expr_" ++ name) [] (RocqIdentifier name)
  | AstTuple elems =>
      let fix map_exprs (ls : list AST) : list RocqExpr :=
        match ls with
        | [] => []
        | x :: xs => emit_rocq_expr x :: map_exprs xs
        end
      in RocqDefinition "_tuple" [] (RocqTuple (map_exprs elems))
  | AstStringLit s => RocqDefinition "_str" [] (RocqString s)
  | AstIntLit n => RocqDefinition "_nat" [] (RocqNat (nat_to_string n))
  | AstBlock stmts ret =>
      RocqDefinition "_block" [] (emit_rocq_expr (AstBlock stmts ret))
  | AstApp func args =>
      let direct := match func with AstRef n => rocq_direct_call n | _ => false end in
      let fix map_exprs (ls : list AST) : list RocqExpr :=
        match ls with
        | [] => []
        | x :: xs => emit_rocq_expr x :: map_exprs xs
        end
      in
      RocqDefinition "_app" []
        (rocq_call_emitted direct (emit_rocq_expr func) (map_exprs args))
  | AstImplicitApp func _args => RocqDefinition "_app" [] (emit_rocq_expr func)
  | AstLam argName _ body =>
      RocqDefinition "_lam" [] (RocqLam [argName] (emit_rocq_expr body))
  | AstFunTy _ _ _ _ => RocqEmpty
  | AstPi _ _ _ _ => RocqEmpty
  | AstDo op args =>
      let op_name := match op with AstRef n => n | _ => "unknown" end in
      let fix map_exprs (ls : list AST) : list RocqExpr :=
        match ls with
        | [] => []
        | x :: xs => emit_rocq_expr x :: map_exprs xs
        end
      in
      RocqDefinition "_perform" []
        (rocq_call (RocqIdentifier "__chester_perform")
          [RocqString op_name; RocqList (map_exprs args)])
  | AstHandle action eff handlers =>
      let fix emit_hs (hs : list (string * AST)) : list (string * RocqExpr) :=
        match hs with
        | [] => []
        | (op, fn) :: rest => (op, emit_rocq_expr fn) :: emit_hs rest
        end
      in
      RocqDefinition "_handle" []
        (rocq_call (RocqIdentifier "__chester_handle")
          [RocqString (effect_label eff);
           RocqLam [] (emit_rocq_expr action);
           RocqPairList (emit_hs handlers)])
  | AstBoolLit b => RocqDefinition "_bool" [] (RocqBool b)
  | AstBox e caps =>
      RocqDefinition "_box" []
        (rocq_call (RocqIdentifier "__chester_box")
          [RocqList (effect_label_rocq_lits caps);
           RocqLam [] (emit_rocq_expr e)])
  | AstUnbox e => RocqDefinition "_unbox" [] (rocq_call (emit_rocq_expr e) [])
  | AstIf cond true_br false_br =>
      RocqDefinition "_if" []
        (RocqIf (rocq_bool_cond (emit_rocq_expr cond))
          (emit_rocq_expr true_br)
          (emit_rocq_expr false_br))
  | AstMatch expr cases =>
      RocqDefinition "_match" [] (emit_rocq_expr (AstMatch expr cases))
  | AstFieldAccess expr field =>
      RocqDefinition "_field" [] (RocqProj (emit_rocq_expr expr) field)
  | AstMeta _ => RocqEmpty
  | AstUniverse _ => RocqEmpty
  | AstError _ => RocqEmpty
  | AstSpan _ inner => emit_rocq_stmt inner
  end.

Definition emit_rocq (ast : AST) : RocqStmt :=
  match ast with
  | AstBlock stmts ret =>
      RocqBlock [RocqDefinition "chester_main" [] (emit_rocq_expr (AstBlock stmts ret))]
  | _ => emit_rocq_stmt ast
  end.

Definition emit_rocq_top (ast : AST) : RocqStmt :=
  match ast with
  | AstBlock stmts ret =>
      let fix map_rocq_stmt (ls : list AST) : list RocqStmt :=
        match ls with
        | [] => []
        | AstRef "Unit" :: xs => map_rocq_stmt xs
        | x :: xs => emit_rocq_stmt x :: map_rocq_stmt xs
        end
      in
      match ret with
      | AstRef "Unit" => RocqBlock (map_rocq_stmt stmts)
      | _ =>
          RocqBlock (map_rocq_stmt stmts
            ++ [RocqDefinition "chester_main" [] (emit_rocq_expr ret)])
      end
  | _ => emit_rocq_stmt ast
  end.
