From Stdlib Require Import Strings.String.
From Stdlib Require Import List.
From Stdlib Require Import Ascii.
Import ListNotations.
Open Scope string_scope.

Require Import Chester.AST.
Require Import Chester.TypeScriptAST.
Require Import Chester.GoAST.

Definition go_str_eq (a b : string) : bool :=
  if string_dec a b then true else false.

(* Chester surface/elaborated types → Go type strings. Fallback is interface{}. *)
Fixpoint chester_to_go_type (ty : AST) {struct ty} : string :=
  match ty with
  | AstRef name =>
      if orb (go_str_eq name "Int") (go_str_eq name "Integer") then "int"
      else if go_str_eq name "String" then "string"
      else if go_str_eq name "Bool" then "bool"
      else if go_str_eq name "Unit" then go_iface
      else if go_str_eq name "Any" then go_iface
      else if go_str_eq name "Type" then go_iface
      else if go_str_eq name "Unknown" then go_iface
      (* Nominal Chester types (records/enums/typarams) stay dynamic until
         we thread a record env; mapping `T`/`Option` to Go names breaks stdlib. *)
      else go_iface
  | AstMeta _ => go_iface
  | AstApp (AstRef "List") _ => go_iface
  | AstApp (AstRef "Pair") _ => go_iface
  | AstPi _ _ _ _ => go_iface
  | AstFunTy _ _ _ _ => go_iface
  | AstSpan _ inner => chester_to_go_type inner
  | _ => go_iface
  end.

Definition go_params_of (params : list (string * AST)) : list (string * string) :=
  map (fun p => (fst p, chester_to_go_type (snd p))) params.

Definition go_fields_of (fields : list (string * AST)) : list (string * string) :=
  map (fun f => (fst f, chester_to_go_type (snd f))) fields.

Definition go_untyped_params (names : list string) : list (string * string) :=
  map (fun n => (n, go_iface)) names.

Definition go_scalar_ret_ty (name : string) : option string :=
  if orb (go_str_eq name "int_add") (orb (go_str_eq name "int_sub") (orb (go_str_eq name "int_mul")
        (orb (go_str_eq name "int_div") (orb (go_str_eq name "int_mod") (orb (go_str_eq name "int_neg")
        (orb (go_str_eq name "prim__int_add") (orb (go_str_eq name "prim__int_sub")
        (orb (go_str_eq name "prim__int_mul") (orb (go_str_eq name "prim__int_div")
        (orb (go_str_eq name "prim__int_mod") (go_str_eq name "prim__int_neg")))))))))))
    then Some "int"
  else if orb (go_str_eq name "int_eq") (orb (go_str_eq name "int_lt") (orb (go_str_eq name "int_gt")
        (orb (go_str_eq name "int_le") (orb (go_str_eq name "int_ge") (orb (go_str_eq name "bool_or")
        (orb (go_str_eq name "bool_and") (orb (go_str_eq name "bool_not") (orb (go_str_eq name "string_eq")
        (orb (go_str_eq name "prim__int_eq") (orb (go_str_eq name "prim__int_lt")
        (orb (go_str_eq name "prim__int_gt") (orb (go_str_eq name "prim__int_le")
        (orb (go_str_eq name "prim__int_ge") (orb (go_str_eq name "prim__bool_or")
        (orb (go_str_eq name "prim__bool_and") (orb (go_str_eq name "prim__bool_not")
        (go_str_eq name "prim__string_eq")))))))))))))))))
    then Some "bool"
  else if orb (go_str_eq name "string_concat") (orb (go_str_eq name "string_length")
        (orb (go_str_eq name "string_substring") (orb (go_str_eq name "int_to_string")
        (orb (go_str_eq name "prim__string_concat") (orb (go_str_eq name "prim__string_length")
        (orb (go_str_eq name "prim__string_substring") (go_str_eq name "prim__int_to_string")))))))
    then Some "string"
  else None.



Definition go_iife (body : list GoStmt) : GoExpr :=
  GoFuncLiteral [] go_iface body.

Definition go_starts_with (pre s : string) : bool :=
  let fix sw (p s : string) : bool :=
    match p, s with
    | EmptyString, _ => true
    | String pc p', String sc s' =>
        if Ascii.eqb pc sc then sw p' s' else false
    | _, _ => false
    end
  in sw pre s.

(* name -> (param go tys, ret go ty) *)
Definition GoSigEnv := list (string * (list string * string)).

Fixpoint go_lookup_sig (env : GoSigEnv) (name : string) : option (list string * string) :=
  match env with
  | [] => None
  | (n, info) :: rest =>
      if go_str_eq n name then Some info else go_lookup_sig rest name
  end.

Fixpoint collect_go_sigs (fuel : nat) (asts : list AST) {struct fuel} : GoSigEnv :=
  match fuel with
  | 0 => []
  | S fuel' =>
      match asts with
      | [] => []
      | AstDef name _ params ret_ty _ :: rest =>
          (name, (map (fun p => chester_to_go_type (snd p)) params, chester_to_go_type ret_ty))
            :: collect_go_sigs fuel' rest
      | AstExtension _ _ _ meths :: rest =>
          app (collect_go_sigs fuel' meths) (collect_go_sigs fuel' rest)
      | _ :: rest => collect_go_sigs fuel' rest
      end
  end.

Definition collect_go_sigs_top (asts : list AST) : GoSigEnv :=
  collect_go_sigs 256 asts.

Definition go_known_ret (sigs : GoSigEnv) (name : string) : option string :=
  match go_scalar_ret_ty name with
  | Some ty => Some ty
  | None =>
      match go_lookup_sig sigs name with
      | Some (_, ret) => Some ret
      | None => None
      end
  end.

Fixpoint go_type_of_ast_value (sigs : GoSigEnv) (e : AST) {struct e} : string :=
  match e with
  | AstIntLit _ => "int"
  | AstStringLit _ => "string"
  | AstBoolLit _ => "bool"
  | AstApp func _ =>
      match func with
      | AstRef name =>
          match go_known_ret sigs name with
          | Some ty => ty
          | None => go_iface
          end
      | _ => go_iface
      end
  | AstSpan _ inner => go_type_of_ast_value sigs inner
  | _ => go_iface
  end.

Definition go_as_helper (ty : string) : string :=
  match go_str_eq ty "int", go_str_eq ty "string", go_str_eq ty "bool" with
  | true, _, _ => "__chester_as_int"
  | _, true, _ => "__chester_as_string"
  | _, _, true => "__chester_as_bool"
  | _, _, _ => EmptyString
  end.

Definition go_coerce_to (ty : string) (e : GoExpr) : GoExpr :=
  let h := go_as_helper ty in
  if go_str_eq h EmptyString then e else GoCall (GoIdentifier h) [e].

Definition go_wrap_ret (sigs : GoSigEnv) (ret_ty : string) (e : GoExpr) : GoExpr :=
  if go_str_eq ret_ty go_iface then e
  else
    match e with
    | GoIntLiteral _ => e
    | GoStringLiteral _ => e
    | GoBoolLiteral _ => e
    | GoTypeAssert _ ty => if go_str_eq ty ret_ty then e else go_coerce_to ret_ty e
    | GoCall (GoIdentifier name) _ =>
        if go_str_eq name (go_as_helper ret_ty) then e
        else
          match go_known_ret sigs name with
          | Some ty => if go_str_eq ty ret_ty then e else go_coerce_to ret_ty e
          | None => go_coerce_to ret_ty e
          end
    | GoCall (GoFuncLiteral _ _ _) _ => go_coerce_to ret_ty e
    | GoCall (GoTypeAssert _ _) _ => go_coerce_to ret_ty e
    | GoIdentifier _ => go_coerce_to ret_ty e
    | _ => go_coerce_to ret_ty e
    end.

Fixpoint go_map_returns (fuel : nat) (sigs : GoSigEnv) (ret_ty : string) (ss : list GoStmt) {struct fuel} : list GoStmt :=
  match fuel with
  | 0 => ss
  | S fuel' =>
      match ss with
      | [] => []
      | GoReturn e :: rest => GoReturn (go_wrap_ret sigs ret_ty e) :: go_map_returns fuel' sigs ret_ty rest
      | GoIfStmt c t e :: rest =>
          GoIfStmt c (go_map_returns fuel' sigs ret_ty t) (go_map_returns fuel' sigs ret_ty e)
            :: go_map_returns fuel' sigs ret_ty rest
      | GoBlock b :: rest =>
          GoBlock (go_map_returns fuel' sigs ret_ty b) :: go_map_returns fuel' sigs ret_ty rest
      | s :: rest => s :: go_map_returns fuel' sigs ret_ty rest
      end
  end.

Definition go_map_returns_top (sigs : GoSigEnv) (ret_ty : string) (ss : list GoStmt) : list GoStmt :=
  go_map_returns 64 sigs ret_ty ss.

Definition go_coerce_arg (sigs : GoSigEnv) (param_ty : string) (arg : GoExpr) : GoExpr :=
  if go_str_eq param_ty go_iface then arg
  else
    match arg with
    | GoIntLiteral _ => arg
    | GoStringLiteral _ => arg
    | GoBoolLiteral _ => arg
    | GoTypeAssert _ ty =>
        if go_str_eq ty param_ty then arg else go_coerce_to param_ty arg
    | GoCall (GoIdentifier name) _ =>
        if go_str_eq name (go_as_helper param_ty) then arg
        else
          match go_known_ret sigs name with
          | Some ty => if go_str_eq ty param_ty then arg else go_coerce_to param_ty arg
          | None => go_coerce_to param_ty arg
          end
    | _ => go_coerce_to param_ty arg
    end.

Fixpoint go_coerce_args (sigs : GoSigEnv) (param_tys : list string) (args : list GoExpr) : list GoExpr :=
  match param_tys, args with
  | [], _ => args
  | _, [] => []
  | ty :: tys, a :: as_ => go_coerce_arg sigs ty a :: go_coerce_args sigs tys as_
  end.

Definition go_builtin_param_tys (name : string) (nargs : nat) : option (list string) :=
  let fix reps (ty : string) (n : nat) : list string :=
    match n with
    | 0 => []
    | S n' => ty :: reps ty n'
    end
  in
  if orb (go_str_eq name "int_add") (orb (go_str_eq name "int_sub") (orb (go_str_eq name "int_mul")
        (orb (go_str_eq name "int_div") (orb (go_str_eq name "int_mod") (orb (go_str_eq name "int_eq")
        (orb (go_str_eq name "int_lt") (orb (go_str_eq name "int_gt") (orb (go_str_eq name "int_le")
        (go_str_eq name "int_ge"))))))))) then Some (reps "int" nargs)
  else if go_str_eq name "int_neg" then Some (reps "int" nargs)
  else if go_str_eq name "int_to_string" then Some (reps "int" nargs)
  else if orb (go_str_eq name "string_eq") (orb (go_str_eq name "string_concat") (go_str_eq name "string_length"))
    then Some (reps "string" nargs)
  else if go_str_eq name "string_substring" then Some ["string"; "int"; "int"]
  else if orb (go_str_eq name "bool_or") (orb (go_str_eq name "bool_and") (go_str_eq name "bool_not"))
    then Some (reps "bool" nargs)
  else if orb (go_str_eq name "prim__int_add") (orb (go_str_eq name "prim__int_sub") (orb (go_str_eq name "prim__int_mul")
        (orb (go_str_eq name "prim__int_div") (orb (go_str_eq name "prim__int_mod") (orb (go_str_eq name "prim__int_eq")
        (orb (go_str_eq name "prim__int_lt") (orb (go_str_eq name "prim__int_gt") (orb (go_str_eq name "prim__int_le")
        (go_str_eq name "prim__int_ge"))))))))) then Some (reps "int" nargs)
  else if go_str_eq name "prim__int_neg" then Some (reps "int" nargs)
  else if go_str_eq name "prim__int_to_string" then Some (reps "int" nargs)
  else if orb (go_str_eq name "prim__string_eq") (orb (go_str_eq name "prim__string_concat") (go_str_eq name "prim__string_length"))
    then Some (reps "string" nargs)
  else if go_str_eq name "prim__string_substring" then Some ["string"; "int"; "int"]
  else if orb (go_str_eq name "prim__bool_or") (orb (go_str_eq name "prim__bool_and") (go_str_eq name "prim__bool_not"))
    then Some (reps "bool" nargs)
  else None.

Definition go_call_args (sigs : GoSigEnv) (func : AST) (args : list GoExpr) : list GoExpr :=
  let nargs := length args in
  match func with
  | AstRef name =>
      match go_builtin_param_tys name nargs with
      | Some tys => go_coerce_args sigs tys args
      | None =>
          match go_lookup_sig sigs name with
          | Some (tys, _) => go_coerce_args sigs tys args
          | None => args
          end
      end
  | _ => args
  end.

Definition digit_char (d : nat) : string :=
  match d with
  | 0 => "0" | 1 => "1" | 2 => "2" | 3 => "3" | 4 => "4"
  | 5 => "5" | 6 => "6" | 7 => "7" | 8 => "8" | _ => "9"
  end.

Fixpoint nat_to_string_fuel (fuel n : nat) (acc : string) : string :=
  match fuel with
  | 0 => acc
  | S f =>
      if Nat.eqb n 0 then acc
      else nat_to_string_fuel f (Nat.div n 10) (digit_char (Nat.modulo n 10) ++ acc)
  end.

Definition nat_to_string (n : nat) : string :=
  if Nat.eqb n 0 then "0" else nat_to_string_fuel 20 n "".

Definition effect_label (eff : EffectRef) : string :=
  match eff with
  | UserEffect n => n
  | BuiltinEffect n => n
  | EffectRowVar n => n
  end.

Fixpoint effect_label_lits (es : EffectSet) : list TypeScriptExpr :=
  match es with
  | [] => []
  | EffectRowVar _ :: xs => effect_label_lits xs
  | e :: xs => TsStringLiteral (effect_label e) :: effect_label_lits xs
  end.

Fixpoint effect_label_go_lits (es : EffectSet) : list GoExpr :=
  match es with
  | [] => []
  | EffectRowVar _ :: xs => effect_label_go_lits xs
  | e :: xs => GoStringLiteral (effect_label e) :: effect_label_go_lits xs
  end.

Definition go_direct_call (name : string) : bool :=
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

(* Package selectors (fmt.Println) and direct builtins call without interface{} asserts. *)

Definition is_upper (s : string) : bool :=
  match s with
  | EmptyString => false
  | String c _ =>
      let n := nat_of_ascii c in
      (PeanoNat.Nat.leb 65 n) && (PeanoNat.Nat.leb n 90)
  end.

Definition go_app_direct (func : AST) : bool :=
  match func with
  | AstRef name => 
      if string_dec name "resume" then false else
      if string_dec name "f" then false else
      if string_dec name "predicate" then false else
      true
  | AstFieldAccess _ _ => true
  | _ => false
  end.

(* Interface{} values (e.g. resume) need a type assert before calling. *)
Definition go_call_emitted (direct : bool) (callee : GoExpr) (args : list GoExpr) : GoExpr :=
  if direct then GoCall callee args
  else
    let fix gen_types (n : nat) : string :=
      match n with
      | 0 => ""
      | S 0 => "interface{}"
      | S n' => append "interface{}, " (gen_types n')
      end
    in
    let assert_ty := append "func(" (append (gen_types (length args)) ") interface{}") in
    GoCall (GoTypeAssert callee assert_ty) args.

Definition go_bool_cond (e : GoExpr) : GoExpr :=
  match e with
  | GoBoolLiteral _ => e
  | GoTypeAssert _ ty => if go_str_eq ty "bool" then e else GoCall (GoIdentifier "__chester_as_bool") [e]
  | GoCall (GoIdentifier name) _ =>
      if go_str_eq name "__chester_as_bool" then e
      else
        match go_scalar_ret_ty name with
        | Some ty => if go_str_eq ty "bool" then e else GoCall (GoIdentifier "__chester_as_bool") [e]
        | None => GoCall (GoIdentifier "__chester_as_bool") [e]
        end
  | _ => GoCall (GoIdentifier "__chester_as_bool") [e]
  end.

(* 
  TypeScript Backend
*)
Definition emit_ts_import (lang alias mod_path : string) (sym_list : list string) : TypeScriptStmt :=
  if string_dec lang "ts" then
    match sym_list with
    | [] =>
        if string_dec alias "" then TsEmpty
        else TsImportNamespace alias mod_path
    | _ => TsImportNamed mod_path sym_list
    end
  else TsEmpty.

Definition emit_go_import (lang mod_path : string) : GoStmt :=
  if string_dec lang "go" then GoImport mod_path else GoEmpty.

Fixpoint emit_ts_expr (ast : AST) {struct ast} : TypeScriptExpr :=
  let fix map_ts_expr (ls : list AST) : list TypeScriptExpr :=
    match ls with
    | [] => []
    | x :: xs => emit_ts_expr x :: map_ts_expr xs
    end
  in
  match ast with
  | AstRef name => TsIdentifier name
  | AstTuple elems => TsArray (map_ts_expr elems)
  | AstStringLit s => TsStringLiteral s
  | AstIntLit n => TsNumberLiteral (nat_to_string n)
  | AstBlock stmts ret =>
      let fix map_ts_stmt (ls : list AST) : list TypeScriptStmt :=
        match ls with
        | [] => []
        | x :: xs => emit_ts_stmt x :: map_ts_stmt xs
        end
      in TsIIFE (map_ts_stmt stmts ++ [TsReturn (emit_ts_expr ret)])
  | AstApp func args => TsCall (emit_ts_expr func) (map_ts_expr args)
  | AstImplicitApp func _args => emit_ts_expr func  (* type args erased — no runtime representation *)
  | AstLam argName argTy body => TsArrow [argName] (emit_ts_block body)
  | AstPi argName argTy retTy effs => TsIdentifier "any"
  | AstFunTy _tparams _params _ret_ty _effs => TsIdentifier "any"  (* non-curried fun type — erased *)
  | AstDo op args =>
      let op_name := match op with AstRef n => n | _ => "unknown" end in
      TsCall (TsIdentifier "__chester_perform") [TsStringLiteral op_name; TsArray (map_ts_expr args)]
  | AstHandle action eff handlers =>
      let fix emit_hs (hs : list (string * AST)) : list (string * TypeScriptExpr) :=
        match hs with
        | [] => []
        | (op, fn) :: rest => (op, emit_ts_expr fn) :: emit_hs rest
        end
      in
      TsCall (TsIdentifier "__chester_handle")
        [TsStringLiteral (effect_label eff);
         TsArrow [] (emit_ts_block action);
         TsObjectLiteral (emit_hs handlers)]
  | AstBoolLit b => TsBooleanLiteral b
  | AstLet name value => TsIIFE [TsLet name (emit_ts_expr value)]
  | AstVar name value => TsIIFE [TsVar name (emit_ts_expr value)]
  | AstAssign name value => TsIIFE [TsAssign name (emit_ts_expr value)]
  | AstIf cond true_br false_br => TsIIFE [TsIfStmt (emit_ts_expr cond) (emit_ts_block true_br) (emit_ts_block false_br)]
  | AstDef name _ params _ body => TsIIFE [TsFunctionDecl name (map fst params) (emit_ts_block body)]
  | AstEnum _ _ _ => TsIdentifier "null"
  | AstExtension _ _ _ _ => TsIdentifier "null"
  | AstBox e caps =>
      TsCall (TsIdentifier "__chester_box")
        [TsArray (effect_label_lits caps); TsArrow [] [TsReturn (emit_ts_expr e)]]
  | AstUnbox e => TsCall (emit_ts_expr e) []
  | AstMatch expr cases => TsIIFE (let fix emit_cases (cs : list (PatternAST * AST)) : list TypeScriptStmt :=
        match cs with
        | [] => [TsThrow "Non-exhaustive match"]
        | (pat, body) :: rest =>
            match pat with
            | PatConstructor cname vars =>
                let cond := TsCall (TsIdentifier "prim__string_eq") [TsPropertyAccess (TsIdentifier "_match_val") "_tag"; TsStringLiteral cname] in
                let body_ts := emit_ts_block body in
                let fix bind_vars (vs : list string) (idx : nat) (acc : list TypeScriptStmt) : list TypeScriptStmt :=
                  match vs with
                  | [] => acc
                  | v :: vs' => bind_vars vs' (S idx) (TsLet v (TsPropertyAccess (TsIdentifier "_match_val") ("_f" ++ nat_to_string idx)) :: acc)
                  end
                in
                [TsIfStmt cond (bind_vars vars 0 [] ++ body_ts) (emit_cases rest)]
            | PatWildcard => [TsIfStmt (TsBooleanLiteral true) (emit_ts_block body) (emit_cases rest)]
            | PatVar v => TsLet v (TsIdentifier "_match_val") :: emit_ts_block body
            end
        end
      in TsLet "_match_val" (emit_ts_expr expr) :: emit_cases cases)
  | AstRecord name _ _ => TsIdentifier "null"
  | AstFieldAccess expr field => TsPropertyAccess (emit_ts_expr expr) field
  | AstImport _ _ _ _ => TsIdentifier "undefined"
  | AstMeta id => TsIdentifier ("/* ?meta_" ++ nat_to_string id ++ " */")
  | AstUniverse _ => TsIIFE [TsThrow "Universe in term"]
  | AstError e => TsIIFE [TsThrow e]
  | AstSpan _ inner => emit_ts_expr inner
  end

with emit_ts_stmt (ast : AST) {struct ast} : TypeScriptStmt :=
  match ast with
  | AstLet name value => TsLet name (emit_ts_expr value)
  | AstVar name value => TsVar name (emit_ts_expr value)
  | AstAssign name value => TsAssign name (emit_ts_expr value)
  | AstImport lang alias mod_path syms => emit_ts_import lang alias mod_path syms
  | AstDef name _ params _ body => TsFunctionDecl name (map fst params) (emit_ts_block body)
  | AstRecord name _ _ => TsInterface name
  | AstExtension _ _ _ meths =>
      let fix map_meths (ls : list AST) : list TypeScriptStmt :=
        match ls with
        | [] => []
        | x :: xs => emit_ts_stmt x :: map_meths xs
        end
      in TsBlock (map_meths meths)
  | AstEnum name _ variants =>
      let fix emit_variant (v : string * list AST * AST) : string * TypeScriptExpr :=
        let vname := fst (fst v) in
        let fields := snd (fst v) in
        let fix field_names (n : nat) (fs : list AST) : list string :=
          match fs with
          | [] => []
          | _ :: rest => ("_f" ++ nat_to_string n) :: field_names (S n) rest
          end
        in
        let params := field_names 0 fields in
        let fix field_pairs (ps : list string) : list (string * TypeScriptExpr) :=
          match ps with
          | [] => []
          | p :: rest => (p, TsIdentifier p) :: field_pairs rest
          end
        in
        let body := TsObjectLiteral (("_tag", TsStringLiteral vname) :: field_pairs params) in
        match params with
        | [] => (vname, body)
        | _ => (vname, TsArrow params [TsReturn body])
        end
      in
      let fix emit_variants (vs : list (string * list AST * AST)) : list (string * TypeScriptExpr) :=
        match vs with
        | [] => []
        | v :: rest => emit_variant v :: emit_variants rest
        end
      in
      TsConst name (TsObjectLiteral (emit_variants variants))
  | AstRef name => TsExprStmt (TsIdentifier name)
  | AstTuple elems => 
      let fix map_ts_expr (ls : list AST) : list TypeScriptExpr :=
        match ls with
        | [] => []
        | x :: xs => emit_ts_expr x :: map_ts_expr xs
        end
      in TsExprStmt (TsArray (map_ts_expr elems))
  | AstStringLit s => TsExprStmt (TsStringLiteral s)
  | AstIntLit n => TsExprStmt (TsNumberLiteral (nat_to_string n))
  | AstBlock stmts ret => 
      let fix map_ts_stmt (ls : list AST) : list TypeScriptStmt :=
        match ls with
        | [] => []
        | x :: xs => emit_ts_stmt x :: map_ts_stmt xs
        end
      in TsExprStmt (TsIIFE (map_ts_stmt stmts ++ [TsReturn (emit_ts_expr ret)]))
  | AstApp func args => 
      let fix map_ts_expr (ls : list AST) : list TypeScriptExpr :=
        match ls with
        | [] => []
        | x :: xs => emit_ts_expr x :: map_ts_expr xs
        end
      in TsExprStmt (TsCall (emit_ts_expr func) (map_ts_expr args))
  | AstImplicitApp func _args => TsExprStmt (emit_ts_expr func)  (* type args erased *)
  | AstLam argName argTy body => TsExprStmt (TsArrow [argName] (emit_ts_block body))
  | AstPi argName argTy retTy effs => TsExprStmt (TsIdentifier "any")
  | AstFunTy _tparams _params _ret_ty _effs => TsExprStmt (TsIdentifier "any")
  | AstDo op args =>
      let op_name := match op with AstRef n => n | _ => "unknown" end in
      let fix map_ts_expr (ls : list AST) : list TypeScriptExpr :=
        match ls with
        | [] => []
        | x :: xs => emit_ts_expr x :: map_ts_expr xs
        end
      in
      TsExprStmt (TsCall (TsIdentifier "__chester_perform")
        [TsStringLiteral op_name; TsArray (map_ts_expr args)])
  | AstHandle action eff handlers =>
      let fix emit_hs (hs : list (string * AST)) : list (string * TypeScriptExpr) :=
        match hs with
        | [] => []
        | (op, fn) :: rest => (op, emit_ts_expr fn) :: emit_hs rest
        end
      in
      TsExprStmt (TsCall (TsIdentifier "__chester_handle")
        [TsStringLiteral (effect_label eff);
         TsArrow [] (emit_ts_block action);
         TsObjectLiteral (emit_hs handlers)])
  | AstBoolLit b => TsExprStmt (TsBooleanLiteral b)
  | AstIf cond true_br false_br => TsExprStmt (TsIIFE [TsIfStmt (emit_ts_expr cond) (emit_ts_block true_br) (emit_ts_block false_br)])
  | AstMatch expr cases => TsExprStmt (TsIIFE (let fix emit_cases (cs : list (PatternAST * AST)) : list TypeScriptStmt :=
        match cs with
        | [] => [TsThrow "Non-exhaustive match"]
        | (pat, body) :: rest =>
            match pat with
            | PatConstructor cname vars =>
                let cond := TsCall (TsIdentifier "prim__string_eq") [TsPropertyAccess (TsIdentifier "_match_val") "_tag"; TsStringLiteral cname] in
                let body_ts := emit_ts_block body in
                let fix bind_vars (vs : list string) (idx : nat) (acc : list TypeScriptStmt) : list TypeScriptStmt :=
                  match vs with
                  | [] => acc
                  | v :: vs' => bind_vars vs' (S idx) (TsLet v (TsIndexAccess (TsPropertyAccess (TsIdentifier "_match_val") "args") (TsNumberLiteral (nat_to_string idx))) :: acc)
                  end
                in
                [TsIfStmt cond (bind_vars vars 0 [] ++ body_ts) (emit_cases rest)]
            | PatWildcard => [TsIfStmt (TsBooleanLiteral true) (emit_ts_block body) (emit_cases rest)]
            | PatVar v => TsLet v (TsIdentifier "_match_val") :: emit_ts_block body
            end
        end
      in TsLet "_match_val" (emit_ts_expr expr) :: emit_cases cases))
  | AstFieldAccess expr field => TsExprStmt (TsPropertyAccess (emit_ts_expr expr) field)
  | AstBox e caps =>
      TsExprStmt (TsCall (TsIdentifier "__chester_box")
        [TsArray (effect_label_lits caps); TsArrow [] [TsReturn (emit_ts_expr e)]])
  | AstUnbox e => TsExprStmt (TsCall (emit_ts_expr e) [])
  | AstMeta id => TsExprStmt (TsIdentifier ("/* ?meta_" ++ nat_to_string id ++ " */"))
  | AstUniverse _ => TsExprStmt (TsIIFE [TsThrow "Universe in term"])
  | AstError e => TsExprStmt (TsIIFE [TsThrow e])
  | AstSpan _ inner => emit_ts_stmt inner
  end

with emit_ts_block (ast : AST) {struct ast} : list TypeScriptStmt :=
  match ast with
  | AstBlock stmts ret =>
      let fix map_ts_stmt (ls : list AST) : list TypeScriptStmt :=
        match ls with
        | [] => []
        | x :: xs => emit_ts_stmt x :: map_ts_stmt xs
        end
      in map_ts_stmt stmts ++ [TsReturn (emit_ts_expr ret)]
  | AstIf cond true_br false_br => [TsIfStmt (emit_ts_expr cond) (emit_ts_block true_br) (emit_ts_block false_br)]
  | AstMatch expr cases => let fix emit_cases (cs : list (PatternAST * AST)) : list TypeScriptStmt :=
        match cs with
        | [] => [TsThrow "Non-exhaustive match"]
        | (pat, body) :: rest =>
            match pat with
            | PatConstructor cname vars =>
                let cond := TsCall (TsIdentifier "prim__string_eq") [TsPropertyAccess (TsIdentifier "_match_val") "_tag"; TsStringLiteral cname] in
                let body_ts := emit_ts_block body in
                let fix bind_vars (vs : list string) (idx : nat) (acc : list TypeScriptStmt) : list TypeScriptStmt :=
                  match vs with
                  | [] => acc
                  | v :: vs' => bind_vars vs' (S idx) (TsLet v (TsIndexAccess (TsPropertyAccess (TsIdentifier "_match_val") "args") (TsNumberLiteral (nat_to_string idx))) :: acc)
                  end
                in
                [TsIfStmt cond (bind_vars vars 0 [] ++ body_ts) (emit_cases rest)]
            | PatWildcard => [TsIfStmt (TsBooleanLiteral true) (emit_ts_block body) (emit_cases rest)]
            | PatVar v => TsLet v (TsIdentifier "_match_val") :: emit_ts_block body
            end
        end
      in TsLet "_match_val" (emit_ts_expr expr) :: emit_cases cases
  | AstUniverse _ => [TsThrow "Universe in term"]
  | AstError e => [TsThrow e]
  | AstSpan _ inner => emit_ts_block inner
  | AstRef name => [TsReturn (TsIdentifier name)]
  | AstTuple elems => 
      let fix map_ts_expr (ls : list AST) : list TypeScriptExpr :=
        match ls with
        | [] => []
        | x :: xs => emit_ts_expr x :: map_ts_expr xs
        end
      in [TsReturn (TsArray (map_ts_expr elems))]
  | AstStringLit s => [TsReturn (TsStringLiteral s)]
  | AstIntLit n => [TsReturn (TsNumberLiteral (nat_to_string n))]
  | AstApp func args => 
      let fix map_ts_expr (ls : list AST) : list TypeScriptExpr :=
        match ls with
        | [] => []
        | x :: xs => emit_ts_expr x :: map_ts_expr xs
        end
      in [TsReturn (TsCall (emit_ts_expr func) (map_ts_expr args))]
  | AstImplicitApp func _args => [TsReturn (emit_ts_expr func)]  (* type args erased *)
  | AstLam argName argTy body => [TsReturn (TsArrow [argName] (emit_ts_block body))]
  | AstPi argName argTy retTy effs => [TsReturn (TsIdentifier "any")]
  | AstFunTy _tparams _params _ret_ty _effs => [TsReturn (TsIdentifier "any")]
  | AstDo op args =>
      let op_name := match op with AstRef n => n | _ => "unknown" end in
      let fix map_ts_expr (ls : list AST) : list TypeScriptExpr :=
        match ls with
        | [] => []
        | x :: xs => emit_ts_expr x :: map_ts_expr xs
        end
      in
      [TsReturn (TsCall (TsIdentifier "__chester_perform")
        [TsStringLiteral op_name; TsArray (map_ts_expr args)])]
  | AstHandle action eff handlers =>
      let fix emit_hs (hs : list (string * AST)) : list (string * TypeScriptExpr) :=
        match hs with
        | [] => []
        | (op, fn) :: rest => (op, emit_ts_expr fn) :: emit_hs rest
        end
      in
      [TsReturn (TsCall (TsIdentifier "__chester_handle")
        [TsStringLiteral (effect_label eff);
         TsArrow [] (emit_ts_block action);
         TsObjectLiteral (emit_hs handlers)])]
  | AstBoolLit b => [TsReturn (TsBooleanLiteral b)]
  | AstLet name value => [TsReturn (TsIIFE [TsLet name (emit_ts_expr value)])]
  | AstVar name value => [TsReturn (TsIIFE [TsVar name (emit_ts_expr value)])]
  | AstAssign name value => [TsAssign name (emit_ts_expr value)]
  | AstBox e caps =>
      [TsReturn (TsCall (TsIdentifier "__chester_box")
        [TsArray (effect_label_lits caps); TsArrow [] [TsReturn (emit_ts_expr e)]])]
  | AstUnbox e => [TsReturn (TsCall (emit_ts_expr e) [])]
  | AstDef name _ params _ body => [TsReturn (TsIIFE [TsFunctionDecl name (map fst params) (emit_ts_block body)])]
  | AstEnum _ _ _ => [TsReturn (TsIdentifier "null")]
  | AstExtension _ _ _ _ => [TsReturn (TsIdentifier "null")]
  | AstRecord name _ _ => [TsReturn (TsIdentifier "null")]
  | AstFieldAccess expr field => [TsReturn (TsPropertyAccess (emit_ts_expr expr) field)]
  | AstImport lang alias mod_path syms => [emit_ts_import lang alias mod_path syms]
  | AstMeta id => [TsReturn (TsIdentifier ("/* ?meta_" ++ nat_to_string id ++ " */"))]
  end.

Definition emit_ts (ast : AST) : TypeScriptStmt :=
  match ast with
  | AstBlock stmts ret =>
      let fix map_ts_stmt (ls : list AST) : list TypeScriptStmt :=
        match ls with
        | [] => []
        | x :: xs => emit_ts_stmt x :: map_ts_stmt xs
        end
      in
      (* Emit as flat top-level sequence so declarations are globally scoped *)
      TsBlock (map_ts_stmt stmts ++ [TsExprStmt (emit_ts_expr ret)])
  | _ => emit_ts_stmt ast
  end.

Fixpoint emit_ts_top_stmt (ast : AST) {struct ast} : TypeScriptStmt :=
  match ast with
  | AstImport lang alias mod_path syms => emit_ts_import lang alias mod_path syms
  | AstDef name _ params _ body => TsExportFunction name (map fst params) (emit_ts_block body)
  | AstLet name value => TsConst name (emit_ts_expr value)
  | AstRef "Unit" => TsEmpty
  | AstSpan _ inner => emit_ts_top_stmt inner
  | _ => emit_ts_stmt ast
  end.

Definition emit_ts_top (ast : AST) : TypeScriptStmt :=
  match ast with
  | AstBlock stmts ret =>
      let fix map_top (ls : list AST) : list TypeScriptStmt :=
        match ls with
        | [] => []
        | AstRef "Unit" :: xs => map_top xs
        | x :: xs =>
            let stmt := emit_ts_top_stmt x in
            match stmt with
            | TsEmpty => map_top xs
            | _ => stmt :: map_top xs
            end
        end
      in
      let tail :=
        match ret with
        | AstRef "Unit" => []
        | _ => [TsExprStmt (emit_ts_expr ret)]
        end
      in
      TsBlock (map_top stmts ++ tail)
  | _ => emit_ts_top_stmt ast
  end.



(* 
  Golang Backend
*)
Fixpoint emit_go_expr (sigs : GoSigEnv) (ast : AST) {struct ast} : GoExpr :=
  let fix map_go_expr (ls : list AST) : list GoExpr :=
    match ls with
    | [] => []
    | x :: xs => emit_go_expr sigs x :: map_go_expr xs
    end
  in
  match ast with
  | AstRef name => GoIdentifier name
  | AstTuple elems => GoArray (map_go_expr elems)
  | AstStringLit s => GoStringLiteral s
  | AstIntLit n => GoIntLiteral (nat_to_string n)
  | AstBlock stmts ret => 
      let fix map_go_stmt (ls : list AST) : list GoStmt :=
        match ls with
        | [] => []
        | x :: xs => emit_go_stmt sigs x :: map_go_stmt xs
        end
      in GoCall (GoFuncLiteral [] go_iface (map_go_stmt stmts ++ [GoReturn (emit_go_expr sigs ret)])) []
  | AstApp func args =>
      let direct := go_app_direct func in
      let raw_args := map_go_expr args in
      go_call_emitted direct (emit_go_expr sigs func) (go_call_args sigs func raw_args)
  | AstImplicitApp func _args => emit_go_expr sigs func  (* type args erased *)
  | AstFunTy _tparams _params _ret_ty _effs => GoIdentifier "interface{}"
  | AstLam argName argTy body => GoFuncLiteral [(argName, go_iface)] go_iface (emit_go_block sigs body)
  | AstPi argName argTy retTy effs => GoIdentifier "interface{}"
  | AstDo op args =>
      let op_name := match op with AstRef n => n | _ => "unknown" end in
      GoCall (GoIdentifier "__chester_perform")
        [GoStringLiteral op_name; GoArray (map_go_expr args)]
  | AstHandle action eff handlers =>
      let fix emit_hs (hs : list (string * AST)) : list (string * GoExpr) :=
        match hs with
        | [] => []
        | (op, fn) :: rest => (op, emit_go_expr sigs fn) :: emit_hs rest
        end
      in
      GoCall (GoIdentifier "__chester_handle")
        [GoStringLiteral (effect_label eff);
         GoFuncLiteral [] go_iface (emit_go_block sigs action);
         GoMapLiteral (emit_hs handlers)]
  | AstBoolLit b => GoBoolLiteral b
  | AstLet name value =>
      GoCall (GoFuncLiteral [] go_iface [GoLet name (go_type_of_ast_value sigs value) (emit_go_expr sigs value); GoDiscardBinding name]) []
  | AstVar name value => GoCall (GoFuncLiteral [] go_iface [GoLet name go_iface (emit_go_expr sigs value)]) []
  | AstAssign name value => GoCall (GoFuncLiteral [] go_iface [GoAssign name (emit_go_expr sigs value)]) []
  | AstBox e caps =>
      GoCall (GoIdentifier "__chester_box")
        [GoArray (effect_label_go_lits caps);
         GoFuncLiteral [] go_iface [GoReturn (emit_go_expr sigs e)]]
  | AstUnbox e =>
      go_call_emitted false (emit_go_expr sigs e) []
  | AstIf cond true_br false_br => GoCall (GoFuncLiteral [] go_iface [GoIfStmt (go_bool_cond (emit_go_expr sigs cond)) (emit_go_block sigs true_br) (emit_go_block sigs false_br)]) []
  | AstDef name _ params ret_ty body => GoCall (GoFuncLiteral [] go_iface [GoFuncDecl name (go_params_of params) (chester_to_go_type ret_ty) (go_map_returns_top sigs (chester_to_go_type ret_ty) (emit_go_block sigs body))]) []
  | AstEnum _ _ _ => GoIdentifier "nil"
  | AstExtension _ _ _ _ => GoIdentifier "nil"
  | AstMatch expr cases => 
      let fix emit_cases (cs : list (PatternAST * AST)) : list GoStmt :=
        match cs with
        | [] => [GoPanic "Non-exhaustive match"]
        | (pat, body) :: rest =>
            match pat with
            | PatConstructor cname vars =>
                let cond := GoCall (GoIdentifier "_match_val.(map[string]interface{})[""_tag""] ==") [GoStringLiteral cname] in
                let body_go := emit_go_block sigs body in
                let fix bind_vars (vs : list string) (idx : nat) (acc : list GoStmt) : list GoStmt :=
                  match vs with
                  | [] => acc
                  | v :: vs' => bind_vars vs' (S idx) (GoLet v go_iface (GoIndex (GoTypeAssert (GoIndex (GoTypeAssert (GoIdentifier "_match_val") "map[string]interface{}") (GoStringLiteral "args")) "[]interface{}") (GoIntLiteral (nat_to_string idx))) :: acc)
                  end
                in
                [GoIfStmt cond (bind_vars vars 0 [] ++ body_go) (emit_cases rest)]
            | PatWildcard => [GoIfStmt (GoBoolLiteral true) (emit_go_block sigs body) (emit_cases rest)]
            | PatVar v => GoLet v go_iface (GoIdentifier "_match_val") :: emit_go_block sigs body
            end
        end
      in GoCall (GoFuncLiteral [] go_iface (GoLet "_match_val" go_iface (emit_go_expr sigs expr) :: emit_cases cases)) []
  | AstRecord name _ _ => GoIdentifier "nil"
  | AstFieldAccess expr field =>       match expr with
      | AstRef n => if is_upper n then GoIdentifier field else GoSelector (emit_go_expr sigs expr) field
      | _ => GoSelector (emit_go_expr sigs expr) field
      end
  | AstImport _ _ _ _ => GoIdentifier "nil"
  | AstMeta id => GoIdentifier ("/* ?meta_" ++ nat_to_string id ++ " */")
  | AstUniverse _ => GoCall (GoFuncLiteral [] go_iface [GoPanic "Universe in term"]) []
  | AstError e => GoCall (GoFuncLiteral [] go_iface [GoPanic e]) []
  | AstSpan _ inner => emit_go_expr sigs inner
  end

with emit_go_stmt (sigs : GoSigEnv) (ast : AST) {struct ast} : GoStmt :=
  match ast with
  | AstImport lang _ mod_path _ => emit_go_import lang mod_path
  | AstLet name value =>
      GoBlock [GoLet name (go_type_of_ast_value sigs value) (emit_go_expr sigs value); GoDiscardBinding name]
  | AstDef name _ params ret_ty body => GoFuncDecl name (go_params_of params) (chester_to_go_type ret_ty) (go_map_returns_top sigs (chester_to_go_type ret_ty) (emit_go_block sigs body))
  | AstRecord name _ fields => GoStruct name (go_fields_of fields)
  | AstEnum name _ variants =>
      let fix emit_variant (v : string * list AST * AST) : GoStmt :=
        let vname := fst (fst v) in
        let fields := snd (fst v) in
        let fix field_names (n : nat) (fs : list AST) : list string :=
          match fs with
          | [] => []
          | _ :: rest => ("_f" ++ nat_to_string n) :: field_names (S n) rest
          end
        in
        let params := field_names 0 fields in
        let fix field_vars (ps : list string) : list GoExpr :=
          match ps with
          | [] => []
          | p :: rest => GoIdentifier p :: field_vars rest
          end
        in
        let body := GoMapLiteral [("_tag", GoStringLiteral vname); ("args", GoArray (field_vars params))] in
        match params with
        | [] => GoLet vname go_iface body
        | _ => GoFuncDecl vname (go_untyped_params params) go_iface [GoReturn body]
        end
      in
      let fix emit_variants (vs : list (string * list AST * AST)) : list GoStmt :=
        match vs with
        | [] => []
        | v :: rest => emit_variant v :: emit_variants rest
        end
      in
      GoBlock (emit_variants variants)
  | AstExtension _ _ _ meths =>
      let fix map_meths (ls : list AST) : list GoStmt :=
        match ls with
        | [] => []
        | x :: xs => emit_go_stmt sigs x :: map_meths xs
        end
      in GoBlock (map_meths meths)
  | AstRef name => GoExprStmt (GoIdentifier name)
  | AstTuple elems => 
      let fix map_go_expr (ls : list AST) : list GoExpr :=
        match ls with
        | [] => []
        | x :: xs => emit_go_expr sigs x :: map_go_expr xs
        end
      in GoExprStmt (GoArray (map_go_expr elems))
  | AstStringLit s => GoExprStmt (GoStringLiteral s)
  | AstIntLit n => GoExprStmt (GoIntLiteral (nat_to_string n))
  | AstBlock stmts ret => 
      let fix map_go_stmt (ls : list AST) : list GoStmt :=
        match ls with
        | [] => []
        | x :: xs => emit_go_stmt sigs x :: map_go_stmt xs
        end
      in GoExprStmt (GoCall (GoFuncLiteral [] go_iface (map_go_stmt stmts ++ [GoReturn (emit_go_expr sigs ret)])) [])
  | AstApp func args =>
      let direct := go_app_direct func in
      let fix map_go_expr (ls : list AST) : list GoExpr :=
        match ls with
        | [] => []
        | x :: xs => emit_go_expr sigs x :: map_go_expr xs
        end
      in
      let raw_args := map_go_expr args in
      GoExprStmt (go_call_emitted direct (emit_go_expr sigs func) (go_call_args sigs func raw_args))
  | AstImplicitApp func _args => GoExprStmt (emit_go_expr sigs func)  (* type args erased *)
  | AstFunTy _tparams _params _ret_ty _effs => GoExprStmt (GoIdentifier "interface{}")
  | AstLam argName argTy body => GoExprStmt (GoFuncLiteral [(argName, go_iface)] go_iface (emit_go_block sigs body))
  | AstPi argName argTy retTy effs => GoExprStmt (GoIdentifier "interface{}")
  | AstDo op args =>
      let op_name := match op with AstRef n => n | _ => "unknown" end in
      let fix map_go_expr (ls : list AST) : list GoExpr :=
        match ls with
        | [] => []
        | x :: xs => emit_go_expr sigs x :: map_go_expr xs
        end
      in
      GoExprStmt (GoCall (GoIdentifier "__chester_perform")
        [GoStringLiteral op_name; GoArray (map_go_expr args)])
  | AstHandle action eff handlers =>
      let fix emit_hs (hs : list (string * AST)) : list (string * GoExpr) :=
        match hs with
        | [] => []
        | (op, fn) :: rest => (op, emit_go_expr sigs fn) :: emit_hs rest
        end
      in
      GoExprStmt (GoCall (GoIdentifier "__chester_handle")
        [GoStringLiteral (effect_label eff);
         GoFuncLiteral [] go_iface (emit_go_block sigs action);
         GoMapLiteral (emit_hs handlers)])
  | AstBoolLit b => GoExprStmt (GoBoolLiteral b)
  | AstVar name value => GoLet name go_iface (emit_go_expr sigs value)
  | AstAssign name value => GoAssign name (emit_go_expr sigs value)
  | AstBox e caps =>
      GoExprStmt (GoCall (GoIdentifier "__chester_box")
        [GoArray (effect_label_go_lits caps);
         GoFuncLiteral [] go_iface [GoReturn (emit_go_expr sigs e)]])
  | AstUnbox e => GoExprStmt (go_call_emitted false (emit_go_expr sigs e) [])
  | AstIf cond true_br false_br => GoExprStmt (GoCall (GoFuncLiteral [] go_iface [GoIfStmt (go_bool_cond (emit_go_expr sigs cond)) (emit_go_block sigs true_br) (emit_go_block sigs false_br)]) [])
  | AstMatch expr cases => 
      let fix emit_cases (cs : list (PatternAST * AST)) : list GoStmt :=
        match cs with
        | [] => [GoPanic "Non-exhaustive match"]
        | (pat, body) :: rest =>
            match pat with
            | PatConstructor cname vars =>
                let cond := GoCall (GoIdentifier "_match_val.(map[string]interface{})[""_tag""] ==") [GoStringLiteral cname] in
                let body_go := emit_go_block sigs body in
                let fix bind_vars (vs : list string) (idx : nat) (acc : list GoStmt) : list GoStmt :=
                  match vs with
                  | [] => acc
                  | v :: vs' => bind_vars vs' (S idx) (GoLet v go_iface (GoIndex (GoTypeAssert (GoIndex (GoTypeAssert (GoIdentifier "_match_val") "map[string]interface{}") (GoStringLiteral "args")) "[]interface{}") (GoIntLiteral (nat_to_string idx))) :: acc)
                  end
                in
                [GoIfStmt cond (bind_vars vars 0 [] ++ body_go) (emit_cases rest)]
            | PatWildcard => [GoIfStmt (GoBoolLiteral true) (emit_go_block sigs body) (emit_cases rest)]
            | PatVar v => GoLet v go_iface (GoIdentifier "_match_val") :: emit_go_block sigs body
            end
        end
      in GoExprStmt (GoCall (GoFuncLiteral [] go_iface (GoLet "_match_val" go_iface (emit_go_expr sigs expr) :: emit_cases cases)) [])
  | AstFieldAccess expr field => GoExprStmt (      match expr with
      | AstRef n => if is_upper n then GoIdentifier field else GoSelector (emit_go_expr sigs expr) field
      | _ => GoSelector (emit_go_expr sigs expr) field
      end)
  | AstMeta id => GoExprStmt (GoIdentifier ("/* ?meta_" ++ nat_to_string id ++ " */"))
  | AstUniverse _ => GoExprStmt (GoCall (GoFuncLiteral [] go_iface [GoPanic "Universe in term"]) [])
  | AstError e => GoExprStmt (GoCall (GoFuncLiteral [] go_iface [GoPanic e]) [])
  | AstSpan _ inner => emit_go_stmt sigs inner
  end

with emit_go_block (sigs : GoSigEnv) (ast : AST) {struct ast} : list GoStmt :=
  match ast with
  | AstBlock stmts ret =>
      let fix to_local (s : GoStmt) : GoStmt :=
        match s with
        | GoFuncDecl n p r b => GoLocalFuncDecl n p r b
        | _ => s
        end
      in
      let fix map_go_stmt (ls : list AST) : list GoStmt :=
        match ls with
        | [] => []
        | AstRef "Unit" :: xs => map_go_stmt xs
        | x :: xs => to_local (emit_go_stmt sigs x) :: map_go_stmt xs
        end
      in map_go_stmt stmts ++ [GoReturn (emit_go_expr sigs ret)]
  | AstIf cond true_br false_br => [GoIfStmt (go_bool_cond (emit_go_expr sigs cond)) (emit_go_block sigs true_br) (emit_go_block sigs false_br)]
  | AstMatch expr cases => 
      let fix emit_cases (cs : list (PatternAST * AST)) : list GoStmt :=
        match cs with
        | [] => [GoPanic "Non-exhaustive match"]
        | (pat, body) :: rest =>
            match pat with
            | PatConstructor cname vars =>
                let cond := GoCall (GoIdentifier "_match_val.(map[string]interface{})[""_tag""] ==") [GoStringLiteral cname] in
                let body_go := emit_go_block sigs body in
                let fix bind_vars (vs : list string) (idx : nat) (acc : list GoStmt) : list GoStmt :=
                  match vs with
                  | [] => acc
                  | v :: vs' => bind_vars vs' (S idx) (GoLet v go_iface (GoIndex (GoTypeAssert (GoIndex (GoTypeAssert (GoIdentifier "_match_val") "map[string]interface{}") (GoStringLiteral "args")) "[]interface{}") (GoIntLiteral (nat_to_string idx))) :: acc)
                  end
                in
                [GoIfStmt cond (bind_vars vars 0 [] ++ body_go) (emit_cases rest)]
            | PatWildcard => [GoIfStmt (GoBoolLiteral true) (emit_go_block sigs body) (emit_cases rest)]
            | PatVar v => GoLet v go_iface (GoIdentifier "_match_val") :: emit_go_block sigs body
            end
        end
      in GoLet "_match_val" go_iface (emit_go_expr sigs expr) :: emit_cases cases
  | AstUniverse _ => [GoPanic "Universe in term"]
  | AstError e => [GoPanic e]
  | AstSpan _ inner => emit_go_block sigs inner
  | AstRef name => [GoReturn (GoIdentifier name)]
  | AstTuple elems => 
      let fix map_go_expr (ls : list AST) : list GoExpr :=
        match ls with
        | [] => []
        | x :: xs => emit_go_expr sigs x :: map_go_expr xs
        end
      in [GoReturn (GoArray (map_go_expr elems))]
  | AstStringLit s => [GoReturn (GoStringLiteral s)]
  | AstIntLit n => [GoReturn (GoIntLiteral (nat_to_string n))]
  | AstApp func args =>
      let direct := go_app_direct func in
      let fix map_go_expr (ls : list AST) : list GoExpr :=
        match ls with
        | [] => []
        | x :: xs => emit_go_expr sigs x :: map_go_expr xs
        end
      in
      let raw_args := map_go_expr args in
      [GoReturn (go_call_emitted direct (emit_go_expr sigs func) (go_call_args sigs func raw_args))]
  | AstImplicitApp func _args => [GoReturn (emit_go_expr sigs func)]  (* type args erased *)
  | AstFunTy _tparams _params _ret_ty _effs => [GoReturn (GoIdentifier "interface{}")]
  | AstLam argName argTy body => [GoReturn (GoFuncLiteral [(argName, go_iface)] go_iface (emit_go_block sigs body))]
  | AstPi argName argTy retTy effs => [GoReturn (GoIdentifier "interface{}")]
  | AstDo op args =>
      let op_name := match op with AstRef n => n | _ => "unknown" end in
      let fix map_go_expr (ls : list AST) : list GoExpr :=
        match ls with
        | [] => []
        | x :: xs => emit_go_expr sigs x :: map_go_expr xs
        end
      in
      [GoReturn (GoCall (GoIdentifier "__chester_perform")
        [GoStringLiteral op_name; GoArray (map_go_expr args)])]
  | AstHandle action eff handlers =>
      let fix emit_hs (hs : list (string * AST)) : list (string * GoExpr) :=
        match hs with
        | [] => []
        | (op, fn) :: rest => (op, emit_go_expr sigs fn) :: emit_hs rest
        end
      in
      [GoReturn (GoCall (GoIdentifier "__chester_handle")
        [GoStringLiteral (effect_label eff);
         GoFuncLiteral [] go_iface (emit_go_block sigs action);
         GoMapLiteral (emit_hs handlers)])]
  | AstBoolLit b => [GoReturn (GoBoolLiteral b)]
  | AstLet name value =>
      [GoReturn (GoCall (GoFuncLiteral [] go_iface [GoLet name (go_type_of_ast_value sigs value) (emit_go_expr sigs value); GoDiscardBinding name]) [])]
  | AstVar name value => [GoReturn (GoCall (GoFuncLiteral [] go_iface [GoLet name go_iface (emit_go_expr sigs value)]) [])]
  | AstAssign name value => [GoAssign name (emit_go_expr sigs value)]
  | AstBox e caps =>
      [GoReturn (GoCall (GoIdentifier "__chester_box")
        [GoArray (effect_label_go_lits caps);
         GoFuncLiteral [] go_iface [GoReturn (emit_go_expr sigs e)]])]
  | AstUnbox e => [GoReturn (go_call_emitted false (emit_go_expr sigs e) [])]
  | AstDef name _ params ret_ty body => [GoReturn (GoCall (GoFuncLiteral [] go_iface [GoFuncDecl name (go_params_of params) (chester_to_go_type ret_ty) (go_map_returns_top sigs (chester_to_go_type ret_ty) (emit_go_block sigs body))]) [])]
  | AstEnum _ _ _ => [GoReturn (GoIdentifier "nil")]
  | AstExtension _ _ _ _ => [GoReturn (GoIdentifier "nil")]
  | AstRecord name _ _ => [GoReturn (GoIdentifier "nil")]
  | AstFieldAccess expr field => [GoReturn (      match expr with
      | AstRef n => if is_upper n then GoIdentifier field else GoSelector (emit_go_expr sigs expr) field
      | _ => GoSelector (emit_go_expr sigs expr) field
      end)]
  | AstImport lang _ mod_path _ => [emit_go_import lang mod_path]
  | AstMeta id => [GoReturn (GoIdentifier ("/* ?meta_" ++ nat_to_string id ++ " */"))]
  end.

Definition emit_go (ast : AST) : GoStmt :=
  match ast with
  | AstBlock stmts ret =>
      let sigs := collect_go_sigs_top stmts in
      let fix to_local (s : GoStmt) : GoStmt :=
        match s with
        | GoFuncDecl n p r b => GoLocalFuncDecl n p r b
        | _ => s
        end
      in
      let fix map_go_stmt (ls : list AST) : list GoStmt :=
        match ls with
        | [] => []
        | AstRef "Unit" :: xs => map_go_stmt xs
        | x :: xs => to_local (emit_go_stmt sigs x) :: map_go_stmt xs
        end
      in GoExprStmt (GoCall (GoFuncLiteral [] go_iface (map_go_stmt stmts ++ [GoExprStmt (emit_go_expr sigs ret)])) [])
  | _ => emit_go_stmt [] ast
  end.

(* Top-level Go emit: keep declarations at package scope (no wrapping IIFE). *)
Definition emit_go_top (ast : AST) : GoStmt :=
  match ast with
  | AstBlock stmts ret =>
      let sigs := collect_go_sigs_top stmts in
      let fix map_go_stmt (ls : list AST) : list GoStmt :=
        match ls with
        | [] => []
        | AstRef "Unit" :: xs => map_go_stmt xs
        | x :: xs => emit_go_stmt sigs x :: map_go_stmt xs
        end
      in
      match ret with
      | AstRef "Unit" => GoBlock (map_go_stmt stmts)
      | _ => GoBlock (map_go_stmt stmts ++ [GoExprStmt (emit_go_expr sigs ret)])
      end
  | _ => emit_go_stmt [] ast
  end.

