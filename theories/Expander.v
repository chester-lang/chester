From Stdlib Require Import Strings.String.
From Stdlib Require Import List.
Import ListNotations.
Open Scope string_scope.
Require Import Chester.CST.

Fixpoint split_at_eq (acc : list CST) (ls : list CST) : option (list CST * list CST) :=
  match ls with
  | [] => None
  | Symbol "=" _ :: rest => Some (rev acc, rest)
  | x :: rest => split_at_eq (x :: acc) rest
  end.

Fixpoint collapse_apps_aux (elems : list CST) (acc : list CST) : list CST :=
  match elems with
  | [] => rev acc
  | Symbol "." _ :: Symbol field span :: rest =>
      match acc with
      | expr :: acc_rest => collapse_apps_aux rest (FieldAccessCST expr field span :: acc_rest)
      | [] => collapse_apps_aux rest (Symbol field span :: Symbol "." empty_span :: [])
      end
  | Tuple args span :: rest =>
      match acc with
      | func :: acc_rest => collapse_apps_aux rest (AppCST func args span :: acc_rest)
      | [] => collapse_apps_aux rest (Tuple args span :: [])
      end
  | ListLiteral targs tspan :: rest =>
      match acc with
      | Symbol name sp :: acc_rest =>
          (* `[]` is implicit app for callables; keep list literals after binders/punct. *)
          if orb (eqb name "=") (orb (eqb name ":") (orb (eqb name "return")
                (orb (eqb name "=>") (orb (eqb name ",") (eqb name ";"))))) then
            collapse_apps_aux rest (ListLiteral targs tspan :: Symbol name sp :: acc_rest)
          else
            collapse_apps_aux rest (ImplicitAppCST (Symbol name sp) targs tspan :: acc_rest)
      | expr :: acc_rest => collapse_apps_aux rest (ImplicitAppCST expr targs tspan :: acc_rest)
      | [] => collapse_apps_aux rest (ListLiteral targs tspan :: [])
      end
  | x :: rest => collapse_apps_aux rest (x :: acc)
  end.
Definition collapse_apps elems := collapse_apps_aux elems [].

Definition op_to_fn (op : string) : string :=
  if string_dec op "+" then "int_add"
  else if string_dec op "-" then "int_sub"
  else if string_dec op "*" then "int_mul"
  else if string_dec op "/" then "int_div"
  else if string_dec op "%" then "int_mod"
  else if string_dec op "<" then "int_lt"
  else if string_dec op ">" then "int_gt"
  else if string_dec op "<=" then "int_le"
  else if string_dec op ">=" then "int_ge"
  else op.

Definition op_prec (op : string) : option nat :=
  if string_dec op "+" then Some 10
  else if string_dec op "-" then Some 10
  else if string_dec op "*" then Some 20
  else if string_dec op "/" then Some 20
  else if string_dec op "%" then Some 20
  else if string_dec op "<" then Some 5
  else if string_dec op ">" then Some 5
  else if string_dec op "<=" then Some 5
  else if string_dec op ">=" then Some 5
  else None.

Definition OpEnv := list (string * nat).

Fixpoint lookup_op_prec_env (op : string) (env : OpEnv) : option nat :=
  match env with
  | [] => op_prec op
  | (o, p) :: rest =>
      if string_dec o op then Some p else lookup_op_prec_env op rest
  end.

Fixpoint upsert_op (op : string) (prec : nat) (env : OpEnv) : OpEnv :=
  (op, prec) ::
    filter (fun e => if string_dec (fst e) op then false else true) env.

Definition default_op_prec : nat := 10.

Definition parse_infix_rel (rest : list CST) : option (string * string * string) :=
  match rest with
  | Symbol op _ :: Symbol rel _ :: Symbol ref_op _ :: _ =>
      Some (op, rel, ref_op)
  | Symbol op _ :: _ => Some (op, "default", "")
  | _ => None
  end.

Definition infix_prec_from_rel (rel ref_op : string) (op_env : OpEnv) : nat :=
  if string_dec rel "same_as" then
    match lookup_op_prec_env ref_op op_env with Some p => p | None => default_op_prec end
  else if string_dec rel "tighter_than" then
    match lookup_op_prec_env ref_op op_env with Some p => S p | None => default_op_prec end
  else default_op_prec.

Definition parse_primary (elems : list CST) : option (CST * list CST) :=
  match elems with
  | e :: rest => Some (e, rest)
  | [] => None
  end.

Fixpoint parse_infix_loop (fuel : nat) (op_env : OpEnv) (lhs : CST) (elems : list CST) (min_prec : nat)
  (span : Span) : option (CST * list CST) :=
  match fuel with
  | 0 => Some (lhs, elems)
  | S fuel' =>
      match elems with
      | Symbol op sp :: rest =>
          match lookup_op_prec_env op op_env with
          | Some prec =>
              if Nat.leb prec min_prec then Some (lhs, elems)
              else
                let rhs_min := Nat.add prec 1 in
                match parse_infix_rhs fuel' op_env rhs_min rest span with
                | Some (rhs, rest2) =>
                    let new_lhs :=
                      AppCST (Symbol (op_to_fn op) sp) [lhs; rhs] span
                    in
                    parse_infix_loop fuel' op_env new_lhs rest2 min_prec span
                | None => Some (lhs, elems)
                end
          | None => Some (lhs, elems)
          end
      | _ => Some (lhs, elems)
      end
  end

with parse_infix_rhs (fuel : nat) (op_env : OpEnv) (min_prec : nat) (elems : list CST) (span : Span)
  : option (CST * list CST) :=
  match fuel with
  | 0 => None
  | S fuel' =>
      match parse_primary elems with
      | Some (atom, rest) => parse_infix_loop fuel' op_env atom rest min_prec span
      | None => None
      end
  end.

Definition infix_fuel (elems : list CST) : nat := Nat.add (length elems) 1.

Definition parse_infix_chain (op_env : OpEnv) (elems : list CST) (span : Span) : option CST :=
  match parse_infix_rhs (infix_fuel elems) op_env 0 elems span with
  | Some (cst, _) => Some cst
  | None => None
  end.

Definition has_infix_op (op_env : OpEnv) (elems : list CST) : bool :=
  existsb
    (fun c =>
       match c with
       | Symbol op _ => match lookup_op_prec_env op op_env with Some _ => true | None => false end
       | _ => false
       end)
    elems.

Definition try_parse_infix (op_env : OpEnv) (elems : list CST) (span : Span) : option CST :=
  if has_infix_op op_env elems then
    match parse_infix_chain op_env elems span with
    | Some cst => Some cst
    | None => parse_infix_chain op_env (collapse_apps elems) span
    end
  else None.

Fixpoint extract_import_syms_from_block (stmts : list CST) : list string :=
  match stmts with
  | [] => []
  | Symbol n _ :: xs => n :: extract_import_syms_from_block xs
  | _ :: xs => extract_import_syms_from_block xs
  end.

Definition extract_import_syms (rest : list CST) : list string :=
  match rest with
  | [] => []
  | [Block syms _ _] => extract_import_syms_from_block syms
  | Block syms _ _ :: _ => extract_import_syms_from_block syms
  | _ => []
  end.

Fixpoint expand_if (elems : list CST) (span : Span) : option CST :=
  match elems with
  | Symbol "if" _ :: cond :: Symbol "then" _ :: thenB :: Symbol "else" _ :: rest =>
      match expand_if rest span with
      | Some else_if_cst => Some (IfCST cond thenB else_if_cst span)
      | None =>
          match rest with
          | [elseB] => Some (IfCST cond thenB elseB span)
          | _ => None
          end
      end
  | Symbol "if" _ :: cond :: Symbol "then" _ :: thenB :: [] =>
      Some (IfCST cond thenB (Tuple [] span) span)
  | _ => None
  end.

Fixpoint expand_seq_expr (op_env : OpEnv) (elems : list CST) (span : Span) : CST :=
  match try_parse_infix op_env elems span with
  | Some infix_cst => infix_cst
  | None =>
  let collapsed := collapse_apps elems in
  match expand_if collapsed span with
  | Some if_cst => if_cst
  | None =>
      match collapsed with
      | [] => Tuple [] span
      | [x] => x
      | Symbol "\" _ :: Symbol arg _ :: Symbol "=>" _ :: body =>
          let body_cst := match body with | [] => Tuple [] span | [x] => x | _ => SeqOf body span end in
          LamCST arg None body_cst span
      | Symbol "match" _ :: expr :: Block cases _ _ :: [] =>
          (* Extract case branches into PatternCST * CST pairs.
             Bodies are left as SeqOf so expand_cst can recurse into them later.
             We CANNOT call expand_seq_expr env here (same fixpoint) — Coq forbids it. *)
          let fix extract_vars (args : list CST) : list string :=
            match args with
            | [] => []
            | Symbol vname _ :: rest => vname :: extract_vars rest
            | _ :: rest => extract_vars rest
            end
          in
          let fix extract_cases (cs : list CST) : list (PatternCST * CST) :=
            match cs with
            | [] => []
            (* case _ => body *)
            | SeqOf (Symbol "case" _ :: Symbol "_" _ :: Symbol "=>" _ :: body) sp :: rest =>
                let body_cst := match body with
                  | [] => Tuple [] sp
                  | [b] => b
                  | _ => SeqOf body sp
                  end in
                (PatWildcardCST sp, body_cst) :: extract_cases rest
            (* case Constructor(vars...) => body  -- field-access form: Ctor.Variant(vars) *)
            | SeqOf (Symbol "case" _ ::
                     FieldAccessCST (Symbol _ _) vname _ ::
                     Tuple args _ ::
                     Symbol "=>" _ :: body) sp :: rest =>
                let body_cst := match body with
                  | [] => Tuple [] sp
                  | [b] => b
                  | _ => SeqOf body sp
                  end in
                (PatConstructorCST vname (extract_vars args) sp, body_cst) :: extract_cases rest
            (* case Constructor(vars...) => body *)
            | SeqOf (Symbol "case" _ ::
                     AppCST (Symbol vname _) args _ ::
                     Symbol "=>" _ :: body) sp :: rest =>
                let body_cst := match body with
                  | [] => Tuple [] sp
                  | [b] => b
                  | _ => SeqOf body sp
                  end in
                (PatConstructorCST vname (extract_vars args) sp, body_cst) :: extract_cases rest
            (* case VarName => body  (bare name: could be wildcard-like binding) *)
            | SeqOf (Symbol "case" _ :: Symbol vname _ :: Symbol "=>" _ :: body) sp :: rest =>
                let body_cst := match body with
                  | [] => Tuple [] sp
                  | [b] => b
                  | _ => SeqOf body sp
                  end in
                (* "_" is wildcard; anything else is a variable binding *)
                let pat := if string_dec vname "_"
                           then PatWildcardCST sp
                           else PatVarCST vname sp in
                (pat, body_cst) :: extract_cases rest
            | _ :: rest => extract_cases rest
            end
          in
          MatchCST expr (extract_cases cases) span
      | Symbol "perform" _ :: AppCST op args _ :: [] => DoCST op args span
      | Symbol "box" _ :: rest =>
          match rest with
          | [e] => BoxCST e span
          | _ => BoxCST (SeqOf rest span) span
          end
      | Symbol "unbox" _ :: rest =>
          match rest with
          | [e] => UnboxCST e span
          | _ => UnboxCST (SeqOf rest span) span
          end
      | Symbol "handle" _ :: Block body_stmts body_tail body_sp :: Symbol "with" _ :: Symbol eff _ :: Block h_stmts h_tail h_sp :: [] =>
          let methods_of_block (b_stmts : list CST) (b_tail : CST) : list CST :=
            match b_tail with
            | Symbol u _ => if eqb u "Unit" then b_stmts else app b_stmts [b_tail]
            | _ => app b_stmts [b_tail]
            end
          in
          HandleCST (Block body_stmts body_tail body_sp) eff
            (methods_of_block h_stmts h_tail) span
      | _ => SeqOf collapsed span
      end
  end
  end.

Fixpoint expand_cst (fuel: nat) (op_env : OpEnv) (c : CST) {struct fuel} : (CST * OpEnv) :=
  match fuel with
  | 0 => (c, op_env)
  | S fuel' =>
  match c with
  | Block stmts tail span =>
      let fix map_expand (env : OpEnv) (cs : list CST) : (list CST * OpEnv) :=
        match cs with
        | [] => ([], env)
        | x :: rest =>
            let (x', env') := expand_cst fuel' env x in
            let (rest', env'') := map_expand env' rest in
            (x' :: rest', env'')
        end
      in
      let (expanded_stmts, env1) := map_expand op_env stmts in
      let (expanded_tail, env2) := expand_cst fuel' env1 tail in
      
      let fix process_stmts (f2 : nat) (env : OpEnv) (ss : list CST) {struct f2} : (list CST * OpEnv) :=
        match f2 with
        | 0 => (ss, env)
        | S f2' =>
        match ss with
        | [] => ([], env)
        | stmt :: rest =>
            let infix_decl :=
              match stmt with
              | SeqOf (Symbol kwd _ :: rest_seq) _ =>
                  if orb (eqb kwd "infixl") (eqb kwd "infixr") then Some rest_seq else None
              | _ => None
              end
            in
            match infix_decl with
            | Some rest_seq =>
                match parse_infix_rel rest_seq with
                | Some (op, rel, ref_op) =>
                    let prec := infix_prec_from_rel rel ref_op env in
                    process_stmts f2' (upsert_op op prec env) rest
                | None => process_stmts f2' env rest
                end
            | None =>
            let processed_stmt := match stmt with
            | SeqOf (Symbol kwd _ :: rest_seq) s =>
                if eqb kwd "let" then
                    match rest_seq with
                    | Symbol name _ :: Symbol kwd2 _ :: val_exprs =>
                        if eqb kwd2 "=" then
                            let val_cst := match val_exprs with [v] => v | _ => expand_seq_expr env val_exprs s end in
                            LetCST name val_cst (Symbol "Unit" empty_span) s
                        else stmt
                    | _ => stmt
                    end
                else if eqb kwd "var" then
                    match rest_seq with
                    | Symbol name _ :: Symbol kwd2 _ :: val_exprs =>
                        if eqb kwd2 "=" then
                            let val_cst := match val_exprs with [v] => v | _ => expand_seq_expr env val_exprs s end in
                            VarCST name val_cst (Symbol "Unit" empty_span) s
                        else stmt
                    | _ => stmt
                    end
                else if eqb kwd "box" then
                    match rest_seq with
                    | [e1] => BoxCST e1 s
                    | _ =>
                        match rest_seq with
                        | [] => stmt
                        | _ => BoxCST (expand_seq_expr env rest_seq s) s
                        end
                    end
                else if eqb kwd "import" then
                    match rest_seq with
                    | Symbol lang _ :: Symbol alias _ :: StringLiteral mod _ :: rest =>
                        ImportCST lang alias mod (extract_import_syms rest) s
                    | Symbol lang _ :: StringLiteral mod _ :: rest =>
                        ImportCST lang lang mod (extract_import_syms rest) s
                    | _ => stmt
                    end
                else if eqb kwd "extern" then
                    match rest_seq with
                    | Symbol lang _ :: StringLiteral mod _ :: Block decls _ _ :: [] =>
                        ExternCST lang mod decls s
                    | _ => stmt
                    end
                else if eqb kwd "unbox" then
                    match rest_seq with
                    | [e1] => UnboxCST e1 s
                    | _ =>
                        match rest_seq with
                        | [] => stmt
                        | _ => UnboxCST (expand_seq_expr env rest_seq s) s
                        end
                    end
                else if eqb kwd "def" then
                    match rest_seq with
                    | AppCST (Symbol name _) args _ :: rest_def =>
                        match split_at_eq [] rest_def with
                        | Some (ty_exprs, body_exprs) =>
                            let ret_ty := match ty_exprs with 
                                          | Symbol kwd2 _ :: tys => 
                                              if eqb kwd2 ":" then expand_seq_expr env tys s else Symbol "Unknown" empty_span
                                          | _ => Symbol "Unknown" empty_span 
                                          end in
                            let body_cst := match body_exprs with [b] => b | _ => expand_seq_expr env body_exprs s end in
                            let extract_arg (a: CST) : (string * CST) := 
                               match a with
                               | Symbol n _ => (n, Symbol "Unknown" empty_span)
                               | SeqOf (Symbol n _ :: Symbol kwd2 _ :: ty :: _) _ => 
                                   if eqb kwd2 ":" then (n, ty) else ("unknown", Symbol "Unknown" empty_span)
                               | _ => ("unknown", Symbol "Unknown" empty_span)
                               end
                            in
                            let params := map extract_arg args in
                            DefCST name [] params ret_ty body_cst s
                        | None => SeqOf (Symbol "def" empty_span :: Symbol name empty_span :: ListLiteral [] empty_span :: Tuple args empty_span :: rest_def) s
                        end
                    | AppCST (ImplicitAppCST (Symbol name _) targs _) args _ :: rest_def =>
                        match split_at_eq [] rest_def with
                        | Some (ty_exprs, body_exprs) =>
                            let ret_ty := match ty_exprs with 
                                          | Symbol kwd2 _ :: tys => 
                                              if eqb kwd2 ":" then expand_seq_expr env tys s else Symbol "Unknown" empty_span
                                          | _ => Symbol "Unknown" empty_span 
                                          end in
                            let body_cst := match body_exprs with [b] => b | _ => expand_seq_expr env body_exprs s end in
                            let extract_arg (a: CST) : (string * CST) := 
                               match a with
                               | Symbol n _ => (n, Symbol "Unknown" empty_span)
                               | SeqOf (Symbol n _ :: Symbol kwd2 _ :: ty :: _) _ => 
                                   if eqb kwd2 ":" then (n, ty) else ("unknown", Symbol "Unknown" empty_span)
                               | _ => ("unknown", Symbol "Unknown" empty_span)
                               end
                            in
                            let extract_targ (a: CST) : string := 
                               match a with Symbol n _ => n | _ => "T" end 
                            in
                            let type_params := map extract_targ targs in
                            let params := map extract_arg args in
                            DefCST name type_params params ret_ty body_cst s
                        | None => SeqOf (Symbol "def" empty_span :: Symbol name empty_span :: ListLiteral targs empty_span :: Tuple args empty_span :: rest_def) s
                        end
                    | _ => stmt
                    end
                else if eqb kwd "extension" then
                    let methods_of_block (b : CST) : list CST :=
                      match b with
                      | Block meths tail _ =>
                          match tail with
                          | Symbol u _ =>
                              if eqb u "Unit" then meths else app meths [tail]
                          | _ => app meths [tail]
                          end
                      | _ => []
                      end in
                    match rest_seq with
                    | [e1; e2; e3; e4] =>
                        match e1 with
                        | Symbol name _ =>
                            match e2 with
                            | Symbol kwd2 _ =>
                                if orb (eqb kwd2 "for") (eqb kwd2 "on") then
                                  match e4 with
                                  | Block _ _ _ =>
                                      ExtensionCST name [] e3 (fst (process_stmts f2' env (methods_of_block e4))) s
                                  | _ => stmt
                                  end
                                else stmt
                            | _ => stmt
                            end
                        | ImplicitAppCST (Symbol name _) targs _ =>
                            match e2 with
                            | Symbol kwd2 _ =>
                                if orb (eqb kwd2 "for") (eqb kwd2 "on") then
                                  match e4 with
                                  | Block _ _ _ =>
                                      let fix extract_vars (vs : list CST) : list string :=
                                        match vs with
                                        | [] => []
                                        | Symbol v _ :: vrest => v :: extract_vars vrest
                                        | _ :: vrest => extract_vars vrest
                                        end in
                                      ExtensionCST name (extract_vars targs) e3 (fst (process_stmts f2' env (methods_of_block e4))) s
                                  | _ => stmt
                                  end
                                else stmt
                            | _ => stmt
                            end
                        | _ => stmt
                        end
                    | [e1; e2; e3; e4; e5] =>
                        match e1 with
                        | Symbol name _ =>
                            match e2 with
                            | ListLiteral targs _ =>
                                match e3 with
                                | Symbol kwd2 _ =>
                                    if orb (eqb kwd2 "for") (eqb kwd2 "on") then
                                      match e5 with
                                      | Block _ _ _ =>
                                          let fix extract_vars (vs : list CST) : list string :=
                                            match vs with
                                            | [] => []
                                            | Symbol v _ :: vrest => v :: extract_vars vrest
                                            | _ :: vrest => extract_vars vrest
                                            end in
                                          ExtensionCST name (extract_vars targs) e4 (fst (process_stmts f2' env (methods_of_block e5))) s
                                      | _ => stmt
                                      end
                                    else stmt
                                | _ => stmt
                                end
                            | _ => stmt
                            end
                        | _ => stmt
                        end
                    | _ => stmt
                    end
                else if eqb kwd "enum" then
                    match rest_seq with
                    | [e1; e2] =>
                        match e1 with
                        | Symbol name _ =>
                            match e2 with
                            | Block variants _ _ =>
                                let fix extract_variants (vs : list CST) : list CST :=
                                  match vs with
                                  | [] => []
                                  | SeqOf (Symbol kwdcase _ :: rest_case) _ :: vrest =>
                                      if eqb kwdcase "case" then
                                          match rest_case with
                                          | [r1; r2; r3] =>
                                              match r1 with
                                              | AppCST (Symbol vname _) vargs _ =>
                                                  match r2 with
                                                  | Symbol kwdcolon _ =>
                                                      if eqb kwdcolon ":" then
                                                          AppCST (Symbol ":" empty_span) [AppCST (Symbol vname empty_span) vargs empty_span; r3] empty_span :: extract_variants vrest
                                                      else extract_variants vrest
                                                  | _ => extract_variants vrest
                                                  end
                                              | Symbol vname _ =>
                                                  match r2 with
                                                  | Symbol kwdcolon _ =>
                                                      if eqb kwdcolon ":" then
                                                          AppCST (Symbol ":" empty_span) [Symbol vname empty_span; r3] empty_span :: extract_variants vrest
                                                      else extract_variants vrest
                                                  | _ => extract_variants vrest
                                                  end
                                              | _ => extract_variants vrest
                                              end
                                          | [r1] =>
                                              match r1 with
                                              | AppCST (Symbol vname _) vargs _ =>
                                                  AppCST (Symbol vname empty_span) vargs empty_span :: extract_variants vrest
                                              | Symbol vname _ =>
                                                  Symbol vname empty_span :: extract_variants vrest
                                              | _ => extract_variants vrest
                                              end
                                          | _ => extract_variants vrest
                                          end
                                      else extract_variants vrest
                                  | _ :: vrest => extract_variants vrest
                                  end
                                in
                                EnumCST name [] (extract_variants variants) s
                            | _ => stmt
                            end
                        | ImplicitAppCST (Symbol name _) targs _ =>
                            match e2 with
                            | Block variants _ _ =>
                                let fix extract_variants (vs : list CST) : list CST :=
                                  match vs with
                                  | [] => []
                                  | SeqOf (Symbol kwdcase _ :: rest_case) _ :: vrest =>
                                      if eqb kwdcase "case" then
                                          match rest_case with
                                          | [r1; r2; r3] =>
                                              match r1 with
                                              | AppCST (Symbol vname _) vargs _ =>
                                                  match r2 with
                                                  | Symbol kwdcolon _ =>
                                                      if eqb kwdcolon ":" then
                                                          AppCST (Symbol ":" empty_span) [AppCST (Symbol vname empty_span) vargs empty_span; r3] empty_span :: extract_variants vrest
                                                      else extract_variants vrest
                                                  | _ => extract_variants vrest
                                                  end
                                              | Symbol vname _ =>
                                                  match r2 with
                                                  | Symbol kwdcolon _ =>
                                                      if eqb kwdcolon ":" then
                                                          AppCST (Symbol ":" empty_span) [Symbol vname empty_span; r3] empty_span :: extract_variants vrest
                                                      else extract_variants vrest
                                                  | _ => extract_variants vrest
                                                  end
                                              | _ => extract_variants vrest
                                              end
                                          | [r1] =>
                                              match r1 with
                                              | AppCST (Symbol vname _) vargs _ =>
                                                  AppCST (Symbol vname empty_span) vargs empty_span :: extract_variants vrest
                                              | Symbol vname _ =>
                                                  Symbol vname empty_span :: extract_variants vrest
                                              | _ => extract_variants vrest
                                              end
                                          | _ => extract_variants vrest
                                          end
                                      else extract_variants vrest
                                  | _ :: vrest => extract_variants vrest
                                  end
                                in
                                let extract_targ (a: CST) : string := 
                                   match a with Symbol n _ => n | _ => "T" end 
                                in
                                let type_params := map extract_targ targs in
                                EnumCST name type_params (extract_variants variants) s
                            | _ => stmt
                            end
                        | _ => stmt
                        end
                    | _ => stmt
                    end
                else if eqb kwd "effect" then
                    let methods_of_block (b : CST) : list CST :=
                      match b with
                      | Block meths tail _ =>
                          match tail with
                          | Symbol u _ =>
                              if eqb u "Unit" then meths else app meths [tail]
                          | _ => app meths [tail]
                          end
                      | _ => []
                      end in
                    let extract_tparams (ts : list CST) : list string :=
                      let fix go (vs : list CST) : list string :=
                        match vs with
                        | [] => []
                        | Symbol v _ :: rest => v :: go rest
                        | _ :: rest => go rest
                        end
                      in go ts in
                    match rest_seq with
                    | [e1; e2] =>
                        match e1 with
                        | Symbol name _ =>
                            match e2 with
                            | Block _ _ _ =>
                                EffectCST name [] (fst (process_stmts f2' env (methods_of_block e2))) s
                            | _ => stmt
                            end
                        | ImplicitAppCST (Symbol name _) targs _ =>
                            match e2 with
                            | Block _ _ _ =>
                                EffectCST name (extract_tparams targs) (fst (process_stmts f2' env (methods_of_block e2))) s
                            | _ => stmt
                            end
                        | _ => stmt
                        end
                    | [e1; e2; e3] =>
                        match e1 with
                        | Symbol name _ =>
                            match e2 with
                            | ListLiteral targs _ =>
                                match e3 with
                                | Block _ _ _ =>
                                    EffectCST name (extract_tparams targs) (fst (process_stmts f2' env (methods_of_block e3))) s
                                | _ => stmt
                                end
                            | _ => stmt
                            end
                        | _ => stmt
                        end
                    | _ => stmt
                    end
                else if eqb kwd "perform" then
                    match rest_seq with
                    | [e1] =>
                        match e1 with
                        | AppCST op args _ => DoCST op args s
                        | _ => stmt
                        end
                    | _ => stmt
                    end
                else if eqb kwd "handle" then
                    let methods_of_block (b : CST) : list CST :=
                      match b with
                      | Block meths tail _ =>
                          match tail with
                          | Symbol u _ =>
                              if eqb u "Unit" then meths else app meths [tail]
                          | _ => app meths [tail]
                          end
                      | _ => []
                      end in
                    match rest_seq with
                    | [e1; e2; e3; e4] =>
                        match e1 with
                        | Block body_stmts body_tail body_sp =>
                            match e2 with
                            | Symbol kwd2 _ =>
                                match e3 with
                                | Symbol eff _ =>
                                    match e4 with
                                    | Block _ _ _ =>
                                        if eqb kwd2 "with" then
                                            HandleCST (Block body_stmts body_tail body_sp) eff
                                              (fst (process_stmts f2' env (methods_of_block e4))) s
                                        else stmt
                                    | _ => stmt
                                    end
                                | _ => stmt
                                end
                            | _ => stmt
                            end
                        | _ => stmt
                        end
                    | _ => stmt
                    end
                else if eqb kwd "record" then
                    match rest_seq with
                    | [e1] =>
                        match e1 with
                        | AppCST (Symbol name _) fields _ =>
                            RecordCST name [] fields s
                        | AppCST (ImplicitAppCST (Symbol name _) targs _) fields _ =>
                            let extract_targ (a: CST) : string := 
                               match a with Symbol n _ => n | _ => "T" end 
                            in
                            let type_params := map extract_targ targs in
                            RecordCST name type_params fields s
                        | _ => stmt
                        end
                    | _ => stmt
                    end
                else
                    (* Assignment: name = expr (not a reserved keyword). *)
                    let is_kw := orb (eqb kwd "let") (orb (eqb kwd "var") (orb (eqb kwd "def")
                      (orb (eqb kwd "if") (orb (eqb kwd "match") (orb (eqb kwd "effect")
                      (orb (eqb kwd "handle") (orb (eqb kwd "perform") (orb (eqb kwd "extension")
                      (orb (eqb kwd "enum") (orb (eqb kwd "record") (orb (eqb kwd "case")
                      (orb (eqb kwd "return") (orb (eqb kwd "box") (orb (eqb kwd "unbox")
                      (orb (eqb kwd "infixl") (eqb kwd "infixr")))))))))))))))) in
                    match rest_seq with
                    | Symbol eqtok _ :: val_exprs =>
                        if andb (eqb eqtok "=") (negb is_kw) then
                          let val_cst := match val_exprs with [v] => v | _ => expand_seq_expr env val_exprs s end in
                          AssignCST kwd val_cst s
                        else stmt
                    | _ => stmt
                    end
            | ExtensionCST name tparams target_ty meths span =>
                let (meths', _) := process_stmts f2' env meths in
                ExtensionCST name tparams target_ty meths' span
            | Error msg span => Error msg span
            | _ => stmt
            end in
            let (rest_processed, env_rest) := process_stmts f2' env rest in
            (processed_stmt :: rest_processed, env_rest)
            end
        end
        end
      in
      let (final_stmts, env3) := process_stmts fuel' env2 expanded_stmts in
      let (final_tail_list, env4) := process_stmts fuel' env3 [expanded_tail] in
      let final_tail := match final_tail_list with
                        | [] => Tuple [] span
                        | t :: _ => t
                        end in
          (* Bare `handle`/`perform` as the last block form should be the block value,
             matching the usual `def main() = { handle { ... } with Eff { ... } }` surface. *)
          let promote_tail (stmts : list CST) (tail : CST) : (list CST * CST) :=
            match tail with
            | Symbol u _ =>
                if eqb u "Unit" then
                  match rev stmts with
                  | (HandleCST _ _ _ _ as h) :: prefix_rev => (rev prefix_rev, h)
                  | (DoCST _ _ _ as d) :: prefix_rev => (rev prefix_rev, d)
                  | (AssignCST _ _ _ as a) :: prefix_rev => (rev prefix_rev, a)
                  | _ => (stmts, tail)
                  end
                else (stmts, tail)
            | _ => (stmts, tail)
            end
          in
          match promote_tail final_stmts final_tail with
          | (stmts', tail') => (Block stmts' tail' span, env4)
          end

  | Tuple elems span =>
      let fix map_elems (env : OpEnv) (cs : list CST) : (list CST * OpEnv) :=
        match cs with
        | [] => ([], env)
        | x :: xs =>
            let (x', e1) := expand_cst fuel' env x in
            let (xs', e2) := map_elems e1 xs in
            (x' :: xs', e2)
        end
      in
      let (elems', env') := map_elems op_env elems in
      (Tuple elems' span, env')
  | ListLiteral elems span =>
      let fix map_elems (env : OpEnv) (cs : list CST) : (list CST * OpEnv) :=
        match cs with
        | [] => ([], env)
        | x :: xs =>
            let (x', e1) := expand_cst fuel' env x in
            let (xs', e2) := map_elems e1 xs in
            (x' :: xs', e2)
        end
      in
      let (elems', env') := map_elems op_env elems in
      (ListLiteral elems' span, env')
  
  | SeqOf elems span => 
      let fix map_elems (env : OpEnv) (cs : list CST) : (list CST * OpEnv) :=
        match cs with
        | [] => ([], env)
        | x :: xs =>
            let (x', e1) := expand_cst fuel' env x in
            let (xs', e2) := map_elems e1 xs in
            (x' :: xs', e2)
        end
      in
      let (expanded_elems, env') := map_elems op_env elems in
      (expand_seq_expr env' expanded_elems span, env')
      
  | FieldAccessCST expr field span =>
      let (expr', env') := expand_cst fuel' op_env expr in
      (FieldAccessCST expr' field span, env')
  | MatchCST expr cases span =>
      let (expr', env') := expand_cst fuel' op_env expr in
      (MatchCST expr' cases span, env')
  | EffectCST name tps decls span =>
      let fix map_elems (env : OpEnv) (cs : list CST) : (list CST * OpEnv) :=
        match cs with
        | [] => ([], env)
        | x :: xs =>
            let (x', e1) := expand_cst fuel' env x in
            let (xs', e2) := map_elems e1 xs in
            (x' :: xs', e2)
        end
      in
      let (decls', env') := map_elems op_env decls in
      (EffectCST name tps decls' span, env')
  | DoCST op args span =>
      let (op', env1) := expand_cst fuel' op_env op in
      let fix map_elems (env : OpEnv) (cs : list CST) : (list CST * OpEnv) :=
        match cs with
        | [] => ([], env)
        | x :: xs =>
            let (x', e1) := expand_cst fuel' env x in
            let (xs', e2) := map_elems e1 xs in
            (x' :: xs', e2)
        end
      in
      let (args', env2) := map_elems env1 args in
      (DoCST op' args' span, env2)
  | HandleCST body eff handlers span =>
      let (body', env1) := expand_cst fuel' op_env body in
      let fix map_elems (env : OpEnv) (cs : list CST) : (list CST * OpEnv) :=
        match cs with
        | [] => ([], env)
        | x :: xs =>
            let (x', e1) := expand_cst fuel' env x in
            let (xs', e2) := map_elems e1 xs in
            (x' :: xs', e2)
        end
      in
      let (handlers', env2) := map_elems env1 handlers in
      (HandleCST body' eff handlers' span, env2)
  | VarCST name value next span =>
      let (value', env1) := expand_cst fuel' op_env value in
      let (next', env2) := expand_cst fuel' env1 next in
      (VarCST name value' next' span, env2)
  | AssignCST name value span =>
      let (value', env') := expand_cst fuel' op_env value in
      (AssignCST name value' span, env')
  | BoxCST e span =>
      let (e', env') := expand_cst fuel' op_env e in
      (BoxCST e' span, env')
  | UnboxCST e span =>
      let (e', env') := expand_cst fuel' op_env e in
      (UnboxCST e' span, env')
  | ImportCST lang alias mod syms span => (ImportCST lang alias mod syms span, op_env)
  | ExternCST lang mod decls span =>
      let fix map_elems (env : OpEnv) (cs : list CST) : (list CST * OpEnv) :=
        match cs with
        | [] => ([], env)
        | x :: xs =>
            let (x', e1) := expand_cst fuel' env x in
            let (xs', e2) := map_elems e1 xs in
            (x' :: xs', e2)
        end
      in
      let (decls', env') := map_elems op_env decls in
      (ExternCST lang mod decls' span, env')
  | _ => (c, op_env)
    end
  end.


Definition expand_cst_top (expr : CST) : CST := fst (expand_cst 1000 [] expr).

Definition expand_cst_top_env (op_env : OpEnv) (expr : CST) : (CST * OpEnv) :=
  expand_cst 1000 op_env expr.
