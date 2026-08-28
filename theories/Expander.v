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

Fixpoint parse_infix_chain (elems : list CST) (span : Span) : option CST :=
  match elems with
  | [] => None
  | [e] => Some e
  | lhs :: Symbol op sp :: rest =>
      match op_prec op, parse_infix_chain rest span with
      | Some _, Some rhs =>
          Some (AppCST (Symbol (op_to_fn op) sp) [lhs; rhs] span)
      | _, _ => None
      end
  | _ => None
  end.

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

Fixpoint expand_seq_expr (elems : list CST) (span : Span) : CST :=
  match parse_infix_chain elems span with
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
             We CANNOT call expand_seq_expr here (same fixpoint) — Coq forbids it. *)
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

Fixpoint expand_cst (fuel: nat) (c : CST) {struct fuel} : CST :=
  match fuel with
  | 0 => c
  | S fuel' =>
  match c with
  | Block stmts tail span =>
      let expanded_stmts := map (expand_cst fuel') stmts in
      let expanded_tail := expand_cst fuel' tail in
      
      let fix process_stmts (f2 : nat) (ss : list CST) {struct f2} : list CST :=
        match f2 with
        | 0 => ss
        | S f2' =>
        match ss with
        | [] => []
        | stmt :: rest =>
            let processed_stmt := match stmt with
            | SeqOf (Symbol kwd _ :: rest_seq) s =>
                if eqb kwd "let" then
                    match rest_seq with
                    | Symbol name _ :: Symbol kwd2 _ :: val_exprs =>
                        if eqb kwd2 "=" then
                            let val_cst := match val_exprs with [v] => v | _ => expand_seq_expr val_exprs s end in
                            LetCST name val_cst (Symbol "Unit" empty_span) s
                        else stmt
                    | _ => stmt
                    end
                else if eqb kwd "var" then
                    match rest_seq with
                    | Symbol name _ :: Symbol kwd2 _ :: val_exprs =>
                        if eqb kwd2 "=" then
                            let val_cst := match val_exprs with [v] => v | _ => expand_seq_expr val_exprs s end in
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
                        | _ => BoxCST (expand_seq_expr rest_seq s) s
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
                        | _ => UnboxCST (expand_seq_expr rest_seq s) s
                        end
                    end
                else if eqb kwd "def" then
                    match rest_seq with
                    | AppCST (Symbol name _) args _ :: rest_def =>
                        match split_at_eq [] rest_def with
                        | Some (ty_exprs, body_exprs) =>
                            let ret_ty := match ty_exprs with 
                                          | Symbol kwd2 _ :: tys => 
                                              if eqb kwd2 ":" then expand_seq_expr tys s else Symbol "Unknown" empty_span
                                          | _ => Symbol "Unknown" empty_span 
                                          end in
                            let body_cst := match body_exprs with [b] => b | _ => expand_seq_expr body_exprs s end in
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
                                              if eqb kwd2 ":" then expand_seq_expr tys s else Symbol "Unknown" empty_span
                                          | _ => Symbol "Unknown" empty_span 
                                          end in
                            let body_cst := match body_exprs with [b] => b | _ => expand_seq_expr body_exprs s end in
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
                                      ExtensionCST name [] e3 (process_stmts f2' (methods_of_block e4)) s
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
                                      ExtensionCST name (extract_vars targs) e3 (process_stmts f2' (methods_of_block e4)) s
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
                                          ExtensionCST name (extract_vars targs) e4 (process_stmts f2' (methods_of_block e5)) s
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
                                EffectCST name [] (process_stmts f2' (methods_of_block e2)) s
                            | _ => stmt
                            end
                        | ImplicitAppCST (Symbol name _) targs _ =>
                            match e2 with
                            | Block _ _ _ =>
                                EffectCST name (extract_tparams targs) (process_stmts f2' (methods_of_block e2)) s
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
                                    EffectCST name (extract_tparams targs) (process_stmts f2' (methods_of_block e3)) s
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
                                              (process_stmts f2' (methods_of_block e4)) s
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
                else if orb (eqb kwd "infixl") (eqb kwd "infixr") then
                    (* Builtin operators are handled in parse_infix_chain; drop decls. *)
                    CommentCST "__skip_infix__" s
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
                          let val_cst := match val_exprs with [v] => v | _ => expand_seq_expr val_exprs s end in
                          AssignCST kwd val_cst s
                        else stmt
                    | _ => stmt
                    end
            | ExtensionCST name tparams target_ty meths span => ExtensionCST name tparams target_ty (process_stmts f2' meths) span
            | Error msg span => Error msg span
            | _ => stmt
            end in
            let rest_processed := process_stmts f2' rest in
            match processed_stmt with
            | CommentCST "__skip_infix__" _ => rest_processed
            | _ => processed_stmt :: rest_processed
            end
        end
        end
      in
                let final_stmts := process_stmts fuel' expanded_stmts in
          let final_tail := match process_stmts fuel' [expanded_tail] with
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
          | (stmts', tail') => Block stmts' tail' span
          end

  | Tuple elems span => Tuple (map (expand_cst fuel') elems) span
  | ListLiteral elems span => ListLiteral (map (expand_cst fuel') elems) span
  
  | SeqOf elems span => 
      let expanded_elems := map (expand_cst fuel') elems in
      expand_seq_expr expanded_elems span
      
  | FieldAccessCST expr field span => FieldAccessCST (expand_cst fuel' expr) field span
  | MatchCST expr cases span => MatchCST (expand_cst fuel' expr) cases span
  | EffectCST name tps decls span =>
      EffectCST name tps (map (expand_cst fuel') decls) span
  | DoCST op args span =>
      DoCST (expand_cst fuel' op) (map (expand_cst fuel') args) span
  | HandleCST body eff handlers span =>
      HandleCST (expand_cst fuel' body) eff (map (expand_cst fuel') handlers) span
  | VarCST name value next span =>
      VarCST name (expand_cst fuel' value) (expand_cst fuel' next) span
  | AssignCST name value span =>
      AssignCST name (expand_cst fuel' value) span
  | BoxCST e span => BoxCST (expand_cst fuel' e) span
  | UnboxCST e span => UnboxCST (expand_cst fuel' e) span
  | ImportCST lang alias mod syms span => ImportCST lang alias mod syms span
  | ExternCST lang mod decls span => ExternCST lang mod (map (expand_cst fuel') decls) span
  | _ => c
    end
  end.


Definition expand_cst_top (expr : CST) : CST := expand_cst 1000 expr.
