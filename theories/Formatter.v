From Stdlib Require Import Strings.String.
From Stdlib Require Import List.
From Stdlib Require Import Ascii.
From Stdlib Require Import Bool.Bool.
Import ListNotations.
Open Scope string_scope.

Require Import Chester.CST.

Definition newline : string := String (ascii_of_nat 10) "".
Definition quote : string := String (ascii_of_nat 34) "".
Definition backslash : string := String (ascii_of_nat 92) "".

Fixpoint gen_spaces (n : nat) : string :=
  match n with
  | 0 => ""
  | S n' => " " ++ gen_spaces n'
  end.

Fixpoint join_strings (sep : string) (ls : list string) : string :=
  match ls with
  | [] => ""
  | [x] => x
  | x :: xs => x ++ sep ++ join_strings sep xs
  end.

Definition string_eqb (left right : string) : bool :=
  if string_dec left right then true else false.

Definition with_utf16_eqb (left right : WithUTF16) : bool :=
  Nat.eqb (unicode left) (unicode right) && Nat.eqb (utf16 left) (utf16 right).

Definition pos_eqb (left right : Pos) : bool :=
  with_utf16_eqb (index left) (index right) &&
  Nat.eqb (line left) (line right) &&
  with_utf16_eqb (column left) (column right).

Definition span_is_empty (span : Span) : bool :=
  string_eqb (file_name span) "" &&
  pos_eqb (start_pos (range span)) zero_pos &&
  pos_eqb (end_pos (range span)) zero_pos.

Fixpoint string_in (needle : string) (haystack : list string) : bool :=
  match haystack with
  | [] => false
  | candidate :: rest =>
      if string_dec needle candidate then true else string_in needle rest
  end.

Definition symbol_is (name : string) (expr : CST) : bool :=
  match expr with
  | Symbol actual _ => string_eqb actual name
  | _ => false
  end.


Fixpoint last_elem (ls : list CST) : option CST :=
  match ls with
  | [] => None
  | [x] => Some x
  | _ :: rest => last_elem rest
  end.

Definition ends_with_block (stmt : CST) : bool :=
  match stmt with
  | Block _ _ _ => true
  | SeqOf elements _ =>
      match last_elem elements with
      | Some (Block _ _ _) => true
      | _ => false
      end
  | _ => false
  end.

Definition starts_with_let (stmt : CST) : bool :=
  match stmt with
  | SeqOf (Symbol "let"%string _ :: _) _ => true
  | _ => false
  end.

Definition needs_semicolon (stmt : CST) : bool :=
  match stmt with
  | CommentCST _ _ => false
  | _ => true
  end.

Definition block_looks_like_match_cases (elements : list CST) : bool :=
  existsb
    (fun stmt =>
       match stmt with
       | SeqOf (Symbol "case"%string _ :: _) _ => true
       | _ => false
       end)
    elements.

Definition is_unit_tail (expr : CST) : bool :=
  match expr with
  | Symbol name span => string_eqb name "Unit" && span_is_empty span
  | _ => false
  end.

Definition stmt_ends_with_match_block (stmt : CST) : bool :=
  match stmt with
  | SeqOf elements _ =>
      match elements with
      | Symbol "match"%string _ :: rest =>
          match last_elem rest with
          | Some (Block els _ _) => block_looks_like_match_cases els
          | _ => false
          end
      | _ => false
      end
  | _ => false
  end.

Definition trailing_semicolon (stmt : CST) (rest : list CST) (tail : CST)
  (in_match_block : bool) : string :=
  if negb (needs_semicolon stmt) then ""
  else
    if (in_match_block && ends_with_block stmt) || stmt_ends_with_match_block stmt then ""
    else ";".

Definition stmt_from_seq (elements : list CST) : CST :=
  match elements with
  | [] => Symbol "Empty" empty_span
  | [single] => single
  | _ => SeqOf elements empty_span
  end.

Definition format_comment (text : string) : string :=
  match text with
  | String "/"%char (String "/"%char _) => text
  | _ => "// " ++ text
  end.

Definition format_comment_cst (expr : CST) : string :=
  match expr with
  | CommentCST text _ => format_comment text
  | _ => ""
  end.

Fixpoint split_trailing_comments (elements : list CST) : list CST * list CST :=
  match elements with
  | [] => ([], [])
  | element :: rest =>
      let (body, comments) := split_trailing_comments rest in
      match body with
      | [] =>
          match element with
          | CommentCST _ _ => ([], element :: comments)
          | _ => ([element], comments)
          end
      | _ => (element :: body, comments)
      end
  end.

Definition tight_before_symbols : list string :=
  [ ","; ";"; ")"; "]"; "."; ":" ].

Definition tight_after_symbols : list string := [ "."; backslash ].

Definition spaced_group_after_symbols : list string :=
  [ "="; "=>"; "->"; ":"; ","; ";"; "+"; "-"; "*"; "/"; "%"; "=="; "!="; "<";
    ">"; "<="; ">="; "&&"; "||"; "|"; "&" ].

Definition cst_adjacent (left right : CST) : bool :=
  pos_eqb (end_pos (range (get_span left))) (start_pos (range (get_span right))).

Definition needs_space_before_group (prev current : CST) : bool :=
  match prev with
  | Symbol name _ =>
      if string_in name tight_after_symbols then false
      else if string_in name spaced_group_after_symbols then true
      else negb (cst_adjacent prev current)
  | Tuple _ _ => false
  | ListLiteral _ _ => false
  | AppCST _ _ _ => false
  | ImplicitAppCST _ _ _ => false
  | FieldAccessCST _ _ _ => false
  | _ => true
  end.

Definition needs_space (prev : option CST) (current : CST) : bool :=
  match prev with
  | None => false
  | Some p =>
      match current with
      | Tuple _ _ => needs_space_before_group p current
      | ListLiteral _ _ => needs_space_before_group p current
      | Symbol name _ =>
          if string_in name tight_before_symbols then false
          else
            match p with
            | Symbol previous _ =>
                if string_in previous tight_after_symbols then false else true
            | _ => true
            end
      | _ =>
          match p with
          | Symbol previous _ =>
              if string_in previous tight_after_symbols then false else true
          | _ => true
          end
      end
  end.

Fixpoint format_cst (fuel : nat) (indent : nat) (expr : CST) : string :=
  match fuel with
  | 0 => "/* ERROR: formatter out of fuel */"
  | S f =>
      match expr with
      | Symbol name _ => name
      | StringLiteral val _ => quote ++ val ++ quote
      | IntegerLiteral val _ => val
      | BoolLiteral b _ => if b then "true" else "false"
      | CommentCST text _ => format_comment text

      | Tuple elements _ =>
          "(" ++ join_strings ", " (map (format_cst f indent) elements) ++ ")"
      | ListLiteral elements _ =>
          "[" ++ join_strings ", " (map (format_cst f indent) elements) ++ "]"

      | Block elements tail _ =>
          let next_indent := indent + 2 in
          let line_prefix := newline ++ gen_spaces next_indent in
          let in_match_block := block_looks_like_match_cases elements in
          let fix format_stmt_lines (rest : list CST) : list string :=
            match rest with
            | [] => []
            | stmt :: rest' =>
                let lines :=
                  match stmt with
                  | CommentCST text _ => [format_comment text]
                  | SeqOf elements _ =>
                      let (body, comments) := split_trailing_comments elements in
                      let body_cst := stmt_from_seq body in
                      let semi := trailing_semicolon body_cst rest' tail in_match_block in
                      let body_lines :=
                        match body with
                        | [] => []
                        | _ => [format_cst f next_indent body_cst ++ semi]
                        end in
                      List.app body_lines (map format_comment_cst comments)
                  | _ =>
                      let semi := trailing_semicolon stmt rest' tail in_match_block in
                      [format_cst f next_indent stmt ++ semi]
                  end in
                List.app lines (format_stmt_lines rest')
            end in
          let stmt_lines := format_stmt_lines elements in
          let tail_lines := if is_unit_tail tail then [] else [format_cst f next_indent tail] in
          let lines := List.app stmt_lines tail_lines in
          match lines with
          | [] => "{}"
          | _ =>
              "{" ++ line_prefix ++ join_strings line_prefix lines ++
              newline ++ gen_spaces indent ++ "}"
          end

      | SeqOf elements _ =>
          let fix format_seq (prev : option CST) (rest : list CST) : string :=
            match rest with
            | [] => ""
            | current :: tail =>
                let separator := if needs_space prev current then " " else "" in
                let formatted := separator ++ format_cst f indent current in
                match current, tail with
                | CommentCST _ _, _ :: _ =>
                    formatted ++ newline ++ gen_spaces indent ++ format_seq None tail
                | _, _ => formatted ++ format_seq (Some current) tail
                end
            end in
          format_seq None elements

      | ImportCST lang alias mod_path syms _ =>
          let syms_str :=
            match syms with
            | [] => ""
            | _ => " { " ++ join_strings ", " syms ++ " }"
            end
          in
          let alias_part :=
            if string_eqb alias "" || string_eqb alias lang then ""
            else " " ++ alias
          in
          "import " ++ lang ++ alias_part ++ " " ++ quote ++ mod_path ++ quote ++ syms_str
      | ModuleCST name params seal body _ =>
          let fix format_decl (d : CST) : string :=
            match d with
            | CommentCST m _ => "// " ++ m
            | _ => format_cst f indent d
            end
          in
          let fix map_decls (ds : list CST) : string :=
            match ds with
            | [] => ""
            | [x] => format_decl x
            | x :: xs => format_decl x ++ "\n" ++ map_decls xs
            end
          in
          let fix format_params (ps : list (string * CST)) : string :=
            match ps with
            | [] => ""
            | (n, ty) :: xs =>
                let one := n ++ ": " ++ format_cst f indent ty in
                match xs with
                | [] => one
                | _ => one ++ ", " ++ format_params xs
                end
            end
          in
          let param_part :=
            match params with
            | [] => ""
            | _ => "(" ++ format_params params ++ ")"
            end
          in
          let seal_part :=
            match seal with
            | Some (AppCST (Symbol "#transparent" _) [inner] _) =>
                " : " ++ format_cst f indent inner
            | Some (AppCST (Symbol "#opaque" _) [inner] _) =>
                " :> " ++ format_cst f indent inner
            | Some (Symbol "#applicative" _) => ""
            | Some (AppCST (Symbol "#applicative" _) _ _) => ""
            | Some s0 => " :> " ++ format_cst f indent s0
            | None => ""
            end
          in
          let app_part :=
            match seal with
            | Some (Symbol "#applicative" _) => "app "
            | Some (AppCST (Symbol "#applicative" _) _ _) => "app "
            | _ => ""
            end
          in
          "module " ++ app_part ++ name ++ param_part ++ seal_part ++ " {\n"
            ++ map_decls body ++ "\n}"
      | SignatureCST name body _ =>
          let fix format_decl (d : CST) : string :=
            match d with
            | CommentCST m _ => "// " ++ m
            | SigValCST n _ params ret _ =>
                let fix fmt_ps (ps : list (string * CST)) : string :=
                  match ps with
                  | [] => ""
                  | (pn, ty) :: xs =>
                      let one := pn ++ ": " ++ format_cst f indent ty in
                      match xs with
                      | [] => one
                      | _ => one ++ ", " ++ fmt_ps xs
                      end
                  end
                in
                "def " ++ n ++ "(" ++ fmt_ps params ++ "): " ++ format_cst f indent ret
            | _ => format_cst f indent d
            end
          in
          let fix map_decls (ds : list CST) : string :=
            match ds with
            | [] => ""
            | [x] => format_decl x
            | x :: xs => format_decl x ++ "\n" ++ map_decls xs
            end
          in
          "signature " ++ name ++ " {\n" ++ map_decls body ++ "\n}"
      | FunctorAppCST func args _ =>
          let fix fmt_args (as_ : list CST) : string :=
            match as_ with
            | [] => ""
            | [x] => format_cst f indent x
            | x :: xs => format_cst f indent x ++ ", " ++ fmt_args xs
            end
          in
          format_cst f indent func ++ "(" ++ fmt_args args ++ ")"
      | ModuleAliasCST name rhs _ =>
          "module " ++ name ++ " = " ++ format_cst f indent rhs
      | SigValCST n _ params ret _ =>
          let fix fmt_ps (ps : list (string * CST)) : string :=
            match ps with
            | [] => ""
            | (pn, ty) :: xs =>
                let one := pn ++ ": " ++ format_cst f indent ty in
                match xs with
                | [] => one
                | _ => one ++ ", " ++ fmt_ps xs
                end
            end
          in
          "def " ++ n ++ "(" ++ fmt_ps params ++ "): " ++ format_cst f indent ret
      | TypeDeclCST name (Some ty) _ =>
          "type " ++ name ++ " = " ++ format_cst f indent ty
      | TypeDeclCST name None _ =>
          "type " ++ name
      | FileImportCST name path _ =>
          if string_eqb path "" then "import " ++ name
          else if string_eqb name path then
            "import " ++ quote ++ path ++ quote
          else "import " ++ name ++ " from " ++ quote ++ path ++ quote
      | SigWithCST base eqs _ =>
          let fix fmt_eqs (es : list (string * CST)) : string :=
            match es with
            | [] => ""
            | (n, ty) :: xs =>
                let one := "type " ++ n ++ " = " ++ format_cst f indent ty in
                match xs with
                | [] => one
                | _ => one ++ " and " ++ fmt_eqs xs
                end
            end
          in
          format_cst f indent base ++ " with " ++ fmt_eqs eqs
      | PackCST m sig _ =>
          "pack " ++ format_cst f indent m ++ " as " ++ format_cst f indent sig
      | UnpackCST x sig e body _ =>
          "unpack (" ++ x ++ " : " ++ format_cst f indent sig ++ ") = "
            ++ format_cst f indent e ++ " in " ++ format_cst f indent body
      | ExternCST lang mod_path decls _ =>
          let fix format_decl (d : CST) : string :=
            match d with
            | DefCST name _ params ret_ty _ _ =>
                "def " ++ name ++ "(...): ..."
            | SeqOf (Symbol "def" _ :: Symbol name _ :: _) _ =>
                "def " ++ name ++ "(...)"
            | _ => "/* extern decl */"
            end
          in
          "extern " ++ lang ++ " " ++ quote ++ mod_path ++ quote ++ " {" ++ newline ++
          join_strings (";" ++ newline) (map format_decl decls) ++ newline ++ "}"
      | LetCST _ _ _ _
      | VarCST _ _ _ _
      | AssignCST _ _ _
      | IfCST _ _ _ _
      | DefCST _ _ _ _ _ _
      | LamCST _ _ _ _
      | AppCST _ _ _
      | ImplicitAppCST _ _ _
      | EnumCST _ _ _ _
      | MatchCST _ _ _
      | RecordCST _ _ _ _
      | EffectCST _ _ _ _
      | DoCST _ _ _
      | FieldAccessCST _ _ _
      | ExtensionCST _ _ _ _ _
      | MacroDefCST _ _ _
      | BoxCST _ _
      | UnboxCST _ _
      | HandleCST _ _ _ _ => "/* EXPANDED NODE */"
      | Error msg _ => "/* ERROR: " ++ msg ++ " */"
      end
  end.

Fixpoint format_program_body (fuel : nat) (indent : nat) (stmts : list CST) (tail : CST) : string :=
  match fuel with
  | 0 => "/* ERROR: formatter out of fuel */"
  | S f =>
      let in_match_block := block_looks_like_match_cases stmts in
      let fix format_stmt_lines (rest : list CST) : list string :=
        match rest with
        | [] => []
        | stmt :: rest' =>
            let lines :=
              match stmt with
              | CommentCST text _ => [format_comment text]
              | SeqOf elements _ =>
                  let (body, comments) := split_trailing_comments elements in
                  let body_cst := stmt_from_seq body in
                  let semi := trailing_semicolon body_cst rest' tail in_match_block in
                  let body_lines :=
                    match body with
                    | [] => []
                    | _ => [format_cst f indent body_cst ++ semi]
                    end in
                  List.app body_lines (map format_comment_cst comments)
              | _ =>
                  let semi := trailing_semicolon stmt rest' tail in_match_block in
                  [format_cst f indent stmt ++ semi]
              end in
            List.app lines (format_stmt_lines rest')
        end in
      let stmt_lines := format_stmt_lines stmts in
      let tail_lines := if is_unit_tail tail then [] else [format_cst f indent tail] in
      join_strings (newline ++ gen_spaces indent) (List.app stmt_lines tail_lines)
  end.

Definition format_program (fuel : nat) (expr : CST) : string :=
  match expr with
  | Block stmts tail _ =>
      format_program_body fuel 0 stmts tail
  | _ => format_cst fuel 0 expr
  end.
