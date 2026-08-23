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

Definition is_unit_tail (expr : CST) : bool :=
  match expr with
  | Symbol name span => string_eqb name "Unit" && span_is_empty span
  | _ => false
  end.

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
  | TypeAppCST _ _ _ => false
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
          let format_stmt_line stmt :=
            match stmt with
            | CommentCST text _ => [format_comment text]
            | SeqOf elements _ =>
                let (body, comments) := split_trailing_comments elements in
                let body_lines :=
                  match body with
                  | [] => []
                  | _ => [format_cst f next_indent (stmt_from_seq body) ++ ";"]
                  end in
                List.app body_lines (map format_comment_cst comments)
            | _ => [format_cst f next_indent stmt ++ ";"]
            end in
          let stmt_lines := List.concat (map format_stmt_line elements) in
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

      | LetCST name val body _ =>
          "let " ++ name ++ " = " ++ format_cst f indent val ++ ";" ++
          newline ++ gen_spaces indent ++ format_cst f indent body

      | IfCST cond thenB elseB _ =>
          "if " ++ format_cst f indent cond ++
          " then " ++ format_cst f indent thenB ++
          " else " ++ format_cst f indent elseB

      | DefCST name type_params params ret_ty body _ =>
          let fix format_params (ps : list (string * CST)) : string :=
            match ps with
            | [] => ""
            | [(n, t)] => n ++ ": " ++ format_cst f indent t
            | (n, t) :: rest => n ++ ": " ++ format_cst f indent t ++ ", " ++ format_params rest
            end in
          let type_params_str :=
            match type_params with
            | [] => ""
            | _ => "[" ++ join_strings ", " type_params ++ "]"
            end in
          "def " ++ name ++ type_params_str ++ "(" ++ format_params params ++ "): " ++
          format_cst f indent ret_ty ++ " = " ++ format_cst f indent body

      | LamCST arg_name opt_arg_ty body _ =>
          let arg_str := match opt_arg_ty with
                         | Some t => "(" ++ arg_name ++ ": " ++ format_cst f indent t ++ ")"
                         | None => arg_name
                         end in
          backslash ++ arg_str ++ " => " ++ format_cst f indent body

      | TypeAppCST func args _ =>
          format_cst f indent func ++ "[" ++ join_strings ", " (map (format_cst f indent) args) ++ "]"
      | AppCST func args _ =>
          format_cst f indent func ++ "(" ++ join_strings ", " (map (format_cst f indent) args) ++ ")"

      | EnumCST name type_params variants _ =>
          let type_params_str :=
            match type_params with
            | [] => ""
            | _ => "[" ++ join_strings ", " type_params ++ "]"
            end in
          let next_indent := indent + 2 in
          let line_prefix := newline ++ gen_spaces next_indent in
          let lines := map (fun v => format_cst f next_indent v ++ ";") variants in
          match lines with
          | [] => "enum " ++ name ++ type_params_str ++ " {}"
          | _ =>
              "enum " ++ name ++ type_params_str ++ " {" ++ line_prefix ++
              join_strings line_prefix lines ++ newline ++ gen_spaces indent ++ "}"
          end

      | MatchCST expr cases _ =>
          let fix format_cases (cs : list (PatternCST * CST)) : list string :=
            match cs with
            | [] => []
            | (pat, body) :: rest =>
                let pat_str := match pat with
                               | PatWildcardCST _ => "_"
                               | PatVarCST v _ => v
                               | PatConstructorCST cname vars _ =>
                                   match vars with
                                   | [] => cname
                                   | _ => cname ++ "(" ++ join_strings ", " vars ++ ")"
                                   end
                               end in
                ("case " ++ pat_str ++ " => " ++ format_cst f (indent + 2) body ++ ";") ::
                format_cases rest
            end in
          "match " ++ format_cst f indent expr ++ " {" ++ newline ++
          gen_spaces (indent + 2) ++
          join_strings (newline ++ gen_spaces (indent + 2)) (format_cases cases) ++
          newline ++ gen_spaces indent ++ "}"

      | RecordCST name type_params fields _ =>
          let type_params_str :=
            match type_params with
            | [] => ""
            | _ => "[" ++ join_strings ", " type_params ++ "]"
            end in
          let next_indent := indent + 2 in
          let line_prefix := newline ++ gen_spaces next_indent in
          let lines := map (fun field => format_cst f next_indent field ++ ";") fields in
          match lines with
          | [] => "record " ++ name ++ type_params_str ++ " {}"
          | _ =>
              "record " ++ name ++ type_params_str ++ " {" ++ line_prefix ++
              join_strings line_prefix lines ++ newline ++ gen_spaces indent ++ "}"
          end

      | FieldAccessCST base field _ =>
          format_cst f indent base ++ "." ++ field

      | MacroDefCST name cases _ =>
          let fix format_cases (cs : list (PatternCST * CST)) : list string :=
            match cs with
            | [] => []
            | (pat, body) :: rest =>
                let pat_str := match pat with
                               | PatWildcardCST _ => "_"
                               | PatVarCST v _ => v
                               | PatConstructorCST cname vars _ =>
                                   match vars with
                                   | [] => cname
                                   | _ => cname ++ "(" ++ join_strings ", " vars ++ ")"
                                   end
                               end in
                ("case " ++ pat_str ++ " => " ++ format_cst f (indent + 2) body ++ ";") ::
                format_cases rest
            end in
          "macro " ++ name ++ " {" ++ newline ++ gen_spaces (indent + 2) ++
          join_strings (newline ++ gen_spaces (indent + 2)) (format_cases cases) ++
          newline ++ gen_spaces indent ++ "}"

      | EffectCST name params decls _ =>
          let params_str :=
            match params with
            | [] => ""
            | _ => "[" ++ join_strings ", " params ++ "]"
            end in
          let next_indent := indent + 2 in
          let line_prefix := newline ++ gen_spaces next_indent in
          let lines := map (fun decl => format_cst f next_indent decl ++ ";") decls in
          match lines with
          | [] => "effect " ++ name ++ params_str ++ " {}"
          | _ =>
              "effect " ++ name ++ params_str ++ " {" ++ line_prefix ++
              join_strings line_prefix lines ++ newline ++ gen_spaces indent ++ "}"
          end

      | DoCST op args _ =>
          "perform " ++ format_cst f indent op ++ "(" ++
          join_strings ", " (map (format_cst f indent) args) ++ ")"
      | HandleCST body eff handlers _ =>
          let next_indent := indent + 2 in
          let line_prefix := newline ++ gen_spaces next_indent in
          let handler_lines := map (fun handler => format_cst f next_indent handler ++ ";") handlers in
          "handle " ++ format_cst f indent body ++ " with " ++ eff ++ " {" ++
          line_prefix ++ join_strings line_prefix handler_lines ++
          newline ++ gen_spaces indent ++ "}"
      | Error msg _ => "/* ERROR: " ++ msg ++ " */"
      end
  end.

Fixpoint format_program_body (fuel : nat) (indent : nat) (stmts : list CST) (tail : CST) : string :=
  match fuel with
  | 0 => "/* ERROR: formatter out of fuel */"
  | S f =>
      let format_stmt_line stmt :=
        match stmt with
        | CommentCST text _ => [format_comment text]
        | SeqOf elements _ =>
            let (body, comments) := split_trailing_comments elements in
            let body_lines :=
              match body with
              | [] => []
              | _ => [format_cst f indent (stmt_from_seq body) ++ ";"]
              end in
            List.app body_lines (map format_comment_cst comments)
        | _ => [format_cst f indent stmt ++ ";"]
        end in
      let stmt_lines := List.concat (map format_stmt_line stmts) in
      let tail_lines := if is_unit_tail tail then [] else [format_cst f indent tail] in
      join_strings (newline ++ gen_spaces indent) (List.app stmt_lines tail_lines)
  end.

Definition format_program (fuel : nat) (expr : CST) : string :=
  match expr with
  | Block stmts tail _ =>
      format_program_body fuel 0 stmts tail
  | _ => format_cst fuel 0 expr
  end.
