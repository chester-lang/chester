From Stdlib Require Import Strings.String.
From Stdlib Require Import List.
From Stdlib Require Import Ascii.
Import ListNotations.
Open Scope string_scope.

Require Import Chester.CST.

(* Helper to generate n spaces *)
Fixpoint gen_spaces (n : nat) : string :=
  match n with
  | 0 => ""
  | S n' => " " ++ gen_spaces n'
  end.

(* Join a list of strings with a separator *)
Fixpoint join_strings (sep : string) (ls : list string) : string :=
  match ls with
  | [] => ""
  | [x] => x
  | x :: xs => x ++ sep ++ join_strings sep xs
  end.

(* 
  The Pretty Printer / Code Formatter 
  Takes a fuel, current indentation level, and CST to format.
*)
Fixpoint format_cst (fuel : nat) (indent : nat) (expr : CST) : string :=
  match fuel with
  | 0 => "/* ERROR: formatter out of fuel */"
  | S f =>
      match expr with
      | Symbol name _ => name
      | StringLiteral val _ => """" ++ val ++ """"
      | IntegerLiteral val _ => val
      | BoolLiteral b _ => if b then "true" else "false"
      | CommentCST text _ => "// " ++ text
      
      | Tuple elements _ => 
          "(" ++ join_strings ", " (map (format_cst f indent) elements) ++ ")"
      | ListLiteral elements _ => 
          "[" ++ join_strings ", " (map (format_cst f indent) elements) ++ "]"
      
      | Block elements tail _ =>
          let next_indent := indent + 2 in
          let nl_indent := String (ascii_of_nat 10) (gen_spaces next_indent) in
          let fix format_stmts (stmts : list CST) : string :=
            match stmts with
            | [] => ""
            | CommentCST text _ :: rest =>
                "// " ++ text ++ nl_indent ++ format_stmts rest
            | s :: rest =>
                format_cst f next_indent s ++ ";" ++ nl_indent ++ format_stmts rest
            end
          in
          let formatted_elems := format_stmts elements in
          let formatted_tail := format_cst f next_indent tail in
          "{" ++ nl_indent ++ 
          formatted_elems ++ 
          formatted_tail ++
          String (ascii_of_nat 10) (gen_spaces indent) ++ "}"
          
      | SeqOf elements _ =>
          let formatted_elems := map (format_cst f indent) elements in
          join_strings " " formatted_elems
          
      | LetCST name val body _ =>
          "let " ++ name ++ " = " ++ format_cst f indent val ++ ";" ++ 
          String (ascii_of_nat 10) (gen_spaces indent) ++ format_cst f indent body
          
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
            end
          in
          "def " ++ name ++ "(" ++ format_params params ++ "): " ++ format_cst f indent ret_ty ++ " = " ++ format_cst f indent body
          
      | LamCST arg_name opt_arg_ty body _ =>
          let arg_str := match opt_arg_ty with 
                         | Some t => "(" ++ arg_name ++ ": " ++ format_cst f indent t ++ ")"
                         | None => arg_name
                         end in
          "\" ++ arg_str ++ " => " ++ format_cst f indent body
          
      | TypeAppCST func args _ =>
          format_cst f indent func ++ "[" ++ join_strings ", " (map (format_cst f indent) args) ++ "]"
      | AppCST func args _ =>
          format_cst f indent func ++ "(" ++ join_strings ", " (map (format_cst f indent) args) ++ ")"
          
      | EnumCST name type_params variants _ =>
          "enum " ++ name ++ " { ... }"
          
      | MatchCST expr cases _ =>
          let fix format_cases (cs : list (PatternCST * CST)) : string :=
            match cs with
            | [] => ""
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
                "case " ++ pat_str ++ " => " ++ format_cst f (indent + 2) body ++ ";" ++
                (match rest with | [] => "" | _ => String (ascii_of_nat 10) (gen_spaces (indent + 2)) ++ format_cases rest end)
            end
          in
          "match " ++ format_cst f indent expr ++ " {" ++ String (ascii_of_nat 10) (gen_spaces (indent + 2)) ++ 
          format_cases cases ++ String (ascii_of_nat 10) (gen_spaces indent) ++ "}"
          
      | RecordCST name type_params fields _ =>
          "record " ++ name ++ " { ... }"
          
      | FieldAccessCST expr field _ =>
          format_cst f indent expr ++ "." ++ field
          
      | Error msg _ => "/* ERROR: " ++ msg ++ " */"
      end
  end.
