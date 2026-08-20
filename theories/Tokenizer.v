From Stdlib Require Import Strings.String.
From Stdlib Require Import Strings.Ascii.
From Stdlib Require Import List.
Import ListNotations.
Open Scope string_scope.

Require Import Chester.CST.
Require Import Chester.Parser.

Fixpoint tokenize (fuel : nat) (input : string) (current_pos : Pos) : list Token :=
  match fuel with
  | 0 => []
  | S fuel' => 
      match input with
      | "" => []
      | String c rest => 
          if (Ascii.eqb c "{"%char) then TokSym "{" empty_span :: tokenize fuel' rest current_pos
          else if (Ascii.eqb c "}"%char) then TokSym "}" empty_span :: tokenize fuel' rest current_pos
          else tokenize fuel' rest current_pos
      end
  end.
