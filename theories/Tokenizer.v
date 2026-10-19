From Stdlib Require Import Strings.String.
From Stdlib Require Import Strings.Ascii.
From Stdlib Require Import List.
Import ListNotations.
Open Scope string_scope.

Require Import Chester.CST.
Require Import Chester.Parser.

Fixpoint tokenize (input : string) (current_pos : Pos) {struct input} : list Token :=
  match input with
  | "" => []
  | String c rest => 
      if (Ascii.eqb c "{"%char) then TokSym "{" empty_span :: tokenize rest current_pos
      else if (Ascii.eqb c "}"%char) then TokSym "}" empty_span :: tokenize rest current_pos
      else tokenize rest current_pos
  end.
