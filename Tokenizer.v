From Stdlib Require Import Strings.String.
From Stdlib Require Import Strings.Ascii.
From Stdlib Require Import List.
Import ListNotations.
Open Scope string_scope.

Require Import Chester.CST.
Require Import Chester.Parser.

(* A functional lexer to convert a string into a list of Tokens *)
(* To be implemented fully in subsequent steps to parse stdlib.chester *)
Fixpoint tokenize (fuel : nat) (input : string) (current_pos : Pos) : list Token :=
  match fuel with
  | 0 => []
  | S fuel' => 
      match input with
      | "" => []
      | String c rest => 
          (* Mock implementation just to pass extraction compilation *)
          if (Ascii.eqb c "{"%char) then TokLBrace empty_span :: tokenize fuel' rest current_pos
          else if (Ascii.eqb c "}"%char) then TokRBrace empty_span :: tokenize fuel' rest current_pos
          else tokenize fuel' rest current_pos
      end
  end.
