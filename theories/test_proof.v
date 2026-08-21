From Chester Require Import CST.
From Chester Require Import Tokenizer.
From Chester Require Import Parser.
Require Import List.
Import ListNotations.
Require Import String.

Ltac solve_parser_cst H_ind :=
  repeat match goal with
         | [ H : (match ?X with _ => _ end) = (_, _) |- _ ] => destruct X eqn:?
         | [ H : (if ?X then _ else _) = (_, _) |- _ ] => destruct X eqn:?
         | [ H : (let (a, b) := ?X in _) = (_, _) |- _ ] => destruct X eqn:?
         end;
  repeat match goal with
         | [ H : parse_loop_fuel _ _ _ = (_, _) |- _ ] => apply H_ind in H
         end;
  match goal with
  | [ H : (_, _) = (_, _) |- _ ] => inversion H; subst; clear H
  | [ H : ?X = ?X |- _ ] => clear H
  | _ => idtac
  end;
  try apply Forall_cons; auto;
  try match goal with
  | |- ParserCST (match ?L with _ => _ end) =>
      destruct L as [|h t];
      [ apply ParserSymbol 
      | destruct t; 
        [ match goal with | H : Forall ParserCST _ |- _ => inversion H; subst; assumption end
        | apply ParserSeq; match goal with | H : Forall ParserCST _ |- _ => exact H end ]
      ]
  end;
  try apply ParserSymbol;
  try apply ParserString;
  try apply ParserInt;
  try apply ParserBool;
  try apply ParserTuple;
  try apply ParserList;
  try apply ParserSeq;
  try apply ParserBlock;
  try apply ParserComment;
  try assumption.

Lemma parse_loop_fuel_produces_ParserCST : forall fuel mode toks csts rest,
  parse_loop_fuel fuel mode toks = (csts, rest) ->
  Forall ParserCST csts.
Proof.
  induction fuel as [| fuel H_ind ]; intros mode toks csts rest H.
  - simpl in H. inversion H. apply Forall_nil.
  - simpl in H. destruct toks as [|tok toks']; inversion H; clear H; try apply Forall_nil.
    destruct tok.
    all: solve_parser_cst H_ind.
    Show.
Abort.
