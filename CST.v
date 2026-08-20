From Stdlib Require Import Strings.String.
From Stdlib Require Import List.
Import ListNotations.

(* A basic span to keep track of source positions *)
Record Span : Type := mkSpan {
  start_pos : nat;
  end_pos : nat
}.

(* The Universal Concrete Syntax Tree (CST) for Chester *)
Inductive CST : Type :=
  | Symbol : string -> Span -> CST
  | Tuple : list CST -> Span -> CST
  | ListLiteral : list CST -> Span -> CST
  | Block : list CST -> list CST -> Span -> CST
  | StringLiteral : string -> Span -> CST
  | IntegerLiteral : string -> Span -> CST
  | SeqOf : list CST -> Span -> CST
  | Error : string -> Span -> CST.

(* Example of an empty span *)
Definition empty_span := mkSpan 0 0.

(* Example: parsing `def main(): Unit = { println("hello") }` 
   This would be represented as a SeqOf containing the symbols and structures *)
Definition example_cst : CST :=
  SeqOf [
    Symbol "def"%string empty_span;
    Symbol "main"%string empty_span;
    Tuple [] empty_span;
    Symbol ":"%string empty_span;
    Symbol "Unit"%string empty_span;
    Symbol "="%string empty_span;
    Block [
      SeqOf [
        Symbol "println"%string empty_span;
        Tuple [StringLiteral "hello"%string empty_span] empty_span
      ] empty_span
    ] [] empty_span
  ] empty_span.
