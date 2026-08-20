From Stdlib Require Import Strings.String.
From Stdlib Require Import List.
Import ListNotations.

(* Keeps track of both Unicode scalar characters and UTF-16 code units.
   Important for interop with IDEs and environments that use UTF-16 strings (e.g. JS/TS). *)
Record WithUTF16 : Type := mkWithUTF16 {
  unicode : nat;
  utf16 : nat
}.

(* Represents a position in a source file *)
Record Pos : Type := mkPos {
  index : WithUTF16;
  line : nat;
  column : WithUTF16
}.

(* Represents a range within a file *)
Record SpanInFile : Type := mkSpanInFile {
  start_pos : Pos;
  end_pos : Pos
}.

(* The final Span keeps track of the file name and the range within it *)
Record Span : Type := mkSpan {
  file_name : string;
  range : SpanInFile;
  context : list nat
}.

Inductive PatternCST : Type :=
  | PatWildcardCST : Span -> PatternCST
  | PatVarCST : string -> Span -> PatternCST
  | PatConstructorCST : string -> list string -> Span -> PatternCST. (* Constructor name and variable bindings *)

(* The Universal Concrete Syntax Tree (CST) for Chester *)
Inductive CST : Type :=
  | Symbol : string -> Span -> CST
  | Tuple : list CST -> Span -> CST
  | ListLiteral : list CST -> Span -> CST
  | Block : list CST -> CST -> Span -> CST
  | StringLiteral : string -> Span -> CST
  | IntegerLiteral : string -> Span -> CST
  | BoolLiteral : bool -> Span -> CST
  | SeqOf : list CST -> Span -> CST
  | CommentCST : string -> Span -> CST
  (* New nodes for stdlib/bootstrap *)
  | LetCST : string -> CST -> CST -> Span -> CST (* name, value, body/next_stmt *)
  | IfCST : CST -> CST -> CST -> Span -> CST (* cond, then_branch, else_branch *)
  | DefCST : string -> list string -> list (string * CST) -> CST -> CST -> Span -> CST (* name, type_params, params, ret_ty, body *)
  | LamCST : string -> option CST -> CST -> Span -> CST (* arg_name, opt_arg_ty, body *)
  | AppCST : CST -> list CST -> Span -> CST (* func, args *)
  | TypeAppCST : CST -> list CST -> Span -> CST
  | EnumCST : string -> list string -> list CST -> Span -> CST (* name, type_params, variants *)
  | MatchCST : CST -> list (PatternCST * CST) -> Span -> CST (* expr, cases *)
  | RecordCST : string -> list string -> list CST -> Span -> CST (* name, type_params, fields *)
  | EffectCST : string -> list string -> list CST -> Span -> CST
  | DoCST : CST -> list CST -> Span -> CST
  | HandleCST : CST -> string -> list CST -> Span -> CST
  | FieldAccessCST : CST -> string -> Span -> CST (* expr, field_name *)
  | MacroDefCST : string -> list (PatternCST * CST) -> Span -> CST (* name, cases *)
  | Error : string -> Span -> CST.

Definition zero_utf16 := mkWithUTF16 0 0.
Definition zero_pos := mkPos (mkWithUTF16 0 0) 1 (mkWithUTF16 0 0).
Definition empty_span := mkSpan "" (mkSpanInFile zero_pos zero_pos) [].

(* Combines two spans (assumes they are from the same file) *)
Definition combine_span (s1 s2 : Span) : Span :=
  mkSpan (file_name s1) (mkSpanInFile (start_pos (range s1)) (end_pos (range s2))) (context s1).

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
    ] (Tuple [] empty_span) empty_span
  ] empty_span.

Definition get_span (c : CST) : Span :=
  match c with
  | Symbol _ span => span
  | Tuple _ span => span
  | ListLiteral _ span => span
  | Block _ _ span => span
  | StringLiteral _ span => span
  | IntegerLiteral _ span => span
  | BoolLiteral _ span => span
  | SeqOf _ span => span
  | LetCST _ _ _ span => span
  | IfCST _ _ _ span => span
  | DefCST _ _ _ _ _ span => span
  | EnumCST _ _ _ span => span
  | MatchCST _ _ span => span
  | TypeAppCST _ _ span => span
  | RecordCST _ _ _ span => span
  | EffectCST _ _ _ span => span
  | DoCST _ _ span => span
  | HandleCST _ _ _ span => span
  | FieldAccessCST _ _ span => span
  | CommentCST _ span => span
  | Error _ span => span
  | _ => empty_span
  end.
