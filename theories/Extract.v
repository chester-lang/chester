Require Import Chester.CST.
Require Import Chester.AST.
Require Import Chester.Tokenizer.
Require Import Chester.Parser.
Require Import Chester.CoreChecker.
Require Import Chester.Elaborator.
Require Import Chester.Backend.
Require Import Chester.TypeScriptAST.
Require Import Chester.GoAST.
Require Import Chester.Formatter.
Require Import Chester.TypeScriptInterop.
Require Import Chester.GoInterop.
Require Import Chester.Expander.

From Stdlib Require ExtrOcamlBasic.
From Stdlib Require ExtrOcamlString.
From Stdlib Require ExtrOcamlNatInt.

Extraction Language OCaml.
Set Extraction Output Directory "extraction".

(* Extract all the relevant functions to a file called 'Compiler.ml' *)
Extraction "Compiler.ml" 
  Chester.CST.CST 
  Chester.AST.AST 
  Chester.Parser.Token
  Chester.Tokenizer.tokenize 
  Chester.Parser.parse
  Chester.Expander.expand_cst_top 
  Chester.CoreChecker.infer_check 
  Chester.Elaborator.elaborate
  Chester.Elaborator.elaborate_top 
  Chester.Elaborator.zonk
  Chester.Elaborator.unify
  Chester.Elaborator.init_elab_state
  Chester.Formatter.format_cst
  Chester.Formatter.format_program
  Chester.Backend.emit_ts 
  Chester.Backend.emit_go
  Chester.Backend.emit_go_top
  Chester.TypeScriptInterop.ts_to_chester
  Chester.GoInterop.go_to_chester
  Chester.TypeScriptAST.stringify_ts_stmt
  Chester.GoAST.stringify_go_stmt.
