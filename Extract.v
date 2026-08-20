Require Import Chester.CST.
Require Import Chester.AST.
Require Import Chester.Tokenizer.
Require Import Chester.Parser.
Require Import Chester.CoreChecker.
Require Import Chester.Elaborator.
Require Import Chester.Backend.
Require Import Chester.TypeScriptAST.
Require Import Chester.GoAST.
Require Import Chester.TypeScriptInterop.
Require Import Chester.GoInterop.

From Stdlib Require ExtrOcamlBasic.
From Stdlib Require ExtrOcamlString.
From Stdlib Require ExtrOcamlNatInt.

Extraction Language OCaml.

(* Extract all the relevant functions to a file called 'Compiler.ml' *)
Extraction "Compiler.ml" 
  Chester.CST.CST 
  Chester.AST.AST 
  Chester.Parser.Token
  Chester.Tokenizer.tokenize 
  Chester.Parser.parse_cst 
  Chester.CoreChecker.infer_check 
  Chester.Elaborator.elaborate 
  Chester.Elaborator.init_elab_state
  Chester.Backend.emit_ts 
  Chester.Backend.emit_go
  Chester.TypeScriptInterop.ts_to_chester
  Chester.GoInterop.go_to_chester
  Chester.TypeScriptAST.stringify_ts
  Chester.GoAST.stringify_go.
