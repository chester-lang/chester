From Stdlib Require Import Strings.String.
From Stdlib Require Import List.
Import ListNotations.

Require Import Chester.AST.
Require Import Chester.GoAST.

(* 
  Golang FFI Interoperability
  Translates a Go AST node (representing an FFI signature) into a Chester AST type.
*)

Fixpoint go_to_chester (go_ast : GoAST) {struct go_ast} : AST :=
  match go_ast with
  | GoIdentifier name => 
      if String.eqb name "int" then AstRef "Int"
      else if String.eqb name "string" then AstRef "String"
      else if String.eqb name "bool" then AstRef "Bool"
      else AstRef name
  | GoFuncLiteral params ret =>
      (* Mock: assume all Go params are typed as 'Any' *)
      let ret_ty := go_to_chester ret in
      let fix build_pi (args : list string) : AST :=
        match args with
        | [] => ret_ty
        | arg :: rest => AstPi arg (AstRef "Any") (build_pi rest) []
        end
      in build_pi params
  | _ => AstRef "Any"
  end.
