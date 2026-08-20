From Stdlib Require Import Strings.String.
From Stdlib Require Import List.
Import ListNotations.

Require Import Chester.AST.
Require Import Chester.TypeScriptAST.

(* 
  TypeScript FFI Interoperability
  Translates a TypeScript AST node (representing an FFI signature) into a Chester AST type.
*)

Fixpoint ts_to_chester (ts : TypeScriptAST) {struct ts} : AST :=
  match ts with
  | TsIdentifier name => 
      if String.eqb name "number" then AstRef "Int"
      else if String.eqb name "string" then AstRef "String"
      else if String.eqb name "boolean" then AstRef "Bool"
      else AstRef name
  | TsArrow params ret =>
      (* Mock: assume all TS params are typed as 'Any' for now, since TsArrow only has strings in our current AST *)
      let ret_ty := ts_to_chester ret in
      let fix build_pi (args : list string) : AST :=
        match args with
        | [] => ret_ty
        | arg :: rest => AstPi arg (AstRef "Any") (build_pi rest) []
        end
      in build_pi params
  | _ => AstRef "Any"
  end.
