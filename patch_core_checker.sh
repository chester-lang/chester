sed -i '' 's/AstLet n1 v1 b1, AstLet n2 v2 b2 => false/AstLet n1 v1, AstLet n2 v2 => false\
  | AstBlock _ _, AstBlock _ _ => false/' theories/CoreChecker.v

sed -i '' '/| AstLet name value body =>/,/| AstIf cond thenB elseB =>/c\
  | AstBlock stmts ret_expr =>\
      let fix check_stmts (current_env : Env) (ls : list AST) : TyResult Env :=\
        match ls with\
        | [] => TyOk current_env\
        | x :: xs =>\
            match x with\
            | AstLet name value =>\
                match infer_check current_env value None with\
                | TyOk valTy => check_stmts ((name, valTy) :: current_env) xs\
                | TyErr e => TyErr e\
                end\
            | _ =>\
                match infer_check current_env x None with\
                | TyOk _ => check_stmts current_env xs\
                | TyErr e => TyErr e\
                end\
            end\
        end\
      in\
      match check_stmts env stmts with\
      | TyOk final_env => infer_check final_env ret_expr expected\
      | TyErr e => TyErr e\
      end\
      \
  | AstLet name value => TyOk (AstSymbol "Unit" TypeUniverse)\
      \
  | AstIf cond thenB elseB =>\
' theories/CoreChecker.v
