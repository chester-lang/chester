sed -i '' '/let fix map_elabs (ls : list CST) : ElabM (list AST) :=/,/stmtsAst <- map_elabs stmts ;/c\
      let fix map_elabs (current_env : Env) (ls : list CST) : ElabM (list AST * Env) :=\
        match ls with\
        | [] => ret ([], current_env)\
        | x :: xs => \
            match x with\
            | LetCST name value _ _ =>\
                valueAst <- elaborate current_env value None ;\
                let new_env := (name, snd valueAst) :: current_env in\
                rest <- map_elabs new_env xs ;\
                ret (AstLet name (fst valueAst) :: fst rest, snd rest)\
            | DefCST name _ _ ret_ty _ _ =>\
                (* Mocking DefCST addition to env just for typing sake (use dummy type) *)\
                tyAst <- elaborate current_env ret_ty (Some TypeUniverse) ;\
                let new_env := (name, fst tyAst) :: current_env in\
                res <- elaborate current_env x None ;\
                rest <- map_elabs new_env xs ;\
                ret (fst res :: fst rest, snd rest)\
            | _ =>\
                res <- elaborate current_env x None ;\
                rest <- map_elabs current_env xs ;\
                ret (fst res :: fst rest, snd rest)\
            end\
        end\
      in\
      stmtsRes <- map_elabs env stmts ;\
      let stmtsAst := fst stmtsRes in\
      let final_env := snd stmtsRes in\
' theories/Elaborator.v
