sed -i '' '/| AppCST func args _ =>/a\
  | TypeAppCST func args _ =>\
      funcAst <- elaborate env func None ;\
      let fix check_args (as_ : list CST) : ElabM (list AST) :=\
        match as_ with\
        | [] => ret []\
        | a :: rest =>\
            aAst <- elaborate env a (Some (AstRef "TypeUniverse" empty_span)) ;\
            restAst <- check_args rest ;\
            ret (fst aAst :: restAst)\
        end\
      in\
      argsRes <- check_args args ;\
      ret (AstTypeApp (fst funcAst) argsRes, AstRef "TypeUniverse" empty_span)\
' theories/Elaborator.v
