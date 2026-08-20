sed -i '' 's/| _, _ => false (\* Simplified for demonstration \*)/| AstSpan _ a1, AstSpan _ a2 => eq_ast a1 a2\
  | AstSpan _ a1, a2 => eq_ast a1 a2\
  | a1, AstSpan _ a2 => eq_ast a1 a2\
  | _, _ => false/' theories/CoreChecker.v

sed -i '' '/| AstError msg => TyErr msg/c\
  | AstError msg => TyErr msg\
  | AstSpan span inner =>\
      (* Here we could store the span in the error monad if infer_check fails! *)\
      match infer_check env inner expected with\
      | TyOk t => TyOk t\
      | TyErr e => TyErr (e) (* In the future: append span info to e *)\
      end\
' theories/CoreChecker.v
