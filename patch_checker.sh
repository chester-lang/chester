sed -i '' '/| AstApp a1 b1, AstApp a2 b2 => false/a\
  | AstTypeApp a1 b1, AstTypeApp a2 b2 => false\
' theories/CoreChecker.v
sed -i '' '/| AstApp func args =>/a\
  | AstTypeApp func args =>\
      TyOk (AstSymbol "TypeUniverse") (* Mock implementation for now *)\
' theories/CoreChecker.v
