sed -i '' '117,119d' theories/CoreChecker.v
sed -i '' '/| AstError msg => TyErr msg/a\
  | AstTypeApp func args => TyOk (AstRef "TypeUniverse")\
' theories/CoreChecker.v
